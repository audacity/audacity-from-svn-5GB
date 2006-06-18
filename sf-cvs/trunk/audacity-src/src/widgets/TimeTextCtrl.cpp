/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTextCtrl.cpp

  Dominic Mazzoni

  *
  * TimeTextCtrl Format String Documentation
  *

  The TimeTextCtrl makes use of a format string to specify the
  exact way that a single time value is split into several fields,
  such as the hh:mm:ss format.  The advantage of this format string
  is that it is very small and compact, but human-readable and
  somewhat intuitive, so that it's easy to add new time layouts
  in the future.  It's also designed to make it easier to add
  i18n support, since the way that times are displayed in different
  languages could conceivably vary a lot.

  The time to be formatted is expressed in seconds, so the format
  string specifies the relationship of each field to the number of
  seconds.

  Let's start by considering an example: here's the format string
  that prints an integer number of seconds in the hour minute
  second h:m:s format:

    *:60:60

  The "*" is a wildcard, saying that the leftmost field can contain
  numbers of arbitrary magnitude.  The next character, ':', since it
  is not a digit or a wildcard, is interpreted as a delimiter, and
  will be displayed between those fields.  The next number, 60,
  indicates that the range of the next field (minutes) is 60.
  Then there's another ':' delimiter, and finally the last field
  (seconds) is 60.  So, if you give it a number like 3758 (note
  format it as:

    3758 seconds, "#:60:60" -> "1:2:38"

  Note that 3758 = 1*60*60 + 2*60 + 38.

  When TimeTextCtrl formats an integer, you can think of its process
  as working from right to left.  Given the value "3758", it fills
  in the seconds by dividing by 60, sticking the remainder in the
  seconds field and then passing the quotient to the next field to
  the left.

  In order to format a field with leading zeros, simply add a leading
  zero to that field, like this:

    3758 seconds, "#:060:060" -> "1:02:38"

  In order to format fractions, simply include a field delimiter
  ending with a decimal point.  If the delimiter is simply '.' with
  nothing else, then the '.' is actually displayed.  Otherwise the
  '.' is dropped, and the other characters in the delimiter are
  displayed instead.

  Here's how we'd display hours, minutes, and seconds with three
  decimal places after the seconds:

    3758.5 seconds, "#:060:060.01000" -> "1:02:38.500"

  Similarly, here's how we'd display the fractional part of
  seconds as film frames (24 per second) instead of milliseconds:

    3758.5 seconds, "#:060:060 and .24 frames" -> "1:02:38 and 12 frames"

  Note that the decimal '.' is associated with the delimeter, not
  with the 24.

  Additionally, the special character '#' can be used in place of a number
  to represent the current sample rate.  Use '0#' to add leading
  zeros to that field.  For example:

    3758.5 seconds, "#:060:060+.#samples" -> "1:02:38+22050samples"

  Finally, there is a rule that allows you to change the units into
  something other than seconds.  To do this, put a "|" character on
  the far right, followed by a number specifying the scaling factor.
  As an exception to previous rules, decimal points are allowed
  in the final scaling factor - the period is not interpreted as it
  would be before the "|" character.  (This is fine, because all
  previous fields must be integers to make sense.)  Anyway, if you
  include a scaling factor after a "|", the time will be
  multiplied by this factor before it is formatted.  For example, to
  express the current time in NTSC frames (~29.97 fps), you could
  use the following formatting:

    3758.5 seconds, "*.01000 frames|29.97002997" -> "112642.358 frames"

  Summary of format string rules:

  * The characters '0-9', '*', and '#' are numeric.  Any sequence of
    these characters is treated as defining a new field by specifying
    its range.  All other characters become delimiters between fields.
    (The one exception is that '.' is treated as numeric after the
    optional '|'.)
  * A field with a range of '*', which only makes sense as the
    leftmost field, means the field should display as large a number
    as necessary.
  * The character '#' represents the current sample rate.
  * If a field specifier beings with a leading zero, it will be formatted
    with leading zeros, too - enough to display the maximum value
    that field can display.  So the number 7 in a field specified
    as '01000' would be formatted as '007'.  Bond.  James Bond.
  * Any non-numeric characters before the first field are treated
    as a prefix, and will be displayed to the left of the first field.
  * A delimiter ending in '.' is treated specially.  All fields after
    this delimeter are fractional fields, after the decimal point.
  * The '|' character is treated as a special delimiter.  The number
    to the right of this character (which is allowed to contain a
    decimal point) is treated as a scaling factor.  The time is
    multiplied by this factor before multiplying.

**********************************************************************/

#include "../Audacity.h"

#include "TimeTextCtrl.h"

#include <math.h>

#include <wx/dcmemory.h>
#include <wx/font.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/tooltip.h>

BEGIN_EVENT_TABLE(TimeTextCtrl, wxPanel)
   EVT_PAINT(TimeTextCtrl::OnPaint)
   EVT_MOUSE_EVENTS(TimeTextCtrl::OnMouse)
   EVT_CHAR(TimeTextCtrl::OnChar)
   EVT_SET_FOCUS(TimeTextCtrl::OnFocus)
   EVT_KILL_FOCUS(TimeTextCtrl::OnFocus)
END_EVENT_TABLE()

IMPLEMENT_CLASS(TimeTextCtrl, wxPanel)

struct BuiltinFormatString {
   const wxChar *name;
   const wxChar *formatStr;
};

BuiltinFormatString BuiltinFormatStrings[] =
   {{_("seconds"),
     _("01000,01000 seconds")},
    {_("hh:mm:ss"),
     _("099 h 060 m 060 s")},
    {_("hh:mm:ss + milliseconds"),
     _("099 h 060 m 060.01000 s")},
    {_("hh:mm:ss + samples"),
     _("099 h 060 m 060 s+.# samples")},
    {_("samples"),
     _("01000,01000,01000 samples|#")},
    {_("hh:mm:ss + film frames (24 fps)"),
     _("099h060m060s+.24 frames")},
    {_("film frames (24 fps)"),
     _("01000,01000 frames |24")},
    {_("hh:mm:ss + NTSC drop frames (29.97 fps)"),
     _("099h060m060s+.2997 frames")},
    {_("NTSC drop frames (29.97 fps)"),
     _("01000,01000 frames|29.97002997")},
    {_("hh:mm:ss + NTSC non-drop frames (30 fps)"),
     _("099h060m060s+.030 frames")},
    {_("NTSC non-drop frames (30 fps)"),
     _("01000,01000 frames|30")},
    {_("hh:mm:ss + PAL frames (25 fps)"),
     _("099h060m060s+.25 frames")},
    {_("PAL frames (25 fps)"),
     _("01000,01000 frames|25")},
    {_("hh:mm:ss + CDDA frames (75 fps)"),
     _("099h060m060s+.75 frames")},
    {_("CDDA frames (75 fps)"),
     _("01000,01000 frames |75")}};

const int kNumBuiltinFormatStrings =
   sizeof(BuiltinFormatStrings) /
   sizeof(BuiltinFormatStrings[0]);

class TimeField {
public:
   TimeField(bool _frac, int _base, int _range, bool _zeropad)
     { frac = _frac; base = _base; range = _range;
       zeropad = _zeropad; digits = 0; }
   bool frac; // is it a fractional field
   int base;  // divide by this (multiply, after decimal point)
   int range; // then take modulo this
   int digits;
   int pos;   // Index of this field in the ValueString
   int fieldX; // x-position of the field on-screen
   int labelX; // x-position of the label on-screen
   bool zeropad;
   wxString label;
   wxString formatStr;
   wxString str;
   void CreateDigitFormatStr() {
      if (range > 1)
         digits = (int)ceil(log10(range-1.0));
      else
         digits = 5; // hack: default
      if (zeropad && range>1)
         formatStr.Printf(wxT("%%0%dd"), digits); // ex. "%03d" if digits is 3
      else {
         //formatStr = wxT("%d");/////////////old code
         formatStr.Printf(wxT("%%0%dd"), digits);
      }
   }
};

class DigitInfo {
public:
   DigitInfo(int _field, int _index, int _pos, wxRect _box)
     { field = _field; index = _index; pos = _pos; digitBox = _box; }
   int field; // Which field
   int index; // Index of this digit within the field
   int pos;   // Position in the ValueString
   wxRect digitBox;
};

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY(TimeFieldArray);
WX_DEFINE_OBJARRAY(DigitInfoArray);

TimeTextCtrl::TimeTextCtrl(wxWindow *parent,
                           wxWindowID id,
                           wxString formatString,
                           double timeValue,
                           double sampleRate,
                           const wxPoint &pos,
                           const wxSize &size):
   wxPanel(parent, id, pos, size, wxWANTS_CHARS),
   mTimeValue(timeValue),
   mSampleRate(sampleRate),
   mFormatString(formatString),
   mBackgroundBitmap(NULL),
   mDigitFont(NULL),
   mLabelFont(NULL),
   mFocusedDigit(0),
   mLastField(-1)
{
   mDigitBoxW = 10;
   mDigitBoxH = 16;

   ParseFormatString();
   Layout();
   Fit();
   ValueToControls();

#if wxUSE_ACCESSIBILITY
   SetName( wxT("") );
   SetAccessible(new TimeTextCtrlAx(this));
#endif
}

TimeTextCtrl::~TimeTextCtrl()
{
   if (mBackgroundBitmap)
      delete mBackgroundBitmap;
   if (mDigitFont)
      delete mDigitFont;
   if (mLabelFont)
      delete mLabelFont;
}

void TimeTextCtrl::SetFormatString(wxString formatString)
{
   mFormatString = formatString;
   ParseFormatString();
   Layout();
   Fit();
   ValueToControls();
}

void TimeTextCtrl::SetSampleRate(double sampleRate)
{
   mSampleRate = sampleRate;
   ParseFormatString();
   Layout();
   Fit();
   ValueToControls();
}

void TimeTextCtrl::SetTimeValue(double newTime)
{
   mTimeValue = newTime;
   ValueToControls();   
}

const double TimeTextCtrl::GetTimeValue()
{
   ControlsToValue();
   return mTimeValue;
}

int TimeTextCtrl::GetNumBuiltins()
{
   return kNumBuiltinFormatStrings;
}

wxString TimeTextCtrl::GetBuiltinName(int index)
{
   if (index >= 0 && index < kNumBuiltinFormatStrings)
      return BuiltinFormatStrings[index].name;
   else
      return wxT("");
}

wxString TimeTextCtrl::GetBuiltinFormat(int index)
{
   if (index >= 0 && index < kNumBuiltinFormatStrings)
      return BuiltinFormatStrings[index].formatStr;
   else
      return wxT("");
}

void TimeTextCtrl::ParseFormatString()
{
   mPrefix = wxT("");
   mFields.Clear();
   mScalingFactor = 1.0;

   const wxString format = mFormatString;
   bool inFrac = false;
   int fracMult = 1;
   int numWholeFields = 0;
   int numFracFields = 0;
   wxString numStr;
   wxString delimStr;
   unsigned int i;

   for(i=0; i<format.Length(); i++) {
      bool handleDelim = false;
      bool handleNum = false;

      if (format[i] == '|') {
         wxString remainder = format.Right(format.Length() - i - 1);

         if (remainder == wxT("#"))
            mScalingFactor = mSampleRate;
         else
            remainder.ToDouble(&mScalingFactor);
         i = format.Length()-1; // force break out of loop
         if (delimStr != wxT(""))
            handleDelim = true;
         if (numStr != wxT(""))
            handleNum = true;
      }
      else if ((format[i] >= '0' && format[i] <='9') ||
          format[i] == wxT('*') || format[i] == wxT('#')) {
         numStr += format[i];
         if (delimStr != wxT(""))
            handleDelim = true;
      }
      else {
         delimStr += format[i];
         if (numStr != wxT(""))
            handleNum = true;
      }

      if (i == format.Length() - 1) {
         if (numStr != wxT(""))
            handleNum = true;
         if (delimStr != wxT(""))
            handleDelim = true;
      }

      if (handleNum) {
         bool zeropad = false;
         long range = 0;

         if (numStr.Right(1) == wxT("#"))
            range = (long int)mSampleRate;
         else if (numStr.Right(1) != wxT("*")) {
            numStr.ToLong(&range);
         }
         if (numStr.GetChar(0)=='0' && numStr.Length()>1)
            zeropad = true;

         // Hack: always zeropad
         zeropad = true;
         
         if (inFrac) {
            int base = fracMult * range;
            mFields.Add(TimeField(inFrac, base, range, zeropad));
            fracMult *= range;
            numFracFields++;
         }
         else {
            unsigned int j;
            for(j=0; j<mFields.GetCount(); j++)
               mFields[j].base *= range;
            mFields.Add(TimeField(inFrac, 1, range, zeropad));
            numWholeFields++;
         }
         numStr = wxT("");
      }

      if (handleDelim) {
         bool goToFrac = false;

         if (!inFrac && delimStr[delimStr.Length()-1]=='.') {
            goToFrac = true;
            if (delimStr.Length() > 1)
               delimStr = delimStr.BeforeLast('.');
         }

         if (inFrac) {
            if (numFracFields == 0) {
               // Should never happen
               return;
            }
            if (handleNum && numFracFields > 1)
               mFields[mFields.GetCount()-2].label = delimStr;
            else
               mFields[mFields.GetCount()-1].label = delimStr;
         }
         else {
            if (numWholeFields == 0)
               mPrefix = delimStr;
            else {
               mFields[numWholeFields-1].label = delimStr;
            }
         }

         if (goToFrac)
            inFrac = true;
         delimStr = wxT("");
      }
   }

   for(i=0; i<mFields.GetCount(); i++)
      mFields[i].CreateDigitFormatStr();

   int pos = 0;
   int j;
   mValueMask = wxT("");
   mValueTemplate = wxT("");

   mValueTemplate += mPrefix;
   for(j=0; j<(int)mPrefix.Length(); j++)
      mValueMask += wxT(".");
   pos += mPrefix.Length();

   for(i=0; i<mFields.GetCount(); i++) {
      mFields[i].pos = pos;

      pos += mFields[i].digits;
      for(j=0; j<mFields[i].digits; j++) {
         mValueTemplate += wxT("0");
         mValueMask += wxT("0");
      }

      pos += mFields[i].label.Length();
      mValueTemplate += mFields[i].label;
      for(j=0; j<(int)mFields[i].label.Length(); j++)
         mValueMask += wxT(".");
   }
}

void TimeTextCtrl::PrintDebugInfo()
{
   unsigned int i;

   printf("%s", (const char *)mPrefix.mb_str());

   for(i=0; i<mFields.GetCount(); i++) {
      if (mFields[i].frac) {
         printf("(t * %d) %% %d '%s' ",
                mFields[i].base,
                mFields[i].range,
                (const char *)mFields[i].label.mb_str());

      }
      else {
         printf("(t / %d) %% %d '%s' ",
                mFields[i].base,
                mFields[i].range,
                (const char *)mFields[i].label.mb_str());
      }
   }

   printf("\n");
}

wxString TimeTextCtrl::GetTimeString()
{
   ValueToControls();

   return mValueString;
}

bool TimeTextCtrl::Layout()
{
   unsigned int i, j;
   int x, pos;

   wxMemoryDC memDC;
   if (mBackgroundBitmap) {
      delete mBackgroundBitmap;
      mBackgroundBitmap = NULL;
   }
   // Placeholder bitmap so the memDC has something to reference
   mBackgroundBitmap = new wxBitmap(1, 1);
   memDC.SelectObject(*mBackgroundBitmap);

   mDigits.Clear();

   mBorderLeft = 1;
   mBorderTop = 1;
   mBorderRight = 1;
   mBorderBottom = 1;

   int fontSize = 4;
   wxCoord strW, strH;
   wxString exampleText = wxT("0");

   // Keep making the font bigger until it's too big, then subtract one.
   memDC.SetFont(wxFont(fontSize, wxFIXED, wxNORMAL, wxNORMAL));
   memDC.GetTextExtent(exampleText, &strW, &strH);
   while(strW <= mDigitBoxW && strH <= mDigitBoxH) {
      fontSize++;
      memDC.SetFont(wxFont(fontSize, wxFIXED, wxNORMAL, wxNORMAL));
      memDC.GetTextExtent(exampleText, &strW, &strH);
   }
   fontSize--;

   if (mDigitFont)
      delete mDigitFont
   mDigitFont = new wxFont(fontSize, wxFIXED, wxNORMAL, wxNORMAL);
   memDC.SetFont(*mDigitFont);
   memDC.GetTextExtent(exampleText, &strW, &strH);
   mDigitW = strW;
   mDigitH = strH;

   // The label font should be a little smaller
   fontSize--;
   if (mLabelFont)
      delete mLabelFont;
   mLabelFont = new wxFont(fontSize, wxFIXED, wxNORMAL, wxNORMAL);

   // Figure out the x-position of each field and label in the box
   x = mBorderLeft;
   pos = 0;

   memDC.SetFont(*mLabelFont);
   memDC.GetTextExtent(mPrefix, &strW, &strH);
   x += strW;
   pos += mPrefix.Length();

   for(i=0; i<mFields.GetCount(); i++) {
      mFields[i].fieldX = x;
      for(j=0; j<(unsigned int)mFields[i].digits; j++) {
         mDigits.Add(DigitInfo(i, j, pos, wxRect(x, mBorderTop,
                                                 mDigitBoxW, mDigitBoxH)));
         x += mDigitBoxW;
         pos++;
      }

      mFields[i].labelX = x;
      memDC.GetTextExtent(mFields[i].label, &strW, &strH);
      pos += mFields[i].label.Length();
      x += strW;
   }

   mWidth = x + mBorderRight;
   mHeight = mDigitBoxH + mBorderTop + mBorderBottom;

   // Draw the background bitmap - it contains black boxes where
   // all of the digits go and all of the other text

   delete mBackgroundBitmap; // Delete placeholder
   mBackgroundBitmap = new wxBitmap(mWidth, mHeight);
   memDC.SelectObject(*mBackgroundBitmap);

   memDC.SetBrush(*wxLIGHT_GREY_BRUSH);
   memDC.SetPen(*wxTRANSPARENT_PEN);
   memDC.DrawRectangle(0, 0, mWidth, mHeight);

   int numberBottom = mBorderTop + (mDigitBoxH - mDigitH)/2 + mDigitH;

   memDC.GetTextExtent(wxT("0"), &strW, &strH);
   int labelTop = numberBottom - strH;

   memDC.SetTextForeground(*wxBLACK);
   memDC.SetTextBackground(*wxLIGHT_GREY);
   memDC.DrawText(mPrefix, mBorderLeft, labelTop);

   memDC.SetBrush(*wxBLACK_BRUSH);
   for(i=0; i<mDigits.GetCount(); i++)
      memDC.DrawRectangle(mDigits[i].digitBox);

   for(i=0; i<mFields.GetCount(); i++)
      memDC.DrawText(mFields[i].label,
                     mFields[i].labelX, labelTop);

   return true;
}

void TimeTextCtrl::Fit()
{
   wxSize size(mWidth, mHeight);
   SetSizeHints(size, size);
   SetSize(size);
}

void TimeTextCtrl::OnPaint(wxPaintEvent &event)
{
   wxPaintDC dc(this);

   dc.DrawBitmap(*mBackgroundBitmap, 0, 0);

   if (FindFocus()==this) {
      dc.SetPen(*wxGREEN_PEN);
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      dc.DrawRectangle(0, 0, mWidth, mHeight);
   }

   dc.SetFont(*mDigitFont);
   dc.SetTextForeground(*wxGREEN);
   dc.SetTextBackground(*wxBLACK);

   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(*wxWHITE_BRUSH);

   int i;
   for(i=0; i<(int)mDigits.GetCount(); i++) {
      wxRect box = mDigits[i].digitBox;
      if (FindFocus()==this && mFocusedDigit == i) {
         dc.DrawRectangle(box);
         dc.SetTextForeground(*wxBLACK);
         dc.SetTextBackground(*wxWHITE);
      }
      int pos = mDigits[i].pos;
      wxString digit = mValueString.Mid(pos, 1);
      int x = box.x + (mDigitBoxW - mDigitW)/2;
      int y = box.y + (mDigitBoxH - mDigitH)/2;
      dc.DrawText(digit, x, y);
      if (FindFocus()==this && mFocusedDigit == i) {
         dc.SetTextForeground(*wxGREEN);
         dc.SetTextBackground(*wxBLACK);
      }
  }
}

void TimeTextCtrl::OnMouse(wxMouseEvent &event)
{
   if (event.ButtonDown()) {
      SetFocus();

      int bestDist = 9999;
      unsigned int i;

      mFocusedDigit = 0;
      for(i=0; i<mDigits.GetCount(); i++) {
         int dist = abs(event.m_x - (mDigits[i].digitBox.x +
                                     mDigits[i].digitBox.width/2));
         if (dist < bestDist) {
            mFocusedDigit = i;
            bestDist = dist;
         }
      }

      Refresh(false);
   }
}

void TimeTextCtrl::OnFocus(wxFocusEvent &event)
{
   Refresh(false);
}

void TimeTextCtrl::SetFieldFocus(int digit)
{
#if wxUSE_ACCESSIBILITY
   if( mLastField != -1 )
   {
      GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_SELECTIONREMOVE,
                   this,
                   wxOBJID_CLIENT,
                   mLastField );
   }

   mLastField = mDigits[mFocusedDigit].field + 1;

   GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_FOCUS,
                this,
                wxOBJID_CLIENT,
                mLastField );

   GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_SELECTION,
                this,
                wxOBJID_CLIENT,
                mLastField );
#endif
}
void TimeTextCtrl::OnChar(wxKeyEvent &event)
{
   int keyCode = event.GetKeyCode();
   int digit = mFocusedDigit;

   if (mFocusedDigit < 0)
      mFocusedDigit = 0;
   if (mFocusedDigit >= (int)mDigits.GetCount())
      mFocusedDigit = mDigits.GetCount()-1;

   if (keyCode >= '0' && keyCode <= '9') {
      mValueString[mDigits[mFocusedDigit].pos] = wxChar(keyCode);
      ControlsToValue();
      ValueToControls();
      mFocusedDigit = (mFocusedDigit+1)%(mDigits.GetCount());
      wxCommandEvent event(wxEVT_COMMAND_TEXT_UPDATED, GetId());
      event.SetEventObject(this);
      GetEventHandler()->ProcessEvent(event);
#if wxUSE_ACCESSIBILITY
      GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_VALUECHANGE,
                                    this,
                                    wxOBJID_CLIENT,
                                    mDigits[ mFocusedDigit ].field + 1 );
#endif
   }

   else if (keyCode == WXK_BACK) {
      // Moves left, replaces that char with '0', stays there...
      mFocusedDigit--;
      mFocusedDigit += mDigits.GetCount();
      mFocusedDigit %= mDigits.GetCount();
      mValueString[mDigits[mFocusedDigit].pos] = '0';
      ControlsToValue();
      ValueToControls();
      wxCommandEvent event(wxEVT_COMMAND_TEXT_UPDATED, GetId());
      event.SetEventObject(this);
      GetEventHandler()->ProcessEvent(event);
   }

   else if (keyCode == WXK_LEFT) {
      mFocusedDigit--;
      mFocusedDigit += mDigits.GetCount();
      mFocusedDigit %= mDigits.GetCount();
      Refresh();
   }

   else if (keyCode == WXK_RIGHT) {
      mFocusedDigit++;
      mFocusedDigit %= mDigits.GetCount();
      Refresh();
   }

   else if (keyCode == WXK_UP) {
      for(unsigned int i=0; i<mFields.GetCount(); i++) {
         if( (mDigits[mFocusedDigit].pos>=mFields[i].pos) && (mDigits[mFocusedDigit].pos<mFields[i].pos+mFields[i].digits)) {   //it's this field
            ControlsToValue();
            mTimeValue *= mScalingFactor;
            if (mFields[i].frac)
               mTimeValue += pow(10.,mFields[i].digits-(mDigits[mFocusedDigit].pos-mFields[i].pos)-1)/(double)mFields[i].base;
            else
               mTimeValue += pow(10.,mFields[i].digits-(mDigits[mFocusedDigit].pos-mFields[i].pos)-1)*(double)mFields[i].base;
            if(mTimeValue<0.)
               mTimeValue=0.;
            mTimeValue /= mScalingFactor;
            ValueToControls();
            break;
         }
      }
      wxCommandEvent event(wxEVT_COMMAND_TEXT_UPDATED, GetId());
      event.SetEventObject(this);
      GetEventHandler()->ProcessEvent(event);
#if wxUSE_ACCESSIBILITY
      GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_VALUECHANGE,
                                    this,
                                    wxOBJID_CLIENT,
                                    mDigits[ mFocusedDigit ].field + 1 );
#endif
   }

   else if (keyCode == WXK_DOWN) {
      for(unsigned int i=0; i<mFields.GetCount(); i++) {
         if( (mDigits[mFocusedDigit].pos>=mFields[i].pos) && (mDigits[mFocusedDigit].pos<mFields[i].pos+mFields[i].digits)) {   //it's this field
            ControlsToValue();
            mTimeValue *= mScalingFactor;
            if (mFields[i].frac)
               mTimeValue -= pow(10.,mFields[i].digits-(mDigits[mFocusedDigit].pos-mFields[i].pos)-1)/(double)mFields[i].base;
            else
               mTimeValue -= pow(10.,mFields[i].digits-(mDigits[mFocusedDigit].pos-mFields[i].pos)-1)*(double)mFields[i].base;
            if(mTimeValue<0.)
               mTimeValue=0.;
            mTimeValue /= mScalingFactor;
            ValueToControls();
            break;
         }
      }
      wxCommandEvent event(wxEVT_COMMAND_TEXT_UPDATED, GetId());
      event.SetEventObject(this);
      GetEventHandler()->ProcessEvent(event);
#if wxUSE_ACCESSIBILITY
      GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_VALUECHANGE,
                                    this,
                                    wxOBJID_CLIENT,
                                    mDigits[ mFocusedDigit ].field + 1 );
#endif
   }

   else if (keyCode == WXK_TAB) {
      wxWindow *parent = GetParent();
      wxNavigationKeyEvent nevent;
      nevent.SetWindowChange( event.ControlDown() );
      nevent.SetDirection( !event.ShiftDown() );
      nevent.SetEventObject( parent );
      nevent.SetCurrentFocus( parent );
      GetParent()->ProcessEvent( nevent );
   }
   else {
      event.Skip();
      return;
   }

   if (digit != mFocusedDigit) {
      SetFieldFocus( mFocusedDigit );
   }

   event.Skip(false);
}

void TimeTextCtrl::ValueToControls()
{
   double theValue = mTimeValue * mScalingFactor+.00000001;
   int t_int = int(theValue);
   double t_frac = (theValue - t_int);
   unsigned int i;

   mValueString = mPrefix;

   for(i=0; i<mFields.GetCount(); i++) {
      int value;
      if (mFields[i].frac) {
         value = (int)(t_frac * mFields[i].base);
         if (mFields[i].range > 0)
            value = value % mFields[i].range;
      }
      else {
         value = (t_int / mFields[i].base);
         if (mFields[i].range > 0)
            value = value % mFields[i].range;
      }
      wxString field = wxString::Format(mFields[i].formatStr, value);
      mValueString += field;
      mValueString += mFields[i].label;
   }

   Refresh(false);
}

void TimeTextCtrl::ControlsToValue()
{
   unsigned int i;
   double t = 0.0;

   for(i=0; i<mFields.GetCount(); i++) {
      long val;
      mFields[i].str = mValueString.Mid(mFields[i].pos,
                                        mFields[i].digits);
      mFields[i].str.ToLong(&val);
      if (mFields[i].frac)
         t += (val / (double)mFields[i].base);
      else
         t += (val * (double)mFields[i].base);
   }

   t /= mScalingFactor;

   mTimeValue = t;
}

#if wxUSE_ACCESSIBILITY

TimeTextCtrlAx::TimeTextCtrlAx( wxWindow *window ):
   wxWindowAccessible( window )
{
   mCtrl = wxDynamicCast( window, TimeTextCtrl );
}

TimeTextCtrlAx::~TimeTextCtrlAx()
{
}

// Performs the default action. childId is 0 (the action for this object)
// or > 0 (the action for a child).
// Return wxACC_NOT_SUPPORTED if there is no default action for this
// window (e.g. an edit control).
wxAccStatus TimeTextCtrlAx::DoDefaultAction(int childId)
{
   return wxACC_NOT_SUPPORTED;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus TimeTextCtrlAx::GetChild( int childId, wxAccessible** child )
{
   if( childId == wxACC_SELF )
   {
      *child = this;
   }
   else
   {
      *child = NULL;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus TimeTextCtrlAx::GetChildCount(int* childCount)
{
   *childCount = mCtrl->mFields.GetCount();

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for
// a child).  Return wxACC_OK even if there is no action. actionName
// is the action, or the empty string if there is no action.  The
// retrieved string describes the action that is performed on an
// object, not what the object does as a result. For example, a
// toolbar button that prints a document has a default action of
// "Press" rather than "Prints the current document."
wxAccStatus TimeTextCtrlAx::GetDefaultAction(int childId, wxString* actionName)
{
   *actionName = wxT("");

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus TimeTextCtrlAx::GetDescription( int childId, wxString *description )
{
   description->Clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus TimeTextCtrlAx::GetFocus(int* childId, wxAccessible** child)
{
   *childId = mCtrl->mDigits[mCtrl->mFocusedDigit].field + 1;
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus TimeTextCtrlAx::GetHelpText( int childId, wxString *helpText )
{
#if wxUSE_TOOLTIPS // Not available in wxX11
   wxToolTip *pTip = mCtrl->GetToolTip();
   if( pTip )
   {
      *helpText = pTip->GetTip();
   }

   return wxACC_OK;
#else
   helpText->Clear();

   return wxACC_NOT_SUPPORTED;
#endif
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus TimeTextCtrlAx::GetKeyboardShortcut( int childId, wxString *shortcut )
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus TimeTextCtrlAx::GetLocation( wxRect& rect, int elementId )
{
   if( elementId == wxACC_SELF )
   {
      rect = mCtrl->GetRect();
      rect.SetPosition( mCtrl->GetParent()->ClientToScreen( rect.GetPosition() ) );
   }
   else
   {
      rect = mCtrl->mDigits[elementId - 1].digitBox;
      rect.SetPosition( mCtrl->ClientToScreen( rect.GetPosition() ) );
   }

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus TimeTextCtrlAx::GetName(int childId, wxString* name)
{
   if( childId == wxACC_SELF )
   {
      *name = mCtrl->GetName();
      if( name->IsEmpty() )
      {
         *name = mCtrl->GetLabel();
      }
   }
   else
   {
      *name = mCtrl->mFields[childId - 1].label;
   }

   if( name->IsEmpty() )
   {
      *name = _("Time Control");
   }

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus TimeTextCtrlAx::GetRole(int childId, wxAccRole* role)
{
   if( childId == wxACC_SELF )
   {
      *role = wxROLE_SYSTEM_STATICTEXT;
   }
   else
   {
      *role = wxROLE_SYSTEM_SPINBUTTON;
   }

   return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus TimeTextCtrlAx::GetSelections( wxVariant *selections )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus TimeTextCtrlAx::GetState(int childId, long* state)
{
   *state = wxACC_STATE_SYSTEM_FOCUSABLE;
   *state |= ( mCtrl == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0 );

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus TimeTextCtrlAx::GetValue(int childId, wxString* strValue)
{
   if( childId == wxACC_SELF )
   {
      *strValue = mCtrl->GetTimeString();
   }
   else
   {
      mCtrl->ControlsToValue();
      *strValue = mCtrl->mFields[childId - 1].str;
   }

   return wxACC_OK;
}

#endif


// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// 



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

#include "TimeTextCtrl.h"

#include <math.h>

#include <wx/dcmemory.h>
#include <wx/font.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

// static
int TimeTextCtrl::sTextHeight = 0;
int TimeTextCtrl::sTextWidth[11];

enum {
   AnyTextCtrlID = 2700
};

BEGIN_EVENT_TABLE(TimeTextCtrl, wxPanel)
   EVT_TEXT(AnyTextCtrlID, TimeTextCtrl::OnText)
END_EVENT_TABLE()

struct BuiltinFormatString {
   const wxChar *name;
   const wxChar *formatStr;
};

const int kNumBuiltinFormatStrings = 7;

BuiltinFormatString BuiltinFormatStrings[kNumBuiltinFormatStrings] =
   {{_("mm:ss"),
     _("*,01000m060.01000s")},
    {_("hh:mm:ss + milliseconds"),
     _("*h060m060.01000s")},
    {_("hh:mm:ss + samples"),
     _("*h060m060s+.#samples")},
    {_("hh:mm:ss + film frames (24 fps)"),
     _("*h060m060s+.24 frames")},
    {_("hh:mm:ss + CDDA frames (75 fps)"),
     _("*h060m060s+.75 frames")},
    {_("seconds"),
     _("*,01000,01000.01000 seconds")},
    {_("NTSC frames"),
     _("*,01000,01000.01000 NTSC frames|29.97002997")}};

class TimeField {
public:
   TimeField(int _base, int _range, bool _zeropad)
     { base = _base; range = _range; zeropad = _zeropad; 
       digits = 0; textCtrl = NULL; staticCtrl = NULL; }
   int base;  // divide by this (multiply, after decimal point)
   int range; // then take modulo this
   int digits;
   bool zeropad;
   wxString label;
   wxString formatStr;
   wxString str;
   wxTextCtrl *textCtrl;
   wxStaticText *staticCtrl;
   void CreateDigitFormatStr() {
      if (range > 1)
         digits = (int)ceil(log10(range-1.0));
      if (zeropad && range>1)
         formatStr.Printf(wxT("%%0%dd"), digits); // ex. "%03d" if digits is 3
      else
         formatStr = wxT("%d");
   }
};

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY(TimeFieldArray);

TimeTextCtrl::TimeTextCtrl(wxWindow *parent,
                           wxWindowID id,
                           wxString formatString,
                           double timeValue,
                           double sampleRate,
                           const wxPoint &pos,
                           const wxSize &size):
   wxPanel(parent, id, pos, size),
   mTimeValue(timeValue),
   mSampleRate(sampleRate),
   mFormatString(formatString),
   mModifyingText(false),
   mPrefixStaticText(NULL)
{
   ParseFormatString();
   CreateControls();
   ValueToControls();
}

void TimeTextCtrl::SetFormatString(wxString formatString)
{
   mFormatString = formatString;
   DeleteControls();
   ParseFormatString();
   CreateControls();
   ValueToControls();
}

void TimeTextCtrl::SetSampleRate(double sampleRate)
{
   mSampleRate = sampleRate;
   DeleteControls();
   ParseFormatString();
   CreateControls();
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
   mWholeFields.Clear();
   mFracFields.Clear();
   mScalingFactor = 1.0;

   const wxString format = mFormatString;
   bool inFrac = false;
   int fracMult = 1;
   wxString numStr;
   wxString delimStr;
   unsigned int i;

   for(i=0; i<format.Length(); i++) {
      bool handleDelim = false;
      bool handleNum = false;

      if (format[i] == '|') {
         wxString remainder = format.Right(format.Length() - i - 1);
         remainder.ToDouble(&mScalingFactor);
         i = format.Length()-1; // force break out of loop
         if (delimStr != wxT(""))
            handleDelim = true;
         if (numStr != wxT(""))
            handleNum = true;
      }
      else if ((format[i] >= '0' && format[i] <='9') ||
          format[i] == '*' || format[i] == '#') {
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

         if (inFrac) {
            int base = fracMult * range;
            mFracFields.Add(TimeField(base, range, zeropad));
            fracMult *= range;
         }
         else {
            unsigned int j;
            for(j=0; j<mWholeFields.GetCount(); j++)
               mWholeFields[j].base *= range;
            mWholeFields.Add(TimeField(1, range, zeropad));
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
            if (mFracFields.GetCount() == 0) {
               // Should never happen
               return;
            }
            if (handleNum && mFracFields.GetCount() > 1)
               mFracFields[mFracFields.GetCount()-2].label = delimStr;
            else
               mFracFields[mFracFields.GetCount()-1].label = delimStr;
         }
         else {
            if (mWholeFields.GetCount() == 0)
               mPrefix = delimStr;
            else {
               mWholeFields[mWholeFields.GetCount()-1].label = delimStr;
            }
         }

         if (goToFrac)
            inFrac = true;
         delimStr = wxT("");
      }
   }

   for(i=0; i<mWholeFields.GetCount(); i++)
      mWholeFields[i].CreateDigitFormatStr();
   for(i=0; i<mFracFields.GetCount(); i++)
      mFracFields[i].CreateDigitFormatStr();
}

void TimeTextCtrl::PrintDebugInfo()
{
   unsigned int i;

   printf("%s", (const char *)mPrefix.mb_str());

   for(i=0; i<mWholeFields.GetCount(); i++)
      printf("(t / %d) %% %d '%s' ",
             mWholeFields[i].base,
             mWholeFields[i].range,
             (const char *)mWholeFields[i].label.mb_str());

   for(i=0; i<mFracFields.GetCount(); i++)
      printf("(t * %d) %% %d '%s' ",
             mFracFields[i].base,
             mFracFields[i].range,
             (const char *)mFracFields[i].label.mb_str());

   printf("\n");
}

wxString TimeTextCtrl::GetTimeString()
{
   int t_int = int(mTimeValue);
   double t_frac = (mTimeValue - t_int);
   wxString result;
   unsigned int i;

   result = mPrefix;
   for(i=0; i<mWholeFields.GetCount(); i++) {
      int value = (t_int / mWholeFields[i].base);
      if (mWholeFields[i].range > 0)
         value = value % mWholeFields[i].range;
      result += wxString::Format(mWholeFields[i].formatStr, value);
      result += mWholeFields[i].label;
   }
   for(i=0; i<mFracFields.GetCount(); i++) {
      int value = (int)(t_frac * mFracFields[i].base);
      if (mFracFields[i].range > 0)
         value = value % mFracFields[i].range;
      result += wxString::Format(mFracFields[i].formatStr, value);
      result += mFracFields[i].label;
   }

   return result;
}

void TimeTextCtrl::DeleteControls()
{
   unsigned int i;

   if (mPrefixStaticText) {
      mPrefixStaticText->Destroy();
      mPrefixStaticText = NULL;
   }

   for(i=0; i<mWholeFields.GetCount(); i++) {
      mWholeFields[i].textCtrl->Destroy();
      mWholeFields[i].staticCtrl->Destroy();
   }

   for(i=0; i<mFracFields.GetCount(); i++) {
      mFracFields[i].textCtrl->Destroy();
      mFracFields[i].staticCtrl->Destroy();
   }
}

void TimeTextCtrl::CreateControls()
{
   wxBoxSizer *mainSizer;
   wxTextCtrl *text;
   wxStaticText *stat;
   unsigned int i;
   int width;
   int digits;

   if (sTextHeight == 0)
      ComputeTextExtents();

   mainSizer = new wxBoxSizer(wxHORIZONTAL);

   if (mPrefix != wxT("")) {
      stat = new wxStaticText(this, -1, mPrefix);
      mainSizer->Add(stat, 0, wxALL | wxALIGN_CENTER_VERTICAL |
                     wxLEFT | wxRIGHT, 1);
      mPrefixStaticText = stat;
   }

   for(i=0; i<mWholeFields.GetCount(); i++) {
      digits = mWholeFields[i].digits;
      if (digits < 1)
         digits = 3;
      if (digits > 10)
         digits = 10;
      width = sTextWidth[digits];
      text = new wxTextCtrl(this, AnyTextCtrlID, wxT(""),
                            wxDefaultPosition, wxSize(width, -1),
                            wxTE_RIGHT);
      if (mWholeFields[i].digits > 0)
         text->SetMaxLength(mWholeFields[i].digits);
      mainSizer->Add(text, 0, wxALL, 0);
      mWholeFields[i].textCtrl = text;

      stat = new wxStaticText(this, -1, mWholeFields[i].label);
      mainSizer->Add(stat, 0, wxALL | wxALIGN_CENTER_VERTICAL |
                     wxLEFT | wxRIGHT, 1);
      mWholeFields[i].staticCtrl = stat;
   }

   for(i=0; i<mFracFields.GetCount(); i++) {
      digits = mFracFields[i].digits;
      if (digits < 1)
         digits = 5;
      if (digits > 10)
         digits = 10;
      width = sTextWidth[digits];
      text = new wxTextCtrl(this, AnyTextCtrlID, wxT(""),
                            wxDefaultPosition, wxSize(width, -1),
                            wxTE_RIGHT);
      if (mFracFields[i].digits > 0)
         text->SetMaxLength(mFracFields[i].digits);
      mainSizer->Add(text, 0, wxALL, 0);
      mFracFields[i].textCtrl = text;

      stat = new wxStaticText(this, -1, mFracFields[i].label);
      mainSizer->Add(stat, 0, wxALL | wxALIGN_CENTER_VERTICAL |
                     wxLEFT | wxRIGHT, 1);
      mFracFields[i].staticCtrl = stat;
   }

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

void TimeTextCtrl::ValueToControls()
{
   long sel0, sel1;
   int t_int = int(mTimeValue);
   double t_frac = (mTimeValue - t_int);
   wxString str;
   unsigned int i;

   mModifyingText = true;

   for(i=0; i<mWholeFields.GetCount(); i++) {
      int value = (t_int / mWholeFields[i].base);
      if (mWholeFields[i].range > 0)
         value = value % mWholeFields[i].range;
      str.Printf(mWholeFields[i].formatStr, value);
      if (mWholeFields[i].str != str) {
         mWholeFields[i].str = str;
         mWholeFields[i].textCtrl->GetSelection(&sel0, &sel1);
         mWholeFields[i].textCtrl->SetValue(str);
         mWholeFields[i].textCtrl->SetSelection(sel0, sel1);
      }
   }
   for(i=0; i<mFracFields.GetCount(); i++) {
      int value = (int)(t_frac * mFracFields[i].base);
      if (mFracFields[i].range > 0)
         value = value % mFracFields[i].range;
      str.Printf(mFracFields[i].formatStr, value);
      if (mFracFields[i].str != str) {
         mFracFields[i].str = str;
         mFracFields[i].textCtrl->GetSelection(&sel0, &sel1);
         mFracFields[i].textCtrl->SetValue(str);
         mFracFields[i].textCtrl->SetSelection(sel0, sel1);
      }
   }

   mModifyingText = false;
}

void TimeTextCtrl::ControlsToValue()
{
   unsigned int i;
   double t = 0.0;

   for(i=0; i<mWholeFields.GetCount(); i++) {
      long val;
      mWholeFields[i].str = mWholeFields[i].textCtrl->GetValue();
      mWholeFields[i].str.ToLong(&val);
      t += (val * (double)mWholeFields[i].base);
   }
   for(i=0; i<mFracFields.GetCount(); i++) {
      long val;
      mFracFields[i].str = mFracFields[i].textCtrl->GetValue();
      mFracFields[i].str.ToLong(&val);
      t += (val / (double)mFracFields[i].base);
   }

   mTimeValue = t;
}

void TimeTextCtrl::OnText(wxCommandEvent &event)
{
   // Don't trigger an event if we were the ones who modified the text
   if (mModifyingText)
      return;

   double oldValue = mTimeValue;

   ControlsToValue();

   if (oldValue == mTimeValue)
      return;

   //Create an event for the parent window to process.
   wxCommandEvent *e =
      new wxCommandEvent(wxEVT_COMMAND_TEXT_UPDATED, GetId());
   GetParent()->AddPendingEvent(*e);
   delete e;
}

bool TimeTextCtrl::HasAnyFocus()
{
   unsigned int i;
   wxWindow *focus = FindFocus();

   if (focus == this)
      return true;

   for(i=0; i<mWholeFields.GetCount(); i++)
      if (focus == mWholeFields[i].textCtrl)
         return true;

   for(i=0; i<mFracFields.GetCount(); i++)
      if (focus == mFracFields[i].textCtrl)
         return true;

   return false;
}

void TimeTextCtrl::ComputeTextExtents()
{
   // Figure out the font used in text controls, and measure it to
   // determine how many pixels wide (and tall) we need to make a
   // TextCtrl in order to have room for any number of digits
   // from 1 to 10.  Because the text control has a margin and border,
   // we need to add some constant pixels to this, which is currently
   // a hardcoded number per platform.

   #if defined(__WXGTK__)
   int xBorder = 2;
   int yBorder = 2;
   #elif defined(__WXMSW__)
   int xBorder = 6;
   int yBorder = 2;
   #elif defined(__WXMAC__)
   int xBorder = 2;
   int yBorder = 2;
   #else
   int xBorder = 2;
   int yBorder = 2;
   #endif

   wxChar str[11];
   wxTextCtrl *text;
   wxCoord width=0, height=0;
   unsigned int i;

   text = new wxTextCtrl(this, -1, wxT(""));
   wxTextAttr attr = text->GetDefaultStyle();
   wxMemoryDC dc;
   wxBitmap bitmap(300, 100);
   dc.SelectObject(bitmap);
   if (attr.HasFont())
      dc.SetFont(attr.GetFont());
   else
      dc.SetFont(text->GetFont());

   for(i=0; i<=10; i++) {
      str[i] = wxT('0');
      str[i+1] = 0;
      dc.GetTextExtent(str, &width, &height);
      sTextWidth[i] = width + xBorder;
      sTextHeight = height + yBorder;
   }

   text->Destroy();
}


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



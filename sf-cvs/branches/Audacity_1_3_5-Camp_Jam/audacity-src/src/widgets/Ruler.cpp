/**********************************************************************

  Audacity: A Digital Audio Editor

  Ruler.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Ruler
\brief Used to display a Ruler.

  This is a generic class which can be used to display just about
  any kind of ruler.  

  At a minimum, the user must specify the dimensions of the
  ruler, its orientation (horizontal or vertical), and the
  values displayed at the two ends of the ruler (min and max).
  By default, this class will display tick marks at reasonable
  round numbers and fractions, for example, 100, 50, 10, 5, 1,
  0.5, 0.1, etc.

  The class is designed to display a small handful of
  labeled Major ticks, and a few Minor ticks between each of
  these.  Minor ticks are labeled if there is enough space.
  Labels will never run into each other.

  In addition to Real numbers, the Ruler currently supports
  two other formats for its display:

  Integer - never shows tick marks for fractions of an integer
  
  Time - Assumes values represent seconds, and labels the tick
         marks in "HH:MM:SS" format, e.g. 4000 seconds becomes
         "1:06:40", for example.  Will display fractions of
         a second, and tick marks are all reasonable round
         numbers for time (i.e. 15 seconds, 30 seconds, etc.)
*//***************************************************************//**

\class RulerPanel
\brief RulerPanel class allows you to work with a Ruler like
  any other wxWindow.

*//***************************************************************//**

\class Ruler::Label
\brief An array of these created by the Ruler is used to determine 
what and where text annotations to the numbers on the Ruler get drawn.

\todo Check whether Ruler is costing too much time in malloc/free of
array of Ruler::Label.

*//******************************************************************/

#include "../Audacity.h"

#include <math.h>

#include <wx/dcscreen.h>
#include <wx/dcmemory.h>
#include <wx/dcbuffer.h>
#include <wx/settings.h>

#include "../Internat.h"
#include "../Project.h"
#include "Ruler.h"
#include "../toolbars/ControlToolBar.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../Experimental.h"

#define max(a,b)  ( (a<b)?b:a )

#define SELECT_TOLERANCE_PIXEL 10

#define PLAY_REGION_TRIANGLE_SIZE 6
#define PLAY_REGION_RECT_WIDTH 1
#define PLAY_REGION_RECT_HEIGHT 3
#define PLAY_REGION_GLOBAL_OFFSET_Y 7

//
// Ruler
//

Ruler::Ruler()
{
   mMin = 0.0;
   mMax = 100.0;
   mOrientation = wxHORIZONTAL;
   mSpacing = 6;
   mHasSetSpacing = false;
   mFormat = RealFormat;
   mFlip = false;
   mLog = false;
   mLabelEdges = false;
   mUnits = wxT("");

   mLeft = -1;
   mTop = -1;
   mRight = -1;
   mBottom = -1;
   mbTicksOnly = true;
   mbTicksAtExtremes = false;
   mTickColour = wxColour(153,153,153);
   mPen.SetColour(mTickColour);

   // Note: the font size is now adjusted automatically whenever
   // Invalidate is called on a horizontal Ruler, unless the user
   // calls SetFonts manually.  So the defaults here are not used
   // often.

   int fontSize = 10;
#ifdef __WXMSW__
   fontSize = 8;
#endif

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   mMinorMinorFont = new wxFont(fontSize-1, wxSWISS, wxNORMAL, wxNORMAL);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   mMinorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
   mMajorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD);
   mUserFonts = false;

   #ifdef __WXMAC__
   #ifdef EXPERIMENTAL_RULER_AUTOSIZE
   mMinorMinorFont->SetNoAntiAliasing(true);
   #endif //EXPERIMENTAL_RULER_AUTOSIZE
   mMinorFont->SetNoAntiAliasing(true);
   mMajorFont->SetNoAntiAliasing(true);
   #endif

   mMajorLabels = 0;
   mMinorLabels = 0;
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   mMinorMinorLabels = 0;
   mLengthOld = 0;
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   mBits = NULL;
   mUserBits = NULL;
   mUserBitLen = 0;
   
   mValid = false;
}

Ruler::~Ruler()
{
   Invalidate();  // frees up our arrays
   if( mUserBits )
      delete [] mUserBits;//JKC
   delete mMinorFont;
   delete mMajorFont;
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   delete mMinorMinorFont;

   if (mMajorLabels)
      delete[] mMajorLabels;
   if (mMinorLabels)
      delete[] mMinorLabels;
   if (mMinorMinorLabels)
      delete[] mMinorMinorLabels;
#endif //EXPERIMENTAL_RULER_AUTOSIZE
}

void Ruler::SetFormat(RulerFormat format)
{
   // IntFormat, RealFormat, RealLogFormat, TimeFormat, or LinearDBFormat

   if (mFormat != format) {
      mFormat = format;
      
      Invalidate();
   }
}

void Ruler::SetLog(bool log)
{
   // Logarithmic

   if (mLog != log) {
      mLog = log;
      
      Invalidate();
   }
}

void Ruler::SetUnits(wxString units)
{
   // Specify the name of the units (like "dB") if you
   // want numbers like "1.6" formatted as "1.6 dB".

   if (mUnits != units) {
      mUnits = units;
      
      Invalidate();
   }
}

void Ruler::SetOrientation(int orient)
{
   // wxHORIZONTAL || wxVERTICAL

   if (mOrientation != orient) {
      mOrientation = orient;
      
      if (mOrientation == wxVERTICAL && !mHasSetSpacing)
         mSpacing = 2;
      
      Invalidate();
   }
}

void Ruler::SetRange(double min, double max)
{
   // For a horizontal ruler,
   // min is the value in the center of pixel "left",
   // max is the value in the center of pixel "right".

   if (mMin != min || mMax != max) {
      mMin = min;
      mMax = max;
      
      Invalidate();
   }
}

void Ruler::SetSpacing(int spacing)
{
   mHasSetSpacing = true;

   if (mSpacing != spacing) {
      mSpacing = spacing;

      Invalidate();
   }
}

void Ruler::SetLabelEdges(bool labelEdges)
{
   // If this is true, the edges of the ruler will always
   // receive a label.  If not, the nearest round number is
   // labeled (which may or may not be the edge).

   if (mLabelEdges != labelEdges) {
      mLabelEdges = labelEdges;
      
      Invalidate();
   }
}

void Ruler::SetFlip(bool flip)
{
   // If this is true, the orientation of the tick marks
   // is reversed from the default; eg. above the line
   // instead of below

   if (mFlip != flip) {
      mFlip = flip;
      
      Invalidate();
   }
}

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
void Ruler::SetFonts(const wxFont &minorFont, const wxFont &majorFont, const wxFont &minorMinorFont)
#else //!EXPERIMENTAL_RULER_AUTOSIZE
void Ruler::SetFonts(const wxFont &minorFont, const wxFont &majorFont)
#endif //EXPERIMENTAL_RULER_AUTOSIZE
{
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   *mMinorMinorFont = minorMinorFont;
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   *mMinorFont = minorFont;
   *mMajorFont = majorFont;

   #ifdef __WXMAC__
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   mMinorMinorFont->SetNoAntiAliasing(true);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   mMinorFont->SetNoAntiAliasing(true);
   mMajorFont->SetNoAntiAliasing(true);
   #endif
   
   // Won't override these fonts
   mUserFonts = true;

   Invalidate();
}

void Ruler::OfflimitsPixels(int start, int end)
{
   int i;

   if (!mUserBits) {
      if (mOrientation == wxHORIZONTAL)
         mLength = mRight-mLeft;
      else
         mLength = mBottom-mTop;      
      mUserBits = new int[mLength+1];
      for(i=0; i<=mLength; i++)
         mUserBits[i] = 0;
      mUserBitLen  = mLength+1;
   }

   if (end < start) {
      i = end;
      end = start;
      start = i;
   }

   if (start < 0)
      start = 0;
   if (end > mLength)
      end = mLength;

   for(i=start; i<=end; i++)
      mUserBits[i] = 1;
}

void Ruler::SetBounds(int left, int top, int right, int bottom)
{
   if (mLeft != left || mTop != top ||
       mRight != right || mBottom != bottom) {
      mLeft = left;
      mTop = top;
      mRight = right;
      mBottom = bottom;

      Invalidate();
   }
}

void Ruler::Invalidate()
{
   mValid = false;

   if (mOrientation == wxHORIZONTAL)
      mLength = mRight-mLeft;
   else
      mLength = mBottom-mTop;

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
#else //!EXPERIMENTAL_RULER_AUTOSIZE
   if (mMajorLabels) {
      delete [] mMajorLabels;
      mMajorLabels = NULL;
   }
   if (mMinorLabels) {
      delete [] mMinorLabels;
      mMinorLabels = NULL;
   }
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   if (mBits) {
      delete [] mBits;
      mBits = NULL;
   }
   if (mUserBits && mLength+1 != mUserBitLen) {
      delete[] mUserBits;
      mUserBits = NULL;
      mUserBitLen = 0;
   }
}

void Ruler::FindLinearTickSizes(double UPP)
{
   // Given the dimensions of the ruler, the range of values it
   // has to display, and the format (i.e. Int, Real, Time),
   // figure out how many units are in one Minor tick, and
   // in one Major tick.
   //
   // The goal is to always put tick marks on nice round numbers
   // that are easy for humans to grok.  This is the most tricky
   // with time.

   double d;

   // As a heuristic, we want at least 16 pixels
   // between each minor tick
   double units = 16 * fabs(UPP);

   mDigits = 0;

   switch(mFormat) {
   case LinearDBFormat:
      if (units < 0.1) {
         mMinor = 0.1;
         mMajor = 0.5;
         return;
      }
      if (units < 1.0) {
         mMinor = 1.0;
         mMajor = 6.0;
         return;
      }
      if (units < 3.0) {
         mMinor = 3.0;
         mMajor = 12.0;
         return;
      }
      if (units < 6.0) {
         mMinor = 6.0;
         mMajor = 24.0;
         return;
      }
      if (units < 12.0) {
         mMinor = 12.0;
         mMajor = 48.0;
         return;
      }
      if (units < 24.0) {
         mMinor = 24.0;
         mMajor = 96.0;
         return;
      }
      d = 20.0;
      for(;;) {
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 5.0;
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 2.0;
      }
      break;

   case IntFormat:
      d = 1.0;
      for(;;) {
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 5.0;
         if (units < d) {
            mMinor = d;
            mMajor = d*2.0;
            return;
         }
         d *= 2.0;
      }
      break;

   case TimeFormat:
      if (units > 0.5) {
         if (units < 1.0) { // 1 sec
            mMinor = 1.0;
            mMajor = 5.0;
            return;
         }
         if (units < 5.0) { // 5 sec
            mMinor = 5.0;
            mMajor = 15.0;
            return;
         }
         if (units < 10.0) {
            mMinor = 10.0;
            mMajor = 30.0;
            return;
         }
         if (units < 15.0) {
            mMinor = 15.0;
            mMajor = 60.0;
            return;
         }
         if (units < 30.0) {
            mMinor = 30.0;
            mMajor = 60.0;
            return;
         }
         if (units < 60.0) { // 1 min
            mMinor = 60.0;
            mMajor = 300.0;
            return;
         }
         if (units < 300.0) { // 5 min
            mMinor = 300.0;
            mMajor = 900.0;
            return;
         }
         if (units < 600.0) { // 10 min
            mMinor = 600.0;
            mMajor = 1800.0;
            return;
         }
         if (units < 900.0) { // 15 min
            mMinor = 900.0;
            mMajor = 3600.0;
            return;
         }
         if (units < 1800.0) { // 30 min
            mMinor = 1800.0;
            mMajor = 3600.0;
            return;
         }
         if (units < 3600.0) { // 1 hr
            mMinor = 3600.0;
            mMajor = 6*3600.0;
            return;
         }
         if (units < 6*3600.0) { // 6 hrs
            mMinor = 6*3600.0;
            mMajor = 24*3600.0;
            return;
         }
         if (units < 24*3600.0) { // 1 day
            mMinor = 24*3600.0;
            mMajor = 7*24*3600.0;
            return;
         }

         mMinor = 24.0 * 7.0 * 3600.0; // 1 week
         mMajor = 24.0 * 7.0 * 3600.0;
      }

      // Otherwise fall through to RealFormat
      // (fractions of a second should be dealt with
      // the same way as for RealFormat)

   case RealFormat:
      d = 0.000001;
      // mDigits is number of digits after the decimal point.
      mDigits = 6;
      for(;;) {
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 5.0;
         if (units < d) {
            mMinor = d;
            mMajor = d*2.0;
            return;
         }
         d *= 2.0;
         mDigits--;
         // More than 10 digit numbers?  Something is badly wrong.
         // Probably units is coming in with too high a value.
         wxASSERT( mDigits >= -10 );
      }
      mMinor = d;
      mMajor = d * 2.0;
      break;

#ifdef LOGARITHMIC_SPECTRUM
   case RealLogFormat:
      d = 0.000001;
      // mDigits is number of digits after the decimal point.
      mDigits = 6;
      for(;;) {
         if (units < d) {
            mMinor = d;
            mMajor = d*5.0;
            return;
         }
         d *= 5.0;
         if (units < d) {
            mMinor = d;
            mMajor = d*2.0;
            return;
         }
         d *= 2.0;
         mDigits--;
         // More than 10 digit numbers?  Something is badly wrong.
         // Probably units is coming in with too high a value.
         wxASSERT( mDigits >= -10 );
      }
      mDigits++;
      mMinor = d;
      mMajor = d * 2.0;
      break;
#endif
   }

}

wxString Ruler::LabelString(double d, bool major)
{
   // Given a value, turn it into a string according
   // to the current ruler format.  The number of digits of
   // accuracy depends on the resolution of the ruler,
   // i.e. how far zoomed in or out you are.

   wxString s;

   // Replace -0 with 0
   if (d < 0.0 && d+mMinor > 0.0)
      d = 0.0;

   switch(mFormat) {
   case IntFormat:
      s.Printf(wxT("%d"), (int)floor(d+0.5));
      break;
   case LinearDBFormat:
      if (mMinor >= 1.0)
         s.Printf(wxT("%d"), (int)floor(d+0.5));
      else {
         s.Printf(wxT("%.1f"), d);
      }
      break;
   case RealFormat:
      if (mMinor >= 1.0)
         s.Printf(wxT("%d"), (int)floor(d+0.5));
      else {
         s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
      }
      break;
#ifdef LOGARITHMIC_SPECTRUM
   case RealLogFormat:
      if (mMinor >= 1.0)
         s.Printf(wxT("%d"), (int)floor(d+0.5));
      else {
         s.Printf(wxString::Format(wxT("%%.%df"), mDigits), d);
      }
      break;
#endif
   case TimeFormat:
      if (major) {
         if (d < 0) {
            s = wxT("-");
            d = -d;
         }

         #if ALWAYS_HH_MM_SS
         int secs = (int)(d + 0.5);
         if (mMinor >= 1.0) {
            s.Printf(wxT("%d:%02d:%02d"), secs/3600, (secs/60)%60, secs%60);
         }
         else {
            wxString t1, t2, format;
            t1.Printf(wxT("%d:%02d:"), secs/3600, (secs/60)%60);
            format.Printf(wxT("%%0%d.%dlf"), mDigits+3, mDigits);
            t2.Printf(format.c_str(), fmod(d, 60.0));
            s += t1 + t2;
         }
         break;
         #endif

         if (mMinor >= 3600.0) {
            int hrs = (int)(d / 3600.0 + 0.5);
            wxString h;
            h.Printf(wxT("%d:00:00"), hrs);
            s += h;
         }
         else if (mMinor >= 60.0) {
            int minutes = (int)(d / 60.0 + 0.5);
            wxString m;
            if (minutes >= 60)
               m.Printf(wxT("%d:%02d:00"), minutes/60, minutes%60);
            else
               m.Printf(wxT("%d:00"), minutes);
            s += m;
         }
         else if (mMinor >= 1.0) {
            int secs = (int)(d + 0.5);
            wxString t;
            if (secs >= 3600)
               t.Printf(wxT("%d:%02d:%02d"), secs/3600, (secs/60)%60, secs%60);
            else if (secs >= 60)
               t.Printf(wxT("%d:%02d"), secs/60, secs%60);
            else
               t.Printf(wxT("%d"), secs);
            s += t;
         }
         else {
            int secs = (int)(d);
            wxString t1, t2, format;

            if (secs >= 3600)
               t1.Printf(wxT("%d:%02d:"), secs/3600, (secs/60)%60);
            else if (secs >= 60)
               t1.Printf(wxT("%d:"), secs/60);

            if (secs >= 60)
               format.Printf(wxT("%%0%d.%dlf"), mDigits+3, mDigits);
            else
               format.Printf(wxT("%%%d.%dlf"), mDigits+3, mDigits);
            t2.Printf(format.c_str(), fmod(d, 60.0));

            s += t1 + t2;
         }
      }
      else {
      }
   }
   
   if (mUnits != wxT(""))
#ifdef LOGARITHMIC_SPECTRUM
      s = (s + mUnits);
#else
      s = (s + wxT(" ") + mUnits);
#endif

   return s;
}

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
void Ruler::Tick(int pos, double d, bool major, bool minor)
#else //!EXPERIMENTAL_RULER_AUTOSIZE
void Ruler::Tick(int pos, double d, bool major)
#endif //EXPERIMENTAL_RULER_AUTOSIZE
{
   wxString l;
   wxCoord strW, strH, strD, strL;
   int strPos, strLen, strLeft, strTop;

   // FIXME: We don't draw a tick if of end of our label arrays
   // But we shouldn't have an array of labels.
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   if( mNumMinorMinor >= mLength )
      return;
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   if( mNumMinor >= mLength )
      return;
   if( mNumMajor >= mLength )
      return;

   Label *label;
   if (major)
      label = &mMajorLabels[mNumMajor++];
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   else if (minor)
      label = &mMinorLabels[mNumMinor++];
   else
      label = &mMinorMinorLabels[mNumMinorMinor++];
#else //!EXPERIMENTAL_RULER_AUTOSIZE
   else
      label = &mMinorLabels[mNumMinor++];
#endif //EXPERIMENTAL_RULER_AUTOSIZE

   label->pos = pos;
   label->lx = mLeft - 1000; // don't display
   label->ly = mTop - 1000;  // don't display
   label->text = wxT("");

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   mDC->SetFont(major? *mMajorFont: minor? *mMinorFont : *mMinorMinorFont);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
   mDC->SetFont(major? *mMajorFont: *mMinorFont);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   l = LabelString(d, major);
   mDC->GetTextExtent(l, &strW, &strH, &strD, &strL);

   if (mOrientation == wxHORIZONTAL) {
      strLen = strW;
      strPos = pos - strW/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strW >= mLength)
         strPos = mLength - strW;
      strLeft = mLeft + strPos;
      if (mFlip) {
         strTop = mTop + 4;
         mMaxHeight = max(mMaxHeight, strH + 4);
      }
      else {
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
         strTop =-strH-mLead;
#else //!EXPERIMENTAL_RULER_AUTOSIZE
         strTop = mBottom - strH -6;
         // Still leaves room for an umlaut or whatever above normal character
         strTop = mTop- mLead+4;// More space was needed...
#endif //EXPERIMENTAL_RULER_AUTOSIZE
         mMaxHeight = max(mMaxHeight, strH + 6);
      }
   }
   else {
      strLen = strH;
      strPos = pos - strH/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strH >= mLength)
         strPos = mLength - strH;
      strTop = mTop + strPos;
      if (mFlip) {
         strLeft = mLeft + 5;
         mMaxWidth = max(mMaxWidth, strW + 5);
      }
      else {
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
         strLeft =-strW-6;
#else //!EXPERIMENTAL_RULER_AUTOSIZE
         strLeft = mRight - strW - 6;
         mMaxWidth = max(mMaxWidth, strW + 6);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
      }
   }


   // FIXME: we shouldn't even get here if strPos < 0.
   // Ruler code currently does  not handle very small or
   // negative sized windows (i.e. don't draw) properly.
   if( strPos < 0 )
      return;

   // See if any of the pixels we need to draw this
   // label is already covered

   int i;
   for(i=0; i<strLen; i++)
      if (mBits[strPos+i])
         return;

   // If not, position the label and give it text

   label->lx = strLeft;
   label->ly = strTop;
   label->text = l;

   // And mark these pixels, plus some surrounding
   // ones (the spacing between labels), as covered
   int leftMargin = mSpacing;
   if (strPos < leftMargin)
      leftMargin = strPos;
   strPos -= leftMargin;
   strLen += leftMargin;

   int rightMargin = mSpacing;
   if (strPos + strLen > mLength - mSpacing)
      rightMargin = mLength - strPos - strLen;
   strLen += rightMargin;

   for(i=0; i<strLen; i++)
      mBits[strPos+i] = 1;

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   wxRect r(strLeft, strTop, strW, strH);
   mRect.Union(r);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
}

void Ruler::Update()
{
  Update(NULL, 0, 0);
}

void Ruler::Update( Envelope *speedEnv, long minSpeed, long maxSpeed )
{
   // This gets called when something has been changed
   // (i.e. we've been invalidated).  Recompute all
   // tick positions and font size.

   int i;
   int j;

   if (!mUserFonts) {
     int fontSize = 4;
     wxCoord strW, strH, strD, strL;
     wxString exampleText = wxT("0.9");   //ignored for height calcs on all platforms
     int desiredPixelHeight;

     if (mOrientation == wxHORIZONTAL)
       desiredPixelHeight = mBottom-mTop-5; // height less ticks and 1px gap
     else
       desiredPixelHeight = (mRight-mLeft)/2;

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
     if (desiredPixelHeight < 10)//8)
       desiredPixelHeight = 10;//8;
#else //!EXPERIMENTAL_RULER_AUTOSIZE
     if (desiredPixelHeight < 8)
       desiredPixelHeight = 8;
#endif //EXPERIMENTAL_RULER_AUTOSIZE
     if (desiredPixelHeight > 12)
       desiredPixelHeight = 12;

      // Keep making the font bigger until it's too big, then subtract one.
      mDC->SetFont(wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD));
      mDC->GetTextExtent(exampleText, &strW, &strH, &strD, &strL);
      while( (strH-strD-strL) <= desiredPixelHeight && fontSize < 40) {
         fontSize++;
         mDC->SetFont(wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD));
         mDC->GetTextExtent(exampleText, &strW, &strH, &strD, & strL);
      }
      fontSize--;
      mDC->SetFont(wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL));
      mDC->GetTextExtent(exampleText, &strW, &strH, &strD, &strL);
      mLead = strL;

     if (mMajorFont)
        delete mMajorFont;
     mMajorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD);

     if (mMinorFont)
        delete mMinorFont;
     mMinorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
     if (mMinorMinorFont)
        delete mMinorMinorFont;
     mMinorMinorFont = new wxFont(fontSize-1, wxSWISS, wxNORMAL, wxNORMAL);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   }

   // If ruler is being resized, we could end up with it being too small.
   // Values of mLength of zero or below cause bad array allocations and 
   // division by zero.  So...
   // IF too small THEN bail out and don't draw.
   if( mLength <= 0 )
      return;

   if (mOrientation == wxHORIZONTAL) {
      mMaxWidth = mLength;
      mMaxHeight = 0;
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
      mRect = wxRect(0,0, mLength,0);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   }
   else {
      mMaxWidth = 0;
      mMaxHeight = mLength;
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
      mRect = wxRect(0,0, 0,mLength);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   }

   // FIXME: Surely we do not need to allocate storage for the labels?
   // We can just recompute them as we need them?
   mNumMajor = 0;
   mNumMinor = 0;
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   mNumMinorMinor = 0;
   if (mLength!=mLengthOld) {
      if (mMajorLabels)
         delete[] mMajorLabels;
      mMajorLabels = new Label[mLength+1];
      if (mMinorLabels)
         delete[] mMinorLabels;
      mMinorLabels = new Label[mLength+1];
      if (mMinorMinorLabels)
         delete[] mMinorMinorLabels;
      mMinorMinorLabels = new Label[mLength+1];
      mLengthOld = mLength;
   }
#else //!EXPERIMENTAL_RULER_AUTOSIZE
   mMajorLabels = new Label[mLength+1];
   mMinorLabels = new Label[mLength+1];
#endif //EXPERIMENTAL_RULER_AUTOSIZE
   
   if (mBits)
      delete[] mBits;
   mBits = new int[mLength+1];
   if (mUserBits)
      for(i=0; i<=mLength; i++)
         mBits[i] = mUserBits[i];
   else
      for(i=0; i<=mLength; i++)
         mBits[i] = 0;

   if(mLog==false) {

      double UPP = (mMax-mMin)/mLength;  // Units per pixel

      FindLinearTickSizes(UPP);
      
      // Left and Right Edges
      if (mLabelEdges) {
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
         Tick(0, mMin, true, false);
         Tick(0, mMin, true, false);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
         Tick(0, mMin, true);
         Tick(mLength, mMax, true);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
      }
      
      // Zero (if it's in the middle somewhere)
      if (mMin * mMax < 0.0) {
         int mid = (int)(mLength*(mMin/(mMin-mMax)) + 0.5);
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
         Tick(mid, 0.0, true, false);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
         Tick(mid, 0.0, true);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
      }
      
      double sg = UPP > 0.0? 1.0: -1.0;
      
      // Major ticks
      double d = mMin - UPP/2;
      double lastD = d;
      int majorInt = (int)floor(sg * d / mMajor);
      i = -1;
      while(i <= mLength) {
         double warpfactor;
         if( d>0 && speedEnv != NULL ) {
            warpfactor = speedEnv->Average( lastD, d );
            // Now we re-scale so that 0.5 is normal speed and
            // 0 and 1.0 are min% and max% of normal speed
            warpfactor = (maxSpeed * (1 - warpfactor) +
                          warpfactor * minSpeed) / 100.0;
         }
         else
            warpfactor = 1.0;
         
         i++;
         lastD = d;
         d += UPP*warpfactor;
         
         if ((int)floor(sg * d / mMajor) > majorInt) {
            majorInt = (int)floor(sg * d / mMajor);
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
            Tick(i, sg * majorInt * mMajor, true, false);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
            Tick(i, sg * majorInt * mMajor, true);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
         }
      }
         
      // Minor ticks
      d = mMin - UPP/2;
      lastD = d;
      int minorInt = (int)floor(sg * d / mMinor);
      i = -1;
      while(i <= mLength) {
         double warpfactor;
         if( d>0 && speedEnv != NULL ) {
            warpfactor = speedEnv->Average( lastD, d );
            // Now we re-scale so that 0.5 is normal speed and
            // 0 and 1.0 are min% and max% of normal speed
            warpfactor = (maxSpeed * (1 - warpfactor) +
                          warpfactor * minSpeed) / 100.0;
         }
         else
            warpfactor = 1.0;
         
         i++;
         lastD = d;
         d += UPP*warpfactor;
         
         if ((int)floor(sg * d / mMinor) > minorInt) {
            minorInt = (int)floor(sg * d / mMinor);
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
            Tick(i, sg * minorInt * mMinor, false, true);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
            Tick(i, sg * minorInt * mMinor, false);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
         }
      }
      
      // Left and Right Edges
      if (mLabelEdges) {
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
         Tick(0, mMin, true, false);
         Tick(mLength, mMax, true, false);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
         Tick(0, mMin, true);
         Tick(mLength, mMax, true);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
      }
      
   }
   else {
      // log case
#ifdef LOGARITHMIC_SPECTRUM
      mDigits=2;	//TODO: implement dynamic digit computation
#endif
      double loLog = log10(mMin);
      double hiLog = log10(mMax);
      double scale = mLength/(hiLog - loLog);
      int loDecade = (int) floor(loLog);
      int hiDecade = (int) ceil(hiLog);
      
      int pos;
      double val;
      double startDecade = pow(10., (double)loDecade);
      
      // Major ticks are the decades
#ifdef LOGARITHMIC_SPECTRUM
      double decade = startDecade;
      double delta=hiLog-loLog, steps=fabs(delta);
      double step = delta>=0 ? 10 : 0.1;
      double rMin=wxMin(mMin, mMax), rMax=wxMax(mMin, mMax);
      for(i=0; i<=steps; i++) 
      {  // if(i!=0) 
         {  val = decade;
            if(val > rMin && val < rMax) {
               pos = (int)(((log10(val) - loLog)*scale)+0.5);
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, true, false);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, true);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
            }
         }
         decade *= step;
      }
#else
      double decade = startDecade;
      for(i=loDecade; i<hiDecade; i++) {
         if(i!=loDecade) {
            val = decade;
            if(val > mMin && val < mMax) {
               pos = (int)(((log10(val) - loLog)*scale)+0.5);
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, true, false);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, true);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
            }
         }
         decade *= 10.;
      }
#endif

      // Minor ticks are multiples of decades
#ifdef LOGARITHMIC_SPECTRUM
      decade = startDecade;
      float start, end, mstep;
      if (delta > 0)
      {  start=2; end=10; mstep=1;
      }else
      {  start=9; end=1; mstep=-1;
      }
      steps++;
      for(i=0; i<=steps; i++) {
         for(j=start; j!=end; j+=mstep) {
            val = decade * j;
            if(val >= rMin && val < rMax) {
               pos = (int)(((log10(val) - loLog)*scale)+0.5);
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, false, true);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, false);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
            }
         }
         decade *= step;
      }

      // MinorMinor ticks are multiples of decades
      decade = startDecade;
      if (delta > 0)
      {  start= 10; end=100; mstep= 1;
      }else
      {  start=100; end= 10; mstep=-1;
      }
      steps++;
      for(i=0; i<=steps; i++) {
         for(int f=start; f!=int(end); f+=mstep) {
            if (int(f/10)!=f/10.0f) {
               val = decade * f/10;
               if(val >= rMin && val < rMax) {
                  pos = (int)(((log10(val) - loLog)*scale)+0.5);
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, false, false);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, false);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
               }
            }
         }
         decade *= step;
      }
#else
      decade = startDecade;
      for(i=loDecade; i<hiDecade; i++) {
         for(j=2; j<=9; j++) {
            val = decade * j;
            if(val >= mMin && val < mMax) {
               pos = (int)(((log10(val) - loLog)*scale)+0.5);
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, false, true);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
               Tick(pos, val, false);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
            }
         }
         decade *= 10.;
      }
#endif
   }

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   int displacementx=0, displacementy=0;
   if (!mFlip) {
      if (mOrientation==wxHORIZONTAL) {
         int d=mTop+mRect.GetHeight()+5;
         mRect.Offset(0,d);
         mRect.Inflate(0,5);
         displacementx=0; 
         displacementy=d;
      }
      else {
         int d=mLeft-mRect.GetLeft()+5;
         mRect.Offset(d,0);
         mRect.Inflate(5,0);
         displacementx=d;
         displacementy=0;
      }
   }
   for(i=0; i<mNumMajor; i++) {
      mMajorLabels[i].lx+= displacementx;
      mMajorLabels[i].ly+= displacementy;
   }
   for(i=0; i<mNumMinor; i++) {
      mMinorLabels[i].lx+= displacementx;
      mMinorLabels[i].ly+= displacementy;
   }
   for(i=0; i<mNumMinorMinor; i++) {
      mMinorMinorLabels[i].lx+= displacementx;
      mMinorMinorLabels[i].ly+= displacementy;
   }
   mMaxWidth = mRect.GetWidth ();
   mMaxHeight= mRect.GetHeight();
#endif //EXPERIMENTAL_RULER_AUTOSIZE

   mValid = true;
}

void Ruler::Draw(wxDC& dc)
{
   Draw( dc, NULL, 0, 0);
}

void Ruler::Draw(wxDC& dc, Envelope *speedEnv, long minSpeed, long maxSpeed)
{
   mDC = &dc;

   if( mLength <=0 )
      return;

   if (!mValid)
      Update( speedEnv, minSpeed, maxSpeed );

#ifdef EXPERIMENTAL_THEMING
   mDC->SetPen(mPen);
   mDC->SetTextForeground(mTickColour);
#elif ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   mDC->SetPen(*wxBLACK_PEN);
   mDC->SetTextForeground(wxColour(231, 110,  52));
#else
   mDC->SetPen(*wxBLACK_PEN);
   mDC->SetTextForeground(*wxBLACK);
#endif

   // Draws a long line the length of the ruler.
   if( !mbTicksOnly )
   {
      if (mOrientation == wxHORIZONTAL) {
         if (mFlip)
            mDC->DrawLine(mLeft, mTop, mRight+1, mTop);
         else
            mDC->DrawLine(mLeft, mBottom, mRight+1, mBottom);
      }
      else {
         if (mFlip)
            mDC->DrawLine(mLeft, mTop, mLeft, mBottom+1);
         else
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
            mDC->DrawLine(mRect.x-mRect.width, mTop, mRect.x-mRect.width, mBottom+1);
#else //!EXPERIMENTAL_RULER_AUTOSIZE
            mDC->DrawLine(mRight, mTop, mRight, mBottom+1);
#endif //EXPERIMENTAL_RULER_AUTOSIZE
      }
   }

   int i;

   mDC->SetFont(*mMajorFont);

   // We may want to not show the ticks at the extremes,
   // though still showing the labels.
   // This gives a better look when the ruler is on a bevelled
   // button, since otherwise the tick is drawn on the bevel.
   int iMaxPos = (mOrientation==wxHORIZONTAL)? mRight : mBottom-5;

   for(i=0; i<mNumMajor; i++) {
      int pos = mMajorLabels[i].pos;

      if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
      {
         if (mOrientation == wxHORIZONTAL) {
            if (mFlip)
               mDC->DrawLine(mLeft + pos, mTop,
                             mLeft + pos, mTop + 4);
            else
               mDC->DrawLine(mLeft + pos, mBottom - 4,
                             mLeft + pos, mBottom);
         }
         else {
            if (mFlip)
               mDC->DrawLine(mLeft, mTop + pos,
                             mLeft + 4, mTop + pos);
            else
               mDC->DrawLine(mRight - 4, mTop + pos,
                             mRight, mTop + pos);
         }
      }

      if (mMajorLabels[i].text != wxT(""))
         mDC->DrawText(mMajorLabels[i].text,
                       mMajorLabels[i].lx,
                       mMajorLabels[i].ly);
   }

   mDC->SetFont(*mMinorFont);

   for(i=0; i<mNumMinor; i++) {
      int pos = mMinorLabels[i].pos;

      if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
      {
         if (mOrientation == wxHORIZONTAL) 
         {
            if (mFlip)
               mDC->DrawLine(mLeft + pos, mTop,
                             mLeft + pos, mTop + 2);
            else
               mDC->DrawLine(mLeft + pos, mBottom - 2,
                             mLeft + pos, mBottom);
         }
         else 
         {
            if (mFlip)
               mDC->DrawLine(mLeft, mTop + pos,
                             mLeft + 2, mTop + pos);
            else
               mDC->DrawLine(mRight - 2, mTop + pos,
                             mRight, mTop + pos);
         }
      }

      if (mMinorLabels[i].text != wxT(""))
         mDC->DrawText(mMinorLabels[i].text,
                       mMinorLabels[i].lx,
                       mMinorLabels[i].ly);
   }

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   mDC->SetFont(*mMinorMinorFont);

   for(i=0; i<mNumMinorMinor; i++) {
      if (mMinorMinorLabels[i].text != wxT(""))
      {
         int pos = mMinorMinorLabels[i].pos;

         if( mbTicksAtExtremes || ((pos!=0)&&(pos!=iMaxPos)))
         {
            if (mOrientation == wxHORIZONTAL) 
            {
               if (mFlip)
                  mDC->DrawLine(mLeft + pos, mTop,
                                mLeft + pos, mTop + 2);
               else
                  mDC->DrawLine(mLeft + pos, mBottom - 2,
                                mLeft + pos, mBottom);
            }
            else 
            {
               if (mFlip)
                  mDC->DrawLine(mLeft, mTop + pos,
                                mLeft + 2, mTop + pos);
               else
                  mDC->DrawLine(mRight - 2, mTop + pos,
                                mRight, mTop + pos);
            }
         }
         mDC->DrawText(mMinorMinorLabels[i].text,
                       mMinorMinorLabels[i].lx,
                       mMinorMinorLabels[i].ly);
      }
   }
#endif //EXPERIMENTAL_RULER_AUTOSIZE
}

void Ruler::GetMaxSize(wxCoord *width, wxCoord *height)
{

   if (!mValid) {
      wxMemoryDC tmpDC;
      wxBitmap tmpBM(1, 1);
      tmpDC.SelectObject(tmpBM);
      mDC = &tmpDC;
      Update( NULL, 0, 0 );
   }

   if (width)
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
      *width = mRect.GetWidth(); //mMaxWidth;
#else //!EXPERIMENTAL_RULER_AUTOSIZE
      *width = mMaxWidth;
#endif //EXPERIMENTAL_RULER_AUTOSIZE

   if (height)
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
      *height = mRect.GetHeight(); //mMaxHeight;
#else //!EXPERIMENTAL_RULER_AUTOSIZE
      *height = mMaxHeight;
#endif //EXPERIMENTAL_RULER_AUTOSIZE
}


//
// RulerPanel
//

BEGIN_EVENT_TABLE(RulerPanel, wxPanel)
   EVT_ERASE_BACKGROUND(RulerPanel::OnErase)
   EVT_PAINT(RulerPanel::OnPaint)
   EVT_SIZE(RulerPanel::OnSize)
END_EVENT_TABLE()

IMPLEMENT_CLASS(RulerPanel, wxPanel)

RulerPanel::RulerPanel(wxWindow* parent, wxWindowID id,
                       const wxPoint& pos /*= wxDefaultPosition*/,
                       const wxSize& size /*= wxDefaultSize*/):
   wxPanel(parent, id, pos, size)
{
   int width, height;
   GetClientSize(&width, &height);

   ruler.SetBounds(0, 0, width-1, height-1);
}

RulerPanel::~RulerPanel()
{
}

void RulerPanel::OnErase(wxEraseEvent &evt)
{
   // Ignore it to prevent flashing
}

void RulerPanel::OnPaint(wxPaintEvent &evt)
{
   wxPaintDC dc(this);

#if defined(__WXGTK__)
   dc.SetBackground(wxBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE)));
#endif

   dc.Clear();
   ruler.Draw(dc);
}

void RulerPanel::OnSize(wxSizeEvent &evt)
{
   int width, height;
   GetClientSize(&width, &height);

   ruler.SetBounds(0, 0, width-1, height-1);

   Refresh(false);
}


/**********************************************************************

  Implementation of AdornedRulerPanel.
  Either we find a way to make this more generic, Or it will move
  out of the widgets subdirectory into its own source file.

**********************************************************************/

#include "../ViewInfo.h"
#include "../AColor.h"

BEGIN_EVENT_TABLE(AdornedRulerPanel, wxPanel)
   EVT_ERASE_BACKGROUND(AdornedRulerPanel::OnErase)
   EVT_PAINT(AdornedRulerPanel::OnPaint)
   EVT_SIZE(AdornedRulerPanel::OnSize)
   EVT_MOUSE_EVENTS(AdornedRulerPanel::OnMouseEvents)
END_EVENT_TABLE()

AdornedRulerPanel::AdornedRulerPanel(wxWindow* parent,
                                     wxWindowID id,
                                     const wxPoint& pos,
                                     const wxSize& size,
                                     ViewInfo *viewinfo):
   wxPanel( parent, id, pos, size )
{
   SetLabel( _("Vertical Ruler") );
   SetName( _("Vertical Ruler") );

   mLeftOffset = 0;
   mCurPos = -1;
   mIndPos = -1;
   mIndType = -1;
   mPlayRegionStart = -1;
   mPlayRegionEnd = -1;
   mMouseEventState = mesNone;

   mBuffer = new wxBitmap( 1, 1 );
   mViewInfo = viewinfo;

   mOuter = GetClientRect();

   mInner = mOuter;
   mInner.x += 1;          // +1 for left bevel
   mInner.y += 1;          // +1 for top bevel
   mInner.width -= 2;      // -2 for left and right bevels
   mInner.height -= 3;     // -3 for top and bottom bevels and bottom line

   ruler.SetBounds( mInner.GetLeft(),
                    mInner.GetTop(),
                    mInner.GetRight(),
                    mInner.GetBottom() );
   ruler.SetLabelEdges( false );
   ruler.SetFormat( Ruler::TimeFormat );
}

AdornedRulerPanel::~AdornedRulerPanel()
{
   delete mBuffer;
}

void AdornedRulerPanel::OnErase(wxEraseEvent &evt)
{
   // Ignore it to prevent flashing
}

void AdornedRulerPanel::OnPaint(wxPaintEvent &evt)
{
#if defined(__WXMAC__)
   wxPaintDC dc(this);
#else
   wxBufferedPaintDC dc(this);
#endif

   DoDrawBorder(&dc);

   if (mViewInfo->sel0 < mViewInfo->sel1)
   {
      DoDrawSelection(&dc);
   }

   if (mIndType >= 0)
   {
      DoDrawIndicator(&dc);
   }

   DoDrawMarks(&dc, true);

   if (mViewInfo->sel0 == mViewInfo->sel1)
   {
      DoDrawCursor(&dc);
   }
   
   DoDrawPlayRegion(&dc);
}

void AdornedRulerPanel::OnSize(wxSizeEvent &evt)
{
   mOuter = GetClientRect();

   mInner = mOuter;
   mInner.x += 1;          // +1 for left bevel
   mInner.y += 1;          // +1 for top bevel
   mInner.width -= 2;      // -2 for left and right bevels
   mInner.height -= 3;     // -3 for top and bottom bevels and bottom line

   ruler.SetBounds( mInner.GetLeft(),
                    mInner.GetTop(),
                    mInner.GetRight(),
                    mInner.GetBottom() );

   if( mBuffer )
   {
      delete mBuffer;
   }

   mBuffer = new wxBitmap( mOuter.GetWidth(), mOuter.GetHeight() );

   Refresh( false );
}

double AdornedRulerPanel::Pos2Time(int p)
{
   return (p-mLeftOffset) / mViewInfo->zoom + mViewInfo->h;
}

int AdornedRulerPanel::Time2Pos(double t)
{
   return mLeftOffset + (int)((t-mViewInfo->h) * mViewInfo->zoom + 0.5);
}

bool AdornedRulerPanel::IsWithinMarker(int mousePosX, double markerTime)
{
   if (markerTime < 0)
      return false;
      
   int pixelPos = Time2Pos(markerTime);
   int boundLeft = pixelPos - SELECT_TOLERANCE_PIXEL;
   int boundRight = pixelPos + SELECT_TOLERANCE_PIXEL;
   
   return mousePosX >= boundLeft && mousePosX < boundRight;
}

void AdornedRulerPanel::OnMouseEvents(wxMouseEvent &evt)
{
   bool isWithinStart = IsWithinMarker(evt.GetX(), mPlayRegionStart);
   bool isWithinEnd = IsWithinMarker(evt.GetX(), mPlayRegionEnd);
   
   if (isWithinStart || isWithinEnd)
      SetCursor(wxCursor(wxCURSOR_SIZEWE));
   else
      SetCursor(wxCursor(wxCURSOR_HAND));
      
   double mouseTime = Pos2Time(evt.GetX());
   if (mouseTime < 0.0) {
      mouseTime = 0.0;
   }
   
   if (evt.LeftDown())
   {
      mButtonDownMousePos = evt.GetX();

      if (isWithinStart && isWithinEnd)
      {
         // Both could be selected, check which marker is nearer
         if (fabs(mouseTime-mPlayRegionStart) < fabs(mouseTime-mPlayRegionEnd))
            mMouseEventState = mesDraggingPlayRegionStart;
         else
            mMouseEventState = mesDraggingPlayRegionEnd;
      } else if (isWithinStart)
         mMouseEventState = mesDraggingPlayRegionStart;
      else if (isWithinEnd)
         mMouseEventState = mesDraggingPlayRegionEnd;
      else {
         // First, we enter "click" mode to avoid selecting a small range
         // accidentially
         mMouseEventState = mesSelectingPlayRegionClick;
         mPlayRegionStart = mouseTime;
         mPlayRegionEnd = mouseTime;
         Refresh();
      }
         
      CaptureMouse();
   }
   
   switch (mMouseEventState)
   {
   case mesNone:
      // do nothing
      break;
   case mesDraggingPlayRegionStart:
      mPlayRegionStart = mouseTime;
      Refresh();
      break;
   case mesDraggingPlayRegionEnd:
      mPlayRegionEnd = mouseTime;
      Refresh();
      break;
   case mesSelectingPlayRegionClick:
      if (abs(evt.GetX() - mButtonDownMousePos) > SELECT_TOLERANCE_PIXEL)
      {
         // User moved mouse at least SELECT_TOLERANCE_PIXEL, so change
         // from "click" mode to "range" mode to allow selecting a range
         mMouseEventState = mesSelectingPlayRegionRange;
         mPlayRegionEnd = mouseTime;
         Refresh();
      }
      break;
   case mesSelectingPlayRegionRange:
      mPlayRegionEnd = mouseTime;
      Refresh();
      break;
   }
   
   if (evt.LeftUp())
   {
      if (HasCapture())
         ReleaseMouse();

      if (mPlayRegionStart < 0 && mPlayRegionEnd >= 0)
      {
         // This makes no sense, remove play region
         mPlayRegionStart = -1;
         mPlayRegionEnd = -1;
      }
      
      if (mPlayRegionEnd < mPlayRegionStart)
      {
         // Swap values to make sure mPlayRegionStart > mPlayRegionEnd
         double tmp = mPlayRegionStart;
         mPlayRegionStart = mPlayRegionEnd;
         mPlayRegionEnd = tmp;
      }
      
      bool startPlaying = mPlayRegionStart >= 0 && 
         (mMouseEventState == mesSelectingPlayRegionClick ||
          mMouseEventState == mesSelectingPlayRegionRange);
          
      mMouseEventState = mesNone;
      
      if (startPlaying)
      {
         ControlToolBar* ctb = mProject->GetControlToolBar();
         ctb->StopPlaying();
         ctb->PlayDefault();
      }
   }
}

void AdornedRulerPanel::DoDrawPlayRegion(wxDC * dc)
{
   double start, end;
   GetPlayRegion(&start, &end);

   if (start >= 0)
   {
      int x1 = Time2Pos(start) + 1;
      int x2 = Time2Pos(end);
      int y = mInner.height/2;

      bool isLocked = mProject->IsPlayRegionLocked();
      AColor::PlayRegionColor(dc, isLocked);
   
      wxPoint tri[3];
      wxRect r;

      tri[0].x = x1;
      tri[0].y = y + PLAY_REGION_GLOBAL_OFFSET_Y;
      tri[1].x = x1 + PLAY_REGION_TRIANGLE_SIZE;
      tri[1].y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      tri[2].x = x1 + PLAY_REGION_TRIANGLE_SIZE;
      tri[2].y = y + PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      dc->DrawPolygon(3, tri);

      r.x = x1;
      r.y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
      r.width = PLAY_REGION_RECT_WIDTH;
      r.height = PLAY_REGION_TRIANGLE_SIZE*2 + 1;
      dc->DrawRectangle(r);
   
      if (end != start)
      {
         tri[0].x = x2;
         tri[0].y = y + PLAY_REGION_GLOBAL_OFFSET_Y;
         tri[1].x = x2 - PLAY_REGION_TRIANGLE_SIZE;
         tri[1].y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         tri[2].x = x2 - PLAY_REGION_TRIANGLE_SIZE;
         tri[2].y = y + PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         dc->DrawPolygon(3, tri);

         r.x = x2 - PLAY_REGION_RECT_WIDTH + 1;
         r.y = y - PLAY_REGION_TRIANGLE_SIZE + PLAY_REGION_GLOBAL_OFFSET_Y;
         r.width = PLAY_REGION_RECT_WIDTH;
         r.height = PLAY_REGION_TRIANGLE_SIZE*2 + 1;
         dc->DrawRectangle(r);
   
         r.x = x1 + PLAY_REGION_TRIANGLE_SIZE;
         r.y = y - PLAY_REGION_RECT_HEIGHT/2 + PLAY_REGION_GLOBAL_OFFSET_Y;
         r.width = x2-x1 - PLAY_REGION_TRIANGLE_SIZE*2;
         r.height = PLAY_REGION_RECT_HEIGHT;
         dc->DrawRectangle(r);
      }
   }
}

void AdornedRulerPanel::DoDrawBorder(wxDC * dc)
{
   // Draw AdornedRulerPanel border
   AColor::MediumTrackInfo( dc, false );
   dc->DrawRectangle( mInner );

   wxRect r = mOuter;
   r.width -= 1;                 // -1 for bevel
   r.height -= 2;                // -2 for bevel and for bottom line
   AColor::BevelTrackInfo( *dc, true, r );

   dc->SetPen( *wxBLACK_PEN );
   dc->DrawLine( mOuter.x,
                 mOuter.y + mOuter.height - 1,
                 mOuter.x + mOuter.width,
                 mOuter.y + mOuter.height - 1 );
}

void AdornedRulerPanel::DoDrawMarks(wxDC * dc, bool /*text */ )
{
   double min = mViewInfo->h - mLeftOffset / mViewInfo->zoom;
   double max = min + mInner.width / mViewInfo->zoom;
   
   ruler.SetTickColour( theTheme.Colour( clrTrackPanelText ) );
   ruler.SetRange( min, max );
   ruler.Draw( *dc );
}

void AdornedRulerPanel::DrawSelection()
{
   Refresh(false);
}

void AdornedRulerPanel::DoDrawSelection(wxDC * dc)
{
   // Draw selection
   double zoom = mViewInfo->zoom;
   double sel0 = mViewInfo->sel0 - mViewInfo->h + mLeftOffset / zoom;
   double sel1 = mViewInfo->sel1 - mViewInfo->h + mLeftOffset / zoom;

   if( sel0 < 0.0 )
      sel0 = 0.0;

   if( sel1 > ( mInner.width / zoom ) )
      sel1 = mInner.width / zoom;

   int p0 = int ( sel0 * zoom + 1.5 );
   int p1 = int ( sel1 * zoom + 2.5 );

   dc->SetBrush( wxBrush( theTheme.Colour( clrRulerBackground )) );
   dc->SetPen(   wxPen(   theTheme.Colour( clrRulerBackground )) );

   wxRect r;
   r.x = p0;
   r.y = 1;
   r.width = p1 - p0 - 1;
   r.height = mInner.height;
   dc->DrawRectangle( r );
}

void AdornedRulerPanel::DrawCursor(double pos)
{
   mCurPos = pos;

   Refresh(false);
}

void AdornedRulerPanel::DoDrawCursor(wxDC * dc)
{
   int x = mLeftOffset + int ( ( mCurPos - mViewInfo->h ) * mViewInfo->zoom );

   // Draw cursor in ruler
   dc->DrawLine( x, 1, x, mInner.height );
}

//
//This draws the little triangular indicator on the 
//AdornedRulerPanel.
//
void AdornedRulerPanel::ClearIndicator()
{
   mIndType = -1;

   Refresh(false);
}

void AdornedRulerPanel::DrawIndicator( double pos, bool rec )
{
   mIndPos = pos;

   if( mIndPos < 0 )
   {
      ClearIndicator();
      return;
   }

   mIndType = ( rec ? 1 : 0 );

   Refresh(false);
}

void AdornedRulerPanel::DoDrawIndicator(wxDC * dc)
{
   if( mIndType < 0 )
   {
      return;
   }

   int indsize = 6;
   int x = mLeftOffset + int ( ( mIndPos - mViewInfo->h ) * mViewInfo->zoom );

   wxPoint tri[ 3 ];
   tri[ 0 ].x = x - indsize;
   tri[ 0 ].y = 1;
   tri[ 1 ].x = x + indsize;
   tri[ 1 ].y = 1;
   tri[ 2 ].x = x;
   tri[ 2 ].y = ( indsize * 3 ) / 2 + 1;

   AColor::IndicatorColor( dc, ( mIndType ? true : false ) );
   dc->DrawPolygon( 3, tri );
}

void AdornedRulerPanel::SetPlayRegion(double playRegionStart,
                                      double playRegionEnd)
{
   // This is called by AudacityProject to make the play region follow
   // the current selection. But while the user is selecting a play region
   // with the mouse directly in the ruler, changes from outside are blocked.
   if (mMouseEventState != mesNone)
      return;
      
   mPlayRegionStart = playRegionStart;
   mPlayRegionEnd = playRegionEnd;
   Refresh();
}

void AdornedRulerPanel::ClearPlayRegion()
{
   mPlayRegionStart = -1;
   mPlayRegionEnd = -1;
   Refresh();
}

void AdornedRulerPanel::GetPlayRegion(double* playRegionStart,
                                      double* playRegionEnd)
{
   if (mPlayRegionStart >= 0 && mPlayRegionEnd >= 0 &&
       mPlayRegionEnd < mPlayRegionStart)
   {
      // swap values to make sure end > start
      *playRegionStart = mPlayRegionEnd;
      *playRegionEnd = mPlayRegionStart;
   } else
   {
      *playRegionStart = mPlayRegionStart;
      *playRegionEnd = mPlayRegionEnd;
   }
}

#ifdef EXPERIMENTAL_RULER_AUTOSIZE
void AdornedRulerPanel::GetMaxSize(wxCoord *width, wxCoord *height)
{
   ruler.GetMaxSize(width, height);
}
#endif //EXPERIMENTAL_RULER_AUTOSIZE

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 126e06c2-f0c8-490f-bdd6-12581013f13f


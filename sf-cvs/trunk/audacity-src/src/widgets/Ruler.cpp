/**********************************************************************

  Audacity: A Digital Audio Editor

  Ruler.cpp

  Dominic Mazzoni

  This file contains the implementations of Ruler and RulerPanel.
  See Ruler.h for information about what these classes are for.

**********************************************************************/

#include <math.h>

#include <wx/dcscreen.h>

#include "Ruler.h"

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
   mLog = false;
   mUnits = "";

   int fontSize = 10;
#ifdef __WXMSW__
   fontSize = 8;
#endif

   mMinorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
   mMajorFont = new wxFont(fontSize, wxSWISS, wxNORMAL, wxBOLD);

   mMajorLabels = 0;
   mMinorLabels = 0;
   
   mValid = false;
}

Ruler::~Ruler()
{
   Invalidate();  // frees up our arrays

   delete mMinorFont;
   delete mMajorFont;
}

void Ruler::SetFormat(RulerFormat format)
{
   // IntFormat, RealFormat, or TimeFormat

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

void Ruler::SetFonts(wxFont &minorFont, wxFont &majorFont)
{
   *mMinorFont = minorFont;
   *mMajorFont = majorFont;

   Invalidate();
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

   if (mMajorLabels) {
      delete [] mMajorLabels;
      mMajorLabels = NULL;
   }
   if (mMinorLabels) {
      delete [] mMinorLabels;
      mMinorLabels = NULL;
   }
}

void Ruler::FindTickSizes()
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
   double units = 16 * fabs(mUPP);

   mDigits = 0;

   switch(mFormat) {
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
      // like 

   case RealFormat:
      d = 0.000001;
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
      }
      break;

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
      s.Printf("%d", (int)floor(d+0.5));
      break;
   case RealFormat:
      if (mMinor >= 1.0)
         s.Printf("%d", (int)floor(d+0.5));
      else {
         wxString format;
         format.Printf("%%.%dlf", mDigits);
         s.Printf(format, d);
      }
      break;
   case TimeFormat:
      if (major) {
         if (d < 0) {
            s = "-";
            d = -d;
         }

         if (mMinor >= 3600.0) {
            int hrs = (int)(d / 3600.0 + 0.5);
            wxString h;
            h.Printf("%d:00:00", hrs);
            s += h;
         }
         else if (mMinor >= 60.0) {
            int minutes = (int)(d / 60.0 + 0.5);
            wxString m;
            if (minutes >= 60)
               m.Printf("%d:%02d:00", minutes/60, minutes%60);
            else
               m.Printf("%d:00", minutes);
            s += m;
         }
         else if (mMinor >= 1.0) {
            int secs = (int)(d + 0.5);
            wxString t;
            if (secs >= 3600)
               t.Printf("%d:%02d:%02d", secs/3600, (secs/60)%60, secs%60);
            else if (secs >= 60)
               t.Printf("%d:%02d", secs/60, secs%60);
            else
               t.Printf("%d", secs);
            s += t;
         }
         else {
            int secs = (int)(d);
            wxString t1, t2, format;
            if (secs >= 3600)
               t1.Printf("%d:%02d:", secs/3600, (secs/60)%60);
            else if (secs >= 60)
               t1.Printf("%d:", secs/60);

            if (secs >= 60)
               format.Printf("%%0%d.%dlf", mDigits+3, mDigits);
            else
               format.Printf("%%%d.%dlf", mDigits+3, mDigits);
            t2.Printf((const char *)format, fmod(d, 60.0));

            s += t1 + t2;
         }
      }
      else {
      }
   }
   
   if (mUnits != "")
      s = (s + " " + mUnits);

   return s;
}

void Ruler::Tick(int pos, double d, bool major)
{
   wxString l;
   wxCoord strW, strH;
   int strPos, strLen, strLeft, strTop;

   if (major)
      mMajorLabels[mNumMajor++].pos = pos;
   else
      mMinorLabels[mNumMinor++].pos = pos;

   mDC->SetFont(major? *mMajorFont: *mMinorFont);

   l = LabelString(d, major);
   mDC->GetTextExtent(l, &strW, &strH);

   if (mOrientation == wxHORIZONTAL) {
      strLen = strW;
      strPos = pos - strW/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strW >= mLength)
         strPos = mLength - strW;
      strLeft = mLeft + strPos;
      strTop = mBottom - strH - 6;
   }
   else {
      strLen = strH;
      strPos = pos - strH/2;
      if (strPos < 0)
         strPos = 0;
      if (strPos + strH >= mLength)
         strPos = mLength - strH;
      strTop = mTop + strPos;
      strLeft = mRight - strW - 6;
   }

   // See if any of the pixels we need to draw this
   // label is already covered

   int i;
   for(i=0; i<strLen; i++)
      if (mBits[strPos+i])
         return;

   // If not, add the label

   Label *label;
   if (major)
      label = &mMajorLabels[mNumMajor-1];
   else
      label = &mMinorLabels[mNumMinor-1];

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
}

void Ruler::Update()
{
   // This gets called when something has been changed
   // (i.e. we've been invalidated).  Recompute all
   // tick positions.

   if (mOrientation == wxHORIZONTAL)
      mLength = mRight-mLeft;
   else
      mLength = mBottom-mTop;

   mUPP = (mMax-mMin)/mLength;

   int i;
   mBits = new int[mLength+1];
   for(i=0; i<=mLength; i++)
      mBits[i] = 0;

   mNumMajor = 0;
   mMajorLabels = new Label[mLength+1];
   mNumMinor = 0;
   mMinorLabels = new Label[mLength+1];

   FindTickSizes();

   // Left and Right Edges
   if (mLabelEdges) {
      Tick(0, mMin, true);
      Tick(mLength, mMax, true);
   }

   // Zero (if it's in the middle somewhere)
   if (mMin * mMax < 0.0) {
      int mid = (int)(mLength*(mMin/(mMin-mMax)) + 0.5);
      Tick(mid, 0.0, true);
   }
   
   double sg = mUPP > 0.0? 1.0: -1.0;

   // Major ticks
   double d = mMin - mUPP/2;
   int majorInt = (int)floor(sg * d / mMajor);
   i = -1;
   while(i <= mLength) {
      i++;
      d += mUPP;

      if ((int)floor(sg * d / mMajor) > majorInt) {
         majorInt = (int)floor(sg * d / mMajor);
         Tick(i, majorInt * mMajor, true);
      }
   }

   // Minor ticks
   d = mMin - mUPP/2;
   int minorInt = (int)floor(sg * d / mMinor);
   i = -1;
   while(i <= mLength) {
      i++;
      d += mUPP;

      if ((int)floor(sg * d / mMinor) > minorInt) {
         minorInt = (int)floor(sg * d / mMinor);
         Tick(i, minorInt * mMinor, false);
      }
   }

   mValid = true;
}

void Ruler::Draw(wxDC& dc)
{
   mDC = &dc;

   if (!mValid)
      Update();

   mDC->SetPen(*wxBLACK_PEN);
   mDC->SetTextForeground(*wxBLACK);

   if (mOrientation == wxHORIZONTAL)
      mDC->DrawLine(mLeft, mBottom, mRight+1, mBottom);
   else
      mDC->DrawLine(mRight, mTop, mRight, mBottom+1);

   int i;

   mDC->SetFont(*mMajorFont);

   for(i=0; i<mNumMajor; i++) {
      int pos = mMajorLabels[i].pos;

      if (mOrientation == wxHORIZONTAL)
         mDC->DrawLine(mLeft + pos, mBottom - 4,
                       mLeft + pos, mBottom);
      else
         mDC->DrawLine(mRight - 4, mTop + pos,
                       mRight, mTop + pos);

      if (mMajorLabels[i].text)
         mDC->DrawText(mMajorLabels[i].text,
                       mMajorLabels[i].lx,
                       mMajorLabels[i].ly);
   }

   mDC->SetFont(*mMinorFont);

   for(i=0; i<mNumMinor; i++) {
      int pos = mMinorLabels[i].pos;

      if (mOrientation == wxHORIZONTAL)
         mDC->DrawLine(mLeft + pos, mBottom - 2,
                       mLeft + pos, mBottom);
      else
         mDC->DrawLine(mRight - 2, mTop + pos,
                       mRight, mTop + pos);

      if (mMinorLabels[i].text)
         mDC->DrawText(mMinorLabels[i].text,
                       mMinorLabels[i].lx,
                       mMinorLabels[i].ly);
   }
}

//
// RulerPanel
//

BEGIN_EVENT_TABLE(RulerPanel, wxPanel)
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

   ruler.SetBounds(0, 0, width, height);
}

RulerPanel::~RulerPanel()
{
}

void RulerPanel::OnPaint(wxPaintEvent &evt)
{
   wxPaintDC dc(this);

   ruler.Draw(dc);
}

void RulerPanel::OnSize(wxSizeEvent &evt)
{
   int width, height;
   GetClientSize(&width, &height);

   ruler.SetBounds(0, 0, width-1, height-1);

   Refresh(false);
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqWindow.cpp

  Dominic Mazzoni

**********************************************************************/

#include "FreqWindow.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/brush.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/msgdlg.h>
#include <wx/file.h>
#include <wx/filedlg.h>
#include <wx/intl.h>
#endif

#include <wx/textfile.h>

#include <math.h>

#include "AColor.h"
#include "FFT.h"
#include "Internat.h"
#include "PitchName.h"


enum {
   FirstID = 7000,

   FreqCloseButtonID,
   FreqExportButtonID,
   FreqAlgChoiceID,
   FreqSizeChoiceID,
   FreqFuncChoiceID,
   FreqAxisChoiceID
};

FreqWindow *gFreqWindow = NULL;

// These specify the minimum plot window width

#define FREQ_WINDOW_WIDTH 480
#define FREQ_WINDOW_HEIGHT 330

void InitFreqWindow(wxWindow * parent)
{
   if (gFreqWindow)
      return;

   wxPoint where;

   where.x = 150;
   where.y = 150;

   gFreqWindow = new FreqWindow(parent, -1, _("Frequency Analysis"), where);
}

// FreqWindow

BEGIN_EVENT_TABLE(FreqWindow, wxFrame)
    EVT_CLOSE(FreqWindow::OnCloseWindow)
    EVT_SIZE(FreqWindow::OnSize)
    EVT_BUTTON(FreqCloseButtonID, FreqWindow::OnCloseButton)
    EVT_BUTTON(FreqExportButtonID, FreqWindow::OnExport)
    EVT_CHOICE(FreqAlgChoiceID, FreqWindow::OnAlgChoice)
    EVT_CHOICE(FreqSizeChoiceID, FreqWindow::OnSizeChoice)
    EVT_CHOICE(FreqFuncChoiceID, FreqWindow::OnFuncChoice)
    EVT_CHOICE(FreqAxisChoiceID, FreqWindow::OnAxisChoice)
END_EVENT_TABLE()

FreqWindow::FreqWindow(wxWindow * parent, wxWindowID id,
                           const wxString & title,
                           const wxPoint & pos):
  wxFrame(parent, id, title, pos),
  mData(NULL), mProcessed(NULL), mBitmap(NULL)
{
   mMouseX = 0;
   mMouseY = 0;

   mArrowCursor = new wxCursor(wxCURSOR_ARROW);
   mCrossCursor = new wxCursor(wxCURSOR_CROSS);

   mLeftMargin = 40;
   mBottomMargin = 20;

   mFreqPlot = new FreqPlot(this, 0,
                            wxDefaultPosition, wxDefaultSize);

   wxString algChoiceStrings[5] = { _("Spectrum"),
      _("Standard Autocorrelation"),
      _("Cuberoot Autocorrelation"),
      _("Enhanced Autocorrelation"),
     /* i18n-hint: This is a technical term, derived from the word
        "spectrum".  Do not translate it unless you are sure you
        know the correct technical word in your language. */
      _("Cepstrum")
   };

   mAlgChoice = new wxChoice(this, FreqAlgChoiceID,
                             wxDefaultPosition, wxDefaultSize,
                             4, algChoiceStrings);

   mAlgChoice->SetSelection(0);

   wxString sizeChoiceStrings[8] = { wxT("128"),
      wxT("256"),
      wxT("512"),
      wxT("1024"),
      wxT("2048"),
      wxT("4096"),
      wxT("8192"),
      wxT("16384")
   };

   mSizeChoice = new wxChoice(this, FreqSizeChoiceID,
                              wxDefaultPosition, wxDefaultSize,
                              8, sizeChoiceStrings);

   mSizeChoice->SetSelection(2);

   int f = NumWindowFuncs();

   wxString *funcChoiceStrings = new wxString[f];
   for (int i = 0; i < f; i++) {
      /* i18n-hint: This refers to a "window function", used in the
         Frequency analyze dialog box. */
      funcChoiceStrings[i] = WindowFuncName(i) + wxString(_(" window"));
   }

   mFuncChoice = new wxChoice(this, FreqFuncChoiceID,
                              wxDefaultPosition, wxDefaultSize,
                              f, funcChoiceStrings);

   mFuncChoice->SetSelection(3);
   delete[]funcChoiceStrings;

   wxString axisChoiceStrings[2] = { _("Linear frequency"),
      _("Log frequency")
   };

   mAxisChoice = new wxChoice(this, FreqAxisChoiceID,
                              wxDefaultPosition, wxDefaultSize,
                              2, axisChoiceStrings);

   mAxisChoice->SetSelection(0);

   mLogAxis = false;

   mExportButton = new wxButton(this, FreqExportButtonID,
                                _("Export..."));

   mCloseButton = new wxButton(this, FreqCloseButtonID,
                               _("Close"));

#ifndef TARGET_CARBON
   mCloseButton->SetDefault();
   mCloseButton->SetFocus();
#endif

   // Set minimum sizes

   mFreqPlot->SetMinSize( wxSize( FREQ_WINDOW_WIDTH, FREQ_WINDOW_HEIGHT ) );

   wxRect r( 0, 0, 0, 0 );

   r.Union( mAlgChoice->GetRect() );
   r.Union( mSizeChoice->GetRect() );
   r.Union( mFuncChoice->GetRect() );
   r.Union( mAxisChoice->GetRect() );

   mAlgChoice->SetMinSize( r.GetSize() );
   mSizeChoice->SetMinSize( r.GetSize() );
   mFuncChoice->SetMinSize( r.GetSize() );
   mAxisChoice->SetMinSize( r.GetSize() );

   // Add everything to the sizers

   wxBoxSizer *vs = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer *hs;

   vs->Add( mFreqPlot, 1, wxEXPAND | wxALL, 5 );

   hs = new wxBoxSizer( wxHORIZONTAL );
   hs->Add( mAlgChoice, 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, 5 );
   hs->Add( mSizeChoice, 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, 5 );
   hs->Add( 1, 1, 1 );
   hs->Add( mExportButton, 0, wxALIGN_CENTER | wxALIGN_RIGHT | wxLEFT | wxRIGHT, 3 );
   vs->Add( hs, 0, wxEXPAND | wxBOTTOM, 5 );

   hs = new wxBoxSizer( wxHORIZONTAL );
   hs->Add( mFuncChoice, 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, 5 );
   hs->Add( mAxisChoice, 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, 5 );
   hs->Add( 1, 1, 1 );
   hs->Add( mCloseButton, 0, wxALIGN_CENTER | wxALIGN_RIGHT | wxLEFT | wxRIGHT, 3 );
   vs->Add( hs, 0, wxEXPAND | wxBOTTOM, 5 );

   SetAutoLayout( true );
   SetSizerAndFit( vs );
   Layout();

  #ifdef __WXMAC__
   mBackgroundBrush.SetColour(wxColour(255, 255, 255));
   mBackgroundPen.SetColour(wxColour(255, 255, 255));
   SetBackgroundColour(wxColour(255, 255, 255));
  #else
   mBackgroundBrush.SetColour(wxColour(204, 204, 204));
   mBackgroundPen.SetColour(wxColour(204, 204, 204));
   SetBackgroundColour(wxColour(204, 204, 204));
  #endif
}

FreqWindow::~FreqWindow()
{
   delete mFreqPlot;
   if (mBitmap)
      delete mBitmap;
   delete mCloseButton;
   delete mAlgChoice;
   delete mSizeChoice;
   delete mFuncChoice;
   delete mArrowCursor;
   delete mCrossCursor;
   if (mData)
      delete[]mData;
}

void FreqWindow::OnSize(wxSizeEvent & event)
{
   Layout();

   mUpdateRect.x = 0;
   mUpdateRect.y = 0;
   mUpdateRect.SetSize( mFreqPlot->GetSize() );

   mInfoRect = mUpdateRect;
   mInfoRect.width -= 1;                     // -1 for AColor::Bevel()
   mInfoRect.y = mInfoRect.GetBottom() - mInfoHeight;
   mInfoRect.height -= ( mInfoRect.y + 1 );  // +1 for AColor::Bevel()

   mPlotRect = mUpdateRect;
   mPlotRect.height = mInfoRect.y - 5;

   if (mBitmap)
   {
      delete mBitmap;
      mBitmap = NULL;
   }

#ifdef __WXMAC__
   Refresh(true);
#else
   mFreqPlot->Refresh( false );
#endif
}

void FreqWindow::PlotMouseEvent(wxMouseEvent & event)
{
   if (event.Moving() && (event.m_x != mMouseX || event.m_y != mMouseY)) {
      mMouseX = event.m_x;
      mMouseY = event.m_y;

      wxRect r = mPlotRect;
      r.x += mLeftMargin;
      r.width -= mLeftMargin;
      r.height -= mBottomMargin;

      if (r.Inside(mMouseX, mMouseY))
         mFreqPlot->SetCursor(*mCrossCursor);
      else
         mFreqPlot->SetCursor(*mArrowCursor);

      mFreqPlot->Refresh(false);
   }
}

void FreqWindow::OnAlgChoice(wxCommandEvent & event)
{
   // Log-frequency axis works for spectrum plots only.
   if (mAlgChoice->GetSelection() == 0) {
      mAxisChoice->Enable(true);
      mLogAxis = (mAxisChoice->GetSelection())?true:false;
   }
   else {
      mAxisChoice->Disable();
      mLogAxis = false;
   }
   Recalc();
}

void FreqWindow::OnSizeChoice(wxCommandEvent & event)
{
   Recalc();
}

void FreqWindow::OnFuncChoice(wxCommandEvent & event)
{
   Recalc();
}

void FreqWindow::OnAxisChoice(wxCommandEvent & event)
{
   mLogAxis = (mAxisChoice->GetSelection())?true:false;

   mFreqPlot->Refresh(false);
}

// If f(0)=y0, f(1)=y1, f(2)=y2, and f(3)=y3, this function finds
// the degree-three polynomial which best fits these points and
// returns the value of this polynomial at a value x.  Usually
// 0 < x < 3

/* Declare Static functions */
static float CubicInterpolate(float y0, float y1, float y2, float y3, float x);
static float CubicMaximize(float y0, float y1, float y2, float y3);

float CubicInterpolate(float y0, float y1, float y2, float y3, float x)
{
   float a, b, c, d;

   a = y0 / -6.0 + y1 / 2.0 - y2 / 2.0 + y3 / 6.0;
   b = y0 - 5.0 * y1 / 2.0 + 2.0 * y2 - y3 / 2.0;
   c = -11.0 * y0 / 6.0 + 3.0 * y1 - 3.0 * y2 / 2.0 + y3 / 3.0;
   d = y0;

   float xx = x * x;
   float xxx = xx * x;

   return (a * xxx + b * xx + c * x + d);
}

float CubicMaximize(float y0, float y1, float y2, float y3)
{
   // Find coefficients of cubic

   float a, b, c, d;

   a = y0 / -6.0 + y1 / 2.0 - y2 / 2.0 + y3 / 6.0;
   b = y0 - 5.0 * y1 / 2.0 + 2.0 * y2 - y3 / 2.0;
   c = -11.0 * y0 / 6.0 + 3.0 * y1 - 3.0 * y2 / 2.0 + y3 / 3.0;
   d = y0;

   // Take derivative

   float da, db, dc;

   da = 3 * a;
   db = 2 * b;
   dc = c;

   // Find zeroes of derivative using quadratic equation

   float discriminant = db * db - 4 * da * dc;
   if (discriminant < 0.0)
      return float(-1.0);              // error

   float x1 = (-db + sqrt(discriminant)) / (2 * da);
   float x2 = (-db - sqrt(discriminant)) / (2 * da);

   // The one which corresponds to a local _maximum_ in the
   // cubic is the one we want - the one with a negative
   // second derivative

   float dda = 2 * da;
   float ddb = db;

   if (dda * x1 + ddb < 0)
      return x1;
   else
      return x2;
}

float FreqWindow::GetProcessedValue(float freq0, float freq1)
{
   int alg = mAlgChoice->GetSelection();

   float bin0, bin1, binwidth;

   if (alg == 0) {
      bin0 = freq0 * mWindowSize / mRate;
      bin1 = freq1 * mWindowSize / mRate;
   } else {
      bin0 = freq0 * mRate;
      bin1 = freq1 * mRate;
   }
   binwidth = bin1 - bin0;

   float value = float(0.0);

   if (binwidth < 1.0) {
      float binmid = (bin0 + bin1) / 2.0;
      int ibin = int (binmid) - 1;
      if (ibin < 1)
         ibin = 1;
      if (ibin >= mProcessedSize - 3)
         ibin = mProcessedSize - 4;

      value = CubicInterpolate(mProcessed[ibin],
                               mProcessed[ibin + 1],
                               mProcessed[ibin + 2],
                               mProcessed[ibin + 3], binmid - ibin);

   } else {
      if (int (bin1) > int (bin0))
         value += mProcessed[int (bin0)] * (int (bin0) + 1 - bin0);
      bin0 = 1 + int (bin0);
      while (bin0 < int (bin1)) {
         value += mProcessed[int (bin0)];
         bin0 += 1.0;
      }
      value += mProcessed[int (bin1)] * (bin1 - int (bin1));

      value /= binwidth;
   }

   return value;
}

void FreqWindow::PlotPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(mFreqPlot);

   if (!mBitmap)
      mBitmap = new wxBitmap(mUpdateRect.width, mUpdateRect.height);

   wxMemoryDC memDC;

   memDC.SelectObject(*mBitmap);

   memDC.SetBrush(mBackgroundBrush);
   memDC.SetPen(mBackgroundPen);
   memDC.DrawRectangle(0, 0, mUpdateRect.width, mUpdateRect.height);

   wxRect r = mPlotRect;
   r.x += mLeftMargin;
   r.width -= mLeftMargin;
   r.height -= mBottomMargin;

   memDC.SetPen(*wxBLACK_PEN);
   memDC.SetBrush(*wxWHITE_BRUSH);
   memDC.DrawRectangle(r);

   if (!mProcessed) {
      if (mData && mDataLen < mWindowSize)
         memDC.DrawText(_("Not enough data selected."), r.x + 5, r.y + 5);

      return;
   }

   float yTotal = (mYMax - mYMin);

   int alg = mAlgChoice->GetSelection();

   int i;

   wxFont freqWindowFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
   memDC.SetFont(freqWindowFont);

   // Draw y axis and gridlines

   if (alg == 0) {
      for (float yi = mYMin; yi <= mYMax; yi += mYStep) {
         i = (int) yi;
         int y =
             (int) (r.y + r.height - 1 -
                    (yi - mYMin) * (r.height - 1) / yTotal);

         // Light blue gridline
         memDC.SetPen(wxPen(wxColour(204, 204, 255), 1, wxSOLID));
         memDC.DrawLine(r.x, y, r.x + r.width, y);

         // Pure blue axis line
         memDC.SetPen(wxPen(wxColour(0, 0, 255), 1, wxSOLID));
         memDC.DrawLine(mPlotRect.x, y, r.x, y);

         if (i != (int) mYMin) {
            wxString label = wxString::Format(wxT("%d dB"), i);
            long labelWidth, labelHeight;
            memDC.GetTextExtent(label, &labelWidth, &labelHeight);
            memDC.DrawText(label, r.x - labelWidth - 2, y + 1);
         }
      }
   } else {
      wxString label;
      long labelWidth, labelHeight;

      if (fabs(mYMax) < 1.0)
         label.Printf(wxT("%.3f"), mYMax);
      else
         label.Printf(wxT("%.1f"), mYMax);
      memDC.GetTextExtent(label, &labelWidth, &labelHeight);
      memDC.DrawText(label, r.x - labelWidth - 2, r.y + 2);

      if (fabs(mYMin) < 1.0)
         label.Printf(wxT("%.3f"), mYMin);
      else
         label.Printf(wxT("%.1f"), mYMin);
      memDC.GetTextExtent(label, &labelWidth, &labelHeight);
      memDC.DrawText(label,
                     r.x - labelWidth - 2,
                     r.y + r.height - labelHeight - 2);

      if (mYMax > 0.0 && mYMin < 0.0) {
         int y = int ((mYMax / (mYMax - mYMin)) * r.height);

         if (y > labelHeight + 4 && y < r.height - 2 * labelHeight - 4) {
            label = wxT("0.0");
            memDC.GetTextExtent(label, &labelWidth, &labelHeight);
            memDC.DrawText(label,
                           r.x - labelWidth - 2, r.y + y - labelHeight);

            // Light blue gridline
            memDC.SetPen(wxPen(wxColour(204, 204, 255), 1, wxSOLID));
            memDC.DrawLine(r.x, r.y + y, r.x + r.width, r.y + y);

            // Pure blue axis line
            memDC.SetPen(wxPen(wxColour(0, 0, 255), 1, wxSOLID));
            memDC.DrawLine(mPlotRect.x, r.y + y, r.x, r.y + y);
         }
      }
   }

   // Draw x axis and gridlines

   int width = r.width - 2;

   float xMin, xMax, xPos, xRatio, xLast, xStep;

   if (alg == 0) {
      xMin = mRate / mWindowSize;
      xMax = mRate / 2;
      xRatio = xMax / xMin;
      xPos = xMin;
      xLast = xPos / 2.0;
      if (mLogAxis)
         xStep = pow(2.0f, (log(xRatio) / log(2.0f)) / width);
      else
         xStep = (xMax - xMin) / width;
   } else {
      xMin = 0;
      xMax = mProcessedSize / mRate;
      xPos = xMin;
      xLast = xPos / 2.0;
      xStep = (xMax - xMin) / width;
   }

   int nextx = 0;

   for (i = 0; i < width; i++) {

      if ((mLogAxis && xPos / xLast >= 2.0) ||
          (!mLogAxis && (i % 60) == 0)) {
         int x = i + 1;

         // Light blue gridline
         memDC.SetPen(wxPen(wxColour(204, 204, 255), 1, wxSOLID));
         memDC.DrawLine(r.x + x, r.y, r.x + x, r.y + r.height);

         if (x >= nextx) {

            // Pure blue axis line
            memDC.SetPen(wxPen(wxColour(0, 0, 255), 1, wxSOLID));
            memDC.DrawLine(r.x + x, r.y + r.height, r.x + x,
                           r.y + r.height + 15);

            // Label

            wxString label;
            if (alg == 0) {
               if (xPos < 950.0)
                  label = wxString::Format(wxT("%dHz"), int (xPos + 0.5));
               else
                  label =
                      wxString::Format(wxT("%dKHz"),
                                       int ((xPos / 1000.0) + 0.5));
            } else
               label = wxString::Format(wxT("%.4f s"), xPos);
            long labelWidth, labelHeight;
            memDC.GetTextExtent(label, &labelWidth, &labelHeight);
            if (x + labelWidth < width)
               memDC.DrawText(label, r.x + x + 3, r.y + r.height + 2);
            nextx = x + labelWidth + 4;
         }

         xLast *= 2.0;
      }

      if (mLogAxis)
         xPos *= xStep;
      else
         xPos += xStep;
   }

   // Draw the plot

   if (alg == 0)
      memDC.SetPen(wxPen(wxColour(140, 60, 190), 1, wxSOLID));
   else
      memDC.SetPen(wxPen(wxColour(200, 50, 150), 1, wxSOLID));

   xPos = xMin;

   for (i = 0; i < width; i++) {
      float y;

      if (mLogAxis)
         y = GetProcessedValue(xPos, xPos * xStep);
      else
         y = GetProcessedValue(xPos, xPos + xStep);

      float ynorm = (y - mYMin) / yTotal;

      int lineheight = int (ynorm * (r.height - 1));

      if (lineheight > r.height - 2)
         lineheight = r.height - 2;

      if (ynorm > 0.0)
         memDC.DrawLine(r.x + 1 + i, r.y + r.height - 1 - lineheight,
                        r.x + 1 + i, r.y + r.height - 1);

      if (mLogAxis)
         xPos *= xStep;
      else
         xPos += xStep;
   }

   // Find the peak nearest the cursor and plot it

   float bestpeak = float(0.0);
   if (r.Inside(mMouseX, mMouseY)) {
      if (mLogAxis)
         xPos = xMin * pow(xStep, mMouseX - (r.x + 1));
      else
         xPos = xMin + xStep * (mMouseX - (r.x + 1));

      bool up = (mProcessed[1] > mProcessed[0]);
      float bestdist = 1000000;
      for (int bin = 2; bin < mProcessedSize; bin++) {
         bool nowUp = mProcessed[bin] > mProcessed[bin - 1];
         if (!nowUp && up) {
            // Local maximum.  Find actual value by cubic interpolation
            int leftbin = bin - 2;
            if (leftbin < 1)
               leftbin = 1;
            float max = leftbin + CubicMaximize(mProcessed[leftbin],
                                                mProcessed[leftbin + 1],
                                                mProcessed[leftbin + 2],
                                                mProcessed[leftbin + 3]);

            float thispeak;
            if (alg == 0)
               thispeak = max * mRate / mWindowSize;
            else
               thispeak = max / mRate;

            if (fabs(thispeak - xPos) < bestdist) {
               bestpeak = thispeak;
               bestdist = fabs(thispeak - xPos);
               if (thispeak > xPos)
                  break;
            }
         }
         up = nowUp;
      }

      int px;
      if (mLogAxis)
         px = int (log(bestpeak / xMin) / log(xStep));
      else
      px = int ((bestpeak - xMin) * width / (xMax - xMin));

      memDC.SetPen(wxPen(wxColour(160,160,160), 1, wxSOLID));
      memDC.DrawLine(r.x + 1 + px, r.y, r.x + 1 + px, r.y + r.height);
   }

   // Outline the graph
   memDC.SetPen(*wxBLACK_PEN);
   memDC.SetBrush(*wxTRANSPARENT_BRUSH);
   memDC.DrawRectangle(r);

   // Draw the bevel around the info rect
   AColor::Bevel( memDC, false, mInfoRect );

   // If the mouse cursor is pointing inside the graph, print out info about the
   // cursor location

   if (r.Inside(mMouseX, mMouseY)) {
      float value;

      if (mLogAxis) {
         xPos = xMin * pow(xStep, mMouseX - (r.x + 1));
         value = GetProcessedValue(xPos, xPos * xStep);
      } else {
         xPos = xMin + xStep * (mMouseX - (r.x + 1));
         value = GetProcessedValue(xPos, xPos + xStep);
      }

      wxString info;

      if (alg == 0) {
         wxString xpitch = PitchName_Absolute(Freq2Pitch(xPos));
         wxString peakpitch = PitchName_Absolute(Freq2Pitch(bestpeak));
         const wxChar *xp = xpitch.c_str();
         const wxChar *pp = peakpitch.c_str();
         info.Printf(_("Cursor: %d Hz (%s) = %d dB    Peak: %d Hz (%s)"),
                     int (xPos + 0.5),
                     xp, int (value + 0.5), int (bestpeak + 0.5), pp);
      } else if (xPos > 0.0 && bestpeak > 0.0) {
         wxString xpitch = PitchName_Absolute(Freq2Pitch(1.0 / xPos));
         wxString peakpitch = PitchName_Absolute(Freq2Pitch(1.0 / bestpeak));
         const wxChar *xp = xpitch.c_str();
         const wxChar *pp = peakpitch.c_str();
         info.Printf(_("Cursor: %.4f sec (%d Hz) (%s) = %f,    Peak: %.4f sec (%d Hz) (%s)"),
                     xPos,
                     int (1.0 / xPos + 0.5),
                     xp, value, bestpeak, int (1.0 / bestpeak + 0.5), pp);
      }

      memDC.DrawText(info, mInfoRect.x + 2, mInfoRect.y + 2);
   }

   dc.Blit(0, 0, mUpdateRect.width, mUpdateRect.height,
           &memDC, 0, 0, wxCOPY, FALSE);
}

void FreqWindow::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
   this->Show(FALSE);
}

void FreqWindow::OnCloseButton(wxCommandEvent & WXUNUSED(event))
{
   this->Show(FALSE);
}

void FreqWindow::Plot(int len, float *data, double rate)
{
   mRate = rate;
   mDataLen = len;
   if (mData)
      delete[]mData;
   mData = new float[len];
   for (int i = 0; i < len; i++)
      mData[i] = data[i];
   Recalc();
}

void FreqWindow::Recalc()
{
   if (mProcessed)
      delete mProcessed;
   mProcessed = NULL;

   if (!mData) {
      mFreqPlot->Refresh(false);
      return;
   }

   int alg = mAlgChoice->GetSelection();
   int windowFunc = mFuncChoice->GetSelection();
   long windowSize = 0;
   (mSizeChoice->GetStringSelection()).ToLong(&windowSize);

   if (!(windowSize >= 32 && windowSize <= 65536 &&
         alg >= 0 && alg <= 3 && windowFunc >= 0 && windowFunc <= 3)) {
      mFreqPlot->Refresh(false);
      return;
   }

   mWindowSize = windowSize;

   if (mDataLen < mWindowSize) {
      mFreqPlot->Refresh(false);
      return;
   }

   mProcessed = new float[mWindowSize];

   int i;
   for (i = 0; i < mWindowSize; i++)
      mProcessed[i] = float(0.0);
   int half = mWindowSize / 2;

   float *in = new float[mWindowSize];
   float *in2 = new float[mWindowSize];
   float *out = new float[mWindowSize];
   float *out2 = new float[mWindowSize];

   int start = 0;
   int windows = 0;
   while (start + mWindowSize <= mDataLen) {
      for (i = 0; i < mWindowSize; i++)
         in[i] = mData[start + i];

      WindowFunc(windowFunc, mWindowSize, in);

      switch (alg) {
      case 0:                  // Spectrum
         PowerSpectrum(mWindowSize, in, out);

         for (i = 0; i < half; i++)
            mProcessed[i] += out[i];
         break;

      case 1:
      case 2:
      case 3:   // Autocorrelation, Cuberoot AC or Enhanced AC

         // Take FFT
         FFT(mWindowSize, false, in, NULL, out, out2);

         // Compute power
         for (i = 0; i < mWindowSize; i++)
            in[i] = (out[i] * out[i]) + (out2[i] * out2[i]);

         if (alg == 1) {
            for (i = 0; i < mWindowSize; i++)
               in[i] = sqrt(in[i]);
         }
         if (alg == 2 || alg == 3) {
            // Tolonen and Karjalainen recommend taking the cube root
            // of the power, instead of the square root

            for (i = 0; i < mWindowSize; i++)
               in[i] = pow(in[i], 1.0f / 3.0f);
         }
         // Take FFT
         FFT(mWindowSize, false, in, NULL, out, out2);

         // Take real part of result
         for (i = 0; i < half; i++)
            mProcessed[i] += out[i];
         break;

      case 4:                  // Cepstrum
         FFT(mWindowSize, false, in, NULL, out, out2);

         // Compute log power
         for (i = 0; i < mWindowSize; i++)
            in[i] = log((out[i] * out[i]) + (out2[i] * out2[i]));

         // Take IFFT
         FFT(mWindowSize, true, in, NULL, out, out2);

         // Take real part of result
         for (i = 0; i < half; i++)
            mProcessed[i] += out[i];

         break;
      }                         //switch

      start += half;
      windows++;
   }

   switch (alg) {
   case 0:                     // Spectrum
      // Convert to decibels
      for (i = 0; i < half; i++)
         mProcessed[i] = 10 * log10(mProcessed[i] / mWindowSize / windows);

      mProcessedSize = half;
      mYMin = -90;
      mYMax = 10;
      mYStep = 10;
      break;

   case 1:                     // Standard Autocorrelation
   case 2:                     // Cuberoot Autocorrelation
      for (i = 0; i < half; i++)
         mProcessed[i] = mProcessed[i] / windows;

      // Find min/max
      mYMin = mProcessed[0];
      mYMax = mProcessed[0];
      for (i = 1; i < half; i++)
         if (mProcessed[i] > mYMax)
            mYMax = mProcessed[i];
         else if (mProcessed[i] < mYMin)
            mYMin = mProcessed[i];

      mYStep = 1;

      mProcessedSize = half;
      break;

   case 3:                     // Enhanced Autocorrelation
      for (i = 0; i < half; i++)
         mProcessed[i] = mProcessed[i] / windows;

      // Peak Pruning as described by Tolonen and Karjalainen, 2000

      // Clip at zero, copy to temp array
      for (i = 0; i < half; i++) {
         if (mProcessed[i] < 0.0)
            mProcessed[i] = float(0.0);
         out[i] = mProcessed[i];
      }

      // Subtract a time-doubled signal (linearly interp.) from the original
      // (clipped) signal
      for (i = 0; i < half; i++)
         if ((i % 2) == 0)
            mProcessed[i] -= out[i / 2];
         else
            mProcessed[i] -= ((out[i / 2] + out[i / 2 + 1]) / 2);

      // Clip at zero again
      for (i = 0; i < half; i++)
         if (mProcessed[i] < 0.0)
            mProcessed[i] = float(0.0);

      // Find new min/max
      mYMin = mProcessed[0];
      mYMax = mProcessed[0];
      for (i = 1; i < half; i++)
         if (mProcessed[i] > mYMax)
            mYMax = mProcessed[i];
         else if (mProcessed[i] < mYMin)
            mYMin = mProcessed[i];

      mYStep = 1;

      mProcessedSize = half;
      break;

   case 4:                     // Cepstrum
      for (i = 0; i < half; i++)
         mProcessed[i] = mProcessed[i] / windows;

      // Find min/max, ignoring first and last few values
      int ignore = 4;
      mYMin = mProcessed[ignore];
      mYMax = mProcessed[ignore];
      for (i = ignore + 1; i < half - ignore; i++)
         if (mProcessed[i] > mYMax)
            mYMax = mProcessed[i];
         else if (mProcessed[i] < mYMin)
            mYMin = mProcessed[i];

      mYStep = 1;

      mProcessedSize = half;
      break;
   }

   delete[]in;
   delete[]in2;
   delete[]out;
   delete[]out2;

   // This rest of this really doesn't belong here, but this routine
   // is called in all the right places and the sizes are based on
   // the calculations performed.  So, it's just convenient.

   // Recalculate the margin sizes

   wxMemoryDC memDC;
   wxString label;

   wxFont freqWindowFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
   memDC.SetFont(freqWindowFont);

   wxRect mr( 0, 0, 0, 0 );
   wxRect lr( 0, 0, 0, 0 );
   if(alg == 0)
   {
      label = wxString::Format(wxT("%d dB"), (int)mYMin);
      memDC.GetTextExtent(label, &lr.width, &lr.height);
      mr.Union(lr);

      label = wxString::Format(wxT("%d dB"), (int)mYMax);
      memDC.GetTextExtent(label, &lr.width, &lr.height);
      mr.Union(lr);
   }
   else
   {
      label.Printf(fabs(mYMin) < 1.0 ? wxT("%.3f") : wxT("%.1f"), mYMin);
      memDC.GetTextExtent(label, &lr.width, &lr.height);
      mr.Union(lr);
 
      label.Printf(fabs(mYMax) < 1.0 ? wxT("%.3f") : wxT("%.1f"), mYMax);
      memDC.GetTextExtent(label, &lr.width, &lr.height);
      mr.Union(lr);
   }
   mLeftMargin = mr.width + 2;
   mBottomMargin = mr.height + 2;

   // This looks fairly silly, but it was done to keep the string
   // the same as the one actually used.  No additional translation
   // required.
   wxChar *p = PitchName_Absolute(440.0);
   label.Printf(_("Cursor: %d Hz (%s) = %d dB    Peak: %d Hz (%s)"),
                  0, p, 0, 0, p);
   memDC.GetTextExtent(label, &lr.width, &lr.height);
   mInfoHeight = lr.height + 4; // +4 to allow for top and bottoms margins

   mFreqPlot->Refresh(false);
}

void FreqWindow::OnExport(wxCommandEvent & WXUNUSED(event))
{
   wxString fName = _("spectrum.txt");

   fName = wxFileSelector(_("Export Spectral Data As:"),
                          NULL, fName, wxT("txt"), wxT("*.txt"), wxSAVE, this);

   if (fName == wxT(""))
      return;

   wxTextFile f(FILENAME(fName).c_str());
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(FILENAME(fName));
   delete temp;
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   if (mAlgChoice->GetSelection() == 0) {
      f.AddLine(_("Frequency (Hz)\tLevel (dB)"));
      for (int i = 1; i < mProcessedSize; i++)
         f.AddLine(wxString::
                   Format(wxT("%f\t%f"), i * mRate / mWindowSize,
                          mProcessed[i]));
   } else {
      f.AddLine(_("Lag (seconds)\tFrequency (Hz)\tLevel"));
      for (int i = 1; i < mProcessedSize; i++)
         f.AddLine(wxString::Format(wxT("%f\t%f\t%f"),
                                    i / mRate, 1.0 / i, mProcessed[i]));
   }

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
   f.Close();
}


BEGIN_EVENT_TABLE(FreqPlot, wxWindow)
    EVT_PAINT(FreqPlot::OnPaint)
    EVT_MOUSE_EVENTS(FreqPlot::OnMouseEvent)
END_EVENT_TABLE()

FreqPlot::FreqPlot(wxWindow * parent, wxWindowID id,
                       const wxPoint & pos,
                       const wxSize & size):wxWindow(parent, id, pos, size)
{
   freqWindow = (FreqWindow *) parent;
}

void FreqPlot::OnPaint(wxPaintEvent & evt)
{
   freqWindow->PlotPaint(evt);
}

void FreqPlot::OnMouseEvent(wxMouseEvent & event)
{
   freqWindow->PlotMouseEvent(event);
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 8fce6648-17b9-4632-b845-3a6d25117783


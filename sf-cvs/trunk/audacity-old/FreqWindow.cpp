/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqWindow.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/msgdlg.h>
#include <wx/file.h>
#include <wx/filedlg.h>
#endif

#include <wx/textfile.h>

#include <math.h>

#include "AColor.h"
#include "FreqWindow.h"
#include "FFT.h"

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

#define FREQ_WINDOW_WIDTH 440
#define FREQ_WINDOW_HEIGHT 330

void InitFreqWindow(wxWindow *parent)
{
  wxPoint where;

  where.x = 150;
  where.y = 150;

  gFreqWindow = new FreqWindow(parent, -1, "Frequency Analysis",
					 where);
}

// FreqWindow

BEGIN_EVENT_TABLE(FreqWindow, wxFrame)
  EVT_CLOSE  (                      FreqWindow::OnCloseWindow)
  EVT_SIZE   (                      FreqWindow::OnSize)
  EVT_PAINT  (                      FreqWindow::OnPaint)
  EVT_BUTTON (FreqCloseButtonID,    FreqWindow::OnCloseWindow)
  EVT_BUTTON (FreqExportButtonID,   FreqWindow::OnExport)
  EVT_CHOICE (FreqAlgChoiceID,      FreqWindow::OnAlgChoice)
  EVT_CHOICE (FreqSizeChoiceID,     FreqWindow::OnSizeChoice)
  EVT_CHOICE (FreqFuncChoiceID,     FreqWindow::OnFuncChoice)
  EVT_CHOICE (FreqAxisChoiceID,     FreqWindow::OnAxisChoice)
END_EVENT_TABLE()

FreqWindow::FreqWindow(wxWindow* parent, wxWindowID id, const wxString& title,
				   const wxPoint& pos) :
  wxFrame(parent, id, title, pos,
	      wxSize(FREQ_WINDOW_WIDTH, FREQ_WINDOW_HEIGHT)),
  mData(NULL),
  mProcessed(NULL),
  mBitmap(NULL)
{
  mMouseX = 0;
  mMouseY = 0;
  
  mArrowCursor = new wxCursor(wxCURSOR_ARROW);
  mCrossCursor = new wxCursor(wxCURSOR_CROSS);

  mFreqPlot = new FreqPlot(this, 0,
						   wxPoint(0, 0),
						   wxSize(FREQ_WINDOW_WIDTH, 250));
  
  mUpdateRect.x = 0;
  mUpdateRect.y = 0;
  mUpdateRect.width = FREQ_WINDOW_WIDTH;
  mUpdateRect.height = 250;

  mPlotRect.x = 10;
  mPlotRect.y = 10;
  mPlotRect.width = 420;
  mPlotRect.height = 215;
  
  mLeftMargin = 40;
  mBottomMargin = 20;

  mInfoRect.x = 10;
  mInfoRect.y = 230;
  mInfoRect.width = 420;
  mInfoRect.height = 15;

  mExportButton = new wxButton(this, FreqExportButtonID,
							   "Export...",
							   wxPoint(350, 260),
							   wxSize(70, 20));

  mCloseButton = new wxButton(this, FreqCloseButtonID,
							  "Close",
							  wxPoint(350, 290),
							  wxSize(70, 20));

  wxString algChoiceStrings[2] = {"Spectrum",
								  "Cepstrum"};
  
  mAlgChoice = new wxChoice(this, FreqAlgChoiceID,
							wxPoint(10, 260),
							wxSize(160, 20),
							2, algChoiceStrings);

  mAlgChoice->SetSelection(0);

  wxString sizeChoiceStrings[8] = {"128",
								   "256",
								   "512",
								   "1024",
								   "2048",
								   "4096",
								   "8192",
								   "16384"};
  
  mSizeChoice = new wxChoice(this, FreqSizeChoiceID,
							 wxPoint(180, 260),
							 wxSize(160, 20),
							 8, sizeChoiceStrings);  

  mSizeChoice->SetSelection(2);

  int f = NumWindowFuncs();
  
  wxString *funcChoiceStrings = new wxString[f];
  for(int i=0; i<f; i++)
    funcChoiceStrings[i] = WindowFuncName(i) + wxString(" window");
  
  mFuncChoice = new wxChoice(this, FreqFuncChoiceID,
							 wxPoint(10, 290),
							 wxSize(160, 20),
							 f, funcChoiceStrings);

  mFuncChoice->SetSelection(3);
  delete[] funcChoiceStrings;
  
  wxString axisChoiceStrings[2] = {"Linear frequency",
								   "Log frequency"};
  
  mAxisChoice = new wxChoice(this, FreqAxisChoiceID,
							 wxPoint(180, 290),
							 wxSize(160, 20),
							 2, axisChoiceStrings);  

  mAxisChoice->SetSelection(0);
  
  mLogAxis = false;
  
  mBackgroundBrush.SetColour(wxColour(204, 204, 204));
  mBackgroundPen.SetColour(wxColour(204, 204, 204));

  // Min size, max size
  SetSizeHints(FREQ_WINDOW_WIDTH, FREQ_WINDOW_HEIGHT,
			   20000, 20000);
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
  if (mData)
	delete[] mData;
}

void FreqWindow::OnSize(wxSizeEvent &event)
{
  int width, height;
  GetClientSize(&width, &height);

  // Original size was 440 x 330

  mUpdateRect.x = 0;
  mUpdateRect.y = 0;
  mUpdateRect.width = width;
  mUpdateRect.height = height - 80;

  mFreqPlot->SetSize(0, 0, 
					 width, height - 80);

  mPlotRect.x = 10;
  mPlotRect.y = 10;
  mPlotRect.width = width - 20;
  mPlotRect.height = height - 115;
  
  mLeftMargin = 40;
  mBottomMargin = 20;

  mInfoRect.x = 10;
  mInfoRect.y = height - 100;
  mInfoRect.width = width - 20;
  mInfoRect.height = 15;

  mExportButton->SetSize(width - 90, height - 70, 70, 20);
  mCloseButton->SetSize(width - 90, height - 40, 70, 20);

  mAlgChoice->SetSize(10, height - 70, 160, 20);
  mSizeChoice->SetSize(180, height - 70, 160, 20);
  mFuncChoice->SetSize(10, height - 40, 160, 20);
  mAxisChoice->SetSize(180, height - 40, 160, 20);

  if (mBitmap)
	delete mBitmap;
  mBitmap = NULL;
  
  #ifdef __WXMAC__
  Refresh(true);
  #endif
}

void FreqWindow::PlotMouseEvent(wxMouseEvent& event)
{
  if (event.Moving() && event.m_x != mMouseX) {
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

void FreqWindow::OnAlgChoice(wxCommandEvent &event)
{
  Recalc();
}

void FreqWindow::OnSizeChoice(wxCommandEvent &event)
{
  Recalc();
}

void FreqWindow::OnFuncChoice(wxCommandEvent &event)
{
  Recalc();
}

void FreqWindow::OnAxisChoice(wxCommandEvent &event)
{
  mLogAxis = mAxisChoice->GetSelection();

  mFreqPlot->Refresh(false);
}

// If f(0)=y0, f(1)=y1, f(2)=y2, and f(3)=y3, this function finds
// the degree-three polynomial which best fits these points and
// returns the value of this polynomial at a value x.  Usually
// 0 < x < 3
float CubicInterpolate(float y0, float y1, float y2, float y3, float x)
{
  float a, b, c, d;
  
  a = y0/-6.0 + y1/2.0 - y2/2.0 + y3/6.0;
  b = y0 - 5.0*y1/2.0 + 2.0*y2 - y3/2.0;
  c = -11.0*y0/6.0 + 3.0*y1 - 3.0*y2/2.0 + y3/3.0;
  d = y0;
  
  float xx = x*x;
  float xxx = xx*x;
  
  return (a*xxx + b*xx + c*x + d);
}

float CubicMaximize(float y0, float y1, float y2, float y3)
{
  // Find coefficients of cubic

  float a, b, c, d;
  
  a = y0/-6.0 + y1/2.0 - y2/2.0 + y3/6.0;
  b = y0 - 5.0*y1/2.0 + 2.0*y2 - y3/2.0;
  c = -11.0*y0/6.0 + 3.0*y1 - 3.0*y2/2.0 + y3/3.0;
  d = y0;

  // Take derivative

  float da, db, dc;
  
  da = 3*a;
  db = 2*b;
  dc = c;

  // Find zeroes of derivative using quadratic equation
  
  float discriminant = db*db - 4*da*dc;
  if (discriminant < 0.0)
    return -1.0; // error
  
  float x1 = (-db + sqrt(discriminant)) / (2 * da);
  float x2 = (-db - sqrt(discriminant)) / (2 * da);
  
  // The one which corresponds to a local _maximum_ in the
  // cubic is the one we want - the one with a negative
  // second derivative
  
  float dda = 2*da;
  float ddb = db;
  
  if (dda*x1 + ddb < 0)
    return x1;
  else
    return x2;
}

float Freq2Pitch(float freq)
{
  return float(69.0 + 12.0 * (log(freq/440.0)/log(2.0)));
}

char gPitchName[10];

char *PitchName(int pitch, bool flats)
{
    char *p = gPitchName;

    switch(pitch%12) {
    case 0:
        *p++ = 'C'; break;
    case 1:
        if (flats) {*p++='D'; *p++='b';} else {*p++='C'; *p++='#';} break;
    case 2:
        *p++ = 'D'; break;
    case 3:
        if (flats) {*p++='E'; *p++='b';} else {*p++='D'; *p++='#';} break;
    case 4:
        *p++ = 'E'; break;
    case 5:
        *p++ = 'F'; break;
    case 6:
        if (flats) {*p++='G'; *p++='b';} else {*p++='F'; *p++='#';} break;
    case 7:
        *p++ = 'G'; break;
    case 8:
        if (flats) {*p++='A'; *p++='b';} else {*p++='G'; *p++='#';} break;
    case 9:
        *p++ = 'A'; break;
    case 10:
        if (flats) {*p++='B'; *p++='b';} else {*p++='A'; *p++='#';} break;
    case 11:
        *p++ = 'B'; break;
    }

    sprintf(p,"%d",(pitch/12)-1);

    return gPitchName;
}

float FreqWindow::GetProcessedValue(float freq0, float freq1)
{
  int alg = mAlgChoice->GetSelection();
  
  float bin0, bin1, binwidth;

  if (alg==0) {
	bin0 = freq0 * mWindowSize / mRate;
	bin1 = freq1 * mWindowSize / mRate;
  }
  else {
	bin0 = freq0 * mRate;
	bin1 = freq1 * mRate;
  }
  binwidth = bin1-bin0;
  
  float value = 0.0;
  
  if (binwidth < 1.0) {
	float binmid = (bin0 + bin1)/2.0;
	int ibin = int(binmid) - 1;
	if (ibin < 1)
	  ibin = 1;
	if (ibin >= mWindowSize/2 - 3)
	  ibin = mWindowSize/2 - 4;
	
	value = CubicInterpolate(mProcessed[ibin],
							 mProcessed[ibin+1],
							 mProcessed[ibin+2],
							 mProcessed[ibin+3],
							 binmid - ibin);
  }
  else {
	if (int(bin1) > int(bin0))
	  value += mProcessed[int(bin0)]*(int(bin0)+1-bin0);
	bin0 = 1+int(bin0);
	while(bin0 < int(bin1)) {
	  value += mProcessed[int(bin0)];
	  bin0 += 1.0;
	}
	value += mProcessed[int(bin1)]*(bin1-int(bin1));    
	
	value /= binwidth;
  }
  
  return value;
}

void FreqWindow::OnPaint(wxPaintEvent &evt)
{
  int width, height;
  GetSize(&width, &height);

  wxPaintDC dc(this);

  dc.SetBrush(mBackgroundBrush);
  dc.SetPen(mBackgroundPen);
  dc.DrawRectangle(0, mUpdateRect.height,
                   width, height - mUpdateRect.height);
}

void FreqWindow::PlotPaint(wxPaintEvent &evt)
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
	  memDC.DrawText("Not enough data selected.",
				  r.x + 5, r.y + 5);

	return;
  }
  
  const int topDB = 10;
  const int bottomDB = -80;
  int totalDB = (topDB - bottomDB);

  int alg = mAlgChoice->GetSelection();

  int i;

#ifdef __WXMSW__
  const int fontSize = 8;
#else
  const int fontSize = 10;
#endif

  wxFont freqWindowFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
  memDC.SetFont(freqWindowFont);

  // Draw y axis and gridlines

  if (alg==0) {
	for(i=bottomDB; i<=topDB; i+=10) {
	  int y = r.y + r.height - 1 - (i - bottomDB)*(r.height-1)/totalDB;
	  
	  // Light blue gridline
	  memDC.SetPen(wxPen(wxColour(204,204,255),1,wxSOLID));
	  memDC.DrawLine(r.x, y, r.x + r.width, y);
	  
	  // Pure blue axis line
	  memDC.SetPen(wxPen(wxColour(0,0,255),1,wxSOLID));
	  memDC.DrawLine(mInfoRect.x, y, r.x, y);
	  
	  if (i != bottomDB) {
		wxString label = wxString::Format("%d dB", i);
		long labelWidth, labelHeight;
		memDC.GetTextExtent(label, &labelWidth, &labelHeight);
		memDC.DrawText(label, 
					   r.x - labelWidth - 2, y+1);
	  }
	}
  }
  else {
	int y = r.y + r.height/2;

	// Light blue gridline
	memDC.SetPen(wxPen(wxColour(204,204,255),1,wxSOLID));
	memDC.DrawLine(r.x, y, r.x + r.width, y);
	
	// Pure blue axis line
	memDC.SetPen(wxPen(wxColour(0,0,255),1,wxSOLID));
	memDC.DrawLine(mInfoRect.x, y, r.x, y);

	wxString label = "0.0";
	long labelWidth, labelHeight;
	memDC.GetTextExtent(label, &labelWidth, &labelHeight);
	memDC.DrawText(label, 
				   r.x - labelWidth - 2, y+1);
  }
  
  // Draw x axis and gridlines
  
  int width = r.width-2;

  float min_freq, max_freq, ratio, freq, last, freq_step;

  if (alg==0) {  
	min_freq = mRate/mWindowSize;
	max_freq = mRate/2;
	ratio = max_freq / min_freq;
	freq = min_freq;
	last = freq/2.0;
	if (mLogAxis)
	  freq_step = pow(2.0, (log(ratio)/log(2)) / width);
	else
	  freq_step = (max_freq - min_freq) / width;
  }
  else {
	min_freq = 0;
	max_freq = mWindowSize/mRate;
	freq = min_freq;
	last = freq/2.0;
	freq_step = (max_freq - min_freq) / width;
  }
  
  int nextx = 0;

  for(i=0; i<width; i++) {
        
	if ((mLogAxis && freq / last >= 2.0) ||
		(!mLogAxis && (i%60)==0)) {
	  int x = i + 1;
        
	  // Light blue gridline
	  memDC.SetPen(wxPen(wxColour(204,204,255),1,wxSOLID));
	  memDC.DrawLine(r.x + x, r.y, r.x + x, r.y + r.height);

	  if (x >= nextx) {
        
		// Pure blue axis line
		memDC.SetPen(wxPen(wxColour(0,0,255),1,wxSOLID));
		memDC.DrawLine(r.x + x, r.y + r.height, r.x + x, r.y + r.height + 15);
        	
		// Label
        	
		wxString label;
		if (alg==0) {
		  if (freq < 950.0)
			label = wxString::Format("%dHz", int(freq+0.5));
		  else
			label = wxString::Format("%dKHz", int((freq/1000.0)+0.5));
		}
		else
		  label = wxString::Format("%.4f s", freq);
		long labelWidth, labelHeight;
		memDC.GetTextExtent(label, &labelWidth, &labelHeight);
		if (x + labelWidth < width)
		  memDC.DrawText(label,
						 r.x + x + 3, r.y + r.height + 2);
		nextx = x + labelWidth + 4;
	  }

	  last *= 2.0;
	}
    
	if (mLogAxis)
	  freq *= freq_step;
	else
	  freq += freq_step;
  }

  // Draw the plot

  if (alg==0)
	memDC.SetPen(wxPen(wxColour(50,150,50),1,wxSOLID));
  else
	memDC.SetPen(wxPen(wxColour(200,50,150),1,wxSOLID));

  freq = min_freq;

  for(i=0; i<width; i++) {
    float y;
    
    if (mLogAxis)
      y = GetProcessedValue(freq, freq*freq_step);
    else
      y = GetProcessedValue(freq, freq+freq_step);
    
    float ynorm;

	if (alg==0)
	  ynorm = (y - bottomDB) / totalDB;
	else
	  ynorm = (y + 30.0) / 60.0;

	int lineheight = int(ynorm * (r.height-1));

	if (lineheight > r.height-2)
	  lineheight = r.height-2;

	if (ynorm > 0.0)
	  memDC.DrawLine(r.x + 1 + i, r.y + r.height - 1 - lineheight,
				  r.x + 1 + i, r.y + r.height - 1);
    
    if (mLogAxis)
      freq *= freq_step;
    else
      freq += freq_step;
  }
  
  // Find the peak nearest the cursor and plot it
  
  float bestpeak = 0.0;
  if (r.Inside(mMouseX, mMouseY)) {
    if (mLogAxis)
      freq = min_freq * pow(freq_step, mMouseX - (r.x + 1));
    else
      freq = min_freq + freq_step * (mMouseX - (r.x + 1));
 
    bool up = (mProcessed[1] > mProcessed[0]);
    float bestdist = 1000000;
    for(int bin=2; bin<mProcessedSize; bin++) {
      bool nowUp = mProcessed[bin] > mProcessed[bin-1];
      if (!nowUp && up) {
        // Local maximum.  Find actual value by cubic interpolation
        int leftbin = bin-2;
        if (leftbin < 1)
          leftbin = 1;
        float max = leftbin + CubicMaximize(mProcessed[leftbin],
                                            mProcessed[leftbin+1],
                                            mProcessed[leftbin+2],
                                            mProcessed[leftbin+3]);
        float thispeak = max * mRate / mWindowSize;
        if (fabs(thispeak - freq) < bestdist) {
          bestpeak = thispeak;
          bestdist = fabs(thispeak - freq);
          if (thispeak > freq)
            break;
        }
      }
      up = nowUp;
    }

    int px;
    if (mLogAxis)
      px = int(log(bestpeak/min_freq)/log(freq_step));
    else
      px = int((bestpeak-min_freq)*width/(max_freq-min_freq));
    
    memDC.SetPen(wxPen(wxColour(200,0,0),1,wxSOLID));
    memDC.DrawLine(r.x + 1 + px, r.y, r.x + 1 + px, r.y + r.height);
  }

  // Outline the graph
  
  memDC.SetPen(*wxBLACK_PEN);
  memDC.SetBrush(*wxTRANSPARENT_BRUSH);
  memDC.DrawRectangle(r);

  // Draw the bevel around the info rect
  
  AColor::Dark(&memDC, false);
  memDC.DrawLine(mInfoRect.x, mInfoRect.y,
			  mInfoRect.x + mInfoRect.width, mInfoRect.y);
  memDC.DrawLine(mInfoRect.x, mInfoRect.y,
			  mInfoRect.x, mInfoRect.y + mInfoRect.height);
  AColor::Light(&memDC, false);
  memDC.DrawLine(mInfoRect.x, mInfoRect.y + mInfoRect.height,
			  mInfoRect.x + mInfoRect.width, mInfoRect.y + mInfoRect.height);
  memDC.DrawLine(mInfoRect.x + mInfoRect.width, mInfoRect.y,
			  mInfoRect.x + mInfoRect.width, mInfoRect.y + mInfoRect.height);

  // If the mouse cursor is pointing inside the graph, print out info about the
  // cursor location

  if (r.Inside(mMouseX, mMouseY)) {    
    float value;
    
    if (mLogAxis) {
      freq = min_freq * pow(freq_step, mMouseX - (r.x + 1));
      value = GetProcessedValue(freq, freq * freq_step);
    }
    else {
      freq = min_freq + freq_step * (mMouseX - (r.x + 1));
      value = GetProcessedValue(freq, freq + freq_step);
    }

	int bestpitch = int(Freq2Pitch(bestpeak)+0.5);
    
    wxString info = wxString::Format("%d Hz: %d dB    Peak: %d Hz (%s)",
                                      int(freq+0.5), int(value+0.5),
                                      int(bestpeak+0.5),
                                      PitchName(bestpitch, false));
    memDC.DrawText(info,
				   mInfoRect.x + 2, mInfoRect.y + 2);
  }  
  
  dc.Blit(0, 0, mUpdateRect.width, mUpdateRect.height,
		  &memDC, 0, 0, wxCOPY, FALSE);
}

void FreqWindow::OnCloseWindow(wxCloseEvent& WXUNUSED(event))
{
  this->Show(FALSE);
}

void FreqWindow::Plot(int len, float *data, double rate)
{
  mRate = rate;
  mDataLen = len;
  if (mData)
	delete[] mData;
  mData = new float[len];
  for(int i=0; i<len; i++)
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
		alg>=0 && alg<=3 &&
		windowFunc >= 0 && windowFunc <= 3)) {
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
  for(i=0; i<mWindowSize; i++)
	mProcessed[i] = 0.0;
  int half = mWindowSize/2;

  float *in = new float[mWindowSize];
  float *out = new float[mWindowSize];
  float *out2 = new float[mWindowSize];

  int start = 0;
  int windows = 0;
  while(start + mWindowSize <= mDataLen) {
	for(i=0; i<mWindowSize; i++)
	  in[i] = mData[start+i];
	  
    WindowFunc(windowFunc, mWindowSize, in);

	if (alg==0) {
	  PowerSpectrum(mWindowSize, in, out);
	  
	  for(i=0; i<half; i++)
		mProcessed[i] += out[i];
	}
	else {
	  FFT(mWindowSize, false, in, NULL, out, out2);

	  for(i=0; i<mWindowSize; i++)
		mProcessed[i] += log(out[i]*out[i] + out2[i]*out2[i]);
	}

	start += half;
	windows++;
  }

  if (alg==0) {
    // Convert to decibels
    for(i=0; i<half; i++)
	  mProcessed[i] = 10*log10(mProcessed[i]/mWindowSize/windows);
	  
	mProcessedSize = mWindowSize / 2;
  }
  else {
    mProcessedSize = mWindowSize;

	// Inverse FFT
    FFT(mProcessedSize, true, mProcessed, NULL, out, out2);

    for(i=0; i<mProcessedSize; i++)
      mProcessed[i] = out[i];

    /*
    // Normalize, ignoring first and last few Cepstral coefficients
    
    float min = mProcessed[15];
    float max = mProcessed[15];
	int imin = 16;
	int imax = 16;
    for(i=17; i<mProcessedSize-16; i++) {
      if (mProcessed[i] < min) {
        min = mProcessed[i];
		imin = i;
	  }
      if (mProcessed[i] > max) {
        max = mProcessed[i];
		imax = i;
	  }
    }

	printf("%f (%d) - %f (%d)\n", min, imin, max, imax);
    
    for(i=0; i<mProcessedSize; i++)
      mProcessed[i] = ((mProcessed[i]-min)/(max-min))*90 - 80;
	*/

  }

  delete[] in;
  delete[] out;
  delete[] out2;

  mFreqPlot->Refresh(false);
}

void FreqWindow::OnExport()
{
  wxString fName = "spectrum.txt";

  fName = wxFileSelector("Export Spectral Data As:",
                         NULL,
                         fName,
                         "txt",
                         "*.txt",
                         wxSAVE,
                         this);

  if (fName == "")
	return;
  
  wxTextFile f(fName);
  #ifdef __WXMAC__
  wxFile *temp = new wxFile();
  temp->Create(fName);
  delete temp;
  #else
  f.Create();
  #endif
  f.Open();
  if (!f.IsOpened()) {
	wxMessageBox("Couldn't write to "+fName);
	return;
  }
  
  f.AddLine("Frequency (Hz)\tLevel (dB)");
  for(int i=1; i<mProcessedSize; i++)
    f.AddLine(wxString::Format("%f\t%f", i*mRate/mWindowSize, mProcessed[i]));
  
  #ifdef __WXMAC__
  f.Write(wxTextFileType_Mac);
  #else
  f.Write();
  #endif
  f.Close();
}


BEGIN_EVENT_TABLE(FreqPlot, wxWindow)
  EVT_PAINT  (                      FreqPlot::OnPaint)
  EVT_MOUSE_EVENTS(                 FreqPlot::OnMouseEvent)
END_EVENT_TABLE()

FreqPlot::FreqPlot(wxWindow *parent, wxWindowID id,
				   const wxPoint& pos,
				   const wxSize& size):
  wxWindow(parent, id, pos, size)
{
  freqWindow = (FreqWindow *)parent;
}

void FreqPlot::OnPaint(wxPaintEvent &evt)
{
  freqWindow->PlotPaint(evt);
}

void FreqPlot::OnMouseEvent(wxMouseEvent& event)
{
  freqWindow->PlotMouseEvent(event);
}


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

#ifdef __WXMAC__
#define FREQ_WINDOW_WIDTH 440
#define FREQ_WINDOW_HEIGHT 330
#endif

#ifdef __WXGTK__
#define FREQ_WINDOW_WIDTH 450
#define FREQ_WINDOW_HEIGHT 330
#endif

#ifdef __WXMSW__
#define FREQ_WINDOW_WIDTH 450
#define FREQ_WINDOW_HEIGHT 330
#endif

void InitFreqWindow(wxFrame *parent)
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
  EVT_MOUSE_EVENTS(                 FreqWindow::OnMouseEvent)
  EVT_BUTTON (FreqCloseButtonID,    FreqWindow::OnCloseWindow)
  EVT_BUTTON (FreqExportButtonID,   FreqWindow::OnExport)
  EVT_CHOICE (FreqAlgChoiceID,      FreqWindow::OnAlgChoice)
  EVT_CHOICE (FreqSizeChoiceID,     FreqWindow::OnSizeChoice)
  EVT_CHOICE (FreqFuncChoiceID,     FreqWindow::OnFuncChoice)
  EVT_CHOICE (FreqAxisChoiceID,     FreqWindow::OnAxisChoice)
END_EVENT_TABLE()

FreqWindow::FreqWindow(wxFrame* parent, wxWindowID id, const wxString& title,
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

  wxString algChoiceStrings[1] = {"Spectrum"};
								  //"Cepstrum"};
  
  mAlgChoice = new wxChoice(this, FreqAlgChoiceID,
							wxPoint(10, 260),
							wxSize(160, 20),
							1, algChoiceStrings);

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
}

FreqWindow::~FreqWindow()
{
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

  //mTrackPanel->SetSize(0, 0,
  //					     width-sbarWidth, height-sbarWidth);

  /*
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

  wxString algChoiceStrings[1] = {"Spectrum"};
								  //"Cepstrum"};
  
  mAlgChoice = new wxChoice(this, FreqAlgChoiceID,
							wxPoint(10, 260),
							wxSize(160, 20),
							1, algChoiceStrings);

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
  */
}

void FreqWindow::OnMouseEvent(wxMouseEvent& event)
{
  if (event.Moving() && event.m_x != mMouseX) {
      mMouseX = event.m_x;
      mMouseY = event.m_y;
      
      wxRect r = mPlotRect;
      r.x += mLeftMargin;
      r.width -= mLeftMargin;
      r.height -= mBottomMargin;

      if (r.Inside(mMouseX, mMouseY))
        SetCursor(*mCrossCursor);
      else
        SetCursor(*mArrowCursor);
      
      Refresh(false, &mUpdateRect);
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

  Refresh(false);
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

float FreqWindow::GetProcessedValue(float freq0, float freq1)
{
    float bin0 = freq0 * mWindowSize / mRate;
    float bin1 = freq1 * mWindowSize / mRate;
    float binwidth = bin1-bin0;

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
  wxPaintDC dc(this);
  
  int width, height;
  GetSize(&width, &height);
  height -= mUpdateRect.height;
  
  dc.SetBrush(mBackgroundBrush);
  dc.SetPen(mBackgroundPen);
  dc.DrawRectangle(0, mUpdateRect.height, width, height);
  
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

  int i;

  wxFont freqWindowFont(10, wxSWISS, wxNORMAL, wxNORMAL);
  memDC.SetFont(freqWindowFont);

  // Draw y axis and gridlines

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
  
  // Draw x axis and gridlines
  
  width = r.width-2;
  
  float min_freq = mRate/mWindowSize;
  float max_freq = mRate/2;
  float ratio = max_freq / min_freq;
  float freq = min_freq;
  float last = freq/2.0;
  float freq_step;
  if (mLogAxis)
    freq_step = pow(2.0, (log(ratio)/log(2)) / width);
  else
    freq_step = (max_freq - min_freq) / width;
  
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
        	if (freq < 950.0)
        	  label = wxString::Format("%dHz", int(freq+0.5));
        	else
        	  label = wxString::Format("%dKHz", int((freq/1000.0)+0.5));
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

  memDC.SetPen(wxPen(wxColour(50,150,50),1,wxSOLID));

  freq = min_freq;

  for(i=0; i<width; i++) {
    float y;
    
    if (mLogAxis)
      y = GetProcessedValue(freq, freq*freq_step);
    else
      y = GetProcessedValue(freq, freq+freq_step);
    
    float ynorm = (y - bottomDB) / totalDB;

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
      px = log(bestpeak/min_freq)/log(freq_step);
    else
      px = (bestpeak-min_freq)*width/(max_freq-min_freq);
    
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
    
    wxString info = wxString::Format("%d Hz: %d dB    Peak: %d Hz",
                                      int(freq+0.5), int(value+0.5),
                                      int(bestpeak+0.5));
    memDC.DrawText(info,
  			    mInfoRect.x + 2, mInfoRect.y + 2);
  }  
  
  dc.Blit(0, 0, mUpdateRect.width, mUpdateRect.height, &memDC, 0, 0, wxCOPY, FALSE);
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
	Refresh(false);
	return;
  }

  int alg = mAlgChoice->GetSelection();
  int windowFunc = mFuncChoice->GetSelection();
  long windowSize = 0;
  (mSizeChoice->GetStringSelection()).ToLong(&windowSize);

  if (!(windowSize >= 32 && windowSize <= 65536 &&
		alg>=0 && alg<=3 &&
		windowFunc >= 0 && windowFunc <= 3)) {
	Refresh(false);
	return;
  }

  mWindowSize = windowSize;

  if (mDataLen < mWindowSize) {
	Refresh(false);
	return;
  }

  mProcessed = new float[mWindowSize];

  int i;
  for(i=0; i<mWindowSize; i++)
	mProcessed[i] = 0.0;
  int half = mWindowSize/2;

  float *in = new float[mWindowSize];
  float *out = new float[mWindowSize];

  int start = 0;
  int windows = 0;
  while(start + mWindowSize <= mDataLen) {
	for(i=0; i<mWindowSize; i++)
	  in[i] = mData[start+i];
	  
    WindowFunc(windowFunc, mWindowSize, in);

	PowerSpectrum(mWindowSize, in, out);

	for(i=0; i<half; i++)
	  mProcessed[i] += out[i];

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
    mProcessedSize = mWindowSize / 4;

    for(i=0; i<half; i++)
	  mProcessed[i] = log(mProcessed[i]/mWindowSize/windows);
  
    // For a Cepstrum, do an IFFT on this data
    
    /*
    
    for(i=0; i<half; i++)
      out[i] = mProcessed[i];
    
    PowerSpectrum(half, out, mProcessed);*/
    
    float *out1 = new float[half];
    float *out2 = new float[half];
    
    FFT(half, true, mProcessed, NULL, out1, out2);

    for(i=0; i<half/2; i++) {
      mProcessed[i] = out1[i]*out1[i] + out2[i]*out2[i];
    }
    
    delete[] out1;
    delete[] out2;
    
    // Normalize
    
    float min = mProcessed[0];
    float max = mProcessed[0];
    for(i=1; i<half/2; i++) {
      if (mProcessed[i] < min)
        min = mProcessed[i];
      if (mProcessed[i] > max)
        max = mProcessed[i];
    }
    
    for(i=0; i<half/2; i++) {
      mProcessed[i] = ((mProcessed[i]-min)/(max-min))*90 - 80;
      mProcessed[i+half/2] = mProcessed[i];
    }
  }

  delete[] in;
  delete[] out;

  Refresh(false);
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

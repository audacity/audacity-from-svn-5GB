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
#include <wx/wx.h>
#include <wx/brush.h>
#include <wx/image.h>
#endif

#include <math.h>

#include "FreqWindow.h"
#include "FFT.h"

enum {
  FirstID = 7000,
  
  FreqCloseButtonID,
  FreqAlgChoiceID,
  FreqSizeChoiceID,
  FreqFuncChoiceID
};

FreqWindow *gFreqWindow = NULL;

#ifdef __WXMAC__
#define FREQ_WINDOW_WIDTH 440
#define FREQ_WINDOW_HEIGHT 300
#endif

#ifdef __WXGTK__
#define FREQ_WINDOW_WIDTH 450
#define FREQ_WINDOW_HEIGHT 320
#endif

#ifdef __WXMSW__
#define FREQ_WINDOW_WIDTH 440
#define FREQ_WINDOW_HEIGHT 300
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

BEGIN_EVENT_TABLE(FreqWindow, wxMiniFrame)
  EVT_CLOSE  (                      FreqWindow::OnCloseWindow)
  EVT_PAINT  (                      FreqWindow::OnPaint)
  EVT_BUTTON (FreqCloseButtonID,    FreqWindow::OnCloseWindow)
  EVT_CHOICE (FreqAlgChoiceID,      FreqWindow::OnAlgChoice)
  EVT_CHOICE (FreqSizeChoiceID,     FreqWindow::OnSizeChoice)
  EVT_CHOICE (FreqFuncChoiceID,     FreqWindow::OnFuncChoice)
END_EVENT_TABLE()

FreqWindow::FreqWindow(wxFrame* parent, wxWindowID id, const wxString& title,
				   const wxPoint& pos) :
  wxMiniFrame(parent, id, title, pos,
			  wxSize(FREQ_WINDOW_WIDTH, FREQ_WINDOW_HEIGHT),
              wxTINY_CAPTION_HORIZ | wxSTAY_ON_TOP),
  mData(NULL),
  mProcessed(NULL)
{
  mPlotRect.x = 10;
  mPlotRect.y = 10;
  mPlotRect.width = 420;
  mPlotRect.height = 240;

  mCloseButton = new wxButton(this, FreqCloseButtonID,
							  "Close",
							  wxPoint(360, 260),
							  wxSize(60, 30));

  wxString algChoiceStrings[4] = {"Spectrum",
								  "Log Spectrum (not working yet)",
								  "Cepstrum (not implemented yet)",
								  "Log Cepstrum (not implemented yet)"};
  
  mAlgChoice = new wxChoice(this, FreqAlgChoiceID,
							wxPoint(10, 260),
							wxSize(100, 30),
							4, algChoiceStrings);

  wxString sizeChoiceStrings[8] = {"128",
								   "256",
								   "512",
								   "1024",
								   "2048",
								   "4096",
								   "8192",
								   "16384"};
  
  mSizeChoice = new wxChoice(this, FreqSizeChoiceID,
							 wxPoint(120, 260),
							 wxSize(100, 30),
							 8, sizeChoiceStrings);  

  mSizeChoice->SetSelection(2);

  wxString funcChoiceStrings[4] = {"Rectangular",
								   "Bartlett",
								   "Hamming",
								   "Hanning"};
  
  mFuncChoice = new wxChoice(this, FreqFuncChoiceID,
							 wxPoint(240, 260),
							 wxSize(110, 30),
							 4, funcChoiceStrings);  

  mFuncChoice->SetSelection(3);
  
  mBackgroundBrush.SetColour(wxColour(204, 204, 204));
  mBackgroundPen.SetColour(wxColour(204, 204, 204));
}

FreqWindow::~FreqWindow()
{
  delete mCloseButton;
  delete mAlgChoice;
  delete mSizeChoice;
  delete mFuncChoice;
  if (mData)
	delete[] mData;
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

void FreqWindow::OnPaint(wxPaintEvent &evt)
{
  wxPaintDC dc(this);

  int width, height;
  GetSize(&width, &height);
  
  dc.SetBrush(mBackgroundBrush);
  dc.SetPen(mBackgroundPen);
  dc.DrawRectangle(0, 0, width, height);

  dc.SetPen(*wxBLACK_PEN);
  dc.SetBrush(*wxWHITE_BRUSH);
  dc.DrawRectangle(mPlotRect);

  if (!mProcessed) {
	if (mData && mDataLen < mWindowSize)
	  dc.DrawText("Not enough data selected.",
				  mPlotRect.x + 5, mPlotRect.y + 5);

	return;
  }

  int i;

  width = mPlotRect.width;
  for(i=0; i<width; i++) {
	float x = i * (mWindowSize/2) /(float)width;
	float xfrac = x - ((int)x);
	int xint = int(x);
	float y =
	  (xint+1>=mWindowSize/2? 0.0: mProcessed[xint+1])*xfrac +
	  (xint<0? 0.0: mProcessed[xint])*(1.0-xfrac);
	if (y < 0.0)
	  y = 0;
	int lineheight = int(y * mPlotRect.height);

	dc.DrawLine(mPlotRect.x + i, mPlotRect.y + mPlotRect.height - lineheight,
				mPlotRect.x + i, mPlotRect.y + mPlotRect.height);
  }
}

void FreqWindow::OnCloseWindow(wxCloseEvent& WXUNUSED(event))
{
  this->Show(FALSE);
}

void FreqWindow::Plot(int len, float *data)
{
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
  while(start + mWindowSize <= mDataLen) {
	for(i=0; i<mWindowSize; i++)
	  in[i] = mData[start+i];
	  
	// Apply window
	
	if (windowFunc == 1) {	  
	  // Bartlett (triangular) window
	  for(i=0; i<half; i++) {
		in[i] *= (i/(float)half);
		in[i+half] *= (1.0 - (i/(float)half));
	  }
	}

	if (windowFunc == 2) {
	  // Hamming
	  for(i=0; i<mWindowSize; i++)
		in[i] *= 0.54 - 0.46 * cos(2 * M_PI * i / (mWindowSize-1));
	}

	if (windowFunc == 3) {
	  // Hanning
	  for(i=0; i<mWindowSize; i++)
		in[i] *= 0.50 - 0.50 * cos(2 * M_PI * i / (mWindowSize-1));
	}

	PowerSpectrum(mWindowSize, in, out);

	if (alg >= 1 && alg <= 3) {
	  // Log spectrum, cepstrum, and log cepstrum all want
	  // us to take the log of the answer

	  for(i=0; i<half; i++)
		out[i] = log(out[i]);
	}

	if (alg >= 2 && alg <= 3) {
	  // TODO: cepstrum
	}

	// We add all of the different spectra just by adding -
	// the normalization takes care of the averaging
	for(i=0; i<half; i++)
	  mProcessed[i] += out[i];

	start += half;
  }

  // Normalize
  float max = 0.0;
  for(i=0; i<half; i++)
	if (mProcessed[i] > max)
	  max = mProcessed[i];

  for(i=0; i<half; i++)
	mProcessed[i] /= max;

  delete[] in;
  delete[] out;

  Refresh(false);
}

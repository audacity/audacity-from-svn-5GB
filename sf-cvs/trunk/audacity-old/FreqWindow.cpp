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
  gFreqWindow->Show(TRUE);
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
								  "Log Spectrum",
								  "Cepstrum",
								  "Log Cepstrum"};
  
  mAlgChoice = new wxChoice(this, FreqAlgChoiceID,
							wxPoint(10, 260),
							wxSize(100, 30),
							4, algChoiceStrings);

  wxString sizeChoiceStrings[7] = {"256",
								   "512",
								   "1024",
								   "2048",
								   "4096",
								   "8192",
								   "16384"};
  
  mSizeChoice = new wxChoice(this, FreqSizeChoiceID,
							 wxPoint(120, 260),
							 wxSize(100, 30),
							 7, sizeChoiceStrings);  

  wxString funcChoiceStrings[3] = {"Rectangular",
								   "Hamming",
								   "Hanning"};
  
  mFuncChoice = new wxChoice(this, FreqFuncChoiceID,
							 wxPoint(240, 260),
							 wxSize(110, 30),
							 3, funcChoiceStrings);  
  
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
}

void FreqWindow::OnCloseWindow(wxCloseEvent& WXUNUSED(event))
{
  this->Show(FALSE);
}

void FreqWindow::Recalc()
{
  
}

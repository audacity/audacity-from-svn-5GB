/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqWindow.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_FREQ_WINDOW__
#define __AUDACITY_FREQ_WINDOW__

#include <wx/brush.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/gdicmn.h>
#include <wx/pen.h>
#include <wx/minifram.h>

class FreqWindow;

class TrackList;

extern FreqWindow *gFreqWindow;

void InitFreqWindow(wxFrame *parent);

class FreqWindow: public wxFrame
{
public:
  FreqWindow(wxFrame *parent, wxWindowID id,
			 const wxString& title,
			 const wxPoint& pos);

  virtual ~FreqWindow();

  void Plot(int len, float *data, double rate);
  
  void OnMouseEvent(wxMouseEvent& event); 
  void OnCloseWindow(wxCloseEvent& event);
  void OnSize(wxSizeEvent &event);
  void OnPaint(wxPaintEvent& event);
  void OnAlgChoice(wxCommandEvent& event);
  void OnSizeChoice(wxCommandEvent& event);
  void OnFuncChoice(wxCommandEvent& event);
  void OnAxisChoice(wxCommandEvent& event);
  void OnExport();

  void Recalc();
  
private:
  
  wxBrush   mBackgroundBrush;
  wxPen     mBackgroundPen;

  wxCursor  *mArrowCursor;
  wxCursor  *mCrossCursor;

  wxButton  *mCloseButton;
  wxButton  *mExportButton;
  wxChoice  *mAlgChoice;
  wxChoice  *mSizeChoice;
  wxChoice  *mFuncChoice;
  wxChoice  *mAxisChoice;

  wxRect    mPlotRect;
  wxRect    mInfoRect;
  wxRect    mUpdateRect;
  
  int       mLeftMargin;
  int       mBottomMargin;

  double    mRate;
  int       mDataLen;
  float     *mData;
  int       mWindowSize;
  float     *mProcessed;
  int       mProcessedSize;
  bool      mLogAxis;
  
  wxBitmap  *mBitmap;
  
  int       mMouseX;
  int       mMouseY;
  
  float     GetProcessedValue(float freq0, float freq1);
    
DECLARE_EVENT_TABLE()
};

#endif

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

class FreqWindow: public wxMiniFrame
{
public:
  FreqWindow(wxFrame *parent, wxWindowID id,
			 const wxString& title,
			 const wxPoint& pos);

  virtual ~FreqWindow();

  void Plot(int len, float *data);
  
  void OnCloseWindow(wxCloseEvent& event);
  void OnPaint(wxPaintEvent& event);
  void OnAlgChoice(wxCommandEvent& event);
  void OnSizeChoice(wxCommandEvent& event);
  void OnFuncChoice(wxCommandEvent& event);

  void Recalc();
  
private:
  
  wxBrush   mBackgroundBrush;
  wxPen     mBackgroundPen;

  wxButton  *mCloseButton;
  wxChoice  *mAlgChoice;
  wxChoice  *mSizeChoice;
  wxChoice  *mFuncChoice;

  wxRect    mPlotRect;

  int       mDataLen;
  float     *mData;
  int       mWindowSize;
  float     *mProcessed;
    
DECLARE_EVENT_TABLE()
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL__
#define __AUDACITY_TRACK_PANEL__

#include <wx/timer.h>
#include <wx/window.h>

class wxRect;

class TrackList;
class VTrack;

struct ViewInfo {

  // Current selection

  double sel0;
  double sel1;

  // Scroll info

  int vpos;       // vertical scroll pos

  double h;       // h pos in secs
  double screen;  // screen width in secs
  double total;   // total width in secs
  double zoom;    // pixels per second
  double lastZoom;

  // Actual scroll bar positions, in pixels
  int sbarH;
  int sbarScreen;
  int sbarTotal;

  int scrollStep;
};

class TrackPanel;

class AudacityTimer: public wxTimer
{
public:
  virtual void Notify();
  TrackPanel *parent;
};

class TrackPanel: public wxWindow
{
public:

  TrackPanel(wxWindow *parent, wxWindowID id,
			 const wxPoint& pos,
			 const wxSize& size,
			 TrackList *tracks,
			 ViewInfo *viewInfo);

  virtual ~TrackPanel();

  void OnPaint(wxPaintEvent& event);
  void OnMouseEvent(wxMouseEvent& event);  

  void OnTimer();
  
  int GetLabelWidth();
  int GetLabelOffset();
  int GetRulerHeight();

  void GetTracksUsableArea(int *width, int *height);

  void SelectNone();

private:

  void HandleCursor(wxMouseEvent& event);
  void HandleResize(wxMouseEvent& event);
  void HandleSelect(wxMouseEvent& event);
  void HandleEnvelope(wxMouseEvent& event);
  void HandleSlide(wxMouseEvent& event);
  void HandleZoom(wxMouseEvent& event);
  void HandleLabelClick(wxMouseEvent& event);

  void MakeParentRedrawScrollbars();

  VTrack *FindTrack(int mouseX, int mouseY, bool label,
					wxRect *trackRect = NULL, int *trackNum = NULL);

  void DrawRuler(wxDC& dc);
  void DrawTracks(wxDC& dc);

  inline void Bevel(wxDC& dc, bool up, wxRect &r);

  TrackList       *mTracks;
  ViewInfo        *mViewInfo;

  AudacityTimer   mTimer;
  int             mTimeCount;
  
  wxBitmap        *mBitmap;
  int             mPrevWidth;
  int             mPrevHeight;

  double          mSelStart;

  VTrack          *mCapturedTrack;
  wxRect          mCapturedRect;
  int             mCapturedNum;
	
  int             mMouseClickX;
  int             mMouseClickY;

  int             mMouseMostRecentX;
  int             mMouseMostRecentY;

  int             mInitialTrackHeight;

  bool            mIsSelecting;
  bool            mIsResizing;
  bool            mIsSliding;
  bool            mIsEnveloping;

  wxCursor        *mArrowCursor;
  wxCursor        *mSelectCursor;
  wxCursor        *mResizeCursor;
  wxCursor        *mSlideCursor;
  wxCursor        *mZoomInCursor;
  wxCursor        *mZoomOutCursor;

public:

  DECLARE_EVENT_TABLE()
};

#endif

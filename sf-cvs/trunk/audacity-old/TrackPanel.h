/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL__
#define __AUDACITY_TRACK_PANEL__

#include <wx/timer.h>
#include <wx/window.h>

class wxMenu;
class wxRect;
class wxStatusBar;

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

class TrackPanelListener {
public:
  virtual void TP_DisplayStatusMessage(const char *msg, int fieldNum) = 0;
  virtual int  TP_GetCurrentTool() = 0;
  virtual void TP_OnPlayKey() = 0;
  virtual void TP_PushState() = 0;
  virtual void TP_RedrawScrollbars() = 0;
  virtual void TP_ScrollLeft() = 0;
  virtual void TP_ScrollRight() = 0;
  virtual void TP_HasMouse() = 0;
};

class TrackPanel: public wxWindow
{
public:

  TrackPanel(wxWindow *parent,
			 wxWindowID id,
			 const wxPoint& pos,
			 const wxSize& size,
			 TrackList *tracks,
			 ViewInfo *viewInfo,
			 TrackPanelListener *listener);

  virtual ~TrackPanel();

  void OnPaint(wxPaintEvent& event);
  void OnMouseEvent(wxMouseEvent& event); 
  void OnKeyEvent(wxKeyEvent& event);

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
  void MakeParentPushState();

  void OnChannelLeft();
  void OnChannelRight();
  void OnChannelMono();

  void OnWaveform();
  void OnSpectrum();

  void OnRate8();
  void OnRate11();
  void OnRate22();
  void OnRate44();
  void OnRate48();
  void OnRateOther();

  // Find track info by coordinate
  VTrack *FindTrack(int mouseX, int mouseY, bool label,
					wxRect *trackRect = NULL, int *trackNum = NULL);

  // Find track info by pointer
  bool FindTrack(VTrack *target, bool label,
				 wxRect *trackRect = NULL, int *trackNum = NULL);

  bool GetLabelFieldRect(wxRect &labelRect,
						 int field, bool rightOnly,
						 wxRect& fieldRect);

  void DrawRuler(wxDC *dc, bool text = true);
  void DrawTracks(wxDC *dc);

  TrackPanelListener *mListener;

  TrackList       *mTracks;
  ViewInfo        *mViewInfo;
  wxStatusBar     *mStatusBar;

  AudacityTimer   mTimer;
  int             mTimeCount;
  
  wxBitmap        *mBitmap;
  int             mPrevWidth;
  int             mPrevHeight;

  double          mSelStart;

  VTrack          *mCapturedTrack;
  wxRect          mCapturedRect;
  int             mCapturedNum;
  
  bool            mIndicatorShowing;
	
  int             mMouseClickX;
  int             mMouseClickY;

  int             mMouseMostRecentX;
  int             mMouseMostRecentY;

  int             mInitialTrackHeight;

  bool            mAutoScrolling;

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

  wxMenu          *mChannelMenu;
  wxMenu          *mRateMenu;
  wxMenu          *mDisplayMenu;

  VTrack          *mPopupMenuTarget;

public:

  DECLARE_EVENT_TABLE()
};

#endif

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
class TrackPanel;
class TrackArtist;
class Ruler;
struct ViewInfo;

class TrackPanelListener {
 public:
   virtual void TP_DisplayStatusMessage(const char *msg, int fieldNum) = 0;
   virtual int TP_GetCurrentTool() = 0;
   virtual void TP_OnPlayKey() = 0;
   virtual void TP_PushState(wxString desc) = 0;
   virtual void TP_RedrawScrollbars() = 0;
   virtual void TP_ScrollLeft() = 0;
   virtual void TP_ScrollRight() = 0;
   virtual void TP_ScrollWindow(double scrollto) = 0;
   virtual void TP_HasMouse() = 0;
   virtual void TP_HandleResize() = 0;
};

class TrackPanel:public wxWindow {
 public:

   TrackPanel(wxWindow * parent,
              wxWindowID id,
              const wxPoint & pos,
              const wxSize & size,
              TrackList * tracks,
              ViewInfo * viewInfo, TrackPanelListener * listener);

   virtual ~ TrackPanel();

   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void TrackSpecificMouseEvent(wxMouseEvent & event);
   void OnKeyEvent(wxKeyEvent & event);

   void OnTimer();

   int GetRulerHeight() const { return 22;}
   int GetLeftOffset() const { return GetLabelWidth() + 1;}

   void GetTracksUsableArea(int *width, int *height) const;

   void SelectNone();

   void ReReadSettings();
   void SetStop(bool bStopped);

 private:

   void DrawCursors();

   void ScrollDuringDrag();
   void UpdateIndicator();

   // AS: Selection handling
   void HandleSelect(wxMouseEvent & event);
   void SelectionHandleDrag(wxMouseEvent &event);
   void SelectionHandleClick(wxMouseEvent &event, 
			     VTrack* pTrack, wxRect r, int num);
   void StartSelection (int, int);
   void ExtendSelection(int, int);

   // AS: Cursor handling
   void HandleCursor(wxMouseEvent & event);

   // AS: Amplitude Envelope editing handlers
   void HandleEnvelope(wxMouseEvent & event);
   void ForwardEventToEnvelope(wxMouseEvent &event);

   // AS: Track sliding handlers
   void HandleSlide(wxMouseEvent & event);
   void StartSlide(wxMouseEvent &event, double& totalOffset, wxString& name);
   void DoSlide(wxMouseEvent &event, double& totalOffset);

   // AS: Handle zooming into tracks
   void HandleZoom(wxMouseEvent & event);
   void DragZoom(int x);
   void DoZoomInOut(wxMouseEvent &event, int x_center);


   void DoPopupMenu(wxMouseEvent &event, wxRect& titleRect, 
		    VTrack* t, wxRect &r, int num);


   void HandleResize(wxMouseEvent & event);


   void HandleLabelClick(wxMouseEvent & event);
   void HandleRearrange(wxMouseEvent & event);
   void CalculateRearrangingThresholds(wxMouseEvent & event);
   void HandleClosing(wxMouseEvent & event);
   void HandleMutingSoloing(wxMouseEvent & event, bool solo);
   bool MuteSoloFunc(VTrack *t, wxRect r, int x, int f, bool solo);
   void MakeParentRedrawScrollbars();
   
   // AS: Pushing the state preserves state for Undo operations.
   void MakeParentPushState(wxString desc);

   void MakeParentResize();

   void OnSetName();

   void OnMoveTrack    (wxEvent &event);
   void OnChangeOctave (wxEvent &event);
   void OnChannelChange(wxEvent &event);
   void OnSetDisplay   (wxEvent &event);

   void SetRate(VTrack *pTrack, double rate);
   void OnRateChange(wxEvent &event);
   void OnRateOther();

   void OnFormatChange(wxEvent &event);

   void OnSplitStereo();
   void OnMergeStereo();

   void RemoveTrack(VTrack * toRemove);

   // Find track info by coordinate
   VTrack *FindTrack(int mouseX, int mouseY, bool label,
                     wxRect * trackRect = NULL, int *trackNum = NULL);

   int GetTitleWidth() const { return 100; }
   int GetTitleOffset() const { return 0; }
   int GetVRulerWidth() const { return 30;}
   int GetVRulerOffset() const { return GetTitleOffset() + GetTitleWidth();}
   int GetLabelWidth() const { return GetTitleWidth() + GetVRulerWidth();}

   void SetLabelFont(wxDC * dc);

   void DrawRuler(wxDC * dc, bool text = true);
   void DrawRulerBorder   (wxDC* dc, wxRect &r);
   void DrawRulerSelection(wxDC* dc, const wxRect r);
   void DrawRulerMarks    (wxDC *dc, const wxRect r, bool text);
   void DrawRulerIndicator(wxDC *dc);

   void DrawTracks(wxDC * dc);

   void GetTrackControlsRect(const wxRect r, wxRect &dest) const;
   void GetCloseBoxRect(const wxRect r, wxRect &dest) const;
   void GetTitleBarRect(const wxRect r, wxRect &dest) const;
   void GetMuteSoloRect(const wxRect r, wxRect &dest, bool solo) const;

   void DrawCloseBox(wxDC * dc, const wxRect r, bool down);
   void DrawTitleBar(wxDC * dc, const wxRect r, VTrack * t, bool down);
   void DrawMuteSolo(wxDC * dc, const wxRect r, VTrack * t, bool down, bool solo);

   void DrawVRuler(wxDC * dc, const wxRect r, VTrack * t);

   void DrawEverythingElse(wxDC *dc, const wxRect panelRect, const wxRect clip);
   void DrawEverythingElse(VTrack *t, wxDC *dc, wxRect &r, wxRect &wxTrackRect);
   void DrawOutside(VTrack *t, wxDC *dc, const wxRect rec, const int labelw, 
		    const int vrul, const wxRect trackRect);
   void DrawZooming(wxDC* dc, const wxRect clip);

   void DrawShadow            (VTrack *t, wxDC* dc, const wxRect r);
   void DrawBordersAroundTrack(VTrack *t, wxDC* dc, const wxRect r, const int labelw, const int vrul);
   void FillInLabel           (VTrack *t, wxDC* dc, const wxRect r, const int labelw);
   void DrawOutsideOfTrack    (VTrack *t, wxDC* dc, const wxRect r);

   wxString TrackSubText(VTrack *t);

   TrackPanelListener *mListener;

   TrackList *mTracks;
   ViewInfo *mViewInfo;
   wxStatusBar *mStatusBar;

   Ruler *mRuler;

   TrackArtist *mTrackArtist;

   class AudacityTimer:public wxTimer {
   public:
     virtual void Notify() { parent->OnTimer(); }
     TrackPanel *parent;
   } mTimer;
   
   int mTimeCount;

   wxBitmap *mBitmap;
   int mPrevWidth;
   int mPrevHeight;

   double mSelStart;

   VTrack *mCapturedTrack;
   wxRect mCapturedRect;
   int mCapturedNum;

   bool mIndicatorShowing;

   int mMouseClickX;
   int mMouseClickY;

   int mMouseMostRecentX;
   int mMouseMostRecentY;

   int mZoomStart;
   int mZoomEnd;

   double PositionToTime(int mouseXCoordinate,
                         int trackLeftEdge) const;

   int mInitialTrackHeight;

   bool mAutoScrolling;

   bool mIsClosing;
   bool mIsSelecting;
   bool mIsResizing;
   bool mIsRearranging;
   bool mIsSliding;
   bool mIsEnveloping;
   bool mIsMuting;
   bool mIsSoloing;

   // JH: if the user is dragging a track, at what y
   //   coordinate should the dragging track move up or down?
   int mMoveUpThreshold;
   int mMoveDownThreshold;
   
   // AS: MAGIC NUMBER: I'm not sure why 3.
   bool IsDragZooming() const { return abs(mZoomEnd - mZoomStart) > 3;}

   wxCursor *mArrowCursor;
   wxCursor *mSelectCursor;
   wxCursor *mResizeCursor;
   wxCursor *mSlideCursor;
   wxCursor *mZoomInCursor;
   wxCursor *mZoomOutCursor;
   wxCursor *mRearrangeCursor;

   wxMenu *mWaveTrackMenu;
   wxMenu *mNoteTrackMenu;
   wxMenu *mLabelTrackMenu;
   wxMenu *mRateMenu;
   wxMenu *mFormatMenu;

   VTrack *mPopupMenuTarget;

 public:

   DECLARE_EVENT_TABLE()
};

#endif

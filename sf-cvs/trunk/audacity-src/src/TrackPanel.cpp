/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.cpp

  Dominic Mazzoni

  AS: The TrackPanel class is responsible for rendering the panel
      displayed to the left of a track.  TrackPanel also takes care
      of the functionality for each of the buttons in that panel.

**********************************************************************/

#include "TrackPanel.h"

#ifdef __WXMAC__
#ifdef __UNIX__
#include <Carbon/Carbon.h>
#endif
#endif

#include <math.h>
//#include <algorithm>

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/textctrl.h>
#include <wx/statusbr.h>
#include <wx/intl.h>

#include "AColor.h"
#include "AudioIO.h"
#include "Envelope.h"
#include "LabelTrack.h"
#include "NoteTrack.h"
#include "Track.h"
#include "TrackArtist.h"
#include "Prefs.h"
#include "Project.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

#include "widgets/Ruler.h"

#define kLeftInset 4
#define kTopInset 4

template <class LOW, class MID, class HIGH>
bool between_inclusive(LOW l, MID m, HIGH h)
{
   return ( m >= l && m <= h);
}

template <class LOW, class MID, class HIGH>
bool between_exclusive(LOW l, MID m, HIGH h)
{
   return ( m > l && m < h);
}

template <class CLIPPEE, class CLIPVAL>
void clip_top(CLIPPEE &clippee, CLIPVAL val)
{
   if (clippee > val) clippee = val;
}

template <class CLIPPEE, class CLIPVAL>
void clip_bottom(CLIPPEE &clippee, CLIPVAL val)
{
   if (clippee < val) clippee = val;
}

enum {
   TrackPanelFirstID = 2000,

   OnSetNameID,

   OnMoveUpID,
   OnMoveDownID,

   OnUpOctaveID,
   OnDownOctaveID,

   OnChannelLeftID,
   OnChannelRightID,
   OnChannelMonoID,

   OnRate8ID,
   OnRate11ID,
   OnRate16ID,
   OnRate22ID,
   OnRate44ID,
   OnRate48ID,
   OnRateOtherID,

   On16BitID,
   On24BitID,
   OnFloatID,

   OnWaveformID,
   OnWaveformDBID,
   OnSpectrumID,
   OnPitchID,

   OnSplitStereoID,
   OnMergeStereoID
};

BEGIN_EVENT_TABLE(TrackPanel, wxWindow)
    EVT_MOUSE_EVENTS(TrackPanel::OnMouseEvent)

    EVT_CHAR(TrackPanel::OnKeyEvent)

    EVT_PAINT(TrackPanel::OnPaint)

    EVT_MENU(OnSetNameID, TrackPanel::OnSetName)

    EVT_MENU_RANGE(OnMoveUpID     , OnMoveDownID   , TrackPanel::OnMoveTrack    )
    EVT_MENU_RANGE(OnUpOctaveID   , OnDownOctaveID , TrackPanel::OnChangeOctave )
    EVT_MENU_RANGE(OnChannelLeftID, OnChannelMonoID, TrackPanel::OnChannelChange)
    EVT_MENU_RANGE(OnWaveformID   , OnPitchID      , TrackPanel::OnSetDisplay   )
    EVT_MENU_RANGE(OnRate8ID      , OnRate48ID     , TrackPanel::OnRateChange   )
    EVT_MENU_RANGE(On16BitID      , OnFloatID     ,  TrackPanel::OnFormatChange   )
    EVT_MENU(OnRateOtherID, TrackPanel::OnRateOther)
    EVT_MENU(OnSplitStereoID, TrackPanel::OnSplitStereo)
    EVT_MENU(OnMergeStereoID, TrackPanel::OnMergeStereo)
END_EVENT_TABLE()

TrackPanel::TrackPanel(wxWindow * parent, wxWindowID id,
                           const wxPoint & pos,
                           const wxSize & size,
                           TrackList * tracks,
                           ViewInfo * viewInfo,
                           TrackPanelListener * listener):wxWindow(parent,
                                                                   id, pos,
                                                                   size,
                                                                   wxWANTS_CHARS),
mListener(listener), mTracks(tracks), mViewInfo(viewInfo), mBitmap(NULL),
mAutoScrolling(false)
{
   mIsClosing = false;
   mIsSelecting = false;
   mIsResizing = false;
   mIsRearranging = false;
   mIsSliding = false;
   mIsEnveloping = false;
   mIsMuting = false;
   mIsSoloing = false;

   mIndicatorShowing = false;

   mArrowCursor = new wxCursor(wxCURSOR_ARROW);
   mSelectCursor = new wxCursor(wxCURSOR_IBEAM);
   mSlideCursor = new wxCursor(wxCURSOR_SIZEWE);
   mResizeCursor = new wxCursor(wxCURSOR_SIZENS);
   mZoomInCursor = new wxCursor(wxCURSOR_MAGNIFIER);
   mZoomOutCursor = new wxCursor(wxCURSOR_MAGNIFIER);
   mRearrangeCursor = new wxCursor(wxCURSOR_HAND);

   mRateMenu = new wxMenu();
   mRateMenu->Append(OnRate8ID, "8000 Hz");
   mRateMenu->Append(OnRate11ID, "11025 Hz");
   mRateMenu->Append(OnRate16ID, "16000 Hz");
   mRateMenu->Append(OnRate22ID, "22050 Hz");
   mRateMenu->Append(OnRate44ID, "44100 Hz");
   mRateMenu->Append(OnRate48ID, "48000 Hz");
   mRateMenu->Append(OnRateOtherID, _("Other..."));

   mFormatMenu = new wxMenu();
   mFormatMenu->Append(On16BitID, GetSampleFormatStr(int16Sample));
   mFormatMenu->Append(On24BitID, GetSampleFormatStr(int24Sample));
   mFormatMenu->Append(OnFloatID, GetSampleFormatStr(floatSample));

   mWaveTrackMenu = new wxMenu();
   mWaveTrackMenu->Append(OnSetNameID, _("Name..."));
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(OnMoveUpID, _("Move Track Up"));
   mWaveTrackMenu->Append(OnMoveDownID, _("Move Track Down"));
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(OnWaveformID, _("Waveform"));
   mWaveTrackMenu->Append(OnWaveformDBID, _("Waveform (dB)"));
   mWaveTrackMenu->Append(OnSpectrumID, _("Spectrum"));
   mWaveTrackMenu->Append(OnPitchID, _("Pitch (EAC)"));
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(OnChannelMonoID, _("Mono"));
   mWaveTrackMenu->Append(OnChannelLeftID, _("Left Channel"));
   mWaveTrackMenu->Append(OnChannelRightID, _("Right Channel"));
   mWaveTrackMenu->Append(OnMergeStereoID, _("Make Stereo Track"));
   mWaveTrackMenu->Append(OnSplitStereoID, _("Split Stereo Track"));
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(0, _("Set Sample Format"), mFormatMenu);
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(0, _("Set Rate"), mRateMenu);

   mNoteTrackMenu = new wxMenu();
   mNoteTrackMenu->Append(OnSetNameID, _("Name..."));
   mNoteTrackMenu->AppendSeparator();
   mNoteTrackMenu->Append(OnMoveUpID, _("Move Track Up"));
   mNoteTrackMenu->Append(OnMoveDownID, _("Move Track Down"));
   mNoteTrackMenu->AppendSeparator();
   mNoteTrackMenu->Append(OnUpOctaveID, _("Up Octave"));
   mNoteTrackMenu->Append(OnDownOctaveID, _("Down Octave"));

   mLabelTrackMenu = new wxMenu();
   mLabelTrackMenu->Append(OnSetNameID, _("Name..."));
   mLabelTrackMenu->AppendSeparator();
   mLabelTrackMenu->Append(OnMoveUpID, _("Move Track Up"));
   mLabelTrackMenu->Append(OnMoveDownID, _("Move Track Down"));

   mTrackArtist = new TrackArtist();
   mTrackArtist->SetInset(1, kTopInset + 1, kLeftInset + 2, 2);

   mCapturedTrack = NULL;

   mPopupMenuTarget = NULL;

   mRuler = new Ruler();
   mRuler->SetLabelEdges(false);
   mRuler->SetFormat(Ruler::TimeFormat);

   mTimeCount = 0;
   mTimer.parent = this;
   mTimer.Start(50, FALSE);
}

TrackPanel::~TrackPanel()
{
   mTimer.Stop();

   if (mBitmap)
      delete mBitmap;

   delete mTrackArtist;

   delete mRuler;

   delete mArrowCursor;
   delete mSelectCursor;
   delete mSlideCursor;
   delete mResizeCursor;
   delete mZoomInCursor;
   delete mZoomOutCursor;
   delete mRearrangeCursor;

   // Note that the submenus (mRateMenu, ...)
   // are deleted by their parent
   delete mWaveTrackMenu;
   delete mNoteTrackMenu;
   delete mLabelTrackMenu;
}

void TrackPanel::ReReadSettings()
{
   gPrefs->Read("/GUI/AutoScroll", &mViewInfo->bUpdateTrackIndicator, true);
   gPrefs->Read("/GUI/UpdateSpectrogram", &mViewInfo->bUpdateSpectrogram, true);
}

void TrackPanel::SetStop(bool bStopped)
{
   mViewInfo->bIsPlaying = !bStopped;

   Refresh(false);
}

void TrackPanel::SelectNone()
{
   TrackListIterator iter(mTracks);
   VTrack *t = iter.First();
   while (t) {
      t->SetSelected(false);

      if (t->GetKind() == VTrack::Label)
         ((LabelTrack *) t)->Unselect();

      t = iter.Next();
   }
}

void TrackPanel::GetTracksUsableArea(int *width, int *height) const
{
   GetSize(width, height);

   *width -= GetLabelWidth();

   // AS: MAGIC NUMBER: What does 2 represent?
   *width -= 2 + kLeftInset;
}

// AS: This function draws the blinky cursor things, both in the
//  ruler as seen at the top of the screen, but also in each of the
//  selected tracks.
void TrackPanel::DrawCursors()
{
  // The difference between a wxClientDC and a wxPaintDC
  // is that the wxClientDC is used outside of a paint loop,
  // whereas the wxPaintDC is used inside a paint loop
  wxClientDC dc(this);
  dc.SetLogicalFunction(wxINVERT);
  dc.SetPen(*wxBLACK_PEN);
  dc.SetBrush(*wxBLACK_BRUSH);
  
  int x = GetLeftOffset() +
    int ((mViewInfo->sel0 - mViewInfo->h) * mViewInfo->zoom);
  
  int y = -mViewInfo->vpos + GetRulerHeight();
  

  // AS: Ah, no, this is where we draw the blinky thing in the ruler.
  if (!mIndicatorShowing) {
    // Draw cursor in ruler
    dc.DrawLine(x, 1, x, GetRulerHeight() - 2);
  }
  
  if (x >= GetLeftOffset()) {
    // Draw cursor in all selected tracks
    TrackListIterator iter(mTracks);
    for (VTrack *t = iter.First(); t; t = iter.Next()) {
      int height = t->GetHeight();
      if (t->GetSelected() && t->GetKind() != VTrack::Label)
	dc.DrawLine(x, y + kTopInset + 1, x, y + height - 2);
      y += height;
    }
  }
}

// AS: This gets called on our wx timer events.
void TrackPanel::OnTimer()
{
   // AS: If the user is dragging the mouse and there is a track that
   //  has captured the mouse, then scroll the screen, as necessary.
   if (mIsSelecting && mCapturedTrack) {
     ScrollDuringDrag();
   }

   // AS: The "indicator" is the little graphical mark shown in the ruler
   //  that indicates where the current play/record position is.  IsBusy
   //  is basically IsPlaying || IsRecording.
   if (mIndicatorShowing ||
       (gAudioIO->IsBusy() &&
        gAudioIO->GetProject() == (AudacityProject *) GetParent())) {
     UpdateIndicator();
   }

   // AS: Um, I get the feeling we want to redraw the cursors
   //  every 10 timer ticks or something...
   mTimeCount = (mTimeCount + 1) % 10;
   if (mTimeCount == 0 &&
       !mTracks->IsEmpty() && mViewInfo->sel0 == mViewInfo->sel1) {
     DrawCursors();
   }
}

// AS: We check on each timer tick to see if we need to scroll.
//  Scrolling is handled by mListener, which is an interface
//  to the window TrackPanel is embedded in.
void TrackPanel::ScrollDuringDrag()
{
   // DM: If we're "autoscrolling" (which means that we're scrolling
   //  because the user dragged from inside to outside the window,
   //  not because the user clicked in the scroll bar), then
   //  the selection code needs to be handled slightly differently.
   //  We set this flag ("mAutoScrolling") to tell the selecting
   //  code that we didn't get here as a result of a mouse event,
   //  and therefore it should ignore the mouseEvent parameter,
   //  and instead use the last known mouse position.  Setting
   //  this flag also causes the Mac to redraw immediately rather
   //  than waiting for the next update event; this makes scrolling
   //  smoother on MacOS 9.
  mAutoScrolling = true;
  
  if (mMouseMostRecentX > mCapturedRect.x + mCapturedRect.width)
    mListener->TP_ScrollRight();
  else if (mMouseMostRecentX < mCapturedRect.x)
    mListener->TP_ScrollLeft();

  // AS: To keep the selection working properly as we scroll,
  //  we fake a mouse event (remember, this function is called
  //  from a timer tick).
  wxMouseEvent e(wxEVT_MOTION); // AS: For some reason, GCC won't let us pass this directly.
  HandleSelect(e);

  mAutoScrolling = false;
}

// AS: This updates the indicator (on a timer tick) that shows
//  where the current play or record position is.  To do this,
//  we cheat a little.  The indicator is drawn during the ruler
//  drawing process (that should probably change, but...), so
//  we create a memory DC and tell the ruler to draw itself there,
//  antd then just blit that to the screen.
void TrackPanel::UpdateIndicator()
{
  double indicator = gAudioIO->GetIndicator();
  bool onScreen = between_inclusive(mViewInfo->h, indicator,
		   mViewInfo->h + mViewInfo->screen);
  
  // BG: Scroll screen if option is set
  if (mViewInfo->bUpdateTrackIndicator && gAudioIO->IsBusy()
                      && gAudioIO->GetProject() && !onScreen)
     mListener->TP_ScrollWindow(indicator);

  if (mIndicatorShowing || onScreen) {
    mIndicatorShowing = (onScreen &&
			 gAudioIO->IsBusy() &&
			 gAudioIO->GetProject() ==
			 (AudacityProject *) GetParent());

    int width, height;
    GetSize(&width, &height);
    height = GetRulerHeight();
    
    wxClientDC dc(this);
    wxMemoryDC memDC;
    wxBitmap rulerBitmap;
    rulerBitmap.Create(width, height);
    
    memDC.SelectObject(rulerBitmap);
    
    DrawRuler(&memDC, true);
    
    dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);
  }
}

// AS: This is what gets called during the normal course of needing
//  to repaint.
void TrackPanel::OnPaint(wxPaintEvent & event)
{
   wxPaintDC dc(this);
   int width, height;
   GetSize(&width, &height);
   if (width != mPrevWidth || height != mPrevHeight || !mBitmap) {
      mPrevWidth = width;
      mPrevHeight = height;

      if (mBitmap)
         delete mBitmap;

      mBitmap = new wxBitmap(width, height);
   }

   wxMemoryDC memDC;

   memDC.SelectObject(*mBitmap);

   DrawTracks(&memDC);
   DrawRuler(&memDC);

   dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);
}

// AS: Make our Parent (well, whoever is listening to us) push their state.
//  this causes application state to be preserved on a stack for undo ops.
void TrackPanel::MakeParentPushState(wxString desc)
{
   mListener->TP_PushState(desc);
}

void TrackPanel::MakeParentRedrawScrollbars()
{
   mListener->TP_RedrawScrollbars();
}

void TrackPanel::MakeParentResize()
{
   mListener->TP_HandleResize();
}

// AS: This is still bad unclean: it's dependant on select=0, envelope=1, 
//  move/slide=2, and zoom=3.  And this should go somewhere else...
const char *pMessages[] = {_("Click and drag to select audio"), 
			   _("Click and drag to edit the amplitude envelope"),
			   _("Click and drag to move a track in time"),
#if defined( __WXMAC__ )
			   _("Click to Zoom In, Shift-Click to Zoom Out")
#elif defined( __WXMSW__ )
			   _("Left-Click to Zoom In, Right-Click to Zoom Out")
#elif defined( __WXGTK__ )
			   _("Left=Zoom In, Right=Zoom Out, Middle=Normal")
#endif
};

// AS: This function is setting what icon your cursor is using
//  Also, it sets a message in the status bar.
void TrackPanel::HandleCursor(wxMouseEvent & event)
{
   // First, try to set the cursor based on the current action
   if      (mIsSelecting ) { SetCursor(*mSelectCursor); return; }
   else if (mIsSliding   ) { SetCursor(*mSlideCursor ); return; }
   else if (mIsEnveloping) { SetCursor(*mArrowCursor ); return; }
   else if (mIsRearranging){ SetCursor(*mRearrangeCursor); return; }

   wxRect r;
   int num;

   VTrack *label = FindTrack(event.m_x, event.m_y, true, &r, &num);
   VTrack *nonlabel = FindTrack(event.m_x, event.m_y, false, &r, &num);

   // if the mouse is over either a label *or* non-label part
   // of any track
   if (label || nonlabel) {

      // see if we are over the label, which can be
      // used to drag the track
      if(label) {
         mListener->
            TP_DisplayStatusMessage(_("Drag this track up or down to change the"
                     " order of the tracks."), 0);
         
      }
      // next test to see if we're over the area that
      // resizes a track
      else if (event.m_y >= (r.y + r.height - 5) &&
          event.m_y < (r.y + r.height + 5)) {
	// AS: MAGIC NUMBER: What is 5?

         mListener->
             TP_DisplayStatusMessage(_("Click and drag to resize the track"),
                                     0);
         SetCursor(*mResizeCursor);
      }
      else {
         int operation = mListener->TP_GetCurrentTool();
         mListener->TP_DisplayStatusMessage(
               pMessages[operation], 0);

         // Change the cursor based on the selected tool.
         switch (operation) {
                case selectTool:   SetCursor(*mSelectCursor); break;
                case envelopeTool: SetCursor(*mArrowCursor);  break;
                case slideTool:    SetCursor(*mSlideCursor);  break;
                case zoomTool:
            SetCursor(event.ShiftDown() ? *mZoomOutCursor : *mZoomInCursor);
            break;
         }
      }
   } else {
      // Not over a track
      SetCursor(*mArrowCursor);
   }
}

// AS: This function handles various ways of starting and extending
//  selections.  These are the selections you make by clicking and
//  dragging over a waveform.
void TrackPanel::HandleSelect(wxMouseEvent & event)
{
   // AS: Ok, did the user just click the mouse, release the mouse,
   //  or drag?
   if (event.ButtonDown(1)|| event.ButtonDown(3)) {
      wxRect r; 
      int num;  

      VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

      // AS: Now, did they click in a track somewhere?  If so, we want
      //  to extend the current selection or start a new selection, 
      //  depending on the shift key.  If not, cancel all selections.
      if (t)
         SelectionHandleClick(event, t, r, num);
      else 
         SelectNone();

      Refresh(false);
   }
   else if (event.ButtonUp(1) || event.ButtonUp(3)) {
      mCapturedTrack = NULL;
      mIsSelecting = false;
   }
   else if (event.ButtonDClick(1) && !event.ShiftDown()) {
      // Deselect all other tracks and select this one.
      SelectNone();
      mViewInfo->sel0 = mTracks->GetMinOffset();
      mViewInfo->sel1 = mTracks->GetMaxLen();
      mTracks->Select(mCapturedTrack);
      Refresh(false);
      mCapturedTrack = NULL;
      mIsSelecting = false;
   }
   else
      SelectionHandleDrag(event);
}

// AS: This function gets called when we're handling selection
//  and the mouse was just clicked.
void TrackPanel::SelectionHandleClick(wxMouseEvent &event, 
                     VTrack* pTrack, wxRect r, int num)
{
   mCapturedTrack = pTrack;
   mCapturedRect = r;
   mCapturedNum = num;

   mMouseClickX = event.m_x;
   mMouseClickY = event.m_y;

   if (event.ShiftDown() || event.ButtonDown(3)) {
      // If the shift button is down, extend the current selection.
      double x = PositionToTime(event.m_x, r.x);

      // Edit the selection boundary nearest the mouse click.
      if (fabs(x - mViewInfo->sel0) < fabs(x - mViewInfo->sel1))
         mSelStart = mViewInfo->sel1;
      else
         mSelStart = mViewInfo->sel0;

      mListener->TP_DisplayStatusMessage(
            wxString::Format(_("Selection: %lf - %lf s"),
                             mViewInfo->sel0, mViewInfo->sel1), 1);
   } else {
      // Otherwise, start a new selection
      SelectNone();
      StartSelection(event.m_x, r.x);

      mTracks->Select(pTrack);

      mListener->TP_DisplayStatusMessage(
            wxString::Format(_("Cursor: %lf s"), mSelStart), 1);
   }

   if (pTrack->GetKind() == VTrack::Label)
      ((LabelTrack *) pTrack)->MouseDown(mMouseClickX, mMouseClickY,
                                         mCapturedRect,
                                         mViewInfo->h,
                                         mViewInfo->zoom);
   mIsSelecting = true;
}

// AS: Reset our selection markers.
void TrackPanel::StartSelection(int mouseXCoordinate,
                                int trackLeftEdge)
{
   mSelStart =
      mViewInfo->h + ((mouseXCoordinate - trackLeftEdge)
            / mViewInfo->zoom);

   mViewInfo->sel0 = mSelStart;
   mViewInfo->sel1 = mSelStart;
}

// AS: If we're dragging to extend a selection (or actually,
//  if the screen is scrolling while you're selecting), we
//  handle it here.
void TrackPanel::SelectionHandleDrag(wxMouseEvent &event)
{
   // AS: If we're not in the process of selecting (set in
   //  the SelectionHandleClick above), fuhggeddaboudit.
   if (!mIsSelecting)
      return;

   if (event.Dragging() || mAutoScrolling) {
      wxRect r = mCapturedRect;
      int num = mCapturedNum;
      VTrack *pTrack = mCapturedTrack;

      // AS: Note that FindTrack will replace r and num's values.
      if (!pTrack)
         pTrack = FindTrack(event.m_x, event.m_y, false, &r, &num);

      if (pTrack) {      // Selecting
         int x = mAutoScrolling ? mMouseMostRecentX : event.m_x;
         int y = mAutoScrolling ? mMouseMostRecentY : event.m_y;

         ExtendSelection(x, r.x);

         mListener->
            TP_DisplayStatusMessage(wxString::
                  Format(_("Selection: %lf - %lf s"),
                     mViewInfo->sel0,
                     mViewInfo->sel1), 1);

         // Handle which tracks are selected
         int num2;
         if (0 != FindTrack(x, y, false, NULL, &num2)) {
            // The tracks numbered num...num2 should be selected

            TrackListIterator iter(mTracks);
            VTrack *t = iter.First();
            for (int i = 1; t; i++, t = iter.Next()) {
               if ((i >= num && i <= num2) || (i >= num2 && i <= num))
                  mTracks->Select(t);
            }
         }

         Refresh(false);

#ifdef __WXMAC__
         if (mAutoScrolling)
            MacUpdateImmediately();
#endif
      }
   }
}
  
// Extend the existing selection
void TrackPanel::ExtendSelection(int mouseXCoordinate,
                                 int trackLeftEdge)
{
   double selend = PositionToTime(mouseXCoordinate, trackLeftEdge);
   clip_bottom(selend, 0.0);

   mViewInfo->sel0 = wxMin(mSelStart, selend);
   mViewInfo->sel1 = wxMax(mSelStart, selend);
}

// DM: Converts a position (mouse X coordinate) to 
//  project time, in seconds.  Needs the left edge of
//  the track as an additional parameter.
double TrackPanel::PositionToTime(int mouseXCoordinate,
                                  int trackLeftEdge) const
{
   return mViewInfo->h + ((mouseXCoordinate - trackLeftEdge)
                          / mViewInfo->zoom);  
}

// AS: HandleEnvelope gets called when the user is changing the
//  amplitude envelope on a track.
void TrackPanel::HandleEnvelope(wxMouseEvent & event)
{
   if (event.ButtonDown(1)) {
      wxRect r;
      int num;
      mCapturedTrack = FindTrack(event.m_x, event.m_y, false, &r, &num);

      if (!mCapturedTrack) 
         return;

      mCapturedRect = r;
      mCapturedRect.y += kTopInset;
      mCapturedRect.height -= kTopInset;
      mCapturedNum = num;
   }

   // AS: if there's actually a selected track, then forward all of the
   //  mouse events to its envelope.
   if (mCapturedTrack)
      ForwardEventToEnvelope(event);

   if (event.ButtonUp(1)) {
      mCapturedTrack = NULL;
      MakeParentPushState(_("Adjusted envelope."));
   }
}

// AS: The Envelope class actually handles things at the mouse
//  event level, so we have to forward the events over.  Envelope
//  will then tell us wether or not we need to redraw.
// AS: I'm not sure why we can't let the Envelope take care of
//  redrawing itself.  ?
void TrackPanel::ForwardEventToEnvelope(wxMouseEvent &event)
{
   if (!mCapturedTrack || mCapturedTrack->GetKind() != VTrack::Wave)
      return;

   WaveTrack* pwavetrack = (WaveTrack*) mCapturedTrack;
   Envelope *penvelope = pwavetrack->GetEnvelope();

   // AS: WaveTracks can be displayed in several different formats.
   //  This asks which one is in use. (ie, Wave, Spectrum, etc)
   int display = pwavetrack->GetDisplay();
   bool needUpdate = false;

   // AS: If we're using the right type of display for envelope operations
   //  ie one of the Wave displays
   if (display <= 1) {
      bool dB = (display == 1);

      // AS: Then forward our mouse event to the envelope.  It'll recalculate
      //  and then tell us wether or not to redraw.
      needUpdate = penvelope->MouseEvent(event, mCapturedRect,
            mViewInfo->h, mViewInfo->zoom, dB);

      // If this track is linked to another track, make the identical
      // change to the linked envelope:
      WaveTrack *link = (WaveTrack*) mTracks->GetLink(mCapturedTrack);
      if (link) {
         Envelope *e2 = link->GetEnvelope();
         needUpdate |= e2->MouseEvent(event, mCapturedRect,
               mViewInfo->h, mViewInfo->zoom, dB);
      }
   }

   if (needUpdate)
      Refresh(false);
}

// AS: "Sliding" is when the user is moving a wavetrack's offset.
//  You can use the slide tool and drag around where a track starts. 
void TrackPanel::HandleSlide(wxMouseEvent & event)
{
   // AS: Why do we have static variables here?!?
   //  this is a MEMBER function, so we could make
   //  member variables or static class members
   //  instead (depending on what we actually need).
   static double totalOffset;
   static wxString name;

   if (event.ButtonDown(1))
      StartSlide(event, totalOffset, name);

   if (!mIsSliding)
      return;

   if (event.Dragging() && mCapturedTrack)
      DoSlide(event, totalOffset);


   if (event.ButtonUp(1)) {
      mCapturedTrack = NULL;
      mIsSliding = false;
      MakeParentRedrawScrollbars();
      if(totalOffset != 0)
         MakeParentPushState(
               wxString::Format(_("Slid track '%s' %s %.02f seconds"), 
                  name.c_str(),
                  totalOffset > 0 ? _("right") : _("left"),
                  totalOffset > 0 ? totalOffset : -totalOffset));
   }
}

// AS: Pepare for sliding.
void TrackPanel::StartSlide(wxMouseEvent &event, double& totalOffset,
                            wxString& name)
{
   totalOffset = 0;

   wxRect r;
   int num;

   VTrack *vt = FindTrack(event.m_x, event.m_y, false, &r, &num);

   if (vt) {
      // AS: This name is used when we put a message in the undo list via
      //  MakeParentPushState on ButtonUp.
      name = vt->GetName();

      mCapturedTrack = vt;
      mCapturedRect = r;
      mCapturedNum = num;

      mMouseClickX = event.m_x;
      mMouseClickY = event.m_y;

      mSelStart = mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);
      mIsSliding = true;
   }
}

//AS: Change the selected track's (and its link's, if one) offset.
void TrackPanel::DoSlide(wxMouseEvent &event, double& totalOffset)
{
   double selend = mViewInfo->h +
      ((event.m_x - mCapturedRect.x) / mViewInfo->zoom);

   clip_bottom(selend, 0.0);

   if (selend != mSelStart) {
      mCapturedTrack->Offset(selend - mSelStart);
      totalOffset += selend - mSelStart;

      VTrack *link = mTracks->GetLink(mCapturedTrack);
      if (link)
         link->Offset(selend - mSelStart);

      Refresh(false);
   }

   mSelStart = selend;
}

// AS: This function takes care of our different zoom 
//  possibilities.  It is possible for a user to just
//  "zoom in" or "zoom out," but it is also possible 
//  for a user to drag and select an area that he
//  or she wants to be zoomed in on.  We use mZoomStart
//  and mZoomEnd to track the beggining and end of such
//  a zoom area.  Note that the ViewInfo member
//  mViewInfo actually keeps track of our zoom constant,
//  so we achieve zooming by altering the zoom constant
//  and forcing a refresh.
void TrackPanel::HandleZoom(wxMouseEvent &event)
{
   if (event.ButtonDown(1) || event.ButtonDClick(1)) {
      mZoomStart = event.m_x;
      mZoomEnd = event.m_x;
   }
   else if (event.Dragging()) {
      mZoomEnd = event.m_x;

      if (IsDragZooming())
         Refresh(false);
   }
   else if (event.ButtonUp()) {
      //if (mZoomEnd < mZoomStart)
         //std::swap(mZoomEnd, mZoomStart);

      wxRect r;

      if (IsDragZooming())
         DragZoom(r.x);
      else
         DoZoomInOut(event, r.x);

      mZoomEnd = mZoomStart = 0;

      MakeParentRedrawScrollbars();
      Refresh(false);
   }
}

// AS: This actually sets the Zoom value when you're done doing
//  a drag zoom.
void TrackPanel::DragZoom(int trackLeftEdge)
{
   double left  = PositionToTime(mZoomStart, trackLeftEdge);
   double right = PositionToTime(mZoomEnd  , trackLeftEdge);      

   mViewInfo->zoom *= mViewInfo->screen/(right-left);
   if (mViewInfo->zoom > 6000000)
      mViewInfo->zoom = 6000000;
   if(mViewInfo->zoom <= 1.0)
      mViewInfo->zoom = 1.0;

   mViewInfo->h = left;
}

// AS: This handles normal Zoom In/Out, if you just clicked;
//  IOW, if you were NOT dragging to zoom an area.
// AS: MAGIC NUMBER: We've got several in this function.
void TrackPanel::DoZoomInOut(wxMouseEvent &event, int trackLeftEdge)
{
   double center_h = PositionToTime(event.m_x, trackLeftEdge);

   if (event.RightUp() || event.RightDClick() || event.ShiftDown())
   {
      mViewInfo->zoom /= 2.0;
      if(mViewInfo->zoom <= 1.0)
         mViewInfo->zoom = 1.0;
   }
   else
   {
      mViewInfo->zoom *= 2.0;
      if (mViewInfo->zoom > 6000000)
         mViewInfo->zoom = 6000000;
   }

   if (event.MiddleUp() || event.MiddleDClick())
      mViewInfo->zoom = 44100.0 / 512.0;  // AS: Reset zoom.

   double new_center_h = PositionToTime(event.m_x, trackLeftEdge);

   mViewInfo->h += (center_h - new_center_h);
}

// AS: This is for when a given track gets the x.
void TrackPanel::HandleClosing(wxMouseEvent & event)
{
   VTrack *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   wxRect closeRect;
   GetCloseBoxRect(r, closeRect);

   wxClientDC dc(this);

   if (event.Dragging())
      DrawCloseBox(&dc, r, closeRect.Inside(event.m_x, event.m_y));
   else if (event.ButtonUp(1)) {
      DrawCloseBox(&dc, r, false);
      if (closeRect.Inside(event.m_x, event.m_y)) {
         if (!gAudioIO->IsRecording(t))
            RemoveTrack(t);
         mCapturedTrack = 0;
      }

      mIsClosing = false;
   }

   // BG: There are no more tracks on screen, so set zoom to normal
   if(mTracks->IsEmpty())
   {
      mViewInfo->zoom = 44100.0 / 512.0;
      mListener->TP_RedrawScrollbars();
      Refresh(false);
   }
}

// This actually removes the specified track.  Called from HandleClosing.
void TrackPanel::RemoveTrack(VTrack * toRemove)
{
   TrackListIterator iter(mTracks);

   VTrack *partner = mTracks->GetLink(toRemove);
   wxString name;

   VTrack *t = iter.First();
   while (t) {
      if (t == toRemove || t == partner) {
         name = t->GetName();
         delete t;
         t = iter.RemoveCurrent();
      }
      else t = iter.Next();
   }

   MakeParentPushState(wxString::Format(_("Removed track '%s.'"),
                                        name.c_str()));
   MakeParentRedrawScrollbars();
   MakeParentResize();
   Refresh(false);
}

// AS: Handle when the mute or solo button is pressed for some track.
void TrackPanel::HandleMutingSoloing(wxMouseEvent & event, bool solo)
{
   VTrack *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   wxRect buttonRect;
   GetMuteSoloRect(r, buttonRect, solo);

   wxClientDC dc(this);

   if (event.Dragging())
      DrawMuteSolo(&dc, r, t, buttonRect.Inside(event.m_x, event.m_y), solo);
   else if (event.ButtonUp(1)) {

      if (buttonRect.Inside(event.m_x, event.m_y))
      {
         if (solo) t->SetSolo(!t->GetSolo()); 
         else t->SetMute(!t->GetMute());
      }

      DrawMuteSolo(&dc, r, t, false, solo);
      if (solo) {
         mIsSoloing = false; 
         DrawMuteSolo(&dc, r, t, false, !solo);
      }
      else mIsMuting = false;
   }
}

// AS: This function gets called when a user clicks on the
//  title of a track, dropping down the menu.
void TrackPanel::DoPopupMenu(wxMouseEvent &event, wxRect& titleRect, 
			     VTrack* t, wxRect& r, int num)
{
   mPopupMenuTarget = t;
   {
      wxClientDC dc(this);
      SetLabelFont(&dc);
      DrawTitleBar(&dc, r, t, true);
   }
   bool canMakeStereo = false;
   VTrack *next = mTracks->GetNext(t);

   wxMenu *theMenu = NULL;

   if (t->GetKind() == VTrack::Wave) {
      theMenu = mWaveTrackMenu;
      if (next && !t->GetLinked() && !next->GetLinked()
               && t->GetKind() == VTrack::Wave
               && next->GetKind() == VTrack::Wave)
         canMakeStereo = true;

      theMenu->Enable(OnMergeStereoID, canMakeStereo);
      theMenu->Enable(OnSplitStereoID, t->GetLinked());
      theMenu->Enable(OnChannelMonoID, !t->GetLinked());
      theMenu->Enable(OnChannelLeftID, !t->GetLinked());
      theMenu->Enable(OnChannelRightID, !t->GetLinked());

      int display = ((WaveTrack *) t)->GetDisplay();

      theMenu->Enable(OnWaveformID, display != WaveTrack::WaveformDisplay);
      theMenu->Enable(OnWaveformDBID, display != WaveTrack::WaveformDBDisplay);
      theMenu->Enable(OnSpectrumID, display != WaveTrack::SpectrumDisplay);
      theMenu->Enable(OnPitchID, display != WaveTrack::PitchDisplay);
   }

   if (t->GetKind() == VTrack::Note)
      theMenu = mNoteTrackMenu;

   if (t->GetKind() == VTrack::Label)
      theMenu = mLabelTrackMenu;

   if (theMenu) {

      theMenu->Enable(OnMoveUpID, mTracks->CanMoveUp(t));
      theMenu->Enable(OnMoveDownID, mTracks->CanMoveDown(t));

#ifdef __WXMAC__
      ::InsertMenu((OpaqueMenuHandle *)mRateMenu->GetHMenu(), -1);
      ::InsertMenu((OpaqueMenuHandle *)mFormatMenu->GetHMenu(), -1);
#endif

      PopupMenu(theMenu, titleRect.x + 1,
            titleRect.y + titleRect.height + 1);

#ifdef __WXMAC__
      ::DeleteMenu(mFormatMenu->MacGetMenuId());
      ::DeleteMenu(mRateMenu->MacGetMenuId());
#endif
   }

   VTrack *t2 = FindTrack(event.m_x, event.m_y, true, &r, &num);
   if (t2 == t) {
      wxClientDC dc(this);
      SetLabelFont(&dc);
      DrawTitleBar(&dc, r, t, false);
   }
}

// AS: This handles when the user clicks on the "Label" area
//  of a track, ie the part with all the buttons and the drop
//  down menu, etc.
void TrackPanel::HandleLabelClick(wxMouseEvent & event)
{
   // AS: If not a click, ignore the mouse event.
   if (!(event.ButtonDown(1) || event.ButtonDClick(1)))
      return;

   wxRect r;
   int num;

   VTrack *t = FindTrack(event.m_x, event.m_y, true, &r, &num);

   // AS: If the user clicked outside all tracks, make nothing
   //  selected.
   if (!t) {
      SelectNone();
      Refresh(false);
      return;
   }

   bool second = false;
   if (!t->GetLinked() && mTracks->GetLink(t))
      second = true;

   wxRect closeRect;
   GetCloseBoxRect(r, closeRect);

   // AS: If they clicked on the x (ie, close button) on this
   //  track, then we capture the mouse and display the x
   //  as depressed.  Somewhere else, when the mouse is released,
   //  we'll see if we're still supposed to close the track.
   if (!second && closeRect.Inside(event.m_x, event.m_y)) {
      wxClientDC dc(this);
      DrawCloseBox(&dc, r, true);
      mIsClosing = true;
      mCapturedTrack = t;
      mCapturedRect = r;
      return;
   }

   wxRect titleRect;
   GetTitleBarRect(r, titleRect);

   // AS: If the clicked on the title area, show the popup menu.
   if (!second && titleRect.Inside(event.m_x, event.m_y)) {
     DoPopupMenu(event, titleRect, t, r, num);
     return;
   }
   
   // DM: Check Mute and Solo buttons on WaveTracks:
   if (!second && t->GetKind() == VTrack::Wave) {
     if (MuteSoloFunc(t, r, event.m_x, event.m_y, false) ||
         MuteSoloFunc(t, r, event.m_x, event.m_y, true ))
       return;
   }      
   
   // DM: If it's a NoteTrack, it has special controls
   if (!second && t && t->GetKind() == VTrack::Note) {
      wxRect midiRect;
      GetTrackControlsRect(r, midiRect);
      if (midiRect.Inside(event.m_x, event.m_y)) {
         ((NoteTrack *) t)->LabelClick(midiRect, event.m_x, event.m_y,
                                       event.RightDown()
                                       || event.RightDClick());
         Refresh(false);
         return;
      }
   }

   // DM: If they weren't clicking on a particular part of a track label,
   //  deselect other tracks and select this one.

   // JH: also, capture the current track for rearranging, so the user
   //  can drag the track up or down to swap it with others
   mCapturedTrack = t;
   mIsRearranging = true;
   TrackPanel::CalculateRearrangingThresholds(event);

   // AS: If the shift botton is being held down, then just invert 
   //  the selection on this track.
   if (event.ShiftDown()) {
      mTracks->Select(t, !t->GetSelected());
      Refresh(false);
      return;
   }

   SelectNone();
   mTracks->Select(t);
   mViewInfo->sel0 = mTracks->GetMinOffset();
   mViewInfo->sel1 = mTracks->GetMaxLen();
   Refresh(false);
}


// JH: the user is dragging one of the tracks: change the track order
//   accordingly
void TrackPanel::HandleRearrange(wxMouseEvent & event)
{
   // are we finishing the drag?
   if(event.ButtonUp(1)) {
      mCapturedTrack = NULL;
      mIsRearranging = false;
      return;
   }
   if(event.m_y < mMoveUpThreshold)
      mTracks->MoveUp(mCapturedTrack);
   else if(event.m_y > mMoveDownThreshold)
      mTracks->MoveDown(mCapturedTrack);
   else
      return;

   // JH: if we moved up or down, recalculate the thresholds
   TrackPanel::CalculateRearrangingThresholds(event);
   Refresh(false);
}
      

// JH: figure out how far the user must drag the mouse up or down
//   before the track will swap with the one above or below
void TrackPanel::CalculateRearrangingThresholds(wxMouseEvent & event)
{
   wxASSERT(mCapturedTrack);

   // JH: this will probably need to be tweaked a bit, I'm just
   //   not sure what formula will have the best feel for the
   //   user.

   if(mTracks->CanMoveUp(mCapturedTrack))
      mMoveUpThreshold = event.m_y - mTracks->GetPrev(mCapturedTrack)->GetHeight();
   else
      mMoveUpThreshold = INT_MIN;

   if(mTracks->CanMoveDown(mCapturedTrack))
      mMoveDownThreshold = event.m_y + mTracks->GetNext(mCapturedTrack)->GetHeight();
   else
      mMoveDownThreshold = INT_MAX;
}

// AS: Mute or solo the given track (t).  If solo is true, we're 
//  soloing, otherwise we're muting.  Basically, check and see 
//  whether x and y fall within the  area of the appropriate button.
bool TrackPanel::MuteSoloFunc(VTrack *t, wxRect r, int x, int y, bool solo)
{
  wxRect buttonRect;
  GetMuteSoloRect(r, buttonRect, solo);
  if (buttonRect.Inside(x, y)) {

    wxClientDC dc(this);
    DrawMuteSolo(&dc, r, t, true, solo);

    if (solo) mIsSoloing = true; 
    else mIsMuting = true;

    mCapturedTrack = t;
    mCapturedRect = r;

    return true;
  }

  return false;
}


// DM: HandleResize gets called when:
//  1. A mouse-down event occurs in the "resize region" of a track,
//     i.e. to change its vertical height.
//  2. A mouse event occurs and mIsResizing==true (i.e. while
//     the resize is going on)
void TrackPanel::HandleResize(wxMouseEvent & event)
{
   // DM: ButtonDown means they just clicked and haven't released yet.
   //  We use this opportunity to save which track they clicked on,
   //  and the initial height of the track, so as they drag we can
   //  update the track size.
   if (event.ButtonDown(1)) {

      wxRect r;
      int num;

      // DM: Figure out what track is about to be resized
      VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

      if (t) {

         // DM: Capture the track so that we continue to resize
         //  THIS track, no matter where the user moves the mouse
         mCapturedTrack = t;
         //mCapturedRect = r;
         //mCapturedNum = num;

         // DM: Save the initial mouse location and the initial height
         mMouseClickX = event.m_x;
         mMouseClickY = event.m_y;
         mInitialTrackHeight = t->GetHeight();

         mIsResizing = true;
      }
   }
   else if (mIsResizing) {

      // DM: Dragging means that the mouse button IS down and has moved
      //  from its initial location.  By the time we get here, we
      //  have already received a ButtonDown() event and saved the
      //  track being resized in mCapturedTrack.
      if (event.Dragging()) {
         int delta = (event.m_y - mMouseClickY);
         int newTrackHeight = mInitialTrackHeight + delta;
         if (newTrackHeight < 20)
            newTrackHeight = 20;
         mCapturedTrack->SetHeight(newTrackHeight);
         Refresh(false);
      }

      // DM: This happens when the button is released from a drag.
      //  Since we actually took care of resizing the track when
      //  we got drag events, all we have to do here is clean up.
      //  We also push the state so that this action is undo-able.
      else if (event.ButtonUp(1)) {
         mCapturedTrack = NULL;
         mIsResizing = false;
         MakeParentRedrawScrollbars();
         MakeParentPushState("TrackPanel::HandleResize() FIXME!!");
      }
   }
}

// AS: Handle key presses by the user.  Notably, play and stop when the
//  user presses the spacebar.  Also, LabelTracks can be typed into.
void TrackPanel::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown() || event.AltDown()) {
      event.Skip();
      return;
   }

   TrackListIterator iter(mTracks);

   // AS: This logic seems really flawed.  We send keyboard input
   //  to the first LabelTrack we find, not to the one with focus?
   //  I guess the problem being that we don't have "focus."
   for (VTrack * t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == VTrack::Label && t->GetSelected()) {
         ((LabelTrack *) t)->KeyEvent(mViewInfo->sel0, mViewInfo->sel1,
                                      event);
         Refresh(false);
         MakeParentPushState("TrackPanel::OnKeyEvent() FIXME!!");
         return;
      }
   }

   switch (event.KeyCode()) {
      case WXK_SPACE:
         mListener->TP_OnPlayKey();
         break;
      case WXK_PRIOR:
         // BG: Page right
         mListener->TP_ScrollWindow((mViewInfo->h + mViewInfo->screen) - (mViewInfo->screen/6));
         break;
      case WXK_NEXT:
         // BG: Page left
         mListener->TP_ScrollWindow((mViewInfo->h - mViewInfo->screen) + (mViewInfo->screen/6));
         break;
      case WXK_RIGHT:
         // BG: Scroll right
         mListener->TP_ScrollWindow((mViewInfo->h + mViewInfo->screen) - (mViewInfo->screen*.95));
         break;
      case WXK_LEFT:
         // BG: Scroll left
         mListener->TP_ScrollWindow((mViewInfo->h - mViewInfo->screen) + (mViewInfo->screen*.95));
         break;
      case WXK_HOME:
         // BG: Skip to beginning
         mViewInfo->sel0 = 0;
         if (!event.ShiftDown() || mViewInfo->sel1 < mViewInfo->sel0)
            mViewInfo->sel1 = 0;
         mListener->TP_ScrollWindow(0);
         break;
      case WXK_END:
         // BG: Skip to end
         mViewInfo->sel1 = mTracks->GetMaxLen();
         if (!event.ShiftDown() || mViewInfo->sel0 > mViewInfo->sel1)
            mViewInfo->sel0 = mViewInfo->sel1;
         mListener->TP_ScrollWindow(mViewInfo->sel1);
         break;
      default:
         event.Skip();
         break;
   }
}

// AS: This handles just generic mouse events.  Then, based
//  on our current state, we forward the mouse events to
//  various interested parties.
void TrackPanel::OnMouseEvent(wxMouseEvent & event)
{
   mListener->TP_HasMouse();

   if (!mAutoScrolling) {
      mMouseMostRecentX = event.m_x;
      mMouseMostRecentY = event.m_y;
   }

   if (event.ButtonDown(1)) {
      mCapturedTrack = NULL;

      wxActivateEvent e;
      GetParent()->ProcessEvent(e);
   }

   if (mIsClosing)
      HandleClosing(event);
   else if (mIsMuting)
      HandleMutingSoloing(event, false);
   else if (mIsSoloing)
      HandleMutingSoloing(event, true);
   else if (mIsResizing) {
      HandleResize(event);
      HandleCursor(event);
   }
   else if(mIsRearranging)
      HandleRearrange(event);
   else
      TrackSpecificMouseEvent(event);
}

// AS: I don't really understand why this code is sectioned off
//  from the other OnMouseEvent code.
void TrackPanel::TrackSpecificMouseEvent(wxMouseEvent & event)
{
   wxRect r;
   int dummy;

   FindTrack(event.m_x, event.m_y, false, &r, &dummy);

   // AS: MAGIC NUMBER: Hey, here's that 5 again.  What's
   //  going on here?
   if (event.ButtonDown(1) &&
         event.m_y >= (r.y + r.height - 5) &&
         event.m_y < (r.y + r.height + 5)) {
      HandleResize(event);
      HandleCursor(event);
      return;
   }

   if (!mCapturedTrack && event.m_x < GetLabelWidth()) {
      HandleLabelClick(event);
      HandleCursor(event);
      return;
   }

   switch (mListener->TP_GetCurrentTool()) {
      case selectTool:   HandleSelect  (event); break;
      case envelopeTool: HandleEnvelope(event); break;
      case slideTool:    HandleSlide   (event); break;
      case zoomTool:     HandleZoom    (event); break;
   }

   if ((event.Moving() || event.ButtonUp(1)) &&
         !mIsSelecting && !mIsEnveloping && !mIsSliding) {
      HandleCursor(event);
   }
   if (event.ButtonUp(1)) {
      mCapturedTrack = NULL;
   }
}

void TrackPanel::SetLabelFont(wxDC * dc)
{
   int fontSize = 10;
#ifdef __WXMSW__
   fontSize = 8;
#endif

   wxFont labelFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc->SetFont(labelFont);
}

void TrackPanel::DrawRuler(wxDC * dc, bool text)
{
   wxRect r;

   GetSize(&r.width, &r.height);
   r.x = 0;
   r.y = 0;
   r.height = GetRulerHeight() - 1;

   DrawRulerBorder(dc, r);

   if (mViewInfo->sel0 < mViewInfo->sel1)
      DrawRulerSelection(dc, r);

   DrawRulerMarks(dc, r, text);

   if (gAudioIO->IsBusy() &&
         gAudioIO->GetProject() == (AudacityProject *) GetParent())
      DrawRulerIndicator(dc);
}

void TrackPanel::DrawRulerBorder(wxDC* dc, wxRect &r)
{
   // Draw ruler border
   AColor::Medium(dc, false);
   dc->DrawRectangle(r);

   r.width--;
   r.height--;
   AColor::Bevel(*dc, true, r);

   dc->SetPen(*wxBLACK_PEN);
   dc->DrawLine(r.x, r.y + r.height + 1, r.x + r.width + 1,
                r.y + r.height + 1);
}

void TrackPanel::DrawRulerSelection(wxDC* dc, const wxRect r)
{
   // Draw selection
   double sel0 = mViewInfo->sel0 - mViewInfo->h +
      GetLeftOffset() / mViewInfo->zoom;
   double sel1 = mViewInfo->sel1 - mViewInfo->h +
      GetLeftOffset() / mViewInfo->zoom;

   if (sel0 < 0.0)
      sel0 = 0.0;
   if (sel1 > (r.width / mViewInfo->zoom))
      sel1 = r.width / mViewInfo->zoom;

   int p0 = int (sel0 * mViewInfo->zoom + 0.5);
   int p1 = int (sel1 * mViewInfo->zoom + 0.5);

   wxBrush selectedBrush;
   selectedBrush.SetColour(148, 148, 170);
   wxPen selectedPen;
   selectedPen.SetColour(148, 148, 170);
   dc->SetBrush(selectedBrush);
   dc->SetPen(selectedPen);

   wxRect sr;
   sr.x = p0;
   sr.y = 1;
   sr.width = p1 - p0 - 1;
   sr.height = GetRulerHeight() - 3;
   dc->DrawRectangle(sr);
}

void TrackPanel::DrawRulerMarks(wxDC *dc, const wxRect r, bool text)
{
   mRuler->SetBounds(r.x, r.y, r.x+r.width-1, r.y+r.height-1);
   double min = mViewInfo->h - GetLeftOffset() / mViewInfo->zoom;
   double max = min + r.width / mViewInfo->zoom;
   mRuler->SetRange(min, max);
   
   mRuler->Draw(*dc);
}

void TrackPanel::DrawRulerIndicator(wxDC *dc)
{
   // Draw indicator
   double ind = gAudioIO->GetIndicator();

   if (ind >= mViewInfo->h && ind <= (mViewInfo->h + mViewInfo->screen)) {
      int indp =
         GetLeftOffset() +
         int ((ind - mViewInfo->h) * mViewInfo->zoom);

      dc->SetPen(*wxTRANSPARENT_PEN);
      dc->SetBrush(*wxBLACK_BRUSH);

      int indsize = 6;

      wxPoint tri[3];
      tri[0].x = indp;
      tri[0].y = indsize + 1;
      tri[1].x = indp - indsize;
      tri[1].y = 1;
      tri[2].x = indp + indsize;
      tri[2].y = 1;

      dc->DrawPolygon(3, tri);
   }
}

void TrackPanel::GetCloseBoxRect(const wxRect r, wxRect & dest) const
{
   dest.x = r.x;
   dest.y = r.y;
   dest.width = 16;
   dest.height = 16;
}

void TrackPanel::GetTitleBarRect(const wxRect r, wxRect & dest) const
{
   dest.x = r.x + 16;
   dest.y = r.y;
   dest.width = GetTitleWidth() - r.x - 16;
   dest.height = 16;
}

void TrackPanel::GetMuteSoloRect(const wxRect r, wxRect &dest, bool solo) const
{
   dest.x = r.x + 8;
   dest.y = r.y + 50;
   dest.width = 36;
   dest.height = 16;

   if (solo) dest.x += 36 + 8;
}

void TrackPanel::GetTrackControlsRect(const wxRect r, wxRect & dest) const
{
   dest = r;
   dest.width = GetTitleWidth();
   dest.x += 4 + kLeftInset;
   dest.width -= (8 + kLeftInset);
   dest.y += 18 + kTopInset;
   dest.height -= (24 + kTopInset);
}

void TrackPanel::DrawCloseBox(wxDC * dc, const wxRect r, bool down)
{
   dc->SetPen(*wxBLACK_PEN);
   dc->DrawLine(r.x + 3, r.y + 3, r.x + 13, r.y + 13);  // close "x"
   dc->DrawLine(r.x + 13, r.y + 3, r.x + 3, r.y + 13);
   wxRect bev;
   GetCloseBoxRect(r, bev);
   bev.Inflate(-1, -1);
   AColor::Bevel(*dc, !down, bev);
}

void TrackPanel::DrawTitleBar(wxDC * dc, const wxRect r, VTrack * t, bool down)
{
   wxRect bev;
   GetTitleBarRect(r, bev);
   bev.Inflate(-1, -1);
   AColor::Bevel(*dc, true, bev);

   // Draw title text
   wxString titleStr = t->GetName();
   int allowableWidth = GetTitleWidth() - 38 - kLeftInset;
   long textWidth, textHeight;
   dc->GetTextExtent(titleStr, &textWidth, &textHeight);
   while (textWidth > allowableWidth) {
      titleStr = titleStr.Left(titleStr.Length() - 1);
      dc->GetTextExtent(titleStr, &textWidth, &textHeight);
   }
   dc->DrawText(titleStr, r.x + 19, r.y + 2);

   // Pop-up triangle
   dc->SetPen(*wxBLACK_PEN);
   int xx = r.x + GetTitleWidth() - 16 - kLeftInset;
   int yy = r.y + 5;
   int triWid = 11;
   while (triWid >= 1) {
      dc->DrawLine(xx, yy, xx + triWid, yy);
      xx++;
      yy++;
      triWid -= 2;
   }

   AColor::Bevel(*dc, !down, bev);
}

// AS: Draw the Mute or the Solo button, depending on the value of solo.
void TrackPanel::DrawMuteSolo(wxDC * dc, const wxRect r, VTrack *t, bool down, bool solo)
{
   wxRect bev;
   GetMuteSoloRect(r, bev, solo);
   bev.Inflate(-1, -1);
   (solo) ? AColor::Solo(dc, t->GetSolo(), t->GetSelected()) : 
            AColor::Mute(dc, t->GetMute(), t->GetSelected(), t->GetSolo());
   dc->DrawRectangle(bev);

   long textWidth, textHeight;
   wxString str = (solo) ? _("Solo") : _("Mute");
   SetLabelFont(dc);
   dc->GetTextExtent(str, &textWidth, &textHeight);
   dc->DrawText(str, bev.x + (bev.width - textWidth)/2, bev.y + 2);

   AColor::Bevel(*dc, !down, bev);
}

// AS: Draw the actual track areas.  We only draw the borders
//  and the little buttons and menues and whatnot here, the
//  actual contents of each track are drawn by the TrackArtist.
void TrackPanel::DrawTracks(wxDC * dc)
{
   wxRect clip;
   GetSize(&clip.width, &clip.height);

   wxRect panelRect = clip;
   panelRect.y = -mViewInfo->vpos;

   // Make room for ruler
   panelRect.y += GetRulerHeight();
   panelRect.height -= GetRulerHeight();

   wxRect tracksRect = panelRect;
   tracksRect.x += GetLabelWidth();
   tracksRect.width -= GetLabelWidth();

   // AS: Presumably 1 indicates that we are using the "Envelope"
   //  adjusting tool (for changing the amplitude envelope).
   bool envelopeFlag = (mListener->TP_GetCurrentTool() == 1);

   // The track artist actually draws the stuff inside each track
   mTrackArtist->DrawTracks(mTracks, *dc, tracksRect,
                            clip, mViewInfo, envelopeFlag);

   DrawEverythingElse(dc, panelRect, clip);
}

void TrackPanel::DrawEverythingElse(wxDC *dc, const wxRect panelRect, const wxRect clip)
{
   // We draw everything else
   TrackListIterator iter(mTracks);

   wxRect trackRect = panelRect;
   wxRect r;

   for (VTrack *t = iter.First();t;t = iter.Next())
      DrawEverythingElse(t, dc, r, trackRect );

   if (IsDragZooming())
      DrawZooming(dc, clip);

   // Paint over the part below the tracks
   GetSize(&trackRect.width, &trackRect.height);
   AColor::Dark(dc, false);
   dc->DrawRectangle(trackRect);
}

// AS: Note that this is being called in a loop and that the parameter values
//  are expected to be maintained each time through.
void TrackPanel::DrawEverythingElse(VTrack *t, wxDC *dc, wxRect &r, wxRect &trackRect)
{
   trackRect.height = t->GetHeight();

   // Draw label area
   SetLabelFont(dc);
   dc->SetTextForeground(wxColour(0, 0, 0));

   int labelw = GetLabelWidth();
   int vrul = GetVRulerOffset();

   // If this track is linked to the next one, display a common
   // border for both, otherwise draw a normal border
   r = trackRect;

   bool skipBorder = false;
   if (t->GetLinked())
      r.height += mTracks->GetLink(t)->GetHeight();
   else if (mTracks->GetLink(t))
      skipBorder = true;

   if (!skipBorder)
      DrawOutside(t, dc, r, labelw, vrul, trackRect);

   r = trackRect;
   r.x += GetVRulerOffset();
   r.y += kTopInset;
   r.width = GetVRulerWidth();
   r.height -= (kTopInset + 2);
   mTrackArtist->DrawVRuler(t, dc, r);

   trackRect.y += t->GetHeight();
}

void TrackPanel::DrawZooming(wxDC* dc, const wxRect clip)
{
   // Draw zooming indicator
   wxRect r;

   r.x = mZoomStart;
   r.y = -1;
   r.width = mZoomEnd - mZoomStart;
   r.height = clip.height+2;

   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_DASHED_PEN);

   dc->DrawRectangle(r);
}

void TrackPanel::DrawOutside(VTrack *t, wxDC* dc, const wxRect rec, const int labelw, 
			     const int vrul, const wxRect trackRect)
{
   wxRect r = rec;

   DrawOutsideOfTrack     (t, dc, r);

   r.x += kLeftInset;
   r.y += kTopInset;
   r.width -= kLeftInset * 2;
   r.height -= kTopInset;

   FillInLabel            (t, dc, r, labelw);
   DrawBordersAroundTrack (t, dc, r, labelw, vrul);
   DrawShadow             (t, dc, r);

   r.width = GetTitleWidth();
   DrawCloseBox(dc, r, false);
   DrawTitleBar(dc, r, t, false);

   if (t->GetKind() == VTrack::Wave) {
      DrawMuteSolo(dc, r, t, false, false);
      DrawMuteSolo(dc, r, t, false, true );
   }

   r = trackRect;

   if (t->GetKind() == VTrack::Wave) {
      dc->DrawText(TrackSubText(t), r.x + 6, r.y + 22);
      dc->DrawText(GetSampleFormatStr(((WaveTrack *)t)->GetSampleFormat()),
                   r.x + 6, r.y + 38);
   }
   else if (t->GetKind() == VTrack::Note) {
      wxRect midiRect;
      GetTrackControlsRect(trackRect, midiRect);
      ((NoteTrack *) t)->DrawLabelControls(*dc, midiRect);

   }
}

void TrackPanel::DrawOutsideOfTrack(VTrack *t, wxDC* dc, const wxRect r)
{
   // Fill in area outside of the track
   AColor::Dark(dc, false);
   wxRect side = r;
   side.width = kLeftInset;
   dc->DrawRectangle(side);
   side = r;
   side.height = kTopInset;
   dc->DrawRectangle(side);
   side = r;
   side.x += side.width - kTopInset;
   side.width = kTopInset;
   dc->DrawRectangle(side);

   if (t->GetLinked()) {
      side = r;
      side.y += t->GetHeight() - 1;
      side.height = kTopInset + 1;
      dc->DrawRectangle(side);
   }
}

void TrackPanel::FillInLabel(VTrack *t, wxDC* dc, const wxRect r, const int labelw)
{
   // fill in label
   wxRect fill = r;
   fill.width = labelw - r.x;
   AColor::Medium(dc, t->GetSelected());
   dc->DrawRectangle(fill);
}

void TrackPanel::DrawBordersAroundTrack(VTrack *t, wxDC* dc, const wxRect r, const int vrul, const int labelw)
{
   // Borders around track and label area
   dc->SetPen(*wxBLACK_PEN);
   dc->DrawLine(r.x, r.y, r.x + r.width - 1, r.y);        // top
   dc->DrawLine(r.x, r.y, r.x, r.y + r.height - 1);       // left
   dc->DrawLine(r.x, r.y + r.height - 2, r.x + r.width - 1, r.y + r.height - 2);  // bottom
   dc->DrawLine(r.x + r.width - 2, r.y, r.x + r.width - 2, r.y + r.height - 1);   // right
   dc->DrawLine(vrul, r.y, vrul, r.y + r.height - 1);
   dc->DrawLine(labelw, r.y, labelw, r.y + r.height - 1); // between vruler and track

   if (t->GetLinked()) {
      int h1 = r.y + t->GetHeight() - kTopInset;
      dc->DrawLine(vrul, h1 - 2, r.x + r.width - 1, h1 - 2);
      dc->DrawLine(vrul, h1 + kTopInset, r.x + r.width - 1,
            h1 + kTopInset);
   }

   dc->DrawLine(r.x, r.y + 16, GetTitleWidth(), r.y + 16);  // title bar
   dc->DrawLine(r.x + 16, r.y, r.x + 16, r.y + 16);         // close box
}

void TrackPanel::DrawShadow(VTrack *t, wxDC* dc, const wxRect r)
{
   // shadow
   AColor::Dark(dc, true);
   // bottom
   dc->DrawLine(r.x + 1, r.y + r.height - 1, r.x + r.width, r.y + r.height - 1);
   // right
   dc->DrawLine(r.x + r.width - 1, r.y + 1, r.x + r.width - 1, r.y + r.height);
}

// AS: Returns the string to be displayed in the track label
//  indicating whether the track is mono, left, right, or 
//  stereo and what sample rate it's using.
wxString TrackPanel::TrackSubText(VTrack *t)
{
   wxString s =
      wxString::Format("%dHz",
            (int) (((WaveTrack *) t)->GetRate() +
                   0.5));
   if (t->GetLinked())
      s = _("Stereo, ") + s;
   else {
      if (t->GetChannel() == VTrack::MonoChannel)
         s = _("Mono, ") + s;
      else if (t->GetChannel() == VTrack::LeftChannel)
         s = _("Left, ") + s;
      else if (t->GetChannel() == VTrack::RightChannel)
         s = _("Right, ") + s;
   }

   return s;
}

// AS: Handle the menu options that change a track between
//  left channel, right channel, and mono.
int channels[] = {VTrack::LeftChannel, VTrack::RightChannel,
		  VTrack::MonoChannel};

const char* channelmsgs[] = {"'left' channel", "'right' channel",
			     "'mono'"};

void TrackPanel::OnChannelChange(wxEvent &event)
{
   int id = event.GetId();
   wxASSERT (id >= OnChannelLeftID && id <= OnChannelMonoID);
   wxASSERT (mPopupMenuTarget);
   mPopupMenuTarget->SetChannel(channels[id - OnChannelLeftID]);
   MakeParentPushState(
         wxString::Format(_("Changed '%s' to %s"),
                          mPopupMenuTarget->GetName().c_str(),
                          channelmsgs[id - OnChannelLeftID]));
   mPopupMenuTarget = NULL;
   Refresh(false);
}

// AS: Split a stereo track into two tracks... ??
void TrackPanel::OnSplitStereo()
{
   wxASSERT (mPopupMenuTarget);
   mPopupMenuTarget->SetLinked(false);
   MakeParentPushState(
         wxString::Format(_("Split stereo track '%s'"),
			  mPopupMenuTarget->GetName().c_str()));
   Refresh(false);
}

// AS: Merge two tracks into one steroe track ??
void TrackPanel::OnMergeStereo()
{
   wxASSERT (mPopupMenuTarget); 
   mPopupMenuTarget->SetLinked(true);
   VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
   if (partner) {
      mPopupMenuTarget->SetChannel(VTrack::LeftChannel);
      partner->SetChannel(VTrack::RightChannel);
      MakeParentPushState(
            wxString::Format(_("Made '%s' a stereo track"),
               mPopupMenuTarget->GetName().c_str()));
   } else
      mPopupMenuTarget->SetLinked(false);
   Refresh(false);
}

// AS: Set the Display mode based on the menu choice in the Track Menu.
//  Note that gModes MUST BE IN THE SAME ORDER AS THE MENU CHOICES!!
const char* gModes[] = {"waveform", "waveformDB", "spectrum", "pitch"};
void TrackPanel::OnSetDisplay(wxEvent &event)
{
   int id = event.GetId();
   wxASSERT (id >= OnWaveformID && id <= OnPitchID);
   wxASSERT (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave);
   ((WaveTrack *) mPopupMenuTarget)->SetDisplay(id - OnWaveformID);
   VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
   if (partner)
      ((WaveTrack *) partner)->SetDisplay(id - OnWaveformID);
   MakeParentPushState(
         wxString::Format(_("Changed '%s' to %s display"),
                          mPopupMenuTarget->GetName().c_str(),
                          gModes[id - OnWaveformID]));
   mPopupMenuTarget = NULL;
   ReReadSettings();
   Refresh(false);
}

// AS: Sets the sample rate for a track, and if it is linked to
//  another track, that one as well.
void TrackPanel::SetRate(VTrack *pTrack, double rate)
{
   ((WaveTrack *) pTrack)->SetRate(rate);
   VTrack *partner = mTracks->GetLink(pTrack);
   if (partner)
      ((WaveTrack *) partner)->SetRate(rate);
   MakeParentPushState(
         wxString::Format(_("Changed '%s' to %d Hz"), 
                          pTrack->GetName().c_str(), rate));
}

// DM: Handles the selection from the Format submenu of the
//  track menu.
void TrackPanel::OnFormatChange(wxEvent &event)
{
   int id = event.GetId();
   wxASSERT(id >= On16BitID && id <= OnFloatID); 
   wxASSERT (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave);

   sampleFormat newFormat = int16Sample;

   switch(id) {
   case On16BitID:
      newFormat = int16Sample;
      break;
   case On24BitID:
      newFormat = int24Sample;
      break;
   case OnFloatID:
      newFormat = floatSample;
      break;
   default:
      // ERROR -- should not happen
      break;
   }

   ((WaveTrack *)mPopupMenuTarget)->ConvertToSampleFormat(newFormat);
   VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
   if (partner)
      ((WaveTrack *) partner)->ConvertToSampleFormat(newFormat);

   MakeParentPushState(
         wxString::Format(_("Changed '%s' to %s"), 
                          mPopupMenuTarget->GetName().c_str(),
                          GetSampleFormatStr(newFormat)));

   mPopupMenuTarget = NULL;
   MakeParentRedrawScrollbars();
   Refresh(false);
}

// AS: Ok, this function handles the selection from the Rate
//  submenu of the track menu, except for "Other" (see OnRateOther).
//  gRates MUST CORRESPOND DIRECTLY TO THE RATES AS LISTED IN THE MENU!!
//  IN THE SAME ORDER!!
int gRates[] = {8000, 11025, 16000, 22050, 44100, 48000};
void TrackPanel::OnRateChange(wxEvent &event)
{
   int id = event.GetId();
   wxASSERT(id >= OnRate8ID && id <= OnRate48ID); 
   wxASSERT (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave);

   SetRate(mPopupMenuTarget, gRates[id - OnRate8ID]);

   mPopupMenuTarget = NULL;
   MakeParentRedrawScrollbars();
   Refresh(false);
}

// AS: This function handles the case when the user selects "Other"
//  from the Rate submenu on the Track menu.
void TrackPanel::OnRateOther()
{
   wxASSERT (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave);

   wxString defaultStr;
   defaultStr.Printf("%d",
         (int)(((WaveTrack *) mPopupMenuTarget)->GetRate()+0.5));

   // AS: TODO: REMOVE ARTIFICIAL CONSTANTS!!
   // AS: Make a real dialog box out of this!!
   double theRate;
   do {
      wxString rateStr =
         wxGetTextFromUser(_("Enter a sample rate in Hz (per second) "
                  "between 1 and 100000:"),
               _("Set Rate"),
               defaultStr);

      // AS: Exit if they type in nothing.
      if ("" == rateStr)
         return;

      rateStr.ToDouble(&theRate);
      if (theRate >= 1 && theRate <= 100000)
         break;
      else
         wxMessageBox(_("Invalid rate."));

   } while (1);

   SetRate(mPopupMenuTarget, theRate);

   mPopupMenuTarget = NULL;
   MakeParentRedrawScrollbars();
   Refresh(false);
}

// AS: Move a track up or down, depending.
const char* gMove[] = {"up", "down"};
void TrackPanel::OnMoveTrack(wxEvent &event)
{
   wxASSERT(event.GetId() == OnMoveUpID || event.GetId() == OnMoveDownID );
   if (mTracks->Move(mPopupMenuTarget, OnMoveUpID == event.GetId())) {
      MakeParentPushState(
            wxString::Format(_("Moved '%s' %s"),
               mPopupMenuTarget->GetName().c_str(),
               gMove[event.GetId() - OnMoveUpID]));
      Refresh(false);
   }
}

// AS: This only applies to MIDI tracks.  Presumably, it shifts the
//  whole sequence by an octave.
void TrackPanel::OnChangeOctave(wxEvent &event)
{
   wxASSERT (event.GetId() == OnUpOctaveID || event.GetId() == OnDownOctaveID);
   wxASSERT(mPopupMenuTarget->GetKind() == VTrack::Note);
   NoteTrack *t = (NoteTrack*) mPopupMenuTarget;

   bool bDown = (OnDownOctaveID == event.GetId());
   t->SetBottomNote( t->GetBottomNote() + ((bDown) ? -12 : 12));

   MakeParentPushState("TrackPanel::OnChangeOctave() FIXME!!");
   Refresh(false);
}

void TrackPanel::OnSetName()
{
   VTrack *t = mPopupMenuTarget;

   if (t) {
      wxString defaultStr = t->GetName();
      wxString newName = wxGetTextFromUser(_("Change track name to:"),
                                           _("Track Name"),
                                           defaultStr);
      if (newName != "")
         t->SetName(newName);
      MakeParentPushState(
          wxString::Format(_("Renamed '%s' to '%s'"),
                           defaultStr.c_str(), newName.c_str()));
      Refresh(false);
   }
}

VTrack *TrackPanel::FindTrack(int mouseX, int mouseY, bool label,
                              wxRect * trackRect, int *trackNum)
{
   wxRect r;
   r.x = 0;
   r.y = -mViewInfo->vpos;
   r.y += GetRulerHeight();
   r.y += kTopInset;
   GetSize(&r.width, &r.height);

   if (label) {
      r.width = GetLabelWidth() - kLeftInset;
   } else {
      r.x += GetLabelWidth() + 1;
      r.width -= GetLabelWidth() - 3;
   }

   TrackListIterator iter(mTracks);

   int n = 1;

   for (VTrack *t = iter.First(); t; r.y += r.height, n++, t = iter.Next()) {
      r.height = t->GetHeight();

      if (r.Inside(mouseX, mouseY)) {
         if (trackRect) {
            r.y -= kTopInset;
            if (label) {
               r.x += kLeftInset;
               r.width -= kLeftInset;
               r.y += kTopInset;
               r.height -= kTopInset;
            }
            *trackRect = r;
         }
         if (trackNum)
            *trackNum = n;
         return t;
      }
   }

   if (mouseY >= r.y && trackNum)
      *trackNum = n - 1;

   return NULL;
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.cpp

  Dominic Mazzoni

  AS: The TrackPanel class is responsible for rendering the panel
      displayed to the left of a track.  TrackPanel also takes care
      of the functionality for each of the buttons in that panel.

**********************************************************************/

#include "Audacity.h"

#include "TrackPanel.h"

#ifdef __MACOSX__
#include <Carbon/Carbon.h>
#endif

#include <math.h>

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/textctrl.h>
#include <wx/intl.h>

#include "AColor.h"
#include "AudioIO.h"
#include "ControlToolBar.h"
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

#if defined(__WXMAC__) && !defined(__UNIX__)
#include <Menus.h>
#endif

#define kLeftInset 4
#define kTopInset 4


// Is the distance between A and B less than D?
template < class A, class B, class DIST > bool within(A a, B b, DIST d)
{
   return (a > b - d) && (a < b + d);
}

template < class LOW, class MID, class HIGH >
    bool between_inclusive(LOW l, MID m, HIGH h)
{
   return (m >= l && m <= h);
}

template < class LOW, class MID, class HIGH >
    bool between_exclusive(LOW l, MID m, HIGH h)
{
   return (m > l && m < h);
}

template < class CLIPPEE, class CLIPVAL >
    void clip_top(CLIPPEE & clippee, CLIPVAL val)
{
   if (clippee > val)
      clippee = val;
}

template < class CLIPPEE, class CLIPVAL >
    void clip_bottom(CLIPPEE & clippee, CLIPVAL val)
{
   if (clippee < val)
      clippee = val;
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

    EVT_MENU_RANGE(OnMoveUpID, OnMoveDownID, TrackPanel::OnMoveTrack)
    EVT_MENU_RANGE(OnUpOctaveID, OnDownOctaveID, TrackPanel::OnChangeOctave)
    EVT_MENU_RANGE(OnChannelLeftID, OnChannelMonoID,
               TrackPanel::OnChannelChange)
    EVT_MENU_RANGE(OnWaveformID, OnPitchID, TrackPanel::OnSetDisplay)
    EVT_MENU_RANGE(OnRate8ID, OnRate48ID, TrackPanel::OnRateChange)
    EVT_MENU_RANGE(On16BitID, OnFloatID, TrackPanel::OnFormatChange)
    EVT_MENU(OnRateOtherID, TrackPanel::OnRateOther)
    EVT_MENU(OnSplitStereoID, TrackPanel::OnSplitStereo)
    EVT_MENU(OnMergeStereoID, TrackPanel::OnMergeStereo)
    END_EVENT_TABLE()

    TrackPanel::TrackPanel(wxWindow * parent, wxWindowID id,
                           const wxPoint & pos,
                           const wxSize & size,
                           TrackList * tracks,
                           ViewInfo * viewInfo,
                           TrackPanelListener * listener)
:wxWindow(parent, id, pos, size, wxWANTS_CHARS),
mListener(listener), mTracks(tracks), mViewInfo(viewInfo), mBitmap(NULL),
mAutoScrolling(false)
{
   mIsClosing = false;
   mIsSelecting = false;
   mIsResizing = false;
   mIsResizingBetweenLinkedTracks = false;
   mIsResizingBelowLinkedTracks = false;
   mIsRearranging = false;
   mIsSliding = false;
   mIsEnveloping = false;
   mIsMuting = false;
   mIsSoloing = false;

   mIndicatorShowing = false;

   mArrowCursor = new wxCursor(wxCURSOR_ARROW);
   mPencilCursor = new wxCursor(wxCURSOR_PENCIL);
   mSelectCursor = new wxCursor(wxCURSOR_IBEAM);
   mSlideCursor = new wxCursor(wxCURSOR_SIZEWE);
   mResizeCursor = new wxCursor(wxCURSOR_SIZENS);
   mZoomInCursor = new wxCursor(wxCURSOR_MAGNIFIER);
   mZoomOutCursor = new wxCursor(wxCURSOR_MAGNIFIER);
   mRearrangeCursor = new wxCursor(wxCURSOR_HAND);
   mAdjustLeftSelectionCursor = new wxCursor(wxCURSOR_POINT_LEFT);
   mAdjustRightSelectionCursor = new wxCursor(wxCURSOR_POINT_RIGHT);
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

   //Initialize the indicator playing state, so that
   //we know that no drawing line needs to be erased.
   mPlayIndicatorExists=false;

}

TrackPanel::~TrackPanel()
{
   mTimer.Stop();

   if (mBitmap)
      delete mBitmap;

   delete mTrackArtist;

   delete mRuler;

   delete mArrowCursor;
   delete mPencilCursor;
   delete mSelectCursor;
   delete mSlideCursor;
   delete mResizeCursor;
   delete mZoomInCursor;
   delete mZoomOutCursor;
   delete mRearrangeCursor;
   delete mAdjustLeftSelectionCursor;
   delete mAdjustRightSelectionCursor;


   // Note that the submenus (mRateMenu, ...)
   // are deleted by their parent
   delete mWaveTrackMenu;
   delete mNoteTrackMenu;
   delete mLabelTrackMenu;
}

void TrackPanel::ReReadSettings()
{
   gPrefs->Read("/GUI/AutoScroll", &mViewInfo->bUpdateTrackIndicator,
                true);
   gPrefs->Read("/GUI/UpdateSpectrogram", &mViewInfo->bUpdateSpectrogram,
                true);
}

void TrackPanel::SetStop(bool bStopped)
{
   mViewInfo->bIsPlaying = !bStopped;

   
   Refresh(false);
}

void TrackPanel::SelectNone()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      t->SetSelected(false);

      if (t->GetKind() == Track::Label)
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
      for (Track * t = iter.First(); t; t = iter.Next()) {
         int height = t->GetHeight();
         if (t->GetSelected() && t->GetKind() != Track::Label)
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
   wxMouseEvent e(wxEVT_MOTION);        // AS: For some reason, GCC won't let us pass this directly.
   HandleSelect(e);

   mAutoScrolling = false;
}

// AS: This updates the indicator (on a timer tick) that shows
//  where the current play or record position is.  To do this,
//  we cheat a little.  The indicator is drawn during the ruler
//  drawing process (that should probably change, but...), so
//  we create a memory DC and tell the ruler to draw itself there,
//  and then just blit that to the screen.
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
    
      //Draw the line across all tracks specifying where play is
      DrawTrackIndicator(&dc);


      wxMemoryDC memDC;
      wxBitmap rulerBitmap;
      rulerBitmap.Create(width, height);

      memDC.SelectObject(rulerBitmap);

      DrawRuler(&memDC, true);
    
      
      dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);
   }
}

// AS: OnPaint( ) is called during the normal course of 
//  completing a repaint operation.
void TrackPanel::OnPaint(wxPaintEvent & /* event */)
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
//  move/slide=2, zoom=3, and draw=4.  And this should go somewhere else...
const char *pMessages[] = { _("Click and drag to select audio"),
   _("Click and drag to edit the amplitude envelope"),
   _("Click and drag to move a track in time"),
#if defined( __WXMAC__ )
   _("Click to Zoom In, Shift-Click to Zoom Out"),
#elif defined( __WXMSW__ )
   _("Left-Click to Zoom In, Right-Click to Zoom Out"),
#elif defined( __WXGTK__ )
   _("Left=Zoom In, Right=Zoom Out, Middle=Normal"),
#endif
   _("Click and drag to edit the samples")
};


// AS: THandleCursor( ) sets the cursor drawn at the mouse location.
//  As this procedure checks which region the mouse is over, it is
//  appropriate to establish the message in the status bar.
void TrackPanel::HandleCursor(wxMouseEvent & event)
{
   // (1), If possible, set the cursor based on the current activity
   //      ( leave the StatusBar alone ).
   if (mIsSelecting) {
      SetCursor(*mSelectCursor);
      return;
   } else if (mIsSliding) {
      SetCursor(*mSlideCursor);
      return;
   } else if (mIsEnveloping) {
      SetCursor(*mArrowCursor);
      return;
   } else if (mIsRearranging) {
      SetCursor(*mRearrangeCursor);
      return;
   } else {
      // none of the above

   }

   // (2) If we are not over a track at all, set the cursor to Arrow and 
   //     clear the StatusBar, 
   wxRect r;
   int num;
   Track *label = FindTrack(event.m_x, event.m_y, true, &r, &num);
   Track *nonlabel = FindTrack(event.m_x, event.m_y, false, &r, &num);
   Track *t = label ? label : nonlabel;

   if (!t) {
      SetCursor(*mArrowCursor);
      mListener->TP_DisplayStatusMessage("", 0);
      return;
   }

   const char *tip = 0;

   // (3) Set a status message if over a label 
   if (label) {
      tip = _("Drag the label vertically to change the "
              "order of the tracks.");
        SetCursor(*mArrowCursor);
   }
   // Otherwise, we must be over the wave window 
   else {
      int operation = mListener->TP_GetCurrentTool();
      tip = pMessages[operation];

      // Change the cursor based on the selected tool.
      switch (operation) {
      case selectTool:
         SetCursor(*mSelectCursor);

         //Make sure we are within the selected track
         if (t && t->GetSelected()) {

            int leftSel = TimeToPosition(mViewInfo->sel0, r.x);
            int rightSel = TimeToPosition(mViewInfo->sel1, r.x);

            if (leftSel < rightSel) {

            } else {

               // Something is wrong if right edge comes before left edge
               wxASSERT(!(rightSel < leftSel));
            }

            // Is the cursor over the left selection boundary?
            if (within(event.m_x, leftSel, SELECTION_RESIZE_REGION)) {
               tip = _("Click and drag to move left selection boundary.");
               SetCursor(*mAdjustLeftSelectionCursor);
            }
            // Is the cursor over the right selection boundary?
            else if (within(event.m_x, rightSel, SELECTION_RESIZE_REGION)) {
               tip = _("Click and drag to move right selection boundary.");
               SetCursor(*mAdjustRightSelectionCursor);
            }
         }
         break;

      case envelopeTool:
         SetCursor(*mArrowCursor);
         break;
      case slideTool:
         SetCursor(*mSlideCursor);
         break;
      case zoomTool:
         SetCursor(event.ShiftDown()? *mZoomOutCursor : *mZoomInCursor);
         break;
      case drawTool:
         SetCursor(*mPencilCursor);
         break;
      }
   }

   // Are we within the vertical resize area?
   if (within(event.m_y, r.y + r.height, TRACK_RESIZE_REGION)) {
      //Check to see whether it is the first channel of a stereo track
      if (t->GetLinked()) {
         if (!label) {
            tip = _("Click and drag to adjust relative size "
                    "of stereo tracks.");
            SetCursor(*mResizeCursor);
         }
      } else {
         tip = _("Click and drag to resize the track.");
         SetCursor(*mResizeCursor);
      }
   }

   if (tip)
      mListener->TP_DisplayStatusMessage(tip, 0);
}


// AS: This function handles various ways of starting and extending
//  selections.  These are the selections you make by clicking and
//  dragging over a waveform.
void TrackPanel::HandleSelect(wxMouseEvent & event)
{
   // AS: Ok, did the user just click the mouse, release the mouse,
   //  or drag?
   if (event.ButtonDown(1)) {
      wxRect r;
      int num;

      Track *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

      // AS: Now, did they click in a track somewhere?  If so, we want
      //  to extend the current selection or start a new selection, 
      //  depending on the shift key.  If not, cancel all selections.
      if (t)
         SelectionHandleClick(event, t, r, num);
      else
         SelectNone();
      
      Refresh(false);
      
   } else if (event.ButtonUp(1) || event.ButtonUp(3)) {
      mCapturedTrack = NULL;
      mIsSelecting = false;
   } else if (event.ButtonDClick(1) && !event.ShiftDown()) {
      if (!mCapturedTrack)
         return;

      // Deselect all other tracks and select this one.
      SelectNone();
      
      mTracks->Select(mCapturedTrack);
      mViewInfo->sel0 = mCapturedTrack->GetOffset();
      mViewInfo->sel1 = mCapturedTrack->GetEndTime();

      Refresh(false);
      DisplaySelection();

      mCapturedTrack = NULL;
      mIsSelecting = false;
   } else
      SelectionHandleDrag(event);
}

// AS: This function gets called when we're handling selection
//  and the mouse was just clicked.
void TrackPanel::SelectionHandleClick(wxMouseEvent & event,
                                      Track * pTrack, wxRect r, int num)
{
   mCapturedTrack = pTrack;
   mCapturedRect = r;
   mCapturedNum = num;

   mMouseClickX = event.m_x;
   mMouseClickY = event.m_y;
   bool startNewSelection = true;

   if (event.ShiftDown()) {
      // If the shift button is down, extend the current selection.
      double x = PositionToTime(event.m_x, r.x);

      // Edit the selection boundary nearest the mouse click.
      if (fabs(x - mViewInfo->sel0) < fabs(x - mViewInfo->sel1))
         mSelStart = mViewInfo->sel1;
      else
         mSelStart = mViewInfo->sel0;

      DisplaySelection();
   } else {

      //Make sure you are within the selected track
      if (pTrack && pTrack->GetSelected()) {

         int leftSel = TimeToPosition(mViewInfo->sel0, r.x);
         int rightSel = TimeToPosition(mViewInfo->sel1, r.x);
         wxASSERT(leftSel <= rightSel);

         // Check to see if the cursor is on top 
         // of the left selection boundary
         if (within(event.m_x, leftSel, SELECTION_RESIZE_REGION)) {
            // Pin the right selection boundary
            mSelStart = mViewInfo->sel1;
            mIsSelecting = true;
            startNewSelection = false;
         } else if (within(event.m_x, rightSel, SELECTION_RESIZE_REGION)) {
            // Pin the left selection boundary
            mSelStart = mViewInfo->sel0;
            mIsSelecting = true;
            startNewSelection = false;
         }
      }

      if (startNewSelection) {
         // If we didn't move a selection boundary, start a new selection
         SelectNone();
         StartSelection(event.m_x, r.x);
         mTracks->Select(pTrack);
         DisplaySelection();
      }

      if (pTrack->GetKind() == Track::Label)
         ((LabelTrack *) pTrack)->MouseDown(mMouseClickX, mMouseClickY,
                                            mCapturedRect,
                                            mViewInfo->h, mViewInfo->zoom);
   }
   mIsSelecting = true;
}


// AS: Reset our selection markers.
void TrackPanel::StartSelection(int mouseXCoordinate, int trackLeftEdge)
{
   mSelStart = mViewInfo->h + ((mouseXCoordinate - trackLeftEdge)
                               / mViewInfo->zoom);

   mViewInfo->sel0 = mSelStart;
   mViewInfo->sel1 = mSelStart;
}

// AS: If we're dragging to extend a selection (or actually,
//  if the screen is scrolling while you're selecting), we
//  handle it here.
void TrackPanel::SelectionHandleDrag(wxMouseEvent & event)
{
   // AS: If we're not in the process of selecting (set in
   //  the SelectionHandleClick above), fuhggeddaboudit.
   if (!mIsSelecting)
      return;

   if (event.Dragging() || mAutoScrolling) {
      wxRect r = mCapturedRect;
      int num = mCapturedNum;
      Track *pTrack = mCapturedTrack;

      // AS: Note that FindTrack will replace r and num's values.
      if (!pTrack)
         pTrack = FindTrack(event.m_x, event.m_y, false, &r, &num);

      if (pTrack) {             // Selecting
         int x = mAutoScrolling ? mMouseMostRecentX : event.m_x;
         int y = mAutoScrolling ? mMouseMostRecentY : event.m_y;

         ExtendSelection(x, r.x);
         DisplaySelection();

         // Handle which tracks are selected
         int num2;
         if (0 != FindTrack(x, y, false, NULL, &num2)) {
            // The tracks numbered num...num2 should be selected

            TrackListIterator iter(mTracks);
            Track *t = iter.First();
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
void TrackPanel::ExtendSelection(int mouseXCoordinate, int trackLeftEdge)
{
   double selend = PositionToTime(mouseXCoordinate, trackLeftEdge);
   clip_bottom(selend, 0.0);

   mViewInfo->sel0 = wxMin(mSelStart, selend);
   mViewInfo->sel1 = wxMax(mSelStart, selend);
}

// DM: Converts a position (mouse X coordinate) to 
//  project time, in seconds.  Needs the left edge of
//  the track as an additional parameter.
double TrackPanel::PositionToTime(int mouseXCoordinate, int trackLeftEdge) const
{
   return mViewInfo->h + ((mouseXCoordinate - trackLeftEdge)
                          / mViewInfo->zoom);
}


// STM: Converts a project time to screen x position.
int TrackPanel::TimeToPosition(double projectTime, int trackLeftEdge) const
{
   return static_cast <
       int >(mViewInfo->zoom * (projectTime - mViewInfo->h) +
             trackLeftEdge);
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
void TrackPanel::ForwardEventToEnvelope(wxMouseEvent & event)
{
   if (!mCapturedTrack || mCapturedTrack->GetKind() != Track::Wave)
      return;

   WaveTrack *pwavetrack = (WaveTrack *) mCapturedTrack;
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
                                         mViewInfo->h, mViewInfo->zoom,
                                         dB);

      // If this track is linked to another track, make the identical
      // change to the linked envelope:
      WaveTrack *link = (WaveTrack *) mTracks->GetLink(mCapturedTrack);
      if (link) {
         Envelope *e2 = link->GetEnvelope();
         needUpdate |= e2->MouseEvent(event, mCapturedRect,
                                      mViewInfo->h, mViewInfo->zoom, dB);
      }
   }

   if (needUpdate) {

      Refresh(false);
   }
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
      if (totalOffset != 0)
         MakeParentPushState(wxString::
                             Format(_("Slid track '%s' %s %.02f seconds"),
                                    name.c_str(),
                                    totalOffset >
                                    0 ? _("right") : _("left"),
                                    totalOffset >
                                    0 ? totalOffset : -totalOffset));
   }
}

// AS: Pepare for sliding.
void TrackPanel::StartSlide(wxMouseEvent & event, double &totalOffset,
                            wxString & name)
{
   totalOffset = 0;

   wxRect r;
   int num;

   Track *vt = FindTrack(event.m_x, event.m_y, false, &r, &num);

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
void TrackPanel::DoSlide(wxMouseEvent & event, double &totalOffset)
{
   double selend = mViewInfo->h +
       ((event.m_x - mCapturedRect.x) / mViewInfo->zoom);

   clip_bottom(selend, 0.0);

   if (selend != mSelStart) {
      mCapturedTrack->Offset(selend - mSelStart);
      totalOffset += selend - mSelStart;

      Track *link = mTracks->GetLink(mCapturedTrack);
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
void TrackPanel::HandleZoom(wxMouseEvent & event)
{
   if (event.ButtonDown(1) || event.ButtonDClick(1)) {
      mZoomStart = event.m_x;
      mZoomEnd = event.m_x;
   } else if (event.Dragging()) {
      mZoomEnd = event.m_x;

      if (IsDragZooming()){

         Refresh(false);

      }
   } else if (event.ButtonUp()) {

      if (mZoomEnd < mZoomStart) {
         int temp = mZoomEnd;
         mZoomEnd = mZoomStart;
         mZoomStart = temp;
      }

      if (IsDragZooming())
         DragZoom(GetLabelWidth()+1);
      else
         DoZoomInOut(event, GetLabelWidth()+1);

      mZoomEnd = mZoomStart = 0;

      MakeParentRedrawScrollbars();
      Refresh(false);
   }
}

// AS: This actually sets the Zoom value when you're done doing
//  a drag zoom.
void TrackPanel::DragZoom(int trackLeftEdge)
{
   double left = PositionToTime(mZoomStart, trackLeftEdge);
   double right = PositionToTime(mZoomEnd, trackLeftEdge);

   mViewInfo->zoom *= mViewInfo->screen / (right - left);
   if (mViewInfo->zoom > gMaxZoom)
      mViewInfo->zoom = gMaxZoom;
   if (mViewInfo->zoom <= gMinZoom)
      mViewInfo->zoom = gMinZoom;

   mViewInfo->h = left;
}

// AS: This handles normal Zoom In/Out, if you just clicked;
//  IOW, if you were NOT dragging to zoom an area.
// AS: MAGIC NUMBER: We've got several in this function.
void TrackPanel::DoZoomInOut(wxMouseEvent & event, int trackLeftEdge)
{
   double center_h = PositionToTime(event.m_x, trackLeftEdge);

   if (event.RightUp() || event.RightDClick() || event.ShiftDown())
      mViewInfo->zoom = wxMax(mViewInfo->zoom / 2.0, gMinZoom);
   else
      mViewInfo->zoom = wxMin(mViewInfo->zoom * 2.0, gMaxZoom);

   if (event.MiddleUp() || event.MiddleDClick())
      mViewInfo->zoom = 44100.0 / 512.0;        // AS: Reset zoom.

   double new_center_h = PositionToTime(event.m_x, trackLeftEdge);

   mViewInfo->h += (center_h - new_center_h);
}

bool TrackPanel::IsDragDrawing()
{
   int result = abs(mDrawMouseEnd - mDrawMouseStart);

   //If threshold is reached, keep it there.
   if(result > 3)
      mDrawMouseStart = -10;

   return (result > 3);
}

// BG: This handles drawing
void TrackPanel::HandleDraw(wxMouseEvent & event)
{
   if(event.ButtonDown(1) || event.ButtonDClick(1) || event.Dragging() || event.ButtonUp()) {

      wxRect r;
      int dummy;

      //BG: Get the track the mouse is over
      Track *selectedTrack = FindTrack(event.m_x, event.m_y, false, &r, &dummy);

      if(selectedTrack == NULL)
         return;

      Sequence *seq = ((WaveTrack *)selectedTrack)->GetSequence();

      double rate = ((WaveTrack *)selectedTrack)->GetRate();

      if (event.ButtonDown(1) || event.ButtonDClick(1)) {

         if(((WaveTrack *)selectedTrack)->GetDisplay() != WaveTrack::WaveformDisplay)
         {
            wxMessageBox("Draw currently only works with waveforms.", "Notice");
            return;
         }

         bool showPoints = (mViewInfo->zoom / rate > 3.0);

         if(!showPoints)
         {
            wxMessageBox("You are not zoomed in enough. Zoom in until you can see the individual samples.", "Notice");
            return;
         }

         double t0 = PositionToTime(event.m_x, GetLeftOffset());

         sampleCount s0 = (sampleCount) (double)(t0 * rate + 0.5);

         seq->Get((samplePtr)&mDrawStart, floatSample, s0, 1);
         mDrawEnd = mDrawStart;

         mDrawMouseXStart = event.m_x;
         mDrawMouseStart = event.m_y;
         mDrawMouseEnd = mDrawMouseStart;

      } else if (event.Dragging()) {

         // The following code handles dragging samples around with the draw tool
         // event is relative to the top of the ruler, so this needs to be accounted for
         // when doing the redrawing.

         //Calculate the height of the sample:
         // start with the negative vertical position of the mViewInfo
         //I can't figure out why this is done:
         float yoffset = -mViewInfo->vpos;

         //Add the ruler height
         yoffset += GetRulerHeight();

         // Add the height of the pixel in question
         // This is done by (1) finding the proportionate level that the current mouse event
         // occurred at and multiplying by the track height to get an actual pixel location.

         yoffset += selectedTrack->GetHeight() * (int)((mViewInfo->vpos + (event.m_y - GetRulerHeight())) 
                                                       / selectedTrack->GetHeight());

         // Now, yoffset should indicate the current vertical position of the sample on the track.  
         // use it to calculate yval, which does???
         float yval = -(event.m_y-yoffset) + (selectedTrack->GetHeight()/2);

         //Calculate the sign of yval
         float sign = (yval >= 0 ? 1 : -1);

         //Take yval and add .5 or -.5 depending on the sign. Then devide it by half the track height.
         mDrawEnd = ((float)(yval + (sign * 0.5)) / (float)(selectedTrack->GetHeight()/2));

         mDrawMouseEnd = event.m_y;

         if (IsDragDrawing()) {

            double t0 = PositionToTime(mDrawMouseXStart, GetLeftOffset());

            sampleCount s0 = (sampleCount) (double)(t0 * rate + 0.5);

            seq->Set((samplePtr)&mDrawEnd, floatSample, s0, 1);

            Refresh(false);

         }

      } else if (event.ButtonUp()) {

         double t0 = PositionToTime(mDrawMouseXStart, GetLeftOffset());

         sampleCount s0 = (sampleCount) (double)(t0 * rate + 0.5);

         if (IsDragDrawing())
         {
            seq->Set((samplePtr)&mDrawEnd, floatSample, s0, 1);
            MakeParentPushState(wxString::Format(_("Moved Sample")));
         }
         else
         {
            seq->Set((samplePtr)&mDrawStart, floatSample, s0, 1);
         }

         Refresh(false);

         mDrawMouseStart = mDrawMouseEnd = mDrawMouseXStart = 0;

      }
   }
}

// AS: This is for when a given track gets the x.
void TrackPanel::HandleClosing(wxMouseEvent & event)
{
   Track *t = mCapturedTrack;
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
   if (mTracks->IsEmpty()) {
      mViewInfo->zoom = 44100.0 / 512.0;

      //STM: Set selection to 0,0
      mViewInfo->sel0 = 0.0;
      mViewInfo->sel1 = 0.0;

      mListener->TP_RedrawScrollbars();

      mListener->TP_DisplayStatusMessage("", 0);        //STM: Clear message if all tracks are removed
      
      Refresh(false);
   }
}

// This actually removes the specified track.  Called from HandleClosing.
void TrackPanel::RemoveTrack(Track * toRemove)
{
   TrackListIterator iter(mTracks);

   Track *partner = mTracks->GetLink(toRemove);
   wxString name;

   Track *t = iter.First();
   while (t) {
      if (t == toRemove || t == partner) {
         name = t->GetName();
         delete t;
         t = iter.RemoveCurrent();
      } else
         t = iter.Next();
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
   Track *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   wxRect buttonRect;
   GetMuteSoloRect(r, buttonRect, solo);

   wxClientDC dc(this);

   if (event.Dragging())
      DrawMuteSolo(&dc, r, t, buttonRect.Inside(event.m_x, event.m_y),
                   solo);
   else if (event.ButtonUp(1)) {

      if (buttonRect.Inside(event.m_x, event.m_y)) {
         if (solo)
            t->SetSolo(!t->GetSolo());
         else
            t->SetMute(!t->GetMute());
      }

      DrawMuteSolo(&dc, r, t, false, solo);
      if (solo) {
         mIsSoloing = false;
         DrawMuteSolo(&dc, r, t, false, !solo);
      } else
         mIsMuting = false;
   }
}

// AS: This function gets called when a user clicks on the
//  title of a track, dropping down the menu.
void TrackPanel::DoPopupMenu(wxMouseEvent & event, wxRect & titleRect,
                             Track * t, wxRect & r, int num)
{
   ReleaseMouse();

   mPopupMenuTarget = t;
   {
      wxClientDC dc(this);
      SetLabelFont(&dc);
      DrawTitleBar(&dc, r, t, true);
   }
   bool canMakeStereo = false;
   Track *next = mTracks->GetNext(t);

   wxMenu *theMenu = NULL;

   if (t->GetKind() == Track::Wave) {
      theMenu = mWaveTrackMenu;
      if (next && !t->GetLinked() && !next->GetLinked()
          && t->GetKind() == Track::Wave
          && next->GetKind() == Track::Wave)
         canMakeStereo = true;

      theMenu->Enable(OnMergeStereoID, canMakeStereo);
      theMenu->Enable(OnSplitStereoID, t->GetLinked());
      theMenu->Enable(OnChannelMonoID, !t->GetLinked());
      theMenu->Enable(OnChannelLeftID, !t->GetLinked());
      theMenu->Enable(OnChannelRightID, !t->GetLinked());

      int display = ((WaveTrack *) t)->GetDisplay();

      theMenu->Enable(OnWaveformID, display != WaveTrack::WaveformDisplay);
      theMenu->Enable(OnWaveformDBID,
                      display != WaveTrack::WaveformDBDisplay);
      theMenu->Enable(OnSpectrumID, display != WaveTrack::SpectrumDisplay);
      theMenu->Enable(OnPitchID, display != WaveTrack::PitchDisplay);
   }

   if (t->GetKind() == Track::Note)
      theMenu = mNoteTrackMenu;

   if (t->GetKind() == Track::Label)
      theMenu = mLabelTrackMenu;

   if (theMenu) {

      theMenu->Enable(OnMoveUpID, mTracks->CanMoveUp(t));
      theMenu->Enable(OnMoveDownID, mTracks->CanMoveDown(t));

#ifdef __WXMAC__
# ifdef __UNIX__
      ::InsertMenu((OpaqueMenuHandle *) mRateMenu->GetHMenu(), -1);
      ::InsertMenu((OpaqueMenuHandle *) mFormatMenu->GetHMenu(), -1);
# else
      ::InsertMenu((MenuRef) mRateMenu->GetHMenu(), -1);
      ::InsertMenu((MenuRef) mFormatMenu->GetHMenu(), -1);
# endif
#endif

      PopupMenu(theMenu, titleRect.x + 1,
                titleRect.y + titleRect.height + 1);

#ifdef __WXMAC__
      ::DeleteMenu(mFormatMenu->MacGetMenuId());
      ::DeleteMenu(mRateMenu->MacGetMenuId());
#endif
   }

   Track *t2 = FindTrack(event.m_x, event.m_y, true, &r, &num);
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

   Track *t = FindTrack(event.m_x, event.m_y, true, &r, &num);

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
   if (!second && t->GetKind() == Track::Wave) {
      if (MuteSoloFunc(t, r, event.m_x, event.m_y, false) ||
          MuteSoloFunc(t, r, event.m_x, event.m_y, true))
         return;
   }
   // DM: If it's a NoteTrack, it has special controls
   if (!second && t && t->GetKind() == Track::Note) {
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
   mViewInfo->sel0 = t->GetOffset();
   mViewInfo->sel1 = t->GetEndTime();

   Refresh(false);  
   DisplaySelection();
}


// JH: the user is dragging one of the tracks: change the track order
//   accordingly
void TrackPanel::HandleRearrange(wxMouseEvent & event)
{
   // are we finishing the drag?
   if (event.ButtonUp(1)) {
      mCapturedTrack = NULL;
      mIsRearranging = false;
      SetCursor(*mArrowCursor);
      return;
   }
   if (event.m_y < mMoveUpThreshold)
      mTracks->MoveUp(mCapturedTrack);
   else if (event.m_y > mMoveDownThreshold)
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

   if (mTracks->CanMoveUp(mCapturedTrack))
      mMoveUpThreshold =
          event.m_y - mTracks->GetPrev(mCapturedTrack)->GetHeight();
   else
      mMoveUpThreshold = INT_MIN;

   if (mTracks->CanMoveDown(mCapturedTrack))
      mMoveDownThreshold =
          event.m_y + mTracks->GetNext(mCapturedTrack)->GetHeight();
   else
      mMoveDownThreshold = INT_MAX;
}

// AS: Mute or solo the given track (t).  If solo is true, we're 
//  soloing, otherwise we're muting.  Basically, check and see 
//  whether x and y fall within the  area of the appropriate button.
bool TrackPanel::MuteSoloFunc(Track * t, wxRect r, int x, int y,
                              bool solo)
{
   wxRect buttonRect;
   GetMuteSoloRect(r, buttonRect, solo);
   if (buttonRect.Inside(x, y)) {

      wxClientDC dc(this);
      DrawMuteSolo(&dc, r, t, true, solo);

      if (solo)
         mIsSoloing = true;
      else
         mIsMuting = true;

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
      wxRect rLabel;
      int num;

      // DM: Figure out what track is about to be resized
      Track *t = FindTrack(event.m_x, event.m_y, false, &r, &num);
      Track *label = FindTrack(event.m_x, event.m_y, true, &rLabel, &num);

      // If the click is at the bottom of a non-linked track label, we
      // treat it as a normal resize.  If the label is of a linked track,
      // we ignore the click.

      if (label && !label->GetLinked())
         t = label;

      if (t) {
         Track *prev = mTracks->GetPrev(t);
         Track *next = mTracks->GetNext(t);

         // DM: Capture the track so that we continue to resize
         //  THIS track, no matter where the user moves the mouse
         mCapturedTrack = t;
         //mCapturedRect = r;
         //mCapturedNum = num;

         mMouseClickX = event.m_x;
         mMouseClickY = event.m_y;
         mIsResizing = true;

         //STM:  Determine whether we should rescale one or two tracks

         if (mTracks->GetLink(prev) == t) {
            // mCapturedTrack is the lower track
            mInitialTrackHeight = t->GetHeight();
            mInitialUpperTrackHeight = prev->GetHeight();
            mIsResizingBelowLinkedTracks = true;
         } else if (next && mTracks->GetLink(t) == next) {
            // mCapturedTrack is the upper track
            mInitialTrackHeight = next->GetHeight();
            mInitialUpperTrackHeight = t->GetHeight();
            mIsResizingBetweenLinkedTracks = true;
         } else {
            // DM: Save the initial mouse location and the initial height
            mInitialTrackHeight = t->GetHeight();
         }
      }
   } else if (mIsResizing) {

      // DM: Dragging means that the mouse button IS down and has moved
      //  from its initial location.  By the time we get here, we
      //  have already received a ButtonDown() event and saved the
      //  track being resized in mCapturedTrack.
      if (event.Dragging()) {

         int delta = (event.m_y - mMouseClickY);

         //STM: We may be dragging one or two (stereo) tracks.  
         // If two, resize proportionally if we are dragging the lower track, and
         // adjust compensatively if we are dragging the upper track.
         if (mIsResizingBelowLinkedTracks) {
            Track *prev = mTracks->GetPrev(mCapturedTrack);

            double proportion = static_cast < double >(mInitialTrackHeight)
                / (mInitialTrackHeight + mInitialUpperTrackHeight);

            int newTrackHeight = static_cast < int >
                (mInitialTrackHeight + delta * proportion);

            int newUpperTrackHeight = static_cast < int >
                (mInitialUpperTrackHeight + delta * (1.0 - proportion));

            //make sure neither track is smaller than 20;
            if (newTrackHeight < 20)
               newTrackHeight = 20;
            if (newUpperTrackHeight < 20)
               newUpperTrackHeight = 20;

            mCapturedTrack->SetHeight(newTrackHeight);
            prev->SetHeight(newUpperTrackHeight);
         } else if (mIsResizingBetweenLinkedTracks) {

            Track *next = mTracks->GetNext(mCapturedTrack);
            int newUpperTrackHeight = mInitialUpperTrackHeight + delta;
            int newTrackHeight = mInitialTrackHeight - delta;

            // make sure neither track is smaller than 20;
            if (newTrackHeight < 20) {
               newTrackHeight = 20;
               newUpperTrackHeight =
                   mInitialUpperTrackHeight + mInitialTrackHeight - 20;
            }
            if (newUpperTrackHeight < 20) {
               newUpperTrackHeight = 20;
               newTrackHeight =
                   mInitialUpperTrackHeight + mInitialTrackHeight - 20;
            }

            mCapturedTrack->SetHeight(newUpperTrackHeight);
            next->SetHeight(newTrackHeight);
         } else {
            int newTrackHeight = mInitialTrackHeight + delta;
            if (newTrackHeight < 20)
               newTrackHeight = 20;
            mCapturedTrack->SetHeight(newTrackHeight);
         }
         
         Refresh(false);
      }
      // DM: This happens when the button is released from a drag.
      //  Since we actually took care of resizing the track when
      //  we got drag events, all we have to do here is clean up.
      //  We also push the state so that this action is undo-able.
      else if (event.ButtonUp(1)) {
         mCapturedTrack = NULL;
         mIsResizing = false;
         mIsResizingBelowLinkedTracks = false;
         mIsResizingBetweenLinkedTracks = false;
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
   for (Track * t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Label && t->GetSelected()) {
         ((LabelTrack *) t)->KeyEvent(mViewInfo->sel0, mViewInfo->sel1,
                                      event);
         
         Refresh(false);
         MakeParentPushState("TrackPanel::OnKeyEvent() FIXME!!");
         event.Skip();
         return;
      }
   }

   switch (event.KeyCode()) {
   case WXK_SPACE:
      mListener->TP_OnPlayKey();
      break;
   case WXK_PRIOR:
      // BG: Page right
      mListener->TP_ScrollWindow((mViewInfo->h + mViewInfo->screen) -
                                 (mViewInfo->screen / 6));
      break;
   case WXK_NEXT:
      // BG: Page left
      mListener->TP_ScrollWindow((mViewInfo->h - mViewInfo->screen) +
                                 (mViewInfo->screen / 6));
      break;
   case WXK_RIGHT:
      // BG: Scroll right
      mListener->TP_ScrollWindow((mViewInfo->h + mViewInfo->screen) -
                                 (mViewInfo->screen * .95));
      break;
   case WXK_LEFT:
      // BG: Scroll left
      mListener->TP_ScrollWindow((mViewInfo->h - mViewInfo->screen) +
                                 (mViewInfo->screen * .95));
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
      mViewInfo->sel1 = mTracks->GetEndTime();
      if (!event.ShiftDown() || mViewInfo->sel0 > mViewInfo->sel1)
         mViewInfo->sel0 = mViewInfo->sel1;
      mListener->TP_ScrollWindow(mViewInfo->sel1);
      break;
   }

   event.Skip();
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

   if (event.ButtonDown())
      CaptureMouse();
   else if (event.ButtonUp())
      ReleaseMouse();

   if (mIsClosing)
      HandleClosing(event);
   else if (mIsMuting)
      HandleMutingSoloing(event, false);
   else if (mIsSoloing)
      HandleMutingSoloing(event, true);
   else if (mIsResizing) {
      HandleResize(event);
      HandleCursor(event);
   } else if (mIsRearranging)
      HandleRearrange(event);
   else
      TrackSpecificMouseEvent(event);
}

// AS: I don't really understand why this code is sectioned off
//  from the other OnMouseEvent code.
void TrackPanel::TrackSpecificMouseEvent(wxMouseEvent & event)
{
   wxRect r;
   wxRect rLabel;
   int dummy;

   FindTrack(event.m_x, event.m_y, false, &r, &dummy);
   FindTrack(event.m_x, event.m_y, true, &rLabel, &dummy);

   //call HandleResize if I'm over the border area 
   if (event.ButtonDown(1) &&
       (within(event.m_y, r.y + r.height, TRACK_RESIZE_REGION)
        || within(event.m_y, rLabel.y + rLabel.height,
                  TRACK_RESIZE_REGION))) {
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
   case selectTool:
      HandleSelect(event);
      break;
   case envelopeTool:
      HandleEnvelope(event);
      break;
   case slideTool:
      HandleSlide(event);
      break;
   case zoomTool:
      HandleZoom(event);
      break;
   case drawTool:
      HandleDraw(event);
      break;
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
#if defined __WXMSW__
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

void TrackPanel::DrawRulerBorder(wxDC * dc, wxRect & r)
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

void TrackPanel::DrawRulerSelection(wxDC * dc, const wxRect r)
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

void TrackPanel::DrawRulerMarks(wxDC * dc, const wxRect r, bool /*text */ )
{
   mRuler->SetBounds(r.x, r.y, r.x + r.width - 1, r.y + r.height - 1);
   double min = mViewInfo->h - GetLeftOffset() / mViewInfo->zoom;
   double max = min + r.width / mViewInfo->zoom;
   mRuler->SetRange(min, max);

   mRuler->Draw(*dc);
}

//
//This draws the little triangular indicator on the 
//ruler.
//
void TrackPanel::DrawRulerIndicator(wxDC * dc)
{
   // Draw indicator
   double ind = gAudioIO->GetIndicator();

   if (ind >= mViewInfo->h && ind <= (mViewInfo->h + mViewInfo->screen)) {
      int indp =
          GetLeftOffset() + int ((ind - mViewInfo->h) * mViewInfo->zoom);

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


//
// This draws the play indicator as a vertical line on each of the tracks
//
void TrackPanel::DrawTrackIndicator(wxDC * dc)
{
   // Draw indicator
   double ind = gAudioIO->GetIndicator();
   int indp = 0;
   if (ind >= mViewInfo->h && ind <= (mViewInfo->h + mViewInfo->screen)) {
      indp = GetLeftOffset() + int ((ind - mViewInfo->h) * mViewInfo->zoom);
 
         dc->SetPen(*wxBLACK_PEN);
         dc->SetLogicalFunction(wxINVERT);

         
         //Get the size of the trackpanel region, so we know where to redraw
         int width, height;
         GetSize(&width, &height);   
         
         

       
         int x = indp;
         int y = -mViewInfo->vpos + GetRulerHeight();
        
          
           if (x >= GetLeftOffset()) {
            // Draw cursor in all selected tracks
            TrackListIterator iter(mTracks);
   
            
            // Iterate through each track
            for (Track * t = iter.First(); t; t = iter.Next()) {
               int height = t->GetHeight();
               if ( t->GetKind() != Track::Label) {
               
                  //If you are supposed to redraw the indicator, do so:
                  if(mPlayIndicatorExists) {
                     dc->DrawLine(mLastIndicator, y + kTopInset + 1, mLastIndicator, y + height - 2);
                  }

                  //Draw the new indicator in its correct location
                  dc->DrawLine(x, y + kTopInset + 1, x, y + height - 2);
               
               }
               //Increment y so you draw on the proper track
               y += height;
            }

           }
           //Set the boolean to true so that things get redrawn next time.
           mPlayIndicatorExists=true;
           mLastIndicator=indp;               // This updates the old indicator value to the new indicator position
      
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

void TrackPanel::GetMuteSoloRect(const wxRect r, wxRect & dest, bool solo) const
{
   dest.x = r.x + 8;
   dest.y = r.y + 50;
   dest.width = 36;
   dest.height = 16;

   if (solo)
      dest.x += 36 + 8;
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


//
// This function overrides Refresh() of wxWindow so that the 
// boolean play indicator can be set to false, so that an old play indicator that is
// no longer there won't get  XORed (to erase it), thus redrawing it on the 
// TrackPanel
//

void TrackPanel::Refresh(bool eraseBackground /* = TRUE */,
                         const wxRect *rect /* = NULL */)
{
   mPlayIndicatorExists=false;
   wxWindow::Refresh(eraseBackground);
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

void TrackPanel::DrawTitleBar(wxDC * dc, const wxRect r, Track * t,
                              bool down)
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
void TrackPanel::DrawMuteSolo(wxDC * dc, const wxRect r, Track * t,
                              bool down, bool solo)
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
   dc->DrawText(str, bev.x + (bev.width - textWidth) / 2, bev.y + 2);

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

void TrackPanel::DrawEverythingElse(wxDC * dc, const wxRect panelRect,
                                    const wxRect clip)
{
   // We draw everything else
   TrackListIterator iter(mTracks);

   wxRect trackRect = panelRect;
   wxRect r;

   for (Track * t = iter.First(); t; t = iter.Next())
      DrawEverythingElse(t, dc, r, trackRect);

   if (IsDragZooming())
      DrawZooming(dc, clip);

   // Paint over the part below the tracks
   GetSize(&trackRect.width, &trackRect.height);
   AColor::Dark(dc, false);
   dc->DrawRectangle(trackRect);
}

// AS: Note that this is being called in a loop and that the parameter values
//  are expected to be maintained each time through.
void TrackPanel::DrawEverythingElse(Track * t, wxDC * dc, wxRect & r,
                                    wxRect & trackRect)
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

void TrackPanel::DrawZooming(wxDC * dc, const wxRect clip)
{
   // Draw zooming indicator
   wxRect r;

   r.x = mZoomStart;
   r.y = -1;
   r.width = mZoomEnd - mZoomStart;
   r.height = clip.height + 2;

   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_DASHED_PEN);

   dc->DrawRectangle(r);
}

void TrackPanel::DrawOutside(Track * t, wxDC * dc, const wxRect rec,
                             const int labelw, const int vrul,
                             const wxRect trackRect)
{
   wxRect r = rec;

   DrawOutsideOfTrack(t, dc, r);

   r.x += kLeftInset;
   r.y += kTopInset;
   r.width -= kLeftInset * 2;
   r.height -= kTopInset;

   FillInLabel(t, dc, r, labelw);
   DrawBordersAroundTrack(t, dc, r, labelw, vrul);
   DrawShadow(t, dc, r);

   r.width = GetTitleWidth();
   DrawCloseBox(dc, r, false);
   DrawTitleBar(dc, r, t, false);

   if (t->GetKind() == Track::Wave) {
      DrawMuteSolo(dc, r, t, false, false);
      DrawMuteSolo(dc, r, t, false, true);
   }

   r = trackRect;

   if (t->GetKind() == Track::Wave) {
      dc->DrawText(TrackSubText(t), r.x + 6, r.y + 22);
      dc->DrawText(GetSampleFormatStr
                   (((WaveTrack *) t)->GetSampleFormat()), r.x + 6,
                   r.y + 38);
   } else if (t->GetKind() == Track::Note) {
      wxRect midiRect;
      GetTrackControlsRect(trackRect, midiRect);
      ((NoteTrack *) t)->DrawLabelControls(*dc, midiRect);

   }
}

void TrackPanel::DrawOutsideOfTrack(Track * t, wxDC * dc, const wxRect r)
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

void TrackPanel::FillInLabel(Track * t, wxDC * dc, const wxRect r,
                             const int labelw)
{
   // fill in label
   wxRect fill = r;
   fill.width = labelw - r.x;
   AColor::Medium(dc, t->GetSelected());
   dc->DrawRectangle(fill);
}

void TrackPanel::DrawBordersAroundTrack(Track * t, wxDC * dc,
                                        const wxRect r, const int vrul,
                                        const int labelw)
{
   // Borders around track and label area
   dc->SetPen(*wxBLACK_PEN);
   dc->DrawLine(r.x, r.y, r.x + r.width - 1, r.y);      // top
   dc->DrawLine(r.x, r.y, r.x, r.y + r.height - 1);     // left
   dc->DrawLine(r.x, r.y + r.height - 2, r.x + r.width - 1, r.y + r.height - 2);        // bottom
   dc->DrawLine(r.x + r.width - 2, r.y, r.x + r.width - 2, r.y + r.height - 1); // right
   dc->DrawLine(vrul, r.y, vrul, r.y + r.height - 1);
   dc->DrawLine(labelw, r.y, labelw, r.y + r.height - 1);       // between vruler and track

   if (t->GetLinked()) {
      int h1 = r.y + t->GetHeight() - kTopInset;
      dc->DrawLine(vrul, h1 - 2, r.x + r.width - 1, h1 - 2);
      dc->DrawLine(vrul, h1 + kTopInset, r.x + r.width - 1,
                   h1 + kTopInset);
   }

   dc->DrawLine(r.x, r.y + 16, GetTitleWidth(), r.y + 16);      // title bar
   dc->DrawLine(r.x + 16, r.y, r.x + 16, r.y + 16);     // close box
}

void TrackPanel::DrawShadow(Track * /* t */ , wxDC * dc, const wxRect r)
{
   // shadow
   AColor::Dark(dc, true);
   // bottom
   dc->DrawLine(r.x + 1, r.y + r.height - 1, r.x + r.width,
                r.y + r.height - 1);
   // right
   dc->DrawLine(r.x + r.width - 1, r.y + 1, r.x + r.width - 1,
                r.y + r.height);
}

// AS: Returns the string to be displayed in the track label
//  indicating whether the track is mono, left, right, or 
//  stereo and what sample rate it's using.
wxString TrackPanel::TrackSubText(Track * t)
{
   wxString s = wxString::Format("%dHz",
                                 (int) (((WaveTrack *) t)->GetRate() +
                                        0.5));
   if (t->GetLinked())
      s = _("Stereo, ") + s;
   else {
      if (t->GetChannel() == Track::MonoChannel)
         s = _("Mono, ") + s;
      else if (t->GetChannel() == Track::LeftChannel)
         s = _("Left, ") + s;
      else if (t->GetChannel() == Track::RightChannel)
         s = _("Right, ") + s;
   }

   return s;
}

// AS: Handle the menu options that change a track between
//  left channel, right channel, and mono.
int channels[] = { Track::LeftChannel, Track::RightChannel,
   Track::MonoChannel
};

const char *channelmsgs[] = { "'left' channel", "'right' channel",
   "'mono'"
};

void TrackPanel::OnChannelChange(wxEvent & event)
{
   int id = event.GetId();
   wxASSERT(id >= OnChannelLeftID && id <= OnChannelMonoID);
   wxASSERT(mPopupMenuTarget);
   mPopupMenuTarget->SetChannel(channels[id - OnChannelLeftID]);
   MakeParentPushState(wxString::Format(_("Changed '%s' to %s"),
                                        mPopupMenuTarget->GetName().
                                        c_str(),
                                        channelmsgs[id -
                                                    OnChannelLeftID]));
   mPopupMenuTarget = NULL;
   Refresh(false);
}

// AS: Split a stereo track into two tracks... ??
void TrackPanel::OnSplitStereo()
{
   wxASSERT(mPopupMenuTarget);
   mPopupMenuTarget->SetLinked(false);
   MakeParentPushState(wxString::Format(_("Split stereo track '%s'"),
                                        mPopupMenuTarget->GetName().
                                        c_str()));

   Refresh(false);
}

// AS: Merge two tracks into one steroe track ??
void TrackPanel::OnMergeStereo()
{
   wxASSERT(mPopupMenuTarget);
   mPopupMenuTarget->SetLinked(true);
   Track *partner = mTracks->GetLink(mPopupMenuTarget);
   if (partner) {
      mPopupMenuTarget->SetChannel(Track::LeftChannel);
      partner->SetChannel(Track::RightChannel);
      MakeParentPushState(wxString::Format(_("Made '%s' a stereo track"),
                                           mPopupMenuTarget->GetName().
                                           c_str()));
   } else
      mPopupMenuTarget->SetLinked(false);

   Refresh(false);
}

// AS: Set the Display mode based on the menu choice in the Track Menu.
//  Note that gModes MUST BE IN THE SAME ORDER AS THE MENU CHOICES!!
const char *gModes[] = { "waveform", "waveformDB", "spectrum", "pitch" };
void TrackPanel::OnSetDisplay(wxEvent & event)
{
   int id = event.GetId();
   wxASSERT(id >= OnWaveformID && id <= OnPitchID);
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);
   ((WaveTrack *) mPopupMenuTarget)->SetDisplay(id - OnWaveformID);
   Track *partner = mTracks->GetLink(mPopupMenuTarget);
   if (partner)
      ((WaveTrack *) partner)->SetDisplay(id - OnWaveformID);
   MakeParentPushState(wxString::Format(_("Changed '%s' to %s display"),
                                        mPopupMenuTarget->GetName().
                                        c_str(),
                                        gModes[id - OnWaveformID]));
   mPopupMenuTarget = NULL;
   ReReadSettings();
   Refresh(false);
}

// AS: Sets the sample rate for a track, and if it is linked to
//  another track, that one as well.
void TrackPanel::SetRate(Track * pTrack, double rate)
{
   ((WaveTrack *) pTrack)->SetRate(rate);
   Track *partner = mTracks->GetLink(pTrack);
   if (partner)
      ((WaveTrack *) partner)->SetRate(rate);
   MakeParentPushState(wxString::Format(_("Changed '%s' to %d Hz"),
                                        pTrack->GetName().c_str(), rate));
}

// DM: Handles the selection from the Format submenu of the
//  track menu.
void TrackPanel::OnFormatChange(wxEvent & event)
{
   int id = event.GetId();
   wxASSERT(id >= On16BitID && id <= OnFloatID);
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);

   sampleFormat newFormat = int16Sample;

   switch (id) {
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

   ((WaveTrack *) mPopupMenuTarget)->ConvertToSampleFormat(newFormat);
   Track *partner = mTracks->GetLink(mPopupMenuTarget);
   if (partner)
      ((WaveTrack *) partner)->ConvertToSampleFormat(newFormat);

   MakeParentPushState(wxString::Format(_("Changed '%s' to %s"),
                                        mPopupMenuTarget->GetName().
                                        c_str(),
                                        GetSampleFormatStr(newFormat)));

   mPopupMenuTarget = NULL;
   MakeParentRedrawScrollbars();
   Refresh(false);
}

// AS: Ok, this function handles the selection from the Rate
//  submenu of the track menu, except for "Other" (see OnRateOther).
//  gRates MUST CORRESPOND DIRECTLY TO THE RATES AS LISTED IN THE MENU!!
//  IN THE SAME ORDER!!
int gRates[] = { 8000, 11025, 16000, 22050, 44100, 48000 };
void TrackPanel::OnRateChange(wxEvent & event)
{
   int id = event.GetId();
   wxASSERT(id >= OnRate8ID && id <= OnRate48ID);
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);

   SetRate(mPopupMenuTarget, gRates[id - OnRate8ID]);

   mPopupMenuTarget = NULL;
   MakeParentRedrawScrollbars();

   Refresh(false);
}

// AS: This function handles the case when the user selects "Other"
//  from the Rate submenu on the Track menu.
void TrackPanel::OnRateOther()
{
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);

   wxString defaultStr;
   defaultStr.Printf("%d",
                     (int) (((WaveTrack *) mPopupMenuTarget)->GetRate() +
                            0.5));

   // AS: TODO: REMOVE ARTIFICIAL CONSTANTS!!
   // AS: Make a real dialog box out of this!!
   double theRate;
   do {
      wxString rateStr =
          wxGetTextFromUser(_("Enter a sample rate in Hz (per second) "
                              "between 1 and 100000:"),
                            _("Set Rate"), defaultStr);

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
void TrackPanel::OnMoveTrack(wxEvent & event)
{
   wxASSERT(event.GetId() == OnMoveUpID || event.GetId() == OnMoveDownID);
   if (mTracks->Move(mPopupMenuTarget, OnMoveUpID == event.GetId())) {
      MakeParentPushState(wxString::Format(_("Moved '%s' %s"),
                                           mPopupMenuTarget->GetName().
                                           c_str(),
                                           event.GetId() ==
                                           OnMoveUpID ? _("up") :
                                           _("down")));
      Refresh(false);
   }
}

// AS: This only applies to MIDI tracks.  Presumably, it shifts the
//  whole sequence by an octave.
void TrackPanel::OnChangeOctave(wxEvent & event)
{
   wxASSERT(event.GetId() == OnUpOctaveID
            || event.GetId() == OnDownOctaveID);
   wxASSERT(mPopupMenuTarget->GetKind() == Track::Note);
   NoteTrack *t = (NoteTrack *) mPopupMenuTarget;

   bool bDown = (OnDownOctaveID == event.GetId());
   t->SetBottomNote(t->GetBottomNote() + ((bDown) ? -12 : 12));

   MakeParentPushState("TrackPanel::OnChangeOctave() FIXME!!");
   Refresh(false);
}

void TrackPanel::OnSetName()
{
   Track *t = mPopupMenuTarget;

   if (t) {
      wxString defaultStr = t->GetName();
      wxString newName = wxGetTextFromUser(_("Change track name to:"),
                                           _("Track Name"), defaultStr);
      if (newName != "")
         t->SetName(newName);
      MakeParentPushState(wxString::Format(_("Renamed '%s' to '%s'"),
                                           defaultStr.c_str(),
                                           newName.c_str()));

      Refresh(false);
   }
}

//  Here, 'label' refers to the rectangle to the left of the track
// or tracks (if stereo); i.e., whether the label should be considered.
Track *TrackPanel::FindTrack(int mouseX, int mouseY, bool label,
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

   for (Track * t = iter.First(); t;
        r.y += r.height, n++, t = iter.Next()) {
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

//This method displays the bounds of the selection
//in the status bar.
void TrackPanel::DisplaySelection()
{

   float start = mViewInfo->sel0;
   float end = mViewInfo->sel1;
   float length = end-start;

   //Do more complex stuff here to support user-based configuration
   //of timescales (m/s/ms/samples/etc.).
   //float scale = float(1.0);
   int minutes1 = int(start/60);
   int minutes2 = int(end/60);
   int minutestot = int(length/60);
   float seconds1 = start - float(minutes1*60);
   float seconds2 = end - float(minutes2*60);
   float secondstot = length - float(minutestot*60);
   

   //Display a message about the selection in the status message window
   if(start == end)
      {
        mListener->
             TP_DisplayStatusMessage(wxString::
                                     Format(_("Cursor: %i:%09.6f"), minutes1, seconds1),
                                     1);
      }
   else
      {
         mListener->
            TP_DisplayStatusMessage(wxString::
                                    Format(_("Selection: %i:%09.6f - %i:%09.6f (%i:%09.6f)"),
                                           minutes1, seconds1, minutes2, seconds2, minutestot, secondstot),
       

                                    1);
      }
}

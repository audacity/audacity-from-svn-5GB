/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/textctrl.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/statusbr.h>

#include "TrackPanel.h"

#include "APalette.h"
#include "AColor.h"
#include "AudioIO.h"
#include "LabelTrack.h"
#include "NoteTrack.h"
#include "Track.h"
#include "TrackArtist.h"
#include "Project.h"
#include "WaveTrack.h"

#ifdef BOUNCE
#include "Bounce.h"
extern Bounce *gBounce;
#endif

#define kLeftInset 4
#define kTopInset 4

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

    EVT_MENU(OnMoveUpID, TrackPanel::OnMoveUp)
    EVT_MENU(OnMoveDownID, TrackPanel::OnMoveDown)

    EVT_MENU(OnUpOctaveID, TrackPanel::OnUpOctave)
    EVT_MENU(OnDownOctaveID, TrackPanel::OnDownOctave)

    EVT_MENU(OnChannelLeftID, TrackPanel::OnChannelLeft)
    EVT_MENU(OnChannelRightID, TrackPanel::OnChannelRight)
    EVT_MENU(OnChannelMonoID, TrackPanel::OnChannelMono)

    EVT_MENU(OnWaveformID, TrackPanel::OnWaveform)
    EVT_MENU(OnWaveformDBID, TrackPanel::OnWaveformDB)
    EVT_MENU(OnSpectrumID, TrackPanel::OnSpectrum)
    EVT_MENU(OnPitchID, TrackPanel::OnPitch)

    EVT_MENU(OnRate8ID, TrackPanel::OnRate8)
    EVT_MENU(OnRate11ID, TrackPanel::OnRate11)
    EVT_MENU(OnRate16ID, TrackPanel::OnRate16)
    EVT_MENU(OnRate22ID, TrackPanel::OnRate22)
    EVT_MENU(OnRate44ID, TrackPanel::OnRate44)
    EVT_MENU(OnRate48ID, TrackPanel::OnRate48)
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
   mIsSliding = false;
   mIsEnveloping = false;
   mIsMuting = false;
   mIsZooming = false;
   mIsSoloing = false;

   mIndicatorShowing = false;

   mArrowCursor = new wxCursor(wxCURSOR_ARROW);
   mSelectCursor = new wxCursor(wxCURSOR_IBEAM);
   mSlideCursor = new wxCursor(wxCURSOR_SIZEWE);
   mResizeCursor = new wxCursor(wxCURSOR_SIZENS);
   mZoomInCursor = new wxCursor(wxCURSOR_MAGNIFIER);
   mZoomOutCursor = new wxCursor(wxCURSOR_MAGNIFIER);

   mRateMenu = new wxMenu();
   mRateMenu->Append(OnRate8ID, "8000 Hz");
   mRateMenu->Append(OnRate11ID, "11025 Hz");
   mRateMenu->Append(OnRate16ID, "16000 Hz");
   mRateMenu->Append(OnRate22ID, "22050 Hz");
   mRateMenu->Append(OnRate44ID, "44100 Hz");
   mRateMenu->Append(OnRate48ID, "48000 Hz");
   mRateMenu->Append(OnRateOtherID, "Other...");

   mWaveTrackMenu = new wxMenu();
   mWaveTrackMenu->Append(OnSetNameID, "Name...");
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(OnMoveUpID, "Move Track Up");
   mWaveTrackMenu->Append(OnMoveDownID, "Move Track Down");
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(OnWaveformID, "Waveform");
   mWaveTrackMenu->Append(OnWaveformDBID, "Waveform (dB)");
   mWaveTrackMenu->Append(OnSpectrumID, "Spectrum");
   mWaveTrackMenu->Append(OnPitchID, "Pitch (EAC)");
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(OnChannelMonoID, "Mono");
   mWaveTrackMenu->Append(OnChannelLeftID, "Left Channel");
   mWaveTrackMenu->Append(OnChannelRightID, "Right Channel");
   mWaveTrackMenu->Append(OnMergeStereoID, "Make Stereo Track");
   mWaveTrackMenu->Append(OnSplitStereoID, "Split Stereo Track");
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(0, "Set Rate", mRateMenu);

   mNoteTrackMenu = new wxMenu();
   mNoteTrackMenu->Append(OnSetNameID, "Name...");
   mNoteTrackMenu->AppendSeparator();
   mNoteTrackMenu->Append(OnMoveUpID, "Move Track Up");
   mNoteTrackMenu->Append(OnMoveDownID, "Move Track Down");
   mNoteTrackMenu->AppendSeparator();
   mNoteTrackMenu->Append(OnUpOctaveID, "Up Octave");
   mNoteTrackMenu->Append(OnDownOctaveID, "Down Octave");

   mLabelTrackMenu = new wxMenu();
   mLabelTrackMenu->Append(OnSetNameID, "Name...");
   mLabelTrackMenu->AppendSeparator();
   mLabelTrackMenu->Append(OnMoveUpID, "Move Track Up");
   mLabelTrackMenu->Append(OnMoveDownID, "Move Track Down");

   mTrackArtist = new TrackArtist();
   mTrackArtist->SetInset(1, kTopInset + 1, kLeftInset + 2, 2);

   mCapturedTrack = NULL;

   mPopupMenuTarget = NULL;

   mTimeCount = 0;
   mTimer.parent = this;
   mTimer.Start(50, FALSE);
}

TrackPanel::~TrackPanel()
{
   if (mBitmap)
      delete mBitmap;

   delete mTrackArtist;

   delete mArrowCursor;
   delete mSelectCursor;
   delete mSlideCursor;
   delete mResizeCursor;
   delete mZoomInCursor;
   delete mZoomOutCursor;

   // Note that the submenus (mChannelMenu and mRateMenu) are deleted by their parent
   delete mWaveTrackMenu;
   delete mNoteTrackMenu;
   delete mLabelTrackMenu;
}

void TrackPanel::SelectNone()
{
   TrackListIterator iter(mTracks);
   VTrack *t = iter.First();
   while (t) {
      t->selected = false;

      if (t->GetKind() == VTrack::Label)
         ((LabelTrack *) t)->Unselect();

      t = iter.Next();
   }
}

void TrackPanel::GetTracksUsableArea(int *width, int *height)
{
   int w, h;
   GetSize(&w, &h);

   w -= GetLabelWidth();
   w -= 2 + kLeftInset;

   *width = w;
   *height = h;
}

void TrackPanel::OnTimer()
{
   if (mIsSelecting && mCapturedTrack) {

      if (mMouseMostRecentX > mCapturedRect.x + mCapturedRect.width) {
         mAutoScrolling = true;
         mListener->TP_ScrollRight();

         wxMouseEvent *e2 = new wxMouseEvent(wxEVT_MOTION);
         HandleSelect(*e2);
         delete e2;

         mAutoScrolling = false;
      }

      if (mMouseMostRecentX < mCapturedRect.x) {
         mAutoScrolling = true;
         mListener->TP_ScrollLeft();

         wxMouseEvent *e2 = new wxMouseEvent(wxEVT_MOTION);
         HandleSelect(*e2);
         delete e2;

         mAutoScrolling = false;
      }
   }
#ifdef BOUNCE
   if (gAudioIO->IsPlaying() &&
       gAudioIO->GetProject() == (AudacityProject *) GetParent())
      gBounce->SetTime(gAudioIO->GetIndicator());
#endif

   if (mIndicatorShowing ||
       (gAudioIO->IsBusy() &&
        gAudioIO->GetProject() == (AudacityProject *) GetParent())) {

      double ind = gAudioIO->GetIndicator();
      bool onScreen = (ind >= mViewInfo->h
                       && ind <= (mViewInfo->h + mViewInfo->screen));

      if (mIndicatorShowing || onScreen) {
         mIndicatorShowing = (onScreen &&
                              gAudioIO->IsBusy() &&
                              gAudioIO->GetProject() ==
                              (AudacityProject *) GetParent());

         wxClientDC dc(this);

         int width, height;
         GetSize(&width, &height);
         height = GetRulerHeight();

         wxMemoryDC *memDC = new wxMemoryDC();
         wxBitmap *rulerBitmap = new wxBitmap();
         rulerBitmap->Create(width, height);

         memDC->SelectObject(*rulerBitmap);

         DrawRuler(memDC, true);

         dc.Blit(0, 0, width, height, memDC, 0, 0, wxCOPY, FALSE);

         delete memDC;
         delete rulerBitmap;

      }
   }

   mTimeCount = (mTimeCount + 1) % 10;
   if (mTimeCount == 0) {
      if (!mTracks->IsEmpty() && mViewInfo->sel0 == mViewInfo->sel1) {
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

         if (!mIndicatorShowing) {
            // Draw cursor in ruler
            dc.DrawLine(x, 1, x, GetRulerHeight() - 2);
         }

         if (x >= GetLeftOffset()) {
            // Draw cursor in all selected tracks
            VTrack *t;
            TrackListIterator iter(mTracks);
            t = iter.First();
            while (t) {
               int height = t->GetHeight();
               if (t->selected && t->GetKind() != VTrack::Label)
                  dc.DrawLine(x, y + kTopInset + 1, x, y + height - 2);
               t = iter.Next();
               y += height;
            }
         }
      }
   }
}

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

void TrackPanel::MakeParentPushState(wxString desc)
{
   mListener->TP_PushState(desc);
}

void TrackPanel::MakeParentRedrawScrollbars()
{
   mListener->TP_RedrawScrollbars();
}

void TrackPanel::HandleCursor(wxMouseEvent & event)
{
   if (mIsSelecting) {
      SetCursor(*mSelectCursor);
      return;
   }

   if (mIsSliding) {
      SetCursor(*mSlideCursor);
      return;
   }

   if (mIsEnveloping) {
      SetCursor(*mArrowCursor);
      return;
   }

   wxRect r;
   int num;

   VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

   if (t) {

      // First test to see if we're over the area that
      // resizes a track

      if (event.m_y >= (r.y + r.height - 5) &&
          event.m_y < (r.y + r.height + 5)) {

         mListener->
             TP_DisplayStatusMessage("Click and drag to resize the track",
                                     0);

         SetCursor(*mResizeCursor);
         return;
      }
      // Otherwise set the cursor based on the current tool

      switch (mListener->TP_GetCurrentTool()) {
      case 0:                  // select
         mListener->
             TP_DisplayStatusMessage("Click and drag to select audio", 0);
         SetCursor(*mSelectCursor);
         break;
      case 1:                  // envelope
         mListener->TP_DisplayStatusMessage("Click and drag to edit the "
                                            "amplitude envelope", 0);
         SetCursor(*mArrowCursor);
         break;
      case 2:                  // move/slide
         mListener->
             TP_DisplayStatusMessage("Click and drag to move a track "
                                     "in time", 0);
         SetCursor(*mSlideCursor);
         break;
      case 3:                  // zoom
#ifdef __WXMAC__
         mListener->
             TP_DisplayStatusMessage
             ("Click to Zoom In, Shift-Click to Zoom Out", 0);
#endif
#ifdef __WXMSW__
         mListener->TP_DisplayStatusMessage("Left-Click to Zoom In, "
                                            "Right-Click to Zoom Out", 0);
#endif
#ifdef __WXGTK__
         mListener->
             TP_DisplayStatusMessage("Left=Zoom In, Right=Zoom Out, "
                                     "Middle=Normal", 0);
#endif
         if (event.ShiftDown())
            SetCursor(*mZoomInCursor);
         else
            SetCursor(*mZoomOutCursor);
         break;
      }
   } else {
      // Not over a track
      SetCursor(*mArrowCursor);
   }
}

void TrackPanel::HandleSelect(wxMouseEvent & event)
{
   if (event.ButtonDown()) {
      wxRect r;
      int num;

      VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

      if (t) {
         mCapturedTrack = t;
         mCapturedRect = r;
         mCapturedNum = num;

         mMouseClickX = event.m_x;
         mMouseClickY = event.m_y;

         if (event.ShiftDown()) {       // Extend selection
            double selend =
                mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);

            if (selend > mViewInfo->sel1) {
               mViewInfo->sel1 = selend;
               mSelStart = mViewInfo->sel0;
            } else if (selend < mViewInfo->sel0) {
               mViewInfo->sel0 = selend;
               mSelStart = mViewInfo->sel1;
            } else {
               mViewInfo->sel1 = selend;
               mSelStart = mViewInfo->sel0;
            }

            mListener->
                TP_DisplayStatusMessage(wxString::
                                        Format("Selection: %lf - %lf s",
                                               mViewInfo->sel0,
                                               mViewInfo->sel1), 1);
         } else {               // Selecting
            mSelStart =
                mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);

            SelectNone();
            mTracks->Select(t);

            mViewInfo->sel0 = mSelStart;
            mViewInfo->sel1 = mSelStart;

            mListener->
                TP_DisplayStatusMessage(wxString::
                                        Format("Cursor: %lf s", mSelStart),
                                        1);

            mIsSelecting = true;

            if (t->GetKind() == VTrack::Label)
               ((LabelTrack *) t)->MouseDown(mMouseClickX, mMouseClickY,
                                             mCapturedRect,
                                             mViewInfo->h,
                                             mViewInfo->zoom);
         }

         Refresh(false);

      } else {
         // No track was selected

         SelectNone();
         Refresh(false);
      }
   }

   if (!mIsSelecting)
      return;

   if (event.Dragging() || mAutoScrolling) {
      VTrack *t;
      wxRect r;
      int num;

      if (mCapturedTrack) {
         t = mCapturedTrack;
         r = mCapturedRect;
         num = mCapturedNum;
      } else
         t = FindTrack(event.m_x, event.m_y, false, &r, &num);

      if (t) {
         // Selecting

         int x, y;
         if (mAutoScrolling) {
            x = mMouseMostRecentX;
            y = mMouseMostRecentY;
         } else {
            x = event.m_x;
            y = event.m_y;
         }

         double selend = mViewInfo->h + ((x - r.x) / mViewInfo->zoom);

         if (selend < 0.0)
            selend = 0.0;

         if (selend >= mSelStart) {
            mViewInfo->sel0 = mSelStart;
            mViewInfo->sel1 = selend;
         } else {
            mViewInfo->sel0 = selend;
            mViewInfo->sel1 = mSelStart;
         }

         mListener->
             TP_DisplayStatusMessage(wxString::
                                     Format("Selection: %lf - %lf s",
                                            mViewInfo->sel0,
                                            mViewInfo->sel1), 1);

         // Handle which tracks are selected

         int num2;
         if (0 != FindTrack(x, y, false, NULL, &num2)) {
            // The tracks numbered num...num2 should be selected

            TrackListIterator iter(mTracks);
            VTrack *t = iter.First();
            int i = 1;
            while (t) {
               if ((i >= num && i <= num2) || (i >= num2 && i <= num))
                  mTracks->Select(t);
               t = iter.Next();
               i++;
            }
         }
         //wxString str;
         //str.Printf("Selection: %lf - %lf seconds",sel0,sel1);
         //status->SetLabel(str);

         Refresh(false);

#ifdef __WXMAC__
         if (mAutoScrolling)
            MacUpdateImmediately();
#endif
      }
      return;
   }

   if (event.ButtonUp()) {
      mCapturedTrack = NULL;
      mIsSelecting = false;
   }
}

void TrackPanel::HandleEnvelope(wxMouseEvent & event)
{
   if (!mCapturedTrack) {
      wxRect r;
      int num;
      VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

      if (t) {
         mCapturedTrack = t;
         mCapturedRect = r;
         mCapturedRect.y += kTopInset;
         mCapturedRect.height -= kTopInset;
         mCapturedNum = num;
      }
   }

   if (mCapturedTrack && mCapturedTrack->GetKind() == VTrack::Wave) {
      Envelope *e = ((WaveTrack *) mCapturedTrack)->GetEnvelope();
      int display = ((WaveTrack *) mCapturedTrack)->GetDisplay();
      bool needUpdate = false;

      if (display <= 1) {
         bool dB = (display == 1);

         needUpdate = e->MouseEvent(event, mCapturedRect,
                                    mViewInfo->h, mViewInfo->zoom, dB);

         // If this track is linked to another track, make the identical
         // change to the linked envelope:

         WaveTrack *link =
             (WaveTrack *) (mTracks->GetLink(mCapturedTrack));
         if (link && link->GetKind() == VTrack::Wave) {
            Envelope *e2 = link->GetEnvelope();
            e2->MouseEvent(event, mCapturedRect,
                           mViewInfo->h, mViewInfo->zoom, dB);
         }
      }

      if (needUpdate)
         Refresh(false);
   }

   if (event.ButtonUp()) {
      mCapturedTrack = NULL;
      MakeParentPushState("Adjusted envelope.");
   }
}

void TrackPanel::HandleSlide(wxMouseEvent & event)
{
   static double totalOffset;
   static wxString name;

   if (event.ButtonDown()) {

      totalOffset = 0;

      wxRect r;
      int num;

      VTrack *vt = FindTrack(event.m_x, event.m_y, false, &r, &num);
      name = vt->GetName();

      /* Scrub

         if (!vt) return;
         if (vt->GetKind() != VTrack::Wave) return;
         WaveTrack *t = (WaveTrack *)vt;

         sampleCount start = mViewInfo->h - t->tOffset;
         if (start < 0)
         start = 0;
         if (start > t->numSamples)
         start = t->numSamples;
         int twidth, theight;
         GetTracksUsableArea(&twidth, &theight);
         sampleCount len = (sampleCount)(twidth * t->rate / mViewInfo->zoom + 0.5);
         if (start + len > t->numSamples)
         len = t->numSamples - start;

         if (len > t->rate * 10) {
         // 10 secs audio is plenty
         ::wxMessageBox("Need to zoom in to scrub.");
         return;
         }

         wxStartTimer();
         long lastTime = TickCount() * 1000 / 60;//::wxGetElapsedTime();
         long curTime;

         sampleType *buffer = new sampleType[len];
         t->Get(buffer, start, len);

         double latency = 0.1;
         sampleCount blockLen = (sampleCount)(t->rate * latency + 0.5);
         sampleType *blockBuffer = new sampleType[blockLen];

         snd_node     audioOut;
         audioOut.device = SND_DEVICE_AUDIO;
         audioOut.write_flag = SND_WRITE;
         audioOut.format.channels = 1;
         audioOut.format.mode = SND_MODE_PCM;
         audioOut.format.bits = 16;
         audioOut.format.srate = t->rate;
         strcpy(audioOut.u.audio.devicename,"");
         strcpy(audioOut.u.audio.interfacename,"");
         audioOut.u.audio.descriptor = 0;
         audioOut.u.audio.protocol = SND_COMPUTEAHEAD;
         audioOut.u.audio.latency = latency;
         audioOut.u.audio.granularity = latency;

         long flags = 0;
         int err = snd_open(&audioOut, &flags);

         if (err) {
         wxMessageBox(wxString::Format("Error opening audio device: %d",err));
         return;
         }

         int pos;    
         int x, y, i;
         wxGetMousePosition(&x, &y);
         ScreenToClient(&x, &y);
         x -= GetLeftOffset();
         pos = int(x * t->rate / mViewInfo->zoom + 0.5);

         while(Button()) {
         curTime = TickCount() * 1000 / 60;//::wxGetElapsedTime();
         if (curTime > lastTime) {
         int block = snd_poll(&audioOut);
         if (block > 0) {
         if (block > blockLen)
         block = blockLen;
         wxGetMousePosition(&x, &y);
         ScreenToClient(&x, &y);
         x -= GetLeftOffset();
         int newPos = int(x * t->rate / mViewInfo->zoom + 0.5);
         int deltaSamples = newPos - pos;
         int deltaTime = curTime - lastTime;
         int sign = 1;
         if (deltaSamples < 0)
         sign = -1;
         int rate = 44100 * sign;
         int playLen = int(rate * block / t->rate + 0.5);          

         if (deltaSamples == 0)
         for(i=0; i<block; i++) {
         blockBuffer[i] = 0;
         }
         else
         for(i=0; i<block; i++) {
         int j = i * rate / (t->rate);
         if (pos+j >= len || pos+j < 0)
         blockBuffer[i] = 0;
         else
         blockBuffer[i] = buffer[pos+j];
         }

         snd_write(&audioOut, blockBuffer, block);          

         if (sign) {
         pos += playLen;
         if (pos > newPos)
         pos = newPos;
         }
         else {
         pos += playLen;
         if (pos < newPos)
         pos = newPos;
         }
         lastTime = curTime;
         }
         }
         }

         snd_close(&audioOut);

         delete[] buffer;
         delete[] blockBuffer;

         End scrub
       */
      if (vt) {

         mCapturedTrack = vt;
         mCapturedRect = r;
         mCapturedNum = num;

         mMouseClickX = event.m_x;
         mMouseClickY = event.m_y;

         mSelStart = mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);
         mIsSliding = true;
      }

   }

   if (!mIsSliding)
      return;

   if (event.Dragging() && mCapturedTrack) {

      double selend = mViewInfo->h +
          ((event.m_x - mCapturedRect.x) / mViewInfo->zoom);

      if (selend < 0.0)
         selend = 0.0;

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

   if (event.ButtonUp()) {
      mCapturedTrack = NULL;
      mIsSliding = false;
      MakeParentRedrawScrollbars();
      if(totalOffset > 0)
         MakeParentPushState(
            wxString::Format("Slid track '%s' %s %.02f seconds", 
                             name.c_str(),
                             totalOffset > 0 ? "right" : "left",
                             totalOffset > 0 ? totalOffset : -totalOffset));
   }
}

void TrackPanel::HandleZoom(wxMouseEvent &event)
{
   if (event.ButtonDown() || event.ButtonDClick()) {
      mZoomStart = event.m_x;
      mZoomEnd = event.m_x;
   }
   else if (event.Dragging()) {
      mZoomEnd = event.m_x;

      int zoomLength = mZoomEnd - mZoomStart;
      if (zoomLength < 0)
         zoomLength = - zoomLength;

      if (zoomLength > 3)
         mIsZooming = true;

      if (mIsZooming)
         Refresh(false);
   }
   else if (event.ButtonUp()) {
      wxRect r;
      int num;
      bool isZooming = mIsZooming;
      mIsZooming = false;

      VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

      int zoomLength = mZoomEnd - mZoomStart;      
      if (zoomLength < 0) {
         zoomLength = - zoomLength;
         int tmp = mZoomEnd;
         mZoomEnd = mZoomStart;
         mZoomStart = tmp;
      }

      if (isZooming || zoomLength > 3) {

         double left =
            mViewInfo->h + ((mZoomStart - r.x) / mViewInfo->zoom);
         double right = 
            mViewInfo->h + ((mZoomEnd - r.x) / mViewInfo->zoom);      

         mViewInfo->zoom *= mViewInfo->screen/(right-left);
         
         if (mViewInfo->zoom > 6000000)
            mViewInfo->zoom = 6000000;
         
         mViewInfo->h = left;
         
         if (mViewInfo->h < 0) mViewInfo->h = 0;
         
         MakeParentRedrawScrollbars();
         Refresh(false);
      }
      else {
         double center_h = mViewInfo->h + (event.m_x - r.x) / mViewInfo->zoom;
         
         if (event.RightUp() || event.RightDClick() || event.ShiftDown())
            mViewInfo->zoom /= 2.0;
         else
            mViewInfo->zoom *= 2.0;
         
         if (event.MiddleUp() || event.MiddleDClick())
            mViewInfo->zoom = 44100.0 / 512.0;
         
            if (mViewInfo->zoom > 6000000)
               mViewInfo->zoom = 6000000;
            
            double new_center_h =
               mViewInfo->h + (event.m_x - r.x) / mViewInfo->zoom;
            
            mViewInfo->h += (center_h - new_center_h);
            
            if (mViewInfo->h < 0)
               mViewInfo->h = 0;
            
            MakeParentRedrawScrollbars();
            Refresh(false);
      }
   }
}

void TrackPanel::HandleClosing(wxMouseEvent & event)
{
   VTrack *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   wxRect closeRect;
   GetCloseBoxRect(r, closeRect);
   if (event.Dragging()) {
      wxClientDC dc(this);
      DrawCloseBox(&dc, r, closeRect.Inside(event.m_x, event.m_y));
      return;
   }
   if (event.ButtonUp()) {
      wxClientDC dc(this);
      DrawCloseBox(&dc, r, false);
      if (closeRect.Inside(event.m_x, event.m_y)) {
         RemoveTrack(t);
      }
      mIsClosing = false;
   }
}

void TrackPanel::HandleMuting(wxMouseEvent & event)
{
   VTrack *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   wxRect muteRect;
   GetMuteRect(r, muteRect);
   if (event.Dragging()) {
      wxClientDC dc(this);
      DrawMute(&dc, r, t, muteRect.Inside(event.m_x, event.m_y));
      return;
   }
   if (event.ButtonUp()) {
      wxClientDC dc(this);
      if (muteRect.Inside(event.m_x, event.m_y)) {
         t->mute = !t->mute;
      }
      DrawMute(&dc, r, t, false);
      mIsMuting = false;
   }
}

void TrackPanel::HandleSoloing(wxMouseEvent & event)
{
   VTrack *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   wxRect soloRect;
   GetSoloRect(r, soloRect);
   if (event.Dragging()) {
      wxClientDC dc(this);
      DrawSolo(&dc, r, t, soloRect.Inside(event.m_x, event.m_y));
      return;
   }
   if (event.ButtonUp()) {
      wxClientDC dc(this);
      if (soloRect.Inside(event.m_x, event.m_y)) {
         t->solo = !t->solo;
      }
      DrawSolo(&dc, r, t, false);
      mIsSoloing = false;
   }
}

void TrackPanel::HandleLabelClick(wxMouseEvent & event)
{
   if (!(event.ButtonDown() || event.ButtonDClick()))
      return;

   wxRect r;
   int num;
   bool second = false;

   VTrack *t = FindTrack(event.m_x, event.m_y, true, &r, &num);

   if (!t) {
      SelectNone();
      Refresh(false);
	  return;
   }

   if (!t->linked && mTracks->GetLink(t))
      second = true;

   if (event.ShiftDown()) {
      mTracks->Select(t, !t->selected);
      Refresh(false);
      return;
   }

   wxRect closeRect;
   GetCloseBoxRect(r, closeRect);
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
   if (!second && titleRect.Inside(event.m_x, event.m_y)) {
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
         if (next && !t->linked && !next->linked &&
             t->GetKind() == VTrack::Wave
             && next->GetKind() == VTrack::Wave)
            canMakeStereo = true;

         theMenu->Enable(theMenu->FindItem
                         ("Make Stereo Track"), canMakeStereo);
         theMenu->Enable(theMenu->FindItem
                         ("Split Stereo Track"), t->linked);
         theMenu->Enable(theMenu->FindItem("Mono"), !t->linked);
         theMenu->Enable(theMenu->FindItem("Left Channel"), !t->linked);
         theMenu->Enable(theMenu->FindItem("Right Channel"), !t->linked);

         int display = ((WaveTrack *) t)->GetDisplay();

         theMenu->Enable(theMenu->FindItem("Waveform"), display != 0);
         theMenu->Enable(theMenu->FindItem("Waveform (dB)"), display != 1);
         theMenu->Enable(theMenu->FindItem("Spectrum"), display != 2);
         theMenu->Enable(theMenu->FindItem("Pitch (EAC)"), display != 3);
      }

      if (t->GetKind() == VTrack::Note) {
         theMenu = mNoteTrackMenu;
      }

      if (t->GetKind() == VTrack::Label) {
         theMenu = mLabelTrackMenu;
      }

      if (theMenu) {

         theMenu->Enable(theMenu->FindItem
                         ("Move Track Up"), mTracks->CanMoveUp(t));
         theMenu->Enable(theMenu->FindItem
                         ("Move Track Down"), mTracks->CanMoveDown(t));

#ifdef __WXMAC__
         ::InsertMenu(mRateMenu->GetHMenu(), -1);
#endif

         PopupMenu(theMenu, titleRect.x + 1,
                   titleRect.y + titleRect.height + 1);

#ifdef __WXMAC__
         ::DeleteMenu(mRateMenu->MacGetMenuId());
#endif
      }

      {
         VTrack *t2 = FindTrack(event.m_x, event.m_y, true, &r, &num);
         if (t2 == t) {
            wxClientDC dc(this);
            SetLabelFont(&dc);
            DrawTitleBar(&dc, r, t, false);
         }
      }
      return;
   }
   
   // Check Mute and Solo buttons on WaveTracks:
   if (!second && t->GetKind() == VTrack::Wave) {
      wxRect muteRect;
      GetMuteRect(r, muteRect);
      if (muteRect.Inside(event.m_x, event.m_y)) {
         wxClientDC dc(this);
         DrawMute(&dc, r, t, true);
         mIsMuting = true;
         mCapturedTrack = t;
         mCapturedRect = r;
         return;
      }
      
      wxRect soloRect;
      GetSoloRect(r, soloRect);
      if (soloRect.Inside(event.m_x, event.m_y)) {
         wxClientDC dc(this);
         DrawSolo(&dc, r, t, true);
         mIsSoloing = true;
         mCapturedTrack = t;
         mCapturedRect = r;
         return;
      }
   }      
   
   // If it's a NoteTrack, it has special controls
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
   // If they weren't clicking on a particular part of a track label,
   // deselect other tracks and select this one.

   SelectNone();
   mTracks->Select(t);
   mViewInfo->sel0 = 0.0;
   mViewInfo->sel1 = mTracks->GetMaxLen();
   Refresh(false);
}

void TrackPanel::HandleResize(wxMouseEvent & event)
{
   if (event.ButtonDown()) {
      wxRect r;
      int num;

      VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

      if (t) {
         mCapturedTrack = t;
         mCapturedRect = r;
         mCapturedNum = num;

         mMouseClickX = event.m_x;
         mMouseClickY = event.m_y;

         mIsResizing = true;
         mInitialTrackHeight = t->GetHeight();
      }
   }

   if (!mIsResizing)
      return;

   if (event.Dragging()) {
      int delta = (event.m_y - mMouseClickY);
      int newTrackHeight = mInitialTrackHeight + delta;
      if (newTrackHeight < 20)
         newTrackHeight = 20;
      mCapturedTrack->SetHeight(newTrackHeight);
      Refresh(false);
   }

   if (event.ButtonUp()) {
      mCapturedTrack = NULL;
      mIsResizing = false;
      MakeParentRedrawScrollbars();
      MakeParentPushState("TrackPanel::HandleResize() FIXME!!");
   }
}

void TrackPanel::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown()) {
      event.Skip();
      return;
   }

   TrackListIterator iter(mTracks);

   switch (event.KeyCode()) {
   case WXK_SPACE:
      mListener->TP_OnPlayKey();
      break;

   default:
      VTrack * t = iter.First();
      while (t) {
         if ( /* t->selected && */ t->GetKind() == VTrack::Label) {
            ((LabelTrack *) t)->KeyEvent(mViewInfo->sel0, mViewInfo->sel1,
                                         event);
            Refresh(false);
            MakeParentPushState("TrackPanel::OnKeyEvent() FIXME!!");
            return;
         }

         t = iter.Next();
      }

   }
}

void TrackPanel::OnMouseEvent(wxMouseEvent & event)
{
   mListener->TP_HasMouse();

   if (!mAutoScrolling) {
      mMouseMostRecentX = event.m_x;
      mMouseMostRecentY = event.m_y;
   }

   if (event.ButtonDown()) {
      mCapturedTrack = NULL;

      wxActivateEvent *e = new wxActivateEvent();
      GetParent()->ProcessEvent(*e);
      delete e;
   }

   if (mIsClosing) {
      HandleClosing(event);
      return;
   }
   
   if (mIsMuting) {
      HandleMuting(event);
      return;
   }
   
   if (mIsSoloing) {
      HandleSoloing(event);
      return;
   }

   if (mIsResizing) {
      HandleResize(event);
      HandleCursor(event);
      return;
   }

   wxRect r;
   int dummy;
   
   FindTrack(event.m_x, event.m_y, false, &r, &dummy);

   if (event.ButtonDown() &&
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

   case 0:                     // select
      HandleSelect(event);
      break;
   case 1:                     // envelope
      HandleEnvelope(event);
      break;
   case 2:                     // move/slide
      HandleSlide(event);
      break;
   case 3:                     // zoom
      HandleZoom(event);
      break;
   }

   if ((event.Moving() || event.ButtonUp()) &&
       !mIsSelecting && !mIsEnveloping && !mIsSliding) {
      HandleCursor(event);
   }
   if (event.ButtonUp()) {
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

   //
   // Draw ruler border
   //

   AColor::Medium(dc, false);
   dc->DrawRectangle(r);

   r.width--;
   r.height--;
   AColor::Bevel(*dc, true, r);

   dc->SetPen(*wxBLACK_PEN);
   dc->DrawLine(r.x, r.y + r.height + 1, r.x + r.width + 1,
                r.y + r.height + 1);

   //
   // Draw selection
   //

   if (mViewInfo->sel0 < mViewInfo->sel1) {
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
   //
   // Draw marks on ruler
   //

   dc->SetPen(*wxBLACK_PEN);

   SetLabelFont(dc);

   int minSpace = 60;           // min pixels between labels

   wxString unitStr;
   double unit = 1.0;
   double base;

   while (unit * mViewInfo->zoom < minSpace)
      unit *= 2.0;
   while (unit * mViewInfo->zoom > minSpace * 2)
      unit /= 2.0;

   if (unit < 0.0005) {
      unitStr = "us";           // microseconds
      base = 0.000001;
   } else if (unit < 0.5) {
      unitStr = "ms";           // milliseconds
      base = 0.001;
   } else if (unit < 30.0) {
      unitStr = "s";            // seconds
      base = 1.0;
   } else if (unit < 1800.0) {
      unitStr = "m";            // minutes
      base = 60.0;
   } else {
      unitStr = "h";            // hours
      base = 3600.0;
   }

   unit = base;

   bool hand = true;

   while (unit * mViewInfo->zoom < minSpace) {
      unit *= (hand ? 5.0 : 2.0);
      hand = !hand;
   }
   while (unit * mViewInfo->zoom > minSpace * (hand ? 2.0 : 5.0)) {
      unit /= (hand ? 2.0 : 5.0);
      hand = !hand;
   }

   unit /= 4;

   double pos = mViewInfo->h - GetLeftOffset() / mViewInfo->zoom;
   int unitcount = (int) (pos / unit);

   dc->SetTextForeground(wxColour(0, 0, 204));

   //int nextxpos = 0;

   for (int pixel = 0; pixel < r.width; pixel++) {

      if (((int) (floor(pos / unit))) > unitcount) {
         unitcount = (int) (floor(pos / unit));

         switch ((unitcount) % 4) {
         case 0:
            dc->DrawLine(r.x + pixel, r.y + 8, r.x + pixel,
                         r.y + r.height);

            if (text) {
               char str[100];
               sprintf(str, "%.1f%s", unitcount * unit / base,
                       (const char *) unitStr);
               /*
                  long textWidth, textHeight;
                  dc.GetTextExtent(str, &textWidth, &textHeight);

                  if (pixel >= nextxpos && pixel+2+textWidth < r.width) { */
               dc->DrawText(str, r.x + pixel + 3, r.y + 2);
               /*
                  nextxpos = pixel + textWidth + 12;
                  } */
            }

            break;

         case 1:
         case 3:
            dc->DrawLine(r.x + pixel, r.y + r.height - 4, r.x + pixel,
                         r.y + r.height);
            break;

         case 2:
            dc->DrawLine(r.x + pixel, r.y + r.height - 6, r.x + pixel,
                         r.y + r.height);
            break;
         }
      }
      pos += 1.0 / mViewInfo->zoom;
   }

   //
   // Draw indicator
   //

   if (gAudioIO->IsBusy() &&
       gAudioIO->GetProject() == (AudacityProject *) GetParent()) {

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
}

void TrackPanel::GetCloseBoxRect(wxRect & r, wxRect & dest)
{
   dest.x = r.x;
   dest.y = r.y;
   dest.width = 16;
   dest.height = 16;
}

void TrackPanel::GetTitleBarRect(wxRect & r, wxRect & dest)
{
   dest.x = r.x + 16;
   dest.y = r.y;
   dest.width = GetTitleWidth() - r.x - 16;
   dest.height = 16;
}

void TrackPanel::GetMuteRect(wxRect & r, wxRect & dest)
{
   dest.x = r.x + 8;
   dest.y = r.y + 34;
   dest.width = 36;
   dest.height = 16;
}

void TrackPanel::GetSoloRect(wxRect & r, wxRect & dest)
{
   dest.x = r.x + 8 + 36 + 8;
   dest.y = r.y + 34;
   dest.width = 36;
   dest.height = 16;
}

void TrackPanel::GetTrackControlsRect(wxRect & r, wxRect & dest)
{
   dest = r;
   dest.width = GetTitleWidth();
   dest.x += 4 + kLeftInset;
   dest.width -= (8 + kLeftInset);
   dest.y += 18 + kTopInset;
   dest.height -= (24 + kTopInset);
}

void TrackPanel::DrawCloseBox(wxDC * dc, wxRect & r, bool down)
{
   dc->SetPen(*wxBLACK_PEN);
   dc->DrawLine(r.x + 3, r.y + 3, r.x + 13, r.y + 13);  // close "x"
   dc->DrawLine(r.x + 13, r.y + 3, r.x + 3, r.y + 13);
   wxRect bev;
   GetCloseBoxRect(r, bev);
   bev.Inflate(-1, -1);
   AColor::Bevel(*dc, !down, bev);
}

void TrackPanel::DrawTitleBar(wxDC * dc, wxRect & r, VTrack * t, bool down)
{
   wxRect bev;
   GetTitleBarRect(r, bev);
   bev.Inflate(-1, -1);
   AColor::Bevel(*dc, true, bev);

   // Draw title text
   wxString titleStr = t->name;
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

void TrackPanel::DrawMute(wxDC * dc, wxRect & r, VTrack * t, bool down)
{
   wxRect bev;
   GetMuteRect(r, bev);
   bev.Inflate(-1, -1);
   AColor::Mute(dc, t->mute, t->selected);
   dc->DrawRectangle(bev);

   long textWidth, textHeight;
   wxString str = "Mute";
   SetLabelFont(dc);
   dc->GetTextExtent(str, &textWidth, &textHeight);
   dc->DrawText(str, bev.x + (bev.width - textWidth)/2, bev.y + 2);

   AColor::Bevel(*dc, !down, bev);
}

void TrackPanel::DrawSolo(wxDC * dc, wxRect & r, VTrack * t, bool down)
{
   wxRect bev;
   GetSoloRect(r, bev);
   bev.Inflate(-1, -1);
   AColor::Solo(dc, t->solo, t->selected);
   dc->DrawRectangle(bev);

   long textWidth, textHeight;
   wxString str = "Solo";
   SetLabelFont(dc);
   dc->GetTextExtent(str, &textWidth, &textHeight);
   dc->DrawText(str, bev.x + (bev.width - textWidth)/2, bev.y + 2);

   AColor::Bevel(*dc, !down, bev);
}

void TrackPanel::DrawTracks(wxDC * dc)
{
   wxRect clip;
   clip.x = 0;
   clip.y = 0;
   GetSize(&clip.width, &clip.height);

   wxRect panelRect = clip;
   panelRect.y = -mViewInfo->vpos;

   // Make room for ruler
   panelRect.y += GetRulerHeight();
   panelRect.height -= GetRulerHeight();

   VTrack *t;
   int num = 0;

   wxRect tracksRect = panelRect;
   tracksRect.x += GetLabelWidth();
   tracksRect.width -= GetLabelWidth();

   bool envelopeFlag = (mListener->TP_GetCurrentTool() == 1);

   // The track artist actually draws the stuff inside each track

   mTrackArtist->DrawTracks(mTracks,
                            *dc, tracksRect,
                            clip, mViewInfo, envelopeFlag);

   // We draw everything else

   TrackListIterator iter(mTracks);

   wxRect trackRect = panelRect;
   wxRect r;

   t = iter.First();
   while (t) {
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
      if (t->linked) {
         r.height += mTracks->GetLink(t)->GetHeight();
      } else if (mTracks->GetLink(t))
         skipBorder = true;

      if (!skipBorder) {

         // Fill in area outside of the track
         AColor::Medium(dc, false);
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

         if (t->linked) {
            side = r;
            side.y += t->GetHeight() - 1;
            side.height = kTopInset + 1;
            dc->DrawRectangle(side);
         }

         r.x += kLeftInset;
         r.y += kTopInset;
         r.width -= kLeftInset * 2;
         r.height -= kTopInset;

         // fill in label
         wxRect fill = r;
         fill.width = labelw - r.x;
         AColor::Medium(dc, t->selected);
         dc->DrawRectangle(fill);

         // fill in track
         /*
            fill = r;
            fill.x = labelw;
            fill.width -= (labelw - r.x);
            AColor::Medium(dc, false);
            dc->DrawRectangle(fill);
          */

         // Borders around track and label area
         dc->SetPen(*wxBLACK_PEN);
         dc->DrawLine(r.x, r.y, r.x + r.width - 1, r.y);        // top
         dc->DrawLine(r.x, r.y, r.x, r.y + r.height - 1);       // left
         dc->DrawLine(r.x, r.y + r.height - 2, r.x + r.width - 1, r.y + r.height - 2);  // bottom
         dc->DrawLine(r.x + r.width - 2, r.y, r.x + r.width - 2, r.y + r.height - 1);   // right
         dc->DrawLine(vrul, r.y, vrul, r.y + r.height - 1);
         dc->DrawLine(labelw, r.y, labelw, r.y + r.height - 1); // between vruler and track

         if (t->linked) {
            int h1 = r.y + t->GetHeight() - kTopInset;
            dc->DrawLine(vrul, h1 - 2, r.x + r.width - 1, h1 - 2);
            dc->DrawLine(vrul, h1 + kTopInset, r.x + r.width - 1,
                         h1 + kTopInset);
         }

         dc->DrawLine(r.x, r.y + 16, GetTitleWidth(), r.y + 16);        // title bar
         dc->DrawLine(r.x + 16, r.y, r.x + 16, r.y + 16);       // close box

         // Shadow
         AColor::Dark(dc, false);
         dc->DrawLine(r.x + 1, r.y + r.height - 1, r.x + r.width, r.y + r.height - 1);  // bottom
         dc->DrawLine(r.x + r.width - 1, r.y + 1, r.x + r.width - 1, r.y + r.height);   // right

         r.width = GetTitleWidth();
         DrawCloseBox(dc, r, false);
         DrawTitleBar(dc, r, t, false);
         
         if (t->GetKind() == VTrack::Wave) {
            DrawMute(dc, r, t, false);
            DrawSolo(dc, r, t, false);
         }

         r = trackRect;

         if (t->GetKind() == VTrack::Wave) {
            wxString s =
                wxString::Format("%dHz",
                                 (int) (((WaveTrack *) t)->GetRate() +
                                        0.5));
            if (t->linked)
               s = "Stereo, " + s;
            else {
               if (t->channel == VTrack::MonoChannel)
                  s = "Mono, " + s;
               if (t->channel == VTrack::LeftChannel)
                  s = "Left, " + s;
               if (t->channel == VTrack::RightChannel)
                  s = "Right, " + s;
            }
            dc->DrawText(s, r.x + 6, r.y + 22);
         }

         if (t->GetKind() == VTrack::Note) {
            wxRect midiRect;
            GetTrackControlsRect(trackRect, midiRect);
            ((NoteTrack *) t)->DrawLabelControls(*dc, midiRect);

         }
      }

      r = trackRect;
      r.x += GetVRulerOffset();
      r.y += kTopInset;
      r.width = GetVRulerWidth();
      r.height -= (kTopInset + 2);
      mTrackArtist->DrawVRuler(t, dc, r);

      trackRect.y += t->GetHeight();
      num++;
      t = iter.Next();
   }

   // Draw zooming indicator

   if (mIsZooming) {
      wxRect r;

      r.x = mZoomStart;
      r.y = -1;
      r.width = mZoomEnd - mZoomStart;
      r.height = clip.height+2;

      dc->SetBrush(*wxTRANSPARENT_BRUSH);
      dc->SetPen(*wxBLACK_DASHED_PEN);

      dc->DrawRectangle(r);
   }

   // Paint over the part below the tracks

   GetSize(&trackRect.width, &trackRect.height);
   AColor::Medium(dc, false);
   dc->DrawRectangle(trackRect);
}

/*

old DrawTracks

void TrackPanel::DrawTracks(wxDC *dc)
{
  int windowWidth;
  int windowHeight;
  GetSize(&windowWidth, &windowHeight);

  wxRect clip;
  clip.x = 0;
  clip.y = 0;
  clip.width = windowWidth;
  clip.height = windowHeight;

  wxRect r;
  r.width = windowWidth;
  r.height = windowHeight;
  r.x = 0;
  r.y = -mViewInfo->vpos;

  // Make room for ruler
  r.y += GetRulerHeight();
  r.height -= GetRulerHeight();
  
  VTrack *t;
  int num=0;

  wxRect allTracksRect = r;
  allTracksRect.x += GetLabelWidth();
  allTracksRect.width -= GetLabelWidth();

  bool envelopeFlag = (mListener->TP_GetCurrentTool() == 1);

  TrackListIterator iter(mTracks);
  
  t = iter.First();
  while(t) {
    r.height = t->GetHeight();

    // Skip it if it's totally offscreen
    if (r.y + r.height < GetRulerHeight() ||
        r.y >windowHeight) {
      
      r.y += r.height;
      num++;
      t = iter.Next();
      continue;
    }

    // Draw label area

    SetLabelFont(dc);
    dc->SetTextForeground(wxColour(0, 0, 0));

    wxRect labelRect = r;
    labelRect.width = GetLabelWidth();  

    // If this track is linked to the next one, display a common
    // border for both, otherwise draw a normal border

    wxRect labelBorder = labelRect;
    
    bool skipBorder = false;
    if (t->linked) {
      labelBorder.height += mTracks->GetLink(t)->GetHeight();
    }
    else if (mTracks->GetLink(t))
      skipBorder = true;

    if (!skipBorder) {
      AColor::Medium(dc, false);
      dc->DrawRectangle(labelBorder);
      
      labelBorder.Inflate(-4, -4);
      AColor::Medium(dc, t->selected);
      dc->DrawRectangle(labelBorder);
      AColor::Bevel(*dc, false, labelBorder);
    }

    wxRect titleRect;
    if (GetLabelFieldRect(labelRect, 0, false, titleRect)) {
      AColor::Bevel(*dc, false, titleRect);
      dc->DrawText(wxString::Format("Track %d", num+1),
                   titleRect.x + 7, titleRect.y + 2);
    }
    
    wxRect channelRect;
    if (GetLabelFieldRect(labelRect, 1, true, channelRect)) {
      dc->DrawText("Channel:", labelRect.x + 7, channelRect.y + 2);
      AColor::Bevel(*dc, true, channelRect);
      wxString str;
      switch(t->channel) {
      case VTrack::MonoChannel: str = "Mono"; break;
      case VTrack::LeftChannel: str = "Left"; break;
      case VTrack::RightChannel: str = "Right"; break;
      default: str = "Other"; break;
      }
      dc->DrawText(str, channelRect.x + 3, channelRect.y + 2);    
    }

    if (t->GetKind()==VTrack::Wave) {
      wxRect rateRect;
      if (GetLabelFieldRect(labelRect, 2, true, rateRect)) {
        dc->DrawText("Rate:", labelRect.x + 7, rateRect.y + 2);
        AColor::Bevel(*dc, true, rateRect);
        dc->DrawText(wxString::Format("%d",int(((WaveTrack *)t)->rate + 0.5)),
                    rateRect.x + 3, rateRect.y + 2);
      }

      wxRect displayRect;
      if (GetLabelFieldRect(labelRect, 3, true, displayRect)) {
        dc->DrawText("Display:", labelRect.x + 7, displayRect.y + 2);
        AColor::Bevel(*dc, true, displayRect);
        wxString str;
        if (((WaveTrack *)t)->GetDisplay() == 1)
          str = "Spectr";
        else
          str = "Wavefm";
        dc->DrawText(str, displayRect.x + 3, displayRect.y + 2);
      }
    }
    
    if (t->GetKind()==VTrack::Note) {
      wxRect midiRect;
      if (GetLabelFieldRect(labelRect, 2, false, midiRect)) {
        midiRect.height = labelRect.height - (midiRect.y - labelRect.y) - (midiRect.x - labelRect.x);
        ((NoteTrack *)t)->DrawLabelControls(*dc, midiRect);
      }
    }

    // Draw track area

    wxRect trackRect = r;
    trackRect.x += GetLabelWidth();
    trackRect.width -= GetLabelWidth();

    AColor::Medium(dc, false);
    dc->DrawRectangle(trackRect);

    trackRect.Inflate(-4, -4);
    AColor::Bevel(*dc, false, trackRect);   

    // Don't draw if it's not visible at all (vertically)
    // if (r.y < (visible->y + visible->height)
    // && (r.y + r.height) > visible->y)

    double h = mViewInfo->h;

    bool sel = t->selected;
    
    // Tell VTrack to draw itself
    
    wxRect innerRect = trackRect;
    innerRect.Inflate(-1, -1);

    // Code duplication warning:  If you add anything here
    // that happens any time in the loop, add it to the top
    // of this loop also for the special case of tracks that
    // are totally offscreen.
    r.y += r.height;
    num++;
    t = iter.Next();
  }

  // The track artist actually draws the stuff in the tracks

  mTrackArtist->DrawTracks(mTracks,
                           *dc, allTracksRect,
                           clip,
                           mViewInfo,
                           envelopeFlag);

  // Paint over the part below the tracks

  GetSize(&r.width, &r.height);
  AColor::Medium(dc, false);
  dc->DrawRectangle(r);
}
*/

void TrackPanel::OnChannelLeft()
{
   if (mPopupMenuTarget) {
      mPopupMenuTarget->channel = VTrack::LeftChannel;
      MakeParentPushState(
            wxString::Format("Changed '%s' to 'left' channel",
                             mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      Refresh(false);
   }
}

void TrackPanel::OnChannelRight()
{
   if (mPopupMenuTarget) {
      mPopupMenuTarget->channel = VTrack::RightChannel;
      MakeParentPushState(
            wxString::Format("Changed '%s' to 'right' channel",
                             mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      Refresh(false);
   }
}

void TrackPanel::OnChannelMono()
{
   if (mPopupMenuTarget) {
      mPopupMenuTarget->channel = VTrack::MonoChannel;
      MakeParentPushState(
            wxString::Format("Changed '%s' to 'mono'",
                             mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      Refresh(false);
   }
}

void TrackPanel::OnSplitStereo()
{
   if (mPopupMenuTarget) {
      mPopupMenuTarget->linked = false;
      MakeParentPushState(
            wxString::Format("Split stereo track '%s'",
                             mPopupMenuTarget->GetName().c_str()));
      Refresh(false);
   }
}

void TrackPanel::OnMergeStereo()
{
   if (mPopupMenuTarget) {
      mPopupMenuTarget->linked = true;
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner) {
         mPopupMenuTarget->channel = VTrack::LeftChannel;
         partner->channel = VTrack::RightChannel;
         MakeParentPushState(
             wxString::Format("Made '%s' a stereo track",
                              mPopupMenuTarget->GetName().c_str()));
      } else
         mPopupMenuTarget->linked = false;
      Refresh(false);
   }
}

void TrackPanel::RemoveTrack(VTrack * toRemove)
{
   TrackListIterator iter(mTracks);
   VTrack *t = iter.First();
   VTrack *partner = mTracks->GetLink(toRemove);
   wxString name;

   while (t) {
      if (t == toRemove || (partner && t == partner)) {
         name = t->GetName();
         t = iter.RemoveCurrent();
      }
      else
         t = iter.Next();
   }

   MakeParentPushState(wxString::Format("Removed track '%s.'", name.c_str()));
   MakeParentRedrawScrollbars();

   Refresh(false);
}

void TrackPanel::OnWaveform()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetDisplay(0);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetDisplay(0);
      MakeParentPushState(
          wxString::Format("Changed '%s' to waveform display",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      Refresh(false);
   }
}

void TrackPanel::OnWaveformDB()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetDisplay(1);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetDisplay(1);
      MakeParentPushState(
          wxString::Format("Changed '%s' to waveformDB display",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      Refresh(false);
   }
}

void TrackPanel::OnSpectrum()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetDisplay(2);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetDisplay(2);
      MakeParentPushState(
          wxString::Format("Changed '%s' to spectrum display",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      Refresh(false);
   }
}

void TrackPanel::OnPitch()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetDisplay(3);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetDisplay(3);
      MakeParentPushState(
          wxString::Format("Changed '%s' to pitch display",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      Refresh(false);
   }
}

void TrackPanel::OnRate8()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetRate(8000.0);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetRate(8000.0);
      MakeParentPushState(
          wxString::Format("Changed '%s' to 8000 Hz",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      MakeParentRedrawScrollbars();
      Refresh(false);
   }
}

void TrackPanel::OnRate11()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetRate(11025.0);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetRate(11025.0);
      MakeParentPushState(
          wxString::Format("Changed '%s' to 11025 Hz",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      MakeParentRedrawScrollbars();
      Refresh(false);
   }
}

void TrackPanel::OnRate16()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetRate(16000.0);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetRate(16000.0);
      MakeParentPushState(
          wxString::Format("Changed '%s' to 16 kHz",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      MakeParentRedrawScrollbars();
      Refresh(false);
   }
}

void TrackPanel::OnRate22()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetRate(22050.0);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetRate(22050.0);
      MakeParentPushState(
          wxString::Format("Changed '%s' to 22 kHz",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      MakeParentRedrawScrollbars();
      Refresh(false);
   }
}

void TrackPanel::OnRate44()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetRate(44100.0);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetRate(44100.0);
      MakeParentPushState(
          wxString::Format("Changed '%s' to 44 kHz",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      MakeParentRedrawScrollbars();
      Refresh(false);
   }
}

void TrackPanel::OnRate48()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      ((WaveTrack *) mPopupMenuTarget)->SetRate(48000.0);
      VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
      if (partner)
         ((WaveTrack *) partner)->SetRate(48000.0);
      MakeParentPushState(
          wxString::Format("Changed '%s' to 48 kHz",
                           mPopupMenuTarget->GetName().c_str()));
      mPopupMenuTarget = NULL;
      MakeParentRedrawScrollbars();
      Refresh(false);
   }
}

void TrackPanel::OnRateOther()
{
   if (mPopupMenuTarget && mPopupMenuTarget->GetKind() == VTrack::Wave) {
      wxString defaultStr;
      defaultStr.Printf("%d",(int)(((WaveTrack *) mPopupMenuTarget)->rate+0.5));
      wxString rateStr =
          wxGetTextFromUser("Enter a rate in Hz (samples per second):",
                            "Set Rate",
                            defaultStr);

      if (rateStr != "") {
         double theRate;
         if (rateStr.ToDouble(&theRate) &&
             theRate >= 1 && theRate <= 100000) {
            ((WaveTrack *) mPopupMenuTarget)->SetRate(theRate);
            VTrack *partner = mTracks->GetLink(mPopupMenuTarget);
            if (partner)
               ((WaveTrack *) partner)->SetRate(theRate);
            MakeParentPushState(
                wxString::Format("Changed '%s' to %.0f Hz",
                                 mPopupMenuTarget->GetName().c_str(), theRate));
            MakeParentRedrawScrollbars();
            Refresh(false);
         } else
            wxMessageBox("Invalid rate.");
      }

      mPopupMenuTarget = NULL;
   }
}

void TrackPanel::OnMoveUp()
{
   VTrack *t = mPopupMenuTarget;
   if (mTracks->MoveUp(t)) {
      MakeParentPushState(
         wxString::Format("Moved '%s' up", mPopupMenuTarget->GetName().c_str()));
      Refresh(false);
   }
}

void TrackPanel::OnMoveDown()
{
   VTrack *t = mPopupMenuTarget;
   if (mTracks->MoveDown(t)) {
      MakeParentPushState(
         wxString::Format("Moved '%s' down", mPopupMenuTarget->GetName().c_str()));
      Refresh(false);
   }
}

void TrackPanel::OnUpOctave()
{
   VTrack *t = mPopupMenuTarget;
   if (t->GetKind() == VTrack::Note) {
      ((NoteTrack *) t)->mBottomNote += 12;
      if (((NoteTrack *) t)->mBottomNote < 0)
         ((NoteTrack *) t)->mBottomNote = 0;
      MakeParentPushState("TrackPanel::OnUpOctave() FIXME!!");
      Refresh(false);
   }
}

void TrackPanel::OnDownOctave()
{
   VTrack *t = mPopupMenuTarget;
   if (t->GetKind() == VTrack::Note) {
      ((NoteTrack *) t)->mBottomNote -= 12;
      if (((NoteTrack *) t)->mBottomNote > 96)
         ((NoteTrack *) t)->mBottomNote = 96;
      MakeParentPushState("TrackPanel::OnDownOctave() FIXME!!");
      Refresh(false);
   }
}

void TrackPanel::OnSetName()
{
   VTrack *t = mPopupMenuTarget;

   if (t) {
      wxString defaultStr = t->name;
      wxString newName = wxGetTextFromUser("Change track name to:",
                                           "Track Name",
                                           defaultStr);
      if (newName != "")
         t->name = newName;
      MakeParentPushState(
          wxString::Format("Renamed '%s' to '%s'",
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
   VTrack *t = iter.First();

   int n = 1;

   while (t) {
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

      r.y += r.height;
      n++;
      t = iter.Next();
   }

   if (mouseY >= r.y && trackNum)
      *trackNum = n - 1;

   return NULL;
}

int TrackPanel::GetRulerHeight()
{
   return 20;
}

int TrackPanel::GetLeftOffset()
{
   return GetLabelWidth() + 1;
}

int TrackPanel::GetTitleWidth()
{
   return 100;
}

int TrackPanel::GetTitleOffset()
{
   return 0;
}

int TrackPanel::GetVRulerWidth()
{
   return 30;
}

int TrackPanel::GetVRulerOffset()
{
   return GetTitleOffset() + GetTitleWidth();
}

int TrackPanel::GetLabelWidth()
{
   return GetTitleWidth() + GetVRulerWidth();
}

//
// AudacityTimer method
//

void AudacityTimer::Notify()
{
   parent->OnTimer();
}

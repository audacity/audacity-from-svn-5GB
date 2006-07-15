/**********************************************************************

  Audacity: A Digital Audio Editor

  ControlToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
  James Crook
 
*******************************************************************//**

\class ControlToolBar
\brief A ToolBar that has the main control buttons.

  This class, which is a child of Toolbar, creates the
  window containing the tool selection (ibeam, envelope,
  move, zoom), the rewind/play/stop/record/ff buttons, and
  the volume control. The window can be embedded within a
  normal project window, or within a ToolbarFrame that is
  managed by a global ToolBarStub called
  gControlToolBarStub.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform. 

*//*******************************************************************/

#include "Audacity.h"
#include "ControlToolBar.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/dcmemory.h>
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/brush.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/settings.h>
#endif

#include <wx/image.h>
#include <wx/tooltip.h>
#include <wx/msgdlg.h>
#include <wx/gbsizer.h>

#include "widgets/AButton.h"
#include "AudioIO.h"
#include "ImageManipulation.h"
#include "Prefs.h"
#include "Project.h"
#include "Track.h"

#include "AColor.h"
#include "MeterToolBar.h"
#include "Theme.h"
#include "AllThemeResources.h"

// #include "../images/ControlButtons.h"

//static
AudacityProject *ControlToolBar::mBusyProject = NULL;

////////////////////////////////////////////////////////////
/// Methods for ControlToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ControlToolBar, ToolBar)
   EVT_CHAR( ControlToolBar::OnKeyEvent )
   EVT_BUTTON( ID_PLAY_BUTTON,   ControlToolBar::OnPlay )
   EVT_BUTTON( ID_STOP_BUTTON,   ControlToolBar::OnStop )
   EVT_BUTTON( ID_RECORD_BUTTON, ControlToolBar::OnRecord )
   EVT_BUTTON( ID_BATCH_BUTTON,  ControlToolBar::OnBatch )
   EVT_BUTTON( ID_REW_BUTTON,    ControlToolBar::OnRewind )
   EVT_BUTTON( ID_FF_BUTTON,     ControlToolBar::OnFF )
   EVT_BUTTON( ID_PAUSE_BUTTON,  ControlToolBar::OnPause )
END_EVENT_TABLE()

//Standard constructor
ControlToolBar::ControlToolBar( wxWindow * parent ):
   ToolBar()
{
   mPaused = false;
//   mSizer = new wxBoxSizer( wxHORIZONTAL );
//   Add( mSizer, 1, wxEXPAND );
   mSizer = NULL;

   mCutPreviewTracks = NULL;

   gPrefs->Read( wxT("/GUI/ErgonomicTransportButtons"), &mErgonomicTransportButtons, true );
   gPrefs->Read( wxT("/Batch/CleanSpeechMode"), &mCleanSpeechMode, false );
   gPrefs->Read( wxT("/GUI/AlwaysEnablePause"), &mAlwaysEnablePause, false );

   InitToolBar( parent,
                ControlBarID,
               _("Audacity Control Toolbar"),
               _("Control") );
}

ControlToolBar::~ControlToolBar()
{
// As the buttons are placed on the window, we should not delete them.
// They will be deleted by wxWidgets.
#if 0
   delete mPause;
   delete mPlay;
   delete mStop;
   delete mRewind;
   delete mFF;
   delete mRecord;
   delete mBatch;
#endif
}

// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments
AButton *ControlToolBar::MakeButton(teBmps eFore, teBmps eDisabled,
                                    int id,
                                    bool processdownevents,
                                    const wxChar *label,
                                    const wxChar *tip)
{
   AButton *r = ToolBar::MakeButton(
      bmpRecoloredUpLarge, bmpRecoloredDownLarge, bmpRecoloredHiliteLarge,
      eFore, eDisabled,
      wxWindowID(id),
      wxDefaultPosition, processdownevents,
      theTheme.ImageSize( bmpRecoloredUpLarge ));
   r->SetLabel(label);
   r->SetFocusRect( r->GetRect().Deflate( 12, 12 ) );

#if wxUSE_TOOLTIPS
   r->SetToolTip(tip);
#endif

   return r;
}

void ControlToolBar::MakeLoopImage()
{
   // JKC: See ToolBar::MakeButton() for almost identical code.  Condense??

   wxSize Size1( theTheme.ImageSize( bmpRecoloredUpLarge ));
   wxSize Size2( theTheme.ImageSize( bmpLoop ));

   int xoff = (Size1.GetWidth()  - Size2.GetWidth())/2;
   int yoff = (Size1.GetHeight() - Size2.GetHeight())/2;

   wxImage * up2        = OverlayImage(bmpRecoloredUpLarge,     bmpLoop, xoff, yoff);
   wxImage * hilite2    = OverlayImage(bmpRecoloredHiliteLarge, bmpLoop, xoff, yoff);
   wxImage * down2      = OverlayImage(bmpRecoloredDownLarge,   bmpLoop, xoff + 1, yoff + 1);
   wxImage * disable2   = OverlayImage(bmpRecoloredUpLarge,     bmpLoopDisabled, xoff, yoff);

   mPlay->SetAlternateImages(up2, hilite2, down2, disable2);

   delete up2;
   delete hilite2;
   delete down2;
   delete disable2;
}

void ControlToolBar::Populate()
{
   MakeButtonBackgroundsLarge();

   mPause = MakeButton(bmpPause,bmpPauseDisabled,
      ID_PAUSE_BUTTON,  true,  _("Pause"), _("Pause"));

   mPlay = MakeButton( bmpPlay, bmpPlayDisabled, 
      ID_PLAY_BUTTON, false, _("Play"), _("Play (Shift for loop-play)"));

   MakeLoopImage();

   mStop = MakeButton( bmpStop, bmpStopDisabled ,
      ID_STOP_BUTTON, false, _("Stop"), _("Stop"));

   mRewind = MakeButton(bmpRewind, bmpRewindDisabled,
      ID_REW_BUTTON, false, _("Start"), _("Skip to Start"));

   mFF = MakeButton(bmpFFwd, bmpFFwdDisabled,
      ID_FF_BUTTON, false, _("End"), _("Skip to End"));

   mRecord = MakeButton(bmpRecord, bmpRecordDisabled,
      ID_RECORD_BUTTON, false, _("Record"), _("Record"));

   mBatch = MakeButton(bmpCleanSpeech,bmpCleanSpeechDisabled,
      ID_BATCH_BUTTON, false, _("Clean Speech"), _("Clean Speech"));

#if wxUSE_TOOLTIPS
#ifdef __WXMAC__
   wxToolTip::Enable(false);    // DM: tooltips are broken in wxMac
#else
// MB: Should make this a pref
   wxToolTip::Enable(true);     
   wxToolTip::SetDelay(1000);
#endif
#endif

   // Set default order and mode
   ArrangeButtons();
}

void ControlToolBar::UpdatePrefs()
{
#if 0
   gPrefs->Read(wxT("/GUI/AlwaysEnablePause"), &mAlwaysEnablePause, false);

   if(mAlwaysEnablePause)
      mPause->Enable();
   else if(!mAlwaysEnablePause && !gAudioIO->IsBusy())
   {
      mPause->PopUp();
      mPause->Disable();
      mPaused = false;
      gAudioIO->SetPaused(false);
   }

   gAudioIO->SetAlwaysEnablePause(mAlwaysEnablePause);
#endif

   bool updated = false;
   bool active;

   gPrefs->Read( wxT("/GUI/ErgonomicTransportButtons"), &active, true );
   if( mErgonomicTransportButtons != active )
   {
      mErgonomicTransportButtons = active;
      updated = true;
   }

   gPrefs->Read( wxT("/Batch/CleanSpeechMode"), &active, false );
   if( mCleanSpeechMode != active )
   {
      mCleanSpeechMode = active;
      updated = true;
   }

   if( updated )
   {
      ArrangeButtons();
      Updated();
   }
}

void ControlToolBar::ArrangeButtons()
{
   int flags = wxALIGN_CENTER | wxRIGHT;

   // (Re)allocate the button sizer
   if( mSizer )
   {
      Detach( mSizer );
      delete mSizer;
   }
   mSizer = new wxBoxSizer( wxHORIZONTAL );
   Add( mSizer, 1, wxEXPAND );

   // Start with a little extra space
   mSizer->Add( 5, 55 );

   // Add the buttons in order based on ergonomic setting
   if( mErgonomicTransportButtons )
   {
      mPause->MoveBeforeInTabOrder( mRecord );
      mPlay->MoveBeforeInTabOrder( mRecord );
      mStop->MoveBeforeInTabOrder( mRecord );
      mRewind->MoveBeforeInTabOrder( mRecord );
      mFF->MoveBeforeInTabOrder( mRecord );

      mSizer->Add( mPause,  0, flags, 2 );
      mSizer->Add( mPlay,   0, flags, 2 );
      mSizer->Add( mStop,   0, flags, 2 );
      mSizer->Add( mRewind, 0, flags, 2 );
      mSizer->Add( mFF,     0, flags, 10 );
      mSizer->Add( mRecord, 0, flags, 5 );
   }
   else
   {
      mRewind->MoveBeforeInTabOrder( mFF );
      mPlay->MoveBeforeInTabOrder( mFF );
      mRecord->MoveBeforeInTabOrder( mFF );
      mPause->MoveBeforeInTabOrder( mFF );
      mStop->MoveBeforeInTabOrder( mFF );

      mSizer->Add( mRewind, 0, flags, 2 );
      mSizer->Add( mPlay,   0, flags, 2 );
      mSizer->Add( mRecord, 0, flags, 2 );
      mSizer->Add( mPause,  0, flags, 2 );
      mSizer->Add( mStop,   0, flags, 2 );
      mSizer->Add( mFF,     0, flags, 5 );
   }

   // Add and possible hide the CleanSpeech button
   mSizer->Add( mBatch,  0, flags | wxLEFT, 5 );
   mSizer->Show( mBatch, mCleanSpeechMode );

   // Layout the sizer
   mSizer->Layout();

   // Layout the toolbar
   Layout();

   // Resize the toolbar to fit the contents
   Fit();

   // And make that size the minimum
   SetMinSize( GetSizer()->GetMinSize() );
}

void ControlToolBar::ReCreateButtons()
{
   // Get rid of mSizer here in case getting rid of 
   // Toolbars mHSizer would try to but leave mSizer non-NULL
   // We do not need to delete the buttons here as they belong
   // to the window.
   Detach( mSizer );
   delete mSizer;
   mSizer = NULL;
//   mSizer = new wxBoxSizer( wxHORIZONTAL );
//   SetSizer( mSizer );
   ToolBar::ReCreateButtons();
}

void ControlToolBar::Repaint( wxDC *dc )
{
#ifndef USE_AQUA_THEME
   wxSize s = mSizer->GetSize();
   wxPoint p = mSizer->GetPosition();

   wxRect bevelRect( p.x, p.y, s.GetWidth() - 1, s.GetHeight() - 1 );
   AColor::Bevel( *dc, true, bevelRect );
#endif
}

void ControlToolBar::EnableDisableButtons()
{
   //TIDY-ME: Button logic could be neater.
   AudacityProject *p = GetActiveProject();
   size_t numProjects = gAudacityProjects.Count();
   bool tracks = false;
   bool cleaningSpeech = mBatch->IsDown();
   bool busy = gAudioIO->IsBusy();
   bool recording = mRecord->IsDown();

   // Only interested in audio type tracks
   if( p )
   {
      TrackListIterator iter( p->GetTracks() );

      for( Track *t = iter.First(); t; t = iter.Next() )
      {
         if( t->GetKind() == Track::Wave )
         {
            tracks = true;
            break;
         }
      }
   }

   //mPlay->SetEnabled(tracks && !busy);
   mPlay->SetEnabled(tracks && !recording && !cleaningSpeech);
   
   if (p && GetActiveProject()->GetCleanSpeechMode())
   {
       bool canRecord = !tracks;
        canRecord &= !cleaningSpeech;
       canRecord &= !busy;
       canRecord &= ((numProjects == 0) || ((numProjects == 1) && !tracks));
       mRecord->SetEnabled(canRecord);
       mBatch->SetEnabled(!busy && !recording);
   }
   
   mStop->SetEnabled(busy && !cleaningSpeech);
   mRewind->SetEnabled(tracks && !busy);
   mFF->SetEnabled(tracks && !busy);
}

void ControlToolBar::SetPlay(bool down)
{
   if (down)
      mPlay->PushDown();
   else {
      mPlay->PopUp();
      mPlay->SetAlternate(false);
   }
}

void ControlToolBar::SetStop(bool down)
{
   if (down)
      mStop->PushDown();
   else {
      if(FindFocus() == mStop)
         mPlay->SetFocus();
      mStop->PopUp();
      mStop->Disable();
      mBatch->Enable();
      mRecord->Enable();
      mPlay->Enable();
      if(!mAlwaysEnablePause)
         mPause->Disable();
      mRewind->Enable();
      mFF->Enable();
   }
}

void ControlToolBar::SetRecord(bool down)
{
   if (down)
      mRecord->PushDown();
   else
      mRecord->PopUp();
}

void ControlToolBar::PlayPlayRegion(double t0, double t1,
                                    bool looped /* = false */,
                                    bool cutpreview /* = false */)
{
   if (gAudioIO->IsBusy()) {
      mPlay->PopUp();
      return;
   }
   
   if (cutpreview && t0==t1)
      return; /* msmeyer: makes no sense */
   
   mStop->Enable();
   mRewind->Disable();
   mBatch->Disable();
   mRecord->Disable();
   mFF->Disable();
   mPause->Enable();
   
   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();
      double maxofmins,minofmaxs;
      
      // JS: clarified how the final play region is computed;
      if (t1 == t0) {
         // msmeyer: When playing looped, we play the whole file, if
         // no range is selected. Otherwise, we play from t0 to end
         if (looped) {
            // msmeyer: always play from start
            t0 = t->GetStartTime();
         } else {
            // move t0 to valid range
            if (t0 < 0)
               t0 = t->GetStartTime();
            if (t0 > t->GetEndTime())
               t0 = t->GetEndTime();
         }
         
         // always play to end
         t1 = t->GetEndTime();
      }
      else {
         // always t0 < t1 right?

         // the set intersection between the play region and the
         // valid range maximum of lower bounds
         if (t0 < t->GetStartTime())
            maxofmins = t->GetStartTime();
         else
            maxofmins = t0;
         
         // minimum of upper bounds
         if (t1 > t->GetEndTime())
            minofmaxs = t->GetEndTime();
         else
            minofmaxs = t1;

         // we test if the intersection has no volume 
         if (minofmaxs <= maxofmins) {
            // no volume; play nothing
            return;
         }
         else {
            t0 = maxofmins;
            t1 = minofmaxs;
         }
      }
      
      bool success = false;
      if (t1 > t0) {
         int token;
         if (cutpreview) {
            double beforeLen, afterLen;
            gPrefs->Read(wxT("/AudioIO/CutPreviewBeforeLen"), &beforeLen, 1.0);
            gPrefs->Read(wxT("/AudioIO/CutPreviewAfterLen"), &afterLen, 1.0);
            double tcp0 = t0-beforeLen;
            double tcp1 = (t1+afterLen) - (t1-t0);
            SetupCutPreviewTracks(tcp0, t0, t1, tcp1);
            token = gAudioIO->StartStream(
                mCutPreviewTracks->GetWaveTrackArray(false),
                WaveTrackArray(), NULL, p->GetRate(), tcp0, tcp1, false,
                t0, t1-t0);
         } else {
            token = gAudioIO->StartStream(t->GetWaveTrackArray(false),
                                  WaveTrackArray(), t->GetTimeTrack(),
                                  p->GetRate(), t0, t1, looped);
         }
         if (token != 0) {
            success = true;
            p->SetAudioIOToken(token);
            mBusyProject = p;
            SetVUMeters(p);
         }
         else {
            // msmeyer: Show error message if stream could not be opened
            wxMessageBox(_(
               "Error while opening sound device. "
               wxT("Please check the output device settings and the project sample rate.")),
               _("Error"), wxOK | wxICON_EXCLAMATION, this);
         }
      }

      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   }
}

void ControlToolBar::SetVUMeters(AudacityProject *p)
{
   MeterToolBar *bar;
   bar = p->GetMeterToolBar();
   if (bar) {
      Meter *play, *record;
      bar->GetMeters(&play, &record);
      gAudioIO->SetMeters(record, play);
   }
}

void ControlToolBar::PlayCurrentRegion(bool looped /* = false */,
                                       bool cutpreview /* = false */)
{
   mPlay->SetAlternate(looped);

   AudacityProject *p = GetActiveProject();
   if (p)
   {
      if (looped)
         p->mLastPlayMode = loopedPlay;
      else
         p->mLastPlayMode = normalPlay;

      double playRegionStart, playRegionEnd;
      p->GetPlayRegion(&playRegionStart, &playRegionEnd);
         
      if (playRegionStart >= 0)
         PlayPlayRegion(playRegionStart,
                        playRegionEnd,
                        looped, cutpreview);
   }
}

void ControlToolBar::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown() || event.AltDown()) {
      event.Skip();
      return;
   }

   if (event.GetKeyCode() == WXK_SPACE) {
      if (gAudioIO->IsStreamActive(GetActiveProject()->GetAudioIOToken())) {
         SetPlay(false);
         SetStop(true);
         StopPlaying();
      }
      else if (!gAudioIO->IsBusy()) {
         SetPlay(true);
         SetStop(false);
         PlayCurrentRegion();
      }
      return;
   }
   event.Skip();
}


void ControlToolBar::OnShiftDown(wxKeyEvent & event)
{
   // Turn the "Play" button into a "Loop" button
   if (!mPlay->IsDown())
      mPlay->SetAlternate(true);
}

void ControlToolBar::OnShiftUp(wxKeyEvent & event)
{
   // Turn the "Loop" button into a "Play" button
   if (!mPlay->IsDown())
      mPlay->SetAlternate(false);
}

void ControlToolBar::OnPlay(wxCommandEvent &evt)
{
   PlayDefault();
}

void ControlToolBar::OnStop(wxCommandEvent &evt)
{
   StopPlaying();
}

void ControlToolBar::PlayDefault()
{
   if(mPlay->WasControlDown())
      PlayCurrentRegion(false, true); /* play with cut preview */
   else if(mPlay->WasShiftDown())
      PlayCurrentRegion(true); /* play looped */
   else
      PlayCurrentRegion(false); /* play normal */
}

void ControlToolBar::StopPlaying()
{
   mStop->PushDown();

   SetStop(false);
   gAudioIO->StopStream();
   SetPlay(false);
   SetRecord(false);

   mPause->PopUp();
   mPaused=false;
   //Make sure you tell gAudioIO to unpause
   gAudioIO->SetPaused(mPaused);
   
   ClearCutPreviewTracks();

   mBusyProject = NULL;
}

void ControlToolBar::OnBatch(wxCommandEvent &evt)
{
   AudacityProject *proj = GetActiveProject();
   proj->OnBatch();

   mPlay->Enable();
   mStop->Enable();
   mRewind->Enable();
   mFF->Enable();
   mPause->Disable();
   mBatch->Enable();
   mBatch->PopUp();
}

void ControlToolBar::OnRecord(wxCommandEvent &evt)
{
   if (gAudioIO->IsBusy()) {
      mRecord->PopUp();
      return;
   }
   AudacityProject *p = GetActiveProject();
   if( p && p->GetCleanSpeechMode() )
   {
      size_t numProjects = gAudacityProjects.Count();
      bool tracks = (p && !p->GetTracks()->IsEmpty());
      if (tracks || (numProjects > 1)) {
         wxMessageBox(_("CleanSpeech only allows recording mono track.\n"
            wxT("Recording not possible when more than one window open.")),
            _("Recording not permitted"),
            wxOK | wxICON_INFORMATION,
            this);
         mRecord->PopUp();
         mRecord->Disable();
         return;
      }
   }
   mPlay->Disable();
   mStop->Enable();
   mRewind->Disable();
   mFF->Disable();
   mPause->Enable();
   mBatch->Enable();

   mRecord->PushDown();

   if (p) 
   {
      TrackList *t = p->GetTracks();
      double t0 = p->GetSel0();
      double t1 = p->GetSel1();
      if (t1 == t0)
         t1 = 1000000000.0;     // record for a long, long time (tens of years)

      /* TODO: set up stereo tracks if that is how the user has set up
       * their preferences, and choose sample format based on prefs */
      WaveTrackArray newRecordingTracks, playbackTracks;

      bool duplex;
      gPrefs->Read(wxT("/AudioIO/Duplex"), &duplex, true);
      int recordingChannels = gPrefs->Read(wxT("/AudioIO/RecordChannels"), 1);

      if( duplex )
         playbackTracks = t->GetWaveTrackArray(false);
      else
         playbackTracks = WaveTrackArray();

      for( int c = 0; c < recordingChannels; c++ )
      {
         WaveTrack *newTrack = p->GetTrackFactory()->NewWaveTrack();
         int initialheight = newTrack->GetHeight();
         newTrack->SetOffset(t0);
         newTrack->SetRate((int) p->GetRate());
         newTrack->SetHeight(initialheight /  recordingChannels);
         if( recordingChannels == 2 )
         {
            if( c == 0 )
            {
               newTrack->SetChannel(Track::LeftChannel);
               newTrack->SetLinked(true);
            }
            else
            {
               newTrack->SetChannel(Track::RightChannel);
               newTrack->SetTeamed(true);
            }
         }
         else
         {
            newTrack->SetChannel( Track::MonoChannel );
         }

         newRecordingTracks.Add(newTrack);
      }

      int token = gAudioIO->StartStream(playbackTracks,
                                        newRecordingTracks, t->GetTimeTrack(),
                                        p->GetRate(), t0, t1);

      bool success = (token != 0);
      for( unsigned int i = 0; i < newRecordingTracks.GetCount(); i++ )
         if (success)
            t->Add(newRecordingTracks[i]);
         else
            delete newRecordingTracks[i];

      if (success) {
         p->SetAudioIOToken(token);
         mBusyProject = p;
         SetVUMeters(p);
      }
      else {
         // msmeyer: Show error message if stream could not be opened
         wxMessageBox(_("Error while opening sound device. "
            wxT("Please check the input device settings and the project sample rate.")),
                      _("Error"), wxOK | wxICON_EXCLAMATION, this);

         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   }
}


void ControlToolBar::OnPause(wxCommandEvent &evt)
{ 
   if(mPaused)
   {
      mPause->PopUp();
      mPaused=false;
   }
   else
   {       
      mPause->PushDown();
      mPaused=true;
   }
   
   gAudioIO->SetPaused(mPaused);
}

void ControlToolBar::OnRewind(wxCommandEvent &evt)
{
   mRewind->PushDown();
   mRewind->PopUp();

   AudacityProject *p = GetActiveProject();
   if (p) {
      p->Rewind(mRewind->WasShiftDown());
   }
}

void ControlToolBar::OnFF(wxCommandEvent &evt)
{
   mFF->PushDown();
   mFF->PopUp();

   AudacityProject *p = GetActiveProject();

   if (p) {
      p->SkipEnd(mFF->WasShiftDown());
   }
}

void ControlToolBar::SetupCutPreviewTracks(double playStart, double cutStart,
                                           double cutEnd, double playEnd)
{
   ClearCutPreviewTracks();
   AudacityProject *p = GetActiveProject();
   if (p) {
      // Find first selected track (stereo or mono) and duplicate it
      Track *track1 = NULL, *track2 = NULL;
      TrackListIterator it(p->GetTracks());
      for (Track *t = it.First(); t; t = it.Next())
      {
         if (t->GetKind() == Track::Wave && t->GetSelected())
         {
            track1 = t;
            track2 = p->GetTracks()->GetLink(track1);
            break;
         }
      }
      
      if (track1)
      {
         // Duplicate and change tracks
         track1 = track1->Duplicate();
         track1->Clear(cutStart, cutEnd);
         if (track2)
         {
            track2 = track2->Duplicate();
            track2->Clear(cutStart, cutEnd);
         }
            
         mCutPreviewTracks = new TrackList();
         mCutPreviewTracks->Add(track1);
         if (track2)
            mCutPreviewTracks->Add(track2);
      }
   }
}

void ControlToolBar::ClearCutPreviewTracks()
{
   if (mCutPreviewTracks)
   {
      mCutPreviewTracks->Clear(true); /* delete track contents too */
      delete mCutPreviewTracks;
      mCutPreviewTracks = NULL;
   }
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: ebfdc42a-6a03-4826-afa2-937a48c0565b

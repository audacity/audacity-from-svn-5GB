/**********************************************************************

  Audacity: A Digital Audio Editor

  ControlToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
 
  See ControlToolBar.h for details

**********************************************************************/

#include "ControlToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

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

// For pow() in GetSoundVol()
#include <math.h>

#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "AudioIO.h"
#include "ImageManipulation.h"
#include "Prefs.h"
#include "Project.h"
#include "Track.h"

#include "../images/ControlButtons.h"

enum {
   ID_SELECT,
   ID_ENVELOPE,
   ID_SLIDE,
   ID_ZOOM,
   ID_DRAW,
   ID_PLAY_BUTTON,
   ID_RECORD_BUTTON,
   ID_PAUSE_BUTTON,
   ID_STOP_BUTTON,
   ID_FF_BUTTON,
   ID_REW_BUTTON,

   ID_FIRST_TOOL = ID_SELECT,
   ID_LAST_TOOL = ID_DRAW
};

const int BUTTON_WIDTH = 50;

//static
AudacityProject *ControlToolBar::mBusyProject = NULL;

////////////////////////////////////////////////////////////
/// Methods for ControlToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ControlToolBar, wxWindow)
   EVT_PAINT(ControlToolBar::OnPaint)
   EVT_CHAR(ControlToolBar::OnKeyEvent)
   EVT_COMMAND_RANGE(ID_FIRST_TOOL, ID_LAST_TOOL,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnTool)
   EVT_COMMAND(ID_PLAY_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnPlay)
   EVT_COMMAND(ID_STOP_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnStop)
   EVT_COMMAND(ID_RECORD_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnRecord)
   EVT_COMMAND(ID_REW_BUTTON,
            wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnRewind)
   EVT_COMMAND(ID_FF_BUTTON,
            wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnFF)
   EVT_COMMAND(ID_PAUSE_BUTTON,
               wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnPause)
END_EVENT_TABLE()

//Standard contructor
ControlToolBar::ControlToolBar(wxWindow * parent):
ToolBar(parent, -1, wxPoint(1, 1), wxSize(400, 55))
{
   InitializeControlToolBar();
}

//Another constructor
ControlToolBar::ControlToolBar(wxWindow * parent, wxWindowID id,
                               const wxPoint & pos,
                               const wxSize & size):ToolBar(parent, id,
                                                            pos, size)
{
   InitializeControlToolBar();
}


// This sets up the ControlToolBar, initializing all the important values
// and creating the buttons.
void ControlToolBar::InitializeControlToolBar()
{
   mIdealSize = wxSize(400, 55);
   mTitle = _("Audacity Control Toolbar");
   mType = ControlToolBarID;

   wxColour backgroundColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour origColour(204, 204, 204);

   MakeButtons();

   wxImage *sliderOriginal = new wxImage(wxBitmap(Slider).ConvertToImage());
   wxImage *thumbOriginal = new wxImage(wxBitmap(SliderThumb).ConvertToImage());

#ifdef __WXMAC__
   wxImage *sliderNew = sliderOriginal;
   wxImage *thumbNew = thumbOriginal;
#else
   wxImage *sliderNew = ChangeImageColour(sliderOriginal,
                                          backgroundColour);
   wxImage *thumbNew = ChangeImageColour(thumbOriginal,
                                         backgroundColour);
#endif

   delete sliderOriginal;
   delete thumbOriginal;
#ifndef __WXMAC__
   delete sliderNew;
   delete thumbNew;
#endif

   mCurrentTool = 0;
   mTool[0]->PushDown();

   gPrefs->Read("/GUI/AlwaysEnablePause", &mAlwaysEnablePause, false);


   mPaused=false;             //Turn the paused state to off
#if 0
   if(!mAlwaysEnablePause)
      mPause->Disable();         //Turn the pause button off.
   gAudioIO->SetAlwaysEnablePause(mAlwaysEnablePause);
#endif

#if 0
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   mDivBitmap = new wxBitmap((const char **) Div);
   mMuteBitmap = new wxBitmap((const char **) Mute);
   mLoudBitmap = new wxBitmap((const char **) Loud);
#endif
#endif
}


wxImage *ControlToolBar::MakeToolImage(wxImage * tool,
                                       wxImage * mask, int style)
{
   // This code takes the image of a tool, and its mask,
   // and creates one of four images of this tool inside
   // a little button, for the toolbar.  The tool
   // is alpha-blended onto the background.

   const char **src;

   switch(style) {
   case 1: // hilite
      src = Hilite;
      break;
   case 2: // down
      src = Down;
      break;
   default:
      src = Up;
      break;
   }

   wxImage *bkgndOriginal = new wxImage(wxBitmap(src).ConvertToImage());
   wxImage *upOriginal = new wxImage(wxBitmap(Up).ConvertToImage());

#ifdef __WXMAC__
   wxImage *background = bkgndOriginal;
#else
   wxColour backgroundColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour baseColour;
   unsigned char *data = upOriginal->GetData();
   baseColour.Set(data[28 * 3], data[28 * 3 + 1], data[28 * 3 + 2]);
   wxImage *background = ChangeImageColour(bkgndOriginal,
                                           baseColour,
                                           backgroundColour);
#endif

   // 
   // Overlay the tool on top of it
   //

   wxImage *result;
   if (style == 2)              // down
      result = OverlayImage(background, tool, mask, 1, 1);
   else
      result = OverlayImage(background, tool, mask, 0, 0);
   delete background;

   #ifndef __WXMAC__
   delete bkgndOriginal;
   delete upOriginal;
   #endif

   return result;
}

AButton *ControlToolBar::MakeTool(const char **tool, const char **alpha,
                                  wxWindowID id, int left, int top)
{
   wxImage *ctr = new wxImage(wxBitmap(tool).ConvertToImage());
   wxImage *mask = new wxImage(wxBitmap(alpha).ConvertToImage());
   wxImage *up = MakeToolImage(ctr, mask, 0);
   wxImage *hilite = MakeToolImage(ctr, mask, 1);
   wxImage *down = MakeToolImage(ctr, mask, 2);
   wxImage *dis = MakeToolImage(ctr, mask, 3);

   AButton *button =
       new AButton(this, id, wxPoint(left, top), wxSize(27, 27),
                   up, hilite, down, dis, false);

   delete ctr;
   delete mask;
   delete up;
   delete hilite;
   delete down;
   delete dis;

   return button;
}


// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments
AButton *ControlToolBar::MakeButton(char const **foreground,
                                    char const **disabled,
                                    char const **alpha, int id, bool processdownevents)
{

   // Windows (TM) has a little extra room for some reason, so the top of the
   // buttons should be a little lower.
   int buttonTop = 4;
#ifdef __WXMSW__
   buttonTop=4;
#endif


   AButton *r = ToolBar::MakeButton(upPattern, downPattern, hilitePattern,
                                    foreground, disabled, alpha, wxWindowID(id),
                                    wxPoint(mButtonPos,buttonTop), processdownevents,
                                    wxSize(48, 48), 16, 16);
   mButtonPos += BUTTON_WIDTH;
   return r;
}


void ControlToolBar::MakeButtons()
{
   wxImage *upOriginal = new wxImage(wxBitmap(UpButton).ConvertToImage());
   wxImage *downOriginal = new wxImage(wxBitmap(DownButton).ConvertToImage());
   wxImage *hiliteOriginal = new wxImage(wxBitmap(HiliteButton).ConvertToImage());

   wxColour newColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);

#ifdef __WXMAC__
   upPattern = upOriginal;
   downPattern = downOriginal;
   hilitePattern = hiliteOriginal;
#else
   upPattern = ChangeImageColour(upOriginal, newColour);
   downPattern = ChangeImageColour(downOriginal, newColour);
   hilitePattern = ChangeImageColour(hiliteOriginal, newColour);
#endif

   /* Buttons */

   mButtonPos = 92;

   mRewind = MakeButton((char const **) Rewind,
                        (char const **) RewindDisabled,
                        (char const **) RewindAlpha, ID_REW_BUTTON,
                        false);
   mRewind->SetToolTip(_("Skip to Start"));

   mPlay = MakeButton((char const **) Play,
                      (char const **) PlayDisabled,
                      (char const **) PlayAlpha, ID_PLAY_BUTTON,
                      false);
   mPlay->SetToolTip(_("Play"));



   mRecord = MakeButton((char const **) Record,
                        (char const **) RecordDisabled,
                        (char const **) RecordAlpha, ID_RECORD_BUTTON,
                        false);
   mRecord->SetToolTip(_("Record"));

   mPause = MakeButton((char const **)Pause,
                      (char const **) PauseDisabled,
                      (char const **) PauseAlpha, ID_PAUSE_BUTTON,
                       true);
   mPause->SetToolTip(_("Pause"));
   


   mStop = MakeButton((char const **) Stop,
                      (char const **) StopDisabled,
                      (char const **) StopAlpha, ID_STOP_BUTTON,
                      false);
   mStop->SetToolTip(_("Stop"));

   mFF = MakeButton((char const **) FFwd,
                    (char const **) FFwdDisabled,
                    (char const **) FFwdAlpha, ID_FF_BUTTON,
                    false);
   mFF->SetToolTip(_("Skip to End"));

#ifndef __WXMAC__
   delete upPattern;
   delete downPattern;
   delete hilitePattern;
#endif

   delete upOriginal;
   delete downOriginal;
   delete hiliteOriginal;

   /* Tools */

   mTool[0] = MakeTool(IBeam, IBeamAlpha, ID_SELECT, 0, 0);
   mTool[0]->SetToolTip(_("Selection Tool"));

   mTool[1] = MakeTool(Envelope, EnvelopeAlpha, ID_ENVELOPE, 28, 0);
   mTool[1]->SetToolTip(_("Envelope Tool"));

   mTool[2] = MakeTool(TimeShift, TimeShiftAlpha, ID_SLIDE, 0, 28);
   mTool[2]->SetToolTip(_("Time Shift Tool"));

   mTool[3] = MakeTool(Zoom, ZoomAlpha, ID_ZOOM, 28, 28);
   mTool[3]->SetToolTip(_("Zoom Tool"));

   mTool[4] = MakeTool(Draw, DrawAlpha, ID_DRAW, 56, 0);
   mTool[4]->SetToolTip(_("Draw Tool"));

#ifdef __WXMAC__
   wxToolTip::Enable(false);    // DM: tooltips are broken in wxMac
#else
   wxToolTip::Enable(true);     // MB: Should make this a pref
   wxToolTip::SetDelay(1000);
#endif
}

ControlToolBar::~ControlToolBar()
{
   for (int i = 0; i < 5; i++)
      delete mTool[i];

   delete mRewind;
   delete mPlay;
   delete mStop;
   delete mRecord;
   delete mFF;
   delete mPause;

   //delete mVolume;

#if 0
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   delete mDivBitmap;
   delete mMuteBitmap;
   delete mLoudBitmap;
#endif
#endif
}

void ControlToolBar::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown() || event.AltDown()) {
      event.Skip();
      return;
   }

   wxCommandEvent dummyEvent;

   if (event.KeyCode() == WXK_SPACE) {
      if (gAudioIO->IsStreamActive(GetActiveProject()->GetAudioIOToken())) {
         SetPlay(false);
         SetStop(true);
         OnStop(dummyEvent);
      } else {
         SetPlay(true);
         SetStop(false);
         OnPlay(dummyEvent);
      }
      return;
   }
   event.Skip();
}


void ControlToolBar::UpdatePrefs()
{
#if 0
   gPrefs->Read("/GUI/AlwaysEnablePause", &mAlwaysEnablePause, false);

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
}

int ControlToolBar::GetCurrentTool()
{
   return mCurrentTool;
}

void ControlToolBar::SetPlay(bool down)
{
   if (down)
      mPlay->PushDown();
   else
      mPlay->PopUp();
}

void ControlToolBar::SetStop(bool down)
{
   if (down)
      mStop->PushDown();
   else {
      mStop->PopUp();
      mStop->Disable();
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


void ControlToolBar::OnPlay(wxCommandEvent &evt)
{
   if (gAudioIO->IsStreamActive())
      return;

   mStop->Enable();
   mRewind->Disable();
   mRecord->Disable();
   mFF->Disable();
   mPause->Enable();

   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();
      double t0 = p->GetSel0();
      double t1 = p->GetSel1();

      if (t1 == t0 || t1 > t->GetEndTime())
         t1 = t->GetEndTime();
      if (t0 > t->GetEndTime())
         t0 = t->GetEndTime();

      bool success = false;
      if (t1 > t0)
      {
         int token =
            gAudioIO->StartStream(t->GetWaveTrackArray(false),
                                  WaveTrackArray(), t->GetTimeTrack(),
                                  p->GetRate(), t0, t1);
         if (token != 0)
         {
            success = true;
            p->SetAudioIOToken(token);
            mBusyProject = p;
         }
      }

      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   }
}

void ControlToolBar::OnStop(wxCommandEvent &evt)
{
   SetStop(false);
   gAudioIO->StopStream();
   SetPlay(false);
   SetRecord(false);

   mPause->PopUp();
   mPaused=false;
   //Make sure you tell gAudioIO to unpause
   gAudioIO->SetPaused(mPaused);

   mBusyProject = NULL;
}

void ControlToolBar::OnRecord(wxCommandEvent &evt)
{
   if (gAudioIO->IsStreamActive())
      return;

   mPlay->Disable();
   mStop->Enable();
   mRewind->Disable();
   mFF->Disable();
   mPause->Enable();

   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();
      double t0 = p->GetSel0();
      double t1 = p->GetSel1();
      if (t1 == t0)
         t1 = 1000000000.0;     // record for a long, long time (tens of years)

      /* TODO: set up stereo tracks if that is how the user has set up
       * their preferences, and choose sample format based on prefs */
      WaveTrackArray newRecordingTracks, playbackTracks;

      bool duplex;
      gPrefs->Read("/AudioIO/Duplex", &duplex, true);
      int recordingChannels = gPrefs->Read("/AudioIO/RecordChannels", 1);

      if( duplex )
         playbackTracks = t->GetWaveTrackArray(false);
      else
         playbackTracks = WaveTrackArray();

      for( int c = 0; c < recordingChannels; c++ )
      {
         WaveTrack *newTrack = p->GetTrackFactory()->NewWaveTrack();
         newTrack->SetOffset(t0);
         newTrack->SetRate(p->GetRate());
         if( recordingChannels == 2 )
         {
            if( c == 0 )
            {
               newTrack->SetChannel(Track::LeftChannel);
               newTrack->SetLinked(true);
            }
            else
               newTrack->SetChannel(Track::RightChannel);
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
      }
      else {
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
      mPaused=false;
   }
   else
   {       
      mPaused=true;
   }
   
   gAudioIO->SetPaused(mPaused);
}

void ControlToolBar::OnRewind(wxCommandEvent &evt)
{
   mRewind->PopUp();

   if (gAudioIO->IsStreamActive(GetActiveProject()->GetAudioIOToken()))
      OnStop(evt);

   AudacityProject *p = GetActiveProject();
   if (p)
      p->Rewind(mRewind->WasShiftDown());
}

void ControlToolBar::OnFF(wxCommandEvent &evt)
{
   mFF->PopUp();

   if (gAudioIO->IsStreamActive(GetActiveProject()->GetAudioIOToken()))
      OnStop(evt);

   AudacityProject *p = GetActiveProject();
   if (p)
      p->SkipEnd(mFF->WasShiftDown());
}

float ControlToolBar::GetSoundVol()
{
   return 1.0; //return mVolume->Get();
}

void ControlToolBar::OnTool(wxCommandEvent & evt)
{
   int prev = mCurrentTool;
   mCurrentTool = evt.GetId() - ID_FIRST_TOOL;

   for (int i = 0; i < 5; i++)
      if (i == mCurrentTool)
         mTool[i]->PushDown();
      else
         mTool[i]->PopUp();

   if (mCurrentTool == envelopeTool || prev == envelopeTool)
      RedrawAllProjects();
}

void ControlToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

   DrawBackground(dc, width, height);

   dc.SetPen(*wxBLACK_PEN);
   dc.DrawLine(27, 0, 27, height);
   dc.DrawLine(55, 0, 55, height);
   dc.DrawLine(0, 27, 83, 27);
#ifdef __WXMAC__
   dc.DrawLine(83, 0, 83, 27);
#else
   dc.DrawLine(83, 0, 83, height);
#endif

}

void ControlToolBar::EnableDisableButtons()
{
   AudacityProject *p = GetActiveProject();

   bool tracks = (p && !p->GetTracks()->IsEmpty());
   bool busy = gAudioIO->IsStreamActive();

#if 0
   if (tracks) {
      if (!busy)
         mPlay->Enable();
   } else mPlay->Disable();
#endif

   mPlay->SetEnabled(tracks && !busy);
   mStop->SetEnabled(busy);
   mRewind->SetEnabled(tracks && !busy);
   mFF->SetEnabled(tracks && !busy);
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  TranscriptionToolBar.cpp

  Shane T. Mueller

**********************************************************************/

#include "TranscriptionToolBar.h"

#include "widgets/AButton.h"
#include "widgets/ASlider.h"

#include "AudioIO.h"
#include "ImageManipulation.h"
#include "LabelTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "WaveTrack.h"
#include "TimeTrack.h"
#include "VoiceKey.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#include <wx/brush.h>
#include <wx/intl.h>
#include <wx/choice.h>
#endif

#include <wx/image.h>

#include <cmath>
#include <iostream>

#include "../images/TranscriptionButtons.h"
#include "../images/ControlButtons/Play.xpm"
#include "../images/ControlButtons/PlayAlpha.xpm"
#include "../images/ControlButtons/PlayDisabled.xpm"

const int BUTTON_WIDTH = 27;
const int SEPARATOR_WIDTH = 14;

using std::cout;
using std::flush;
using std::endl;
///////////////////////////////////////////
///  Methods for TranscriptionToolBar
///////////////////////////////////////////
 

BEGIN_EVENT_TABLE(TranscriptionToolBar, wxWindow)
   EVT_PAINT(TranscriptionToolBar::OnPaint)
   EVT_CHAR(TranscriptionToolBar::OnKeyEvent)
   EVT_COMMAND_RANGE(TTB_PlaySpeed, TTB_PlaySpeed,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnPlaySpeed)
   EVT_SLIDER(TTB_PlaySpeedSlider, TranscriptionToolBar::OnSpeedSlider)
   EVT_COMMAND_RANGE(TTB_StartOn, TTB_StartOn,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnStartOn)
   EVT_COMMAND_RANGE(TTB_StartOff, TTB_StartOff,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnStartOff)
   EVT_COMMAND_RANGE(TTB_EndOn, TTB_EndOn,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnEndOn)
   EVT_COMMAND_RANGE(TTB_EndOff, TTB_EndOff,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnEndOff)
   EVT_COMMAND_RANGE(TTB_SelectSound, TTB_SelectSound,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnSelectSound)
   EVT_COMMAND_RANGE(TTB_SelectSilence, TTB_SelectSilence,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnSelectSilence)
   EVT_COMMAND_RANGE(TTB_AutomateSelection, TTB_AutomateSelection,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnAutomateSelection)
   EVT_COMMAND_RANGE(TTB_MakeLabel, TTB_MakeLabel,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnMakeLabel)
   EVT_COMMAND_RANGE(TTB_Calibrate, TTB_Calibrate,
                     wxEVT_COMMAND_BUTTON_CLICKED, TranscriptionToolBar::OnCalibrate)
   EVT_SLIDER(TTB_SensitivitySlider, TranscriptionToolBar::OnSensitivitySlider)

   EVT_CHOICE(TTB_KeyType, TranscriptionToolBar::SetKeyType)
END_EVENT_TABLE()
   ;   //semicolon enforces  proper automatic indenting in emacs.

////Standard Constructor
TranscriptionToolBar::TranscriptionToolBar(wxWindow * parent):
      ToolBar(parent, -1, wxPoint(1,1), wxSize(377,27),gTranscriptionToolBarStub)
{
   InitializeTranscriptionToolBar();
}

//Full-service Constructor
TranscriptionToolBar::TranscriptionToolBar(wxWindow * parent, wxWindowID id,
					   const wxPoint & pos,
					   const wxSize & size):
   ToolBar(parent, id, pos, size,gTranscriptionToolBarStub)
{

   InitializeTranscriptionToolBar();
}

void TranscriptionToolBar::InitializeTranscriptionToolBar()
{
   mTitle = _("Audacity Transcription Toolbar");
   SetLabel(_("Transcription"));
   mType = TranscriptionToolBarID;
   
   mIdealSize = wxSize(550, 27);
   
   
   vk = new VoiceKey();
   mBackgroundBrush.SetColour(wxColour(204, 204, 204));
   mBackgroundPen.SetColour(wxColour(204, 204, 204));
   
   mBackgroundBitmap = NULL;
   mBackgroundHeight = 0;
   mBackgroundWidth = 0;
 
   MakeButtons();
 
   mButtons[TTB_StartOn]->Disable();
   mButtons[TTB_StartOff]->Disable();
   mButtons[TTB_EndOn]->Disable();
   mButtons[TTB_EndOff]->Disable();
   mButtons[TTB_SelectSound]->Disable();
   mButtons[TTB_SelectSilence]->Disable();
   mButtons[TTB_Calibrate]->Enable();
   mButtons[TTB_AutomateSelection]->Disable();
   mButtons[TTB_MakeLabel]->Enable();

   AudacityProject * p = GetActiveProject();
   if(p)
      {
         mTimeTrack = new TimeTrack(p->GetDirManager());
         mTimeTrack->SetRangeUpper(100);
         mTimeTrack->SetRangeLower(100);
      }
   else
      mTimeTrack = NULL;
   mPlaySpeed = 1.0;



   //Process a dummy event to set up the slider
   wxCommandEvent dummy;
   OnSpeedSlider(dummy);

}



// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments

void TranscriptionToolBar::AddButton(const char **fg, const char **disabled, const char **alpha,
				     int id, const wxChar *tooltip, const wxChar *label)
{
   // Windows (TM) has a little extra room for some reason, so the top of the
   // buttons should be a little lower.
   int buttonTop = 0;
#ifdef __WXMSW__
   buttonTop=0;
#endif
   //The image needs to be centered in the button--not all icons are (e.g., the playback button).

   mButtons[id] = ToolBar::MakeButton(
                                      upImage, downImage, hiliteImage, fg,
                                      disabled, alpha,
                                      wxWindowID(id), wxPoint(mxButtonPos, buttonTop),
                                      false /*No buttons should process down events.*/,
                                      wxSize(BUTTON_WIDTH, BUTTON_WIDTH), 0, 0);
#if wxUSE_TOOLTIPS // Not available in wxX11
   mButtons[id]->SetToolTip(tooltip);
#endif
   mButtons[id]->SetLabel( label );
   mxButtonPos += BUTTON_WIDTH+1;
}

void TranscriptionToolBar::MakeButtons()
{
   wxColour newColour =
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour baseColour = wxColour(204, 204, 204);
   
   wxImage *upOriginal = new wxImage(wxBitmap(Up).ConvertToImage());
   wxImage *downOriginal = new wxImage(wxBitmap(Down).ConvertToImage());
   wxImage *hiliteOriginal = new wxImage(wxBitmap(Hilite).ConvertToImage());

#ifdef __WXGTK__
   /* dmazzoni: hack to get around XPM color bugs in GTK */
   unsigned char *data = upOriginal->GetData();
   baseColour.Set(data[28 * 3], data[28 * 3 + 1], data[28 * 3 + 2]);
#endif

   upImage = ChangeImageColour(upOriginal, baseColour, newColour);
   downImage = ChangeImageColour(downOriginal, baseColour, newColour);
   hiliteImage = ChangeImageColour(hiliteOriginal, baseColour, newColour);


   mxButtonPos = 0;
   AddButton(Play,     PlayDisabled,   PlayAlpha,   TTB_PlaySpeed,
      _("Play at selected speed"),
      _("Play-at-speed"));
   
   //Add a slider that controls the speed of playback.
   const int SliderWidth=100;
   mPlaySpeedSlider = new ASlider(this,TTB_PlaySpeedSlider,  _("Playback Speed"), 
                                  wxPoint(mxButtonPos,0),wxSize(SliderWidth,28),
                                  SPEED_SLIDER);
   mPlaySpeedSlider->Set(1.0);
   mPlaySpeedSlider->SetLabel(_("Playback Speed"));
   mxButtonPos +=  SliderWidth;

   
   AddButton(StartOn,     StartOnDisabled,   StartOnAlpha,   TTB_StartOn,
             _("Adjust left selection to  next onset"),
             _("Left-to-On"));
   AddButton(EndOn,       EndOnDisabled,     EndOnAlpha,     TTB_EndOn,
             _("Adjust right selection to previous offset"),
             _("Right-to-Off"));
   AddButton(StartOff,    StartOffDisabled,  StartOffAlpha,  TTB_StartOff,
             _("Adjust left selection to next offset"),
             _("Left-to-Off"));
   AddButton(EndOff,      EndOffDisabled,    EndOffAlpha,    TTB_EndOff,
             _("Adjust right selection to previous onset"),
             _("Right-to-On"));

   AddButton(SelectSound, SelectSoundDisabled, SelectSoundAlpha, TTB_SelectSound,
             _("Select region of sound around cursor"),
             _("Select-Sound"));

   AddButton(SelectSilence, SelectSilenceDisabled, SelectSilenceAlpha, TTB_SelectSilence,
             _("Select region of silence around cursor"),
             _("Select-Silence"));

   AddButton(AutomateSelection,   AutomateSelectionDisabled,   AutomateSelectionAlpha,      TTB_AutomateSelection,
      _("Automatically make labels from words"),
      _("Make Labels"));
   AddButton(MakeTag,     MakeTagDisabled,   MakeTagAlpha,   TTB_MakeLabel,  
      _("Add label at selection"),
      _("Add Label"));
  
   AddButton(CalibrateUp, CalibrateDisabled, CalibrateAlpha, TTB_Calibrate,
      _("Calibrate voicekey"),
      _("Calibrate"));
 
   mSensitivitySlider = new ASlider(this, TTB_SensitivitySlider, _("Adjust Sensitivity"),
                                    wxPoint(mxButtonPos,0),wxSize(SliderWidth,28),SPEED_SLIDER);
  
   mSensitivitySlider->SetLabel(_("Sensitivity"));
   mxButtonPos +=  SliderWidth;

   
   mKeyTypeChoice = new wxChoice(this, TTB_KeyType,
                                 wxPoint(mxButtonPos, 2),
                                 wxSize(120, 27));

   mKeyTypeChoice->Append(_("Energy"));
   mKeyTypeChoice->Append(_("Sign Changes (Low Threshold)"));
   mKeyTypeChoice->Append(_("Sign Changes (High Threshold)"));
   mKeyTypeChoice->Append(_("Direction Changes (Low Threshold)"));
   mKeyTypeChoice->Append(_("Direction Changes (High Threshold)"));
   mKeyTypeChoice->SetSelection(0);
   mxButtonPos += 120;

   mIdealSize = wxSize(mxButtonPos+3, 27);
   SetSize(mIdealSize );

   mSensitivitySlider->Set(.5);

}
  

TranscriptionToolBar::~TranscriptionToolBar()
{
   delete vk;	
   
   if (mBackgroundBitmap) delete mBackgroundBitmap;
   if(mPlaySpeedSlider)delete mPlaySpeedSlider;
   if(mSensitivitySlider)delete mSensitivitySlider;

   
   for (int i=0; i<TTBNumButtons; i++)
      if(mButtons[i]) delete mButtons[i];

   if(mKeyTypeChoice) delete mKeyTypeChoice;
}




//This handles key-stroke events????
void TranscriptionToolBar::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown()) {
      event.Skip();
      return;
   }
  
   if (event.GetKeyCode() == WXK_SPACE)
      {
         if (gAudioIO->IsBusy()) {
            /*Do Stuff Here*/
	
         } else {
	
            /*Do other stuff Here*/
         }
      }
}



//This changes the state of the various buttons
void TranscriptionToolBar::SetButton(bool down, AButton* button)
{
   if (down)
      button->PushDown();
   else
      button->PopUp();
}



void TranscriptionToolBar::GetSamples(WaveTrack *t, sampleCount *s0, sampleCount *slen)
{
   // GetSamples attempts to translate the start and end selection markers into sample indices 
   // These selection numbers are doubles.


   //First, get the current selection. It is part of the mViewInfo, which is
   //part of the project
   
   AudacityProject *p = GetActiveProject();
   if(p) {
      double start = p->GetSel0();
      double end = p->GetSel1();
   	   
      sampleCount ss0 = sampleCount( (start - t->GetOffset()) * t->GetRate() );
      sampleCount ss1 = sampleCount( (end - t->GetOffset()) * t->GetRate() );

      if (start < t->GetOffset())
         ss0 = 0;
#if 0
      //This adjusts the right samplecount to the maximum sample.
      if (ss1 >= t->GetNumSamples())
         ss1 = t->GetNumSamples();
#endif

      if (ss1 < ss0)
         ss1 = ss0;
	    

      *s0 = ss0;
      *slen = ss1 - ss0;
   }
}

void TranscriptionToolBar::OnPlaySpeed(wxCommandEvent & event)
{
 //If IO is busy, abort immediately
   if (gAudioIO->IsBusy()){
      SetButton(false,mButtons[TTB_PlaySpeed]);
      return;
   }

   AudacityProject * p = GetActiveProject();
   if(p)
      {
         
         if(!mTimeTrack)
            {   
               AudacityProject * p = GetActiveProject();
               if(p)
                  {
                     mTimeTrack = new TimeTrack(p->GetDirManager());
                     mTimeTrack->SetRangeUpper((long int)mPlaySpeed);
                     mTimeTrack->SetRangeLower((long int)mPlaySpeed);
                  }
               
            }

         if(mTimeTrack)
            {   
               mTimeTrack->SetRangeLower((long int)mPlaySpeed);
               mTimeTrack->SetRangeUpper((long int)mPlaySpeed);
            }



         double t0 = p->GetSel0();
         double t1 = p->GetSel1();
         double maxofmins,minofmaxs;
         bool looped = false;
         TrackList *t = p->GetTracks();
             
         // JS: clarified how the final play region is computed;
         
         if (t1 == t0) 
            {
               // msmeyer: When playing looped, we play the whole file, if
               // no range is selected. Otherwise, we play from t0 to end
               if (looped)
                  {
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
         else 
            {
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

         //Now, t1 is correct for the 'unwarped' wave.  This is how it _Should_ be, 
         //but is not how it is.  Adjust t1 for the speed adjustment.
         t1 = t0 + (t1-t0)* 100.0/mPlaySpeed;
         
         bool success = false;
         if (t1 > t0) 
            {
               int token =   gAudioIO->StartStream(t->GetWaveTrackArray(false),
                                                   WaveTrackArray(), mTimeTrack,
                                                   p->GetRate(), t0, t1, looped);
                                                   
               if (token != 0) {
                  success = true;
                  p->SetAudioIOToken(token);
                  
               }
            }
         SetButton(false, mButtons[TTB_PlaySpeed]); 
      }
}

void TranscriptionToolBar::OnSpeedSlider(wxCommandEvent& event)
{
   mPlaySpeed = (mPlaySpeedSlider->Get()) * 100;
   if(!mTimeTrack)
      {   
         AudacityProject * p = GetActiveProject();
         if(p)
            {
               mTimeTrack = new TimeTrack(p->GetDirManager());
               mTimeTrack->SetRangeUpper((long int)mPlaySpeed);
               mTimeTrack->SetRangeLower((long int)mPlaySpeed);
            }

      }

   if(mTimeTrack)
      {   
         mTimeTrack->SetRangeLower((long int)mPlaySpeed);
         mTimeTrack->SetRangeUpper((long int)mPlaySpeed);
      }
}

void TranscriptionToolBar::OnStartOn(wxCommandEvent &event)
{
   //If IO is busy, abort immediately
   if (gAudioIO->IsBusy()){
      SetButton(false,mButtons[TTB_StartOn]);
      return;
   }
	
   vk->AdjustThreshold(GetSensitivity());
   AudacityProject *p = GetActiveProject();
	
 
   TrackList *tl = p->GetTracks();
   TrackListIterator iter(tl);

   Track *t = iter.First();   //Make a track
   if(t) 
      {		
         sampleCount start,len;
         GetSamples((WaveTrack*)t, &start,&len);
        
         //Adjust length to end if selection is null
         //if(len == 0)
         //len = (WaveTrack*)t->GetSequence()->GetNumSamples()-start;
        
         sampleCount newstart = vk->OnForward(*(WaveTrack*)t,start,len);
         double newpos = newstart / ((WaveTrack*)t)->GetRate();
        
         p->SetSel0(newpos);
         p->RedrawProject();

         SetButton(false, mButtons[TTB_StartOn]); 
      }
}

void TranscriptionToolBar::OnStartOff(wxCommandEvent &event)
{
   //If IO is busy, abort immediately
   if (gAudioIO->IsBusy()){
      SetButton(false,mButtons[TTB_StartOff]);
      return;
   }
   vk->AdjustThreshold(GetSensitivity());
   AudacityProject *p = GetActiveProject();
   
   TrackList *tl = p->GetTracks();
   TrackListIterator iter(tl);
   
   SetButton(false, mButtons[TTB_StartOff]);
   Track *t = iter.First();   //Make a track
   if(t) 
      {		
         sampleCount start,len;
         GetSamples((WaveTrack*)t, &start,&len);
         
         //Adjust length to end if selection is null
         //if(len == 0)
         //len = (WaveTrack*)t->GetSequence()->GetNumSamples()-start;
         
         sampleCount newstart = vk->OffForward(*(WaveTrack*)t,start,len);
         double newpos = newstart / ((WaveTrack*)t)->GetRate();
         
         p->SetSel0(newpos);
         p->RedrawProject();
         
         SetButton(false, mButtons[TTB_StartOn]); 
      }
   
	
}

void TranscriptionToolBar::OnEndOn(wxCommandEvent &event)
{
	
   //If IO is busy, abort immediately
   if (gAudioIO->IsBusy()){
      SetButton(false,mButtons[TTB_EndOn]);
      return;
   }
  
   vk->AdjustThreshold(GetSensitivity());
   AudacityProject *p = GetActiveProject();
   TrackList *tl = p->GetTracks();
   TrackListIterator iter(tl);



   Track *t = iter.First();   //Make a track
   if(t) 
      {		
         sampleCount start,len;
         GetSamples((WaveTrack*)t, &start,&len);
        
         //Adjust length to end if selection is null
         if(len == 0)
            {
               len = start;
               start = 0;
            }
         sampleCount newEnd = vk->OnBackward(*(WaveTrack*)t,start+ len,len);
         double newpos = newEnd / ((WaveTrack*)t)->GetRate();
        
         p->SetSel1(newpos);
         p->RedrawProject();
        
         SetButton(false, mButtons[TTB_EndOn]);		

      }
}



void TranscriptionToolBar::OnEndOff(wxCommandEvent &event)
{
	
   //If IO is busy, abort immediately
   if (gAudioIO->IsBusy()){
      SetButton(false,mButtons[TTB_EndOff]);
      return;
   }
   vk->AdjustThreshold(GetSensitivity());
   AudacityProject *p = GetActiveProject();
   TrackList *tl = p->GetTracks();
   TrackListIterator iter(tl);

   Track *t = iter.First();   //Make a track
   if(t) 
      {		
         sampleCount start,len;
         GetSamples((WaveTrack*)t, &start,&len);
        
         //Adjust length to end if selection is null
         if(len == 0)
            {
               len = start;
               start = 0;
            }
         sampleCount newEnd = vk->OffBackward(*(WaveTrack*)t,start+ len,len);
         double newpos = newEnd / ((WaveTrack*)t)->GetRate();
        
         p->SetSel1(newpos);
         p->RedrawProject();
        
         SetButton(false, mButtons[TTB_EndOff]);
      }

}



void TranscriptionToolBar::OnSelectSound(wxCommandEvent &event)
{

   //If IO is busy, abort immediately
   if (gAudioIO->IsBusy()){
      SetButton(false,mButtons[TTB_SelectSound]);
      return;
   }
	

   vk->AdjustThreshold(GetSensitivity());
   AudacityProject *p = GetActiveProject();
   
   
   TrackList *tl = p->GetTracks();
   TrackListIterator iter(tl);

   Track *t = iter.First();   //Make a track
   if(t) 
      {		
         sampleCount start,len;
         GetSamples((WaveTrack*)t, &start,&len);
        
         //Adjust length to end if selection is null
         //if(len == 0)
         //len = (WaveTrack*)t->GetSequence()->GetNumSamples()-start;
        
         double rate =  ((WaveTrack*)t)->GetRate();
         sampleCount newstart = vk->OffBackward(*(WaveTrack*)t,start,start);
         sampleCount newend   = vk->OffForward(*(WaveTrack*)t,start+len,(int)(tl->GetEndTime()*rate));

         //reset the selection bounds.
         p->SetSel0(newstart / rate);
         p->SetSel1(newend /  rate);
         p->RedrawProject();

      }
   
   SetButton(false,mButtons[TTB_SelectSound]);
}

void TranscriptionToolBar::OnSelectSilence(wxCommandEvent &event)
{

   //If IO is busy, abort immediately
   if (gAudioIO->IsBusy()){
      SetButton(false,mButtons[TTB_SelectSilence]);
      return;
   }

   vk->AdjustThreshold(GetSensitivity());
   AudacityProject *p = GetActiveProject();
   
   
   TrackList *tl = p->GetTracks();
   TrackListIterator iter(tl);

   Track *t = iter.First();   //Make a track
   if(t) 
      {		
         sampleCount start,len;
         GetSamples((WaveTrack*)t, &start,&len);
        
         //Adjust length to end if selection is null
         //if(len == 0)
         //len = (WaveTrack*)t->GetSequence()->GetNumSamples()-start;
         double rate =  ((WaveTrack*)t)->GetRate();
         sampleCount newstart = vk->OnBackward(*(WaveTrack*)t,start,start);
         sampleCount newend   = vk->OnForward(*(WaveTrack*)t,start+len,(int)(tl->GetEndTime()*rate));

         //reset the selection bounds.
         p->SetSel0(newstart /  rate);
         p->SetSel1(newend / rate);
         p->RedrawProject();

      }
   
   SetButton(false,mButtons[TTB_SelectSilence]);

}



void TranscriptionToolBar::OnCalibrate(wxCommandEvent &event)
{
   //If IO is busy, abort immediately
   if (gAudioIO->IsBusy()){
      SetButton(false,mButtons[TTB_Calibrate]);
      return;
   }


   AudacityProject *p = GetActiveProject();

   TrackList *tl = p->GetTracks();
   TrackListIterator iter(tl);
   Track *t = iter.First();   //Get a track
  
   if(t) 
      {		
         sampleCount start,len;
         GetSamples((WaveTrack*)t, &start,&len);
        
         vk->CalibrateNoise(*((WaveTrack*)t),start,len);
         vk->AdjustThreshold(3);
		
         mButtons[TTB_StartOn]->Enable();
         mButtons[TTB_StartOff]->Enable();
         mButtons[TTB_EndOn]->Enable();
         mButtons[TTB_EndOff]->Enable();
         //mThresholdSensitivity->Set(3);

         SetButton(false,mButtons[TTB_Calibrate]);    
      }
  
   mButtons[TTB_StartOn]->Enable();
   mButtons[TTB_StartOff]->Enable();
   mButtons[TTB_EndOn]->Enable();
   mButtons[TTB_EndOff]->Enable();
   mButtons[TTB_SelectSound]->Enable();  
   mButtons[TTB_SelectSilence]->Enable();  
   mButtons[TTB_AutomateSelection]->Enable();

   //Make the sensititivy slider set the sensitivity by processing an event.
   wxCommandEvent dummy;
   OnSensitivitySlider(dummy);

}

//This automates selection through a selected region,
//selecting its best guess for words and creating labels at those points.

void TranscriptionToolBar::OnAutomateSelection(wxCommandEvent &event)
{

	
   //If IO is busy, abort immediately
   if (gAudioIO->IsBusy())
      {
         SetButton(false,mButtons[TTB_EndOff]);
         return;
      }

   wxBusyCursor busy;

   vk->AdjustThreshold(GetSensitivity());
   AudacityProject *p = GetActiveProject();
   TrackList *tl = p->GetTracks();
   TrackListIterator iter(tl);
   
   Track *t = iter.First();   //Make a track
   if(t) 
      {		
         sampleCount start,len;
         GetSamples((WaveTrack*)t, &start,&len);
         
         //Adjust length to end if selection is null
         if(len == 0)
            {
               len = start;
               start = 0;
            }
         int lastlen = 0;
         sampleCount newStart, newEnd;
         double newStartPos, newEndPos;
         

         //This is the minumum word size in samples (.05 is 50 ms)
         int minWordSize = (int)(((WaveTrack*)t)->GetRate() * .05);
         
         //Continue until we have processed the entire
         //region, or we are making no progress.
         while(len > 0 && lastlen != len)
            {
               
               lastlen = len;
               
               newStart = vk->OnForward(*(WaveTrack*)t,start,len);

               //JKC: If no start found then don't add any labels.
               if( newStart==start)
                  break;
               
               //Adjust len by the new start position
               len -= (newStart - start);
               
               //Adjust len by the minimum word size
               len -= minWordSize;
               
               

               //OK, now we have found a new starting point.  A 'word' should be at least 
               //50 ms long, so jump ahead minWordSize
               
               newEnd   = vk->OffForward(*(WaveTrack*)t,newStart+minWordSize, len);
               
               //If newEnd didn't move, we should give up, because
               // there isn't another end before the end of the selection.
               if(newEnd == (newStart + minWordSize))
                  break;


               //Adjust len by the new word end
               len -= (newEnd - newStart);
               
               //Calculate the start and end of the words, in seconds
               newStartPos = newStart / ((WaveTrack*)t)->GetRate();
               newEndPos = newEnd / ((WaveTrack*)t)->GetRate();
               
               
               //Increment
               start = newEnd;
               
               p->DoAddLabel(newStartPos, newEndPos);
               p->RedrawProject();
            }
         SetButton(false, mButtons[TTB_AutomateSelection]);
      }
}

void TranscriptionToolBar::OnMakeLabel(wxCommandEvent &event)
{
   AudacityProject *p = GetActiveProject();
   SetButton(false, mButtons[TTB_MakeLabel]);  
   p->DoAddLabel(p->GetSel0(),  p->GetSel1());
}

//This returns a double z-score between 0 and 10.
double TranscriptionToolBar::GetSensitivity()
{
   return (double)mSensitivity;
}

void TranscriptionToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

#if defined(__WXMAC__)          // && defined(TARGET_CARBON)

   if (mBackgroundWidth < width) {
      if (mBackgroundBitmap)
         delete mBackgroundBitmap;

      mBackgroundBitmap = new wxBitmap(width, height);

      wxMemoryDC memDC;
      memDC.SelectObject(*mBackgroundBitmap);

      int y;
      memDC.SetPen(wxPen(wxColour(231, 231, 231), 1, wxSOLID));
      for (y = 0; y < height; y += 4)
         memDC.DrawLine(0, y, width, y);
      memDC.SetPen(wxPen(wxColour(239, 239, 239), 1, wxSOLID));
      for (y = 1; y < height; y += 2)
         memDC.DrawLine(0, y, width, y);
      memDC.SetPen(wxPen(wxColour(255, 255, 255), 1, wxSOLID));
      for (y = 2; y < height; y += 4)
         memDC.DrawLine(0, y, width, y);

   }

   wxMemoryDC memDC;
   memDC.SelectObject(*mBackgroundBitmap);

   dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);

#else

   dc.SetBrush(mBackgroundBrush);
   dc.SetPen(mBackgroundPen);
   dc.DrawRectangle(0, 0, width, height);

   dc.SetPen(*wxBLACK_PEN);

#endif
}

void TranscriptionToolBar::OnSensitivitySlider(wxCommandEvent & event)
{
   mSensitivity = (mSensitivitySlider->Get());
}

void TranscriptionToolBar::EnableDisableButtons()
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;
   // Is anything selected?
   bool selection = false;
   TrackListIterator iter(p->GetTracks());
   for (Track *t = iter.First(); t; t = iter.Next())
      if (t->GetSelected()) {
         selection = true;
         break;
      }
   selection &= (p->GetSel0() < p->GetSel1());

   mButtons[TTB_Calibrate]->SetEnabled(selection);
}

void TranscriptionToolBar::PlaceButton(int i, wxWindow *pWind)
{
   wxSize Size;
   if( i==0 )
   {
      mxButtonPos = 0;
   }
   Size = pWind->GetSize();
   pWind->SetSize( mxButtonPos, 0, Size.GetX(), Size.GetY());
   mxButtonPos+=Size.GetX()+1;
   mIdealSize = wxSize(mxButtonPos+3, 27);
   SetSize(mIdealSize );
}

void TranscriptionToolBar::SetKeyType(wxCommandEvent & event)
{
   int value = mKeyTypeChoice->GetSelection();

   //Only use one key type at a time.
   switch(value)
      {
      case 0:
         vk->SetKeyType(true,0,0,0,0);
         break;
      case 1:
         vk->SetKeyType(0,true,0,0,0);
         break;
      case 2:
         vk->SetKeyType(0,0,true,0,0);
         break;
      case 3:
         vk->SetKeyType(0,0,0,true,0);
         break;
      case 4:
         vk->SetKeyType(0,0,0,0,true);
         break;
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
// arch-tag: ToDo


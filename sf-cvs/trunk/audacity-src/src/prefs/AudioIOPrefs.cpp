/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni

  Presents interface for user to select the recording device and
  playback device, from the list of choices that PortAudio
  makes available.

  Also lets user decide whether or not to record in stereo, and
  whether or not to play other tracks while recording one (duplex).

**********************************************************************/

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/utils.h>
#include <wx/window.h>

#include "../Prefs.h"
#include "../AudioIO.h"
#include "../Project.h"
#include "../MixerToolBar.h"
#include "AudioIOPrefs.h"

#include "portaudio.h"

enum {
   RecChoiceID = 1000,
   PlayChoiceID
};

BEGIN_EVENT_TABLE(AudioIOPrefs, wxPanel)
END_EVENT_TABLE()	

AudioIOPrefs::AudioIOPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   /* read prefs all at once, then set up the dialog */
   gPrefs->SetPath("/AudioIO");
   mPlayDevice = gPrefs->Read("PlaybackDevice", "");
   mRecDevice = gPrefs->Read("RecordingDevice", "");

   long recordChannels = 1;
   gPrefs->Read("RecordChannels", &recordChannels, 1L);
   bool duplex;
   gPrefs->Read("Duplex", &duplex, false);

   #ifdef __MACOSX__
   bool playthrough;
   gPrefs->Read("Playthrough", &playthrough, false);
   #endif

   bool swplaythrough;
   gPrefs->Read("SWPlaythrough", &swplaythrough, false);

   gPrefs->SetPath("/");

   topSizer = new wxBoxSizer( wxVERTICAL );

   //
   // Playback
   //

   wxStaticBoxSizer *playbackSizer =
      new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Playback")),
            wxVERTICAL);

   wxFlexGridSizer *fileSizer[2];// = new wxFlexGridSizer *[2];
   fileSizer[0] = new wxFlexGridSizer(1, 2, GENERIC_CONTROL_BORDER, GENERIC_CONTROL_BORDER);
   fileSizer[0]->AddGrowableCol(1);

   int j, k;
   int playIndex = 0;
   int numDevices = 0;

   // Count the number of devices which do output
#if USE_PORTAUDIO_V19
   for(j=0; j<Pa_GetDeviceCount(); j++) {
#else
   for(j=0; j<Pa_CountDevices(); j++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxOutputChannels > 0)
         numDevices++;
   }

   wxString *playLabels=NULL;
   mPlayNames=NULL;
   if(numDevices){
      mPlayNames = new wxString[numDevices];
      playLabels = new wxString[numDevices];
   }

   k = 0;
#if USE_PORTAUDIO_V19
   for(j=0; j<Pa_GetDeviceCount(); j++) {
#else
   for(j=0; j<Pa_CountDevices(); j++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxOutputChannels > 0) {
         mPlayNames[k] = info->name;
#if USE_PORTAUDIO_V19
         playLabels[k].Printf("%s: %s",
                             Pa_GetHostApiInfo(info->hostApi)->name,
                             info->name);
#else
         playLabels[k] = info->name;
#endif

         if (mPlayNames[k] == mPlayDevice)
            playIndex = k;
         k++;
      }
   }
      
   mPlayChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numDevices, playLabels);
   mPlayChoice->SetSelection(playIndex);
   if(playLabels) delete [] playLabels;

   // declare three box sizers for static text
   wxBoxSizer *textSizer[3];
   
   // device label and choice box
   textSizer[0] = new wxBoxSizer(wxHORIZONTAL);
   
   textSizer[0]->Add(
      new wxStaticText(this, -1, _("Device:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 1, 
      wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   fileSizer[0]->Add(
      textSizer[0], 0, 
      wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   fileSizer[0]->Add(mPlayChoice, 1, 
      wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);


   //
   // Recording
   //

   wxStaticBoxSizer *recordingSizer =
      new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Recording")),
            wxVERTICAL);

   fileSizer[1] = new wxFlexGridSizer(2, 2, GENERIC_CONTROL_BORDER, GENERIC_CONTROL_BORDER);
   fileSizer[1]->AddGrowableCol(1);

   int recIndex = 0;
   numDevices = 0;

   // Count the number of devices which do input
#if USE_PORTAUDIO_V19
   for(j=0; j<Pa_GetDeviceCount(); j++) {
#else
   for(j=0; j<Pa_CountDevices(); j++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxInputChannels > 0)
         numDevices++;
   }

   mRecNames = NULL;
   wxString *recLabels = NULL;
   if(numDevices){
      mRecNames = new wxString[numDevices];
      recLabels = new wxString[numDevices];
   }

   k = 0;
#if USE_PORTAUDIO_V19
   for(j=0; j<Pa_GetDeviceCount(); j++) {
#else
   for(j=0; j<Pa_CountDevices(); j++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxInputChannels > 0) {
         mRecNames[k] = info->name;
#if USE_PORTAUDIO_V19
         recLabels[k].Printf("%s: %s",
                             Pa_GetHostApiInfo(info->hostApi)->name,
                             info->name);
#else
         recLabels[k] = info->name;
#endif
         if (mRecNames[k] == mRecDevice)
            recIndex = k;
         k++;
      }
   }

   mRecChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numDevices, recLabels);
   mRecChoice->SetSelection(recIndex);
   if(recLabels) delete[] recLabels;

   // device label and choice box
   textSizer[1] = new wxBoxSizer(wxHORIZONTAL);
   
   textSizer[1]->Add(
      new wxStaticText(this, -1, _("Device:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 1, 
      wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   fileSizer[1]->Add(
      textSizer[1], 0, 
      wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   fileSizer[1]->Add(mRecChoice, 1, 
      wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);


   const int numChannels = 16;
   wxString channelNames[16];
   for(int c=0; c<numChannels; c++)
      channelNames[c] = wxString::Format("%d", c+1);
   channelNames[0] = wxString::Format(_("1 (Mono)"));
   channelNames[1] = wxString::Format(_("2 (Stereo)"));

   mChannelsChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                                  numChannels, channelNames);
   mChannelsChoice->SetSelection(recordChannels-1);

   // channel label and choice box
   textSizer[2] = new wxBoxSizer(wxHORIZONTAL);
   
   textSizer[2]->Add(
      new wxStaticText(this, -1, _("Channels:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 1, 
      wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   fileSizer[1]->Add(
      textSizer[2], 0, 
      wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);
   
   fileSizer[1]->Add(mChannelsChoice, 1, 
      wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   // find out the biggest minimum size of labels
   int maxIndex = 0,r;
   wxSize maxMinSize = textSizer[0]->GetMinSize();
   for (r = 1; r < 3; r++) {
      if (textSizer[r]->GetMinSize().GetWidth() > maxMinSize.GetWidth()) {
         maxMinSize = textSizer[r]->GetMinSize();
         maxIndex = r;
      }
   }

   // set small minimum sizes to max minumum size
   for (r = 0; r < 3; r++) {
      if (r != maxIndex) 
         textSizer[r]->SetMinSize( maxMinSize );
   }

   // add flexgrid sizer to static sizer
   playbackSizer->Add(fileSizer[0], 0,
      wxGROW|wxALL, GENERIC_CONTROL_BORDER);
   recordingSizer->Add(fileSizer[1], 0,
      wxGROW|wxALL, GENERIC_CONTROL_BORDER);

   // add static sizer to top sizer
   topSizer->Add(playbackSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);
   topSizer->Add(recordingSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);


   wxStaticBoxSizer *staticSizer = new wxStaticBoxSizer(new wxStaticBox(this, -1, _("Playthrough")), wxVERTICAL);
   mDuplex = new wxCheckBox(this, -1,
                            _("Play other tracks while recording new one"));
   mDuplex->SetValue(duplex);
   staticSizer->Add(mDuplex, 0, wxGROW|wxALL, GENERIC_CONTROL_BORDER);

   #ifdef __MACOSX__
   mPlaythrough = new wxCheckBox(this, -1,
                                 _("Hardware Playthrough (Play new track while recording it)"));
   mPlaythrough->SetValue(playthrough);
   staticSizer->Add(mPlaythrough, 0, wxGROW|wxALL, GENERIC_CONTROL_BORDER);
   #endif

   mSWPlaythrough = new wxCheckBox(this, -1,
                                 _("Software Playthrough (Play new track while recording it)"));
   mSWPlaythrough->SetValue(swplaythrough);
   staticSizer->Add(mSWPlaythrough, 0, wxGROW|wxALL, GENERIC_CONTROL_BORDER);
   topSizer->Add(staticSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
   SetSizer(outSizer);
}

AudioIOPrefs::~AudioIOPrefs()
{
   if(mPlayNames)delete[] mPlayNames;
   if(mRecNames) delete[] mRecNames;
}

bool AudioIOPrefs::Apply()
{
   mPlayDevice = wxEmptyString;
   mRecDevice = wxEmptyString;

   if(mPlayNames) mPlayDevice = mPlayNames[mPlayChoice->GetSelection()];
   if(mRecNames) mRecDevice = mRecNames[mRecChoice->GetSelection()];

   long recordChannels = mChannelsChoice->GetSelection()+1;
   bool duplex = mDuplex->GetValue();

   /* Step 2: Write to gPrefs */
   gPrefs->SetPath("/AudioIO");

   gPrefs->Write("PlaybackDevice", mPlayDevice);
   gPrefs->Write("RecordingDevice", mRecDevice);

   gPrefs->Write("RecordChannels", recordChannels);
   gPrefs->Write("Duplex", duplex);

   #ifdef __MACOSX__
   gPrefs->Write("Playthrough", mPlaythrough->GetValue());
   #endif

   gPrefs->Write("SWPlaythrough", mSWPlaythrough->GetValue());

   gPrefs->SetPath("/");

   /* Step 3: Make audio sub-system re-read preferences */

#if USE_PORTMIXER
   if (gAudioIO)
      gAudioIO->HandleDeviceChange();

   for( unsigned int i = 0; i < gAudacityProjects.GetCount(); i++ )
     {
       if(gAudacityProjects[i]->GetMixerToolBar())
	 gAudacityProjects[i]->GetMixerToolBar()->UpdateControls();
     }
#endif // USE_PORTMIXER

   return true;
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
// arch-tag: d6904b91-a320-4194-8d60-caa9175b6bb4


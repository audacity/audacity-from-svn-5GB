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

#include <wx/window.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/gdicmn.h>
#include <wx/statbox.h>
#include <wx/msgdlg.h>
#include <wx/button.h>
#include <wx/utils.h>
#include <wx/sizer.h>
#include <wx/intl.h>

#include "../Prefs.h"
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
   bool recordStereo;
   gPrefs->Read("RecordStereo", &recordStereo, false);
   bool duplex;
   gPrefs->Read("Duplex", &duplex, false);
   gPrefs->SetPath("/");

   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this,
                      -1,
                     _("Audio I/O Settings")),
      wxVERTICAL);

   //
   // Playback
   //

   wxStaticBoxSizer *playbackSizer =
      new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Playback Device")),
            wxVERTICAL);

   wxBoxSizer *pFileSizer = new wxBoxSizer(wxHORIZONTAL);

   int j, k;
   int playIndex = 0;
   int numDevices = 0;

   // Count the number of devices which do output
   for(j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxOutputChannels > 0)
         numDevices++;
   }

   wxString *playNames = new wxString[numDevices];
   k = 0;
   for(j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxOutputChannels > 0) {
         playNames[k] = info->name;
         if (playNames[k] == mPlayDevice)
            playIndex = k-1;
         k++;
      }
   }

   mPlayChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numDevices, playNames);
   mPlayChoice->SetSelection(playIndex+1);

   pFileSizer->Add(
      new wxStaticText(this, -1, _("Device:")), 0, 
      wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   pFileSizer->Add(mPlayChoice, 1, 
      wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   playbackSizer->Add(pFileSizer, 0,
      wxGROW|wxALL, GENERIC_CONTROL_BORDER);

   topSizer->Add(playbackSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);

   //
   // Recording
   //

   wxStaticBoxSizer *recordingSizer =
      new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Recording Device")),
            wxVERTICAL);

   wxBoxSizer *rFileSizer = new wxBoxSizer(wxHORIZONTAL);

   int recIndex = 0;
   numDevices = 0;

   // Count the number of devices which do input
   for(j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxInputChannels > 0)
         numDevices++;
   }

   wxString *recNames = new wxString[numDevices];
   k = 0;
   for(j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxInputChannels > 0) {
         recNames[k] = info->name;
         if (recNames[k] == mRecDevice)
            recIndex = k-1;
         k++;
      }
   }

   mRecChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numDevices, recNames);
   mRecChoice->SetSelection(recIndex+1);

   rFileSizer->Add(
      new wxStaticText(this, -1, _("Device:")), 0, 
      wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   rFileSizer->Add(mRecChoice, 1, 
      wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   recordingSizer->Add(rFileSizer, 0,
      wxGROW|wxALL, GENERIC_CONTROL_BORDER);

   topSizer->Add(recordingSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);

   mRecordStereo = new wxCheckBox(this, -1, _("Record in Stereo"));
   mRecordStereo->SetValue(recordStereo);
   topSizer->Add(mRecordStereo, 0, wxGROW|wxALL, 2);

   mDuplex = new wxCheckBox(this, -1,
                            _("Play other tracks while recording new one"));
   mDuplex->SetValue(duplex);
   topSizer->Add(mDuplex, 0, wxGROW|wxALL, 2);

   SetAutoLayout(true);
   topSizer->Fit(this);
   topSizer->SetSizeHints(this);
   SetSizer(topSizer);
}

AudioIOPrefs::~AudioIOPrefs()
{
}

bool AudioIOPrefs::Apply()
{
   mPlayDevice = mPlayChoice->GetStringSelection();
   mRecDevice = mRecChoice->GetStringSelection();   

   bool recordStereo = mRecordStereo->GetValue();
   bool duplex = mDuplex->GetValue();

   /* Step 2: Write to gPrefs */
   gPrefs->SetPath("/AudioIO");

   gPrefs->Write("PlaybackDevice", mPlayDevice);
   gPrefs->Write("RecordingDevice", mRecDevice);

   gPrefs->Write("RecordStereo", recordStereo);
   gPrefs->Write("Duplex", duplex);
   gPrefs->SetPath("/");

   /* Step 3: Make audio sub-system re-read preferences */

   return true;
}


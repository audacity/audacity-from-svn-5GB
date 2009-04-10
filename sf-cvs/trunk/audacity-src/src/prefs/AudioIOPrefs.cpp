/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class AudioIOPrefs
\brief A PrefsPanel used to select recording and playback devices and
other settings.

  Presents interface for user to select the recording device and
  playback device, from the list of choices that PortAudio
  makes available.

  Also lets user decide whether or not to record in stereo, and
  whether or not to play other tracks while recording one (duplex).

*//********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/intl.h>

#include "portaudio.h"

#include "../AudioIO.h"
#include "../Envelope.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"


#include "AudioIOPrefs.h"

enum {
   HostID = 10000
};

BEGIN_EVENT_TABLE(DevicePrefs, PrefsPanel)
   EVT_CHOICE(HostID, DevicePrefs::OnHost)
END_EVENT_TABLE()

DevicePrefs::DevicePrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Devices"))
{
   Populate();
}

DevicePrefs::~DevicePrefs()
{
}

void DevicePrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();

   // Get current setting for devices
   mPlayDevice = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   mRecordDevice = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   wxCommandEvent e;
   OnHost(e);
}

void DevicePrefs::GetNamesAndLabels()
{
   // Gather list of hosts.  Only added hosts that have devices attached.
   int nDevices = Pa_GetDeviceCount();
   for (int i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->maxOutputChannels > 0 || info->maxInputChannels > 0) {
         wxString name = LAT1CTOWX(Pa_GetHostApiInfo(info->hostApi)->name);
         if (mHostNames.Index(name) == wxNOT_FOUND) {
            mHostNames.Add(name);
            mHostLabels.Add(name);
         }
      }
   }

   // Channel counts, mono, stereo etc...
   for (int i = 0; i < 16; i++) {
      mChannelNames.Add(wxString::Format(wxT("%d"), i + 1));
      mChannelLabels.Add(i + 1);
   }
   mChannelNames[0] = _("1 (Mono)");
   mChannelNames[1] = _("2 (Stereo)");
}

void DevicePrefs::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString empty;

   S.SetBorder(2);

   S.StartStatic(_("Interface"));
   {
      S.StartMultiColumn(2);
      {
         mHost = S.Id(HostID).TieChoice(_("Host") + wxString(wxT(":")),
                                        wxT("/AudioIO/Host"), 
                                        wxT(""),
                                        mHostNames,
                                        mHostLabels);
         S.SetSizeHints(mHostNames);

         S.AddPrompt(_("Using:"));

         S.AddFixedText(LAT1CTOWX(Pa_GetVersionText()));
      }
      S.EndMultiColumn();
   }                              
   S.EndStatic();

   S.StartStatic(_("Playback"));
   {
      S.StartMultiColumn(2);
      {
         mPlay = S.AddChoice(_("Device") + wxString(wxT(":")),
                             wxEmptyString,
                             &empty);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Recording"));
   {
      S.StartMultiColumn(2);
      {
         mRecord = S.AddChoice(_("Device") + wxString(wxT(":")),
                               wxEmptyString,
                               &empty);

         S.TieChoice(_("Channels") + wxString(wxT(":")),
                     wxT("/AudioIO/RecordChannels"),
                     2,
                     mChannelNames,
                     mChannelLabels);
         S.SetSizeHints(mChannelNames);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

void DevicePrefs::OnHost(wxCommandEvent & e)
{
   int index = mHost->GetCurrentSelection();
   int nDevices = Pa_GetDeviceCount();

   mPlay->Clear();
   mRecord->Clear();

   wxArrayString playnames;
   wxArrayString recordnames;

   for (int i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->hostApi == index) {
         wxString name = LAT1CTOWX(info->name);
         wxString device = DeviceName(info);
         int index;

         if (info->maxOutputChannels > 0) {
            playnames.Add(name);
            index = mPlay->Append(name, (void *) info);
            if (device == mPlayDevice) {
               mPlay->SetSelection(index);
            }
         }

         if (info->maxInputChannels > 0) {
            recordnames.Add(name);
            index = mRecord->Append(name, (void *) info);
            if (device == mRecordDevice) {
               mRecord->SetSelection(index);
            }
         }
      }
   }

   if (mPlay->GetCount() && mPlay->GetSelection() == wxNOT_FOUND) {
      mPlay->SetSelection(0);
   }

   if (mRecord->GetCount() && mRecord->GetSelection() == wxNOT_FOUND) {
      mRecord->SetSelection(0);
   }

   ShuttleGui S(this, eIsCreating);
   S.SetSizeHints(mPlay, playnames);
   S.SetSizeHints(mRecord, recordnames);
}

bool DevicePrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   const PaDeviceInfo *info;

   info = (const PaDeviceInfo *) mPlay->GetClientData(mPlay->GetSelection());
   gPrefs->Write(wxT("/AudioIO/PlaybackDevice"),
                 DeviceName(info));

   info = (const PaDeviceInfo *) mRecord->GetClientData(mRecord->GetSelection());
   gPrefs->Write(wxT("/AudioIO/RecordingDevice"),
                 DeviceName(info));

   return true;
}

PlaybackPrefs::PlaybackPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Playback"))
{
   Populate();
}

PlaybackPrefs::~PlaybackPrefs()
{
}

void PlaybackPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void PlaybackPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Effects Preview"));
   {
      S.StartThreeColumn();
      {
         S.TieTextBox(_("Length of preview:"),
                      wxT("/AudioIO/EffectsPreviewLen"),
                      3.0,
                      9);
         S.AddUnits(_("seconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Cut Preview"));
   {
      S.StartThreeColumn();
      {
         S.TieTextBox(_("Preview before cut region:"),
                      wxT("/AudioIO/CutPreviewBeforeLen"),
                      1.0,
                      9);
         S.AddUnits(_("seconds"));

         S.TieTextBox(_("Preview after cut region:"),
                      wxT("/AudioIO/CutPreviewAfterLen"),
                      1.0,
                      9);
         S.AddUnits(_("seconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Seek Time when playing"));
   {
      S.StartThreeColumn();
      {
         S.TieTextBox(_("Short period:"),
                      wxT("/AudioIO/SeekShortPeriod"),
                      1.0,
                      9);
         S.AddUnits(_("seconds"));

         S.TieTextBox(_("Long period:"),
                      wxT("/AudioIO/SeekLongPeriod"),
                      15.0,
                      9);
         S.AddUnits(_("seconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();
}

bool PlaybackPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

RecordingPrefs::RecordingPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Recording"))
{
   Populate();
}

RecordingPrefs::~RecordingPrefs()
{
}

void RecordingPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void RecordingPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Playthrough"));
   {
      S.TieCheckBox(_("Overdub: &Play other tracks while recording new one"),
                    wxT("/AudioIO/Duplex"),
                    true);
#if defined(__WXMAC__)
      S.TieCheckBox(_("&Hardware Playthrough: Play new track while recording it"),
                    wxT("/AudioIO/Playthrough"),
                    false);
#endif
      S.TieCheckBox(_("&Software Playthrough: Play new track while recording or monitoring"),
                    wxT("/AudioIO/SWPlaythrough"),
                    false);
#if !defined(__WXMAC__)
      S.AddUnits(wxString(wxT("     ")) + _("(uncheck when recording \"stereo mix\")"));
#endif
   }
   S.EndStatic();

   S.StartStatic( _("Latency"));
   {
      S.StartThreeColumn();
      {
         // only show the following controls if we use Portaudio v19, because
         // for Portaudio v18 we always use default buffer sizes
         S.TieTextBox(_("Audio to buffer:"),
                      wxT("/AudioIO/LatencyDuration"),
                      DEFAULT_LATENCY_DURATION,
                      9);
         S.AddUnits(_("milliseconds (higher = more latency)"));

         S.TieTextBox(_("Latency correction:"),
                      wxT("/AudioIO/LatencyCorrection"),
                      DEFAULT_LATENCY_CORRECTION,
                      9);
         S.AddUnits(_("milliseconds (negative = backwards)"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Sound Activated Recording"));
   {
      S.TieCheckBox(_("Sound Activated Recording"),
                    wxT("/AudioIO/SoundActivatedRecord"),
                    false);

      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);

         int dBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
         S.TieSlider(_("Sound Activation Level (dB):"),
                     wxT("/AudioIO/SilenceLevel"),
                     -50,
                     0,
                     -dBRange);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

bool RecordingPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   double latencyDuration = DEFAULT_LATENCY_DURATION;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);
   if (latencyDuration < 0) {
      gPrefs->Write(wxT("/AudioIO/LatencyDuration"), DEFAULT_LATENCY_DURATION);
   }

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


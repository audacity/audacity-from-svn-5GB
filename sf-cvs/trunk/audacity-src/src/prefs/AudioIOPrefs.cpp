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
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   // GUI is built, now do any post processing of it.
}

void PlaybackPrefs::GetNamesAndLabels()
{
   // Get lists of devices both for play and record.
#if USE_PORTAUDIO_V19
   int nDevices = Pa_GetDeviceCount();
#else
   int nDevices = Pa_CountDevices();
#endif

   for (int i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->maxOutputChannels > 0) {
         wxString name = DeviceName(info);
         mPlayNames.Add(name);
         mPlayLabels.Add(name);
      }
   }
}

void PlaybackPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Playback"));
   {
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);
         mPlay = S.TieChoice(_("Device") + wxString(wxT(":")),
                             wxT("/AudioIO/PlaybackDevice"), 
                             wxT(""),
                             mPlayNames,
                             mPlayLabels);

         S.AddPrompt(_("Using:"));

         S.AddFixedText(_("Portaudio v") +
                        wxString((USE_PORTAUDIO_V19 ? wxT("19") : wxT("18"))));
      }
      S.EndMultiColumn();
   }                              
   S.EndStatic();

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
      S.EndThreeColumn();
   }
   S.EndStatic();
}

bool PlaybackPrefs::Apply()
{
#if 0
#if USE_PORTAUDIO_V19
   if (!AudioIO::ValidateDeviceNames(mPlay->GetStringSelection(), mRec->GetStringSelection())) {
      wxMessageBox(_("Playback and Recording device must use the same type, i.e., MME, DirectSound, etc."));
      return false;
   }
#endif
#endif

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

#if 0
#if USE_PORTMIXER
   if (gAudioIO)
      gAudioIO->HandleDeviceChange();
#endif // USE_PORTMIXER
#endif

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
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Gets the lists of names and lists of labels which are
/// used in the choice controls.
/// The names are what the user sees in the wxChoice.
/// The corresponding labels are what gets stored.
void RecordingPrefs::GetNamesAndLabels()
{
   // Get lists of devices both for play and record.
#if USE_PORTAUDIO_V19
   int nDevices = Pa_GetDeviceCount();
#else
   int nDevices = Pa_CountDevices();
#endif

//   int numChannels = 0; // find max no. of record channels available
   for (int i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->maxInputChannels > 0) {
         wxString name = DeviceName(info);
         mRecordNames.Add(name);
         mRecordNames.Add(name);
      }
   }

   // Channel counts, mono, stereo etc...
   for (int i = 0; i < 16; i++)
   {
      mChannelNames.Add(wxString::Format(wxT("%d"), i + 1));
      mChannelLabels.Add(i + 1);
   }
   mChannelNames[0] = _("1 (Mono)");
   mChannelNames[1] = _("2 (Stereo)");
}

void RecordingPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Recording"));
   {
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);
         mRec = S.TieChoice(_("Device") + wxString(wxT(":")),
                            wxT("/AudioIO/RecordingDevice"),
                            wxT(""),
                            mRecordNames,
                            mRecordLabels);

         S.TieChoice(_("Channels") + wxString(wxT(":")),
                     wxT("/AudioIO/RecordChannels"),
                     2,
                     mChannelNames,
                     mChannelLabels);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

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
#if USE_PORTAUDIO_V19
         // only show the following controls if we use Portaudio v19, because
         // for Portaudio v18 we always use default buffer sizes
         S.TieTextBox(_("Audio to buffer:"),
                      wxT("/AudioIO/LatencyDuration"),
                      DEFAULT_LATENCY_DURATION,
                      9);
         S.AddUnits(_("milliseconds (higher = more latency)"));
#endif

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
#if 0
#if USE_PORTAUDIO_V19
   if (!AudioIO::ValidateDeviceNames(mPlay->GetStringSelection(), mRec->GetStringSelection())) {
      wxMessageBox(_("Playback and Recording device must use the same type, i.e., MME, DirectSound, etc."));
      return false;
   }
#endif
#endif

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   double latencyDuration = DEFAULT_LATENCY_DURATION;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);
   if (latencyDuration < 0) {
      gPrefs->Write(wxT("/AudioIO/LatencyDuration"), DEFAULT_LATENCY_DURATION);
   }

#if 0
#if USE_PORTMIXER
   if (gAudioIO)
      gAudioIO->HandleDeviceChange();
#endif // USE_PORTMIXER
#endif
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


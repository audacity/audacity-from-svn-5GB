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
#include <wx/intl.h>

#include "../Prefs.h"
#include "../AudioIO.h"
#include "../Project.h"
#include "../Internat.h"
#include "../ShuttleGui.h"
#include "portaudio.h"
#include "AudioIOPrefs.h"

AudioIOPrefs::AudioIOPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   SetLabel(_("Audio I/O"));         // Provide visual label
   SetName(_("Audio I/O"));          // Provide audible label
   Populate();
}

void AudioIOPrefs::Populate( )
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

   // Fit(); // JKC: Doesn't seem to make any difference...
}

/// Gets the lists of names and lists of labels which are
/// used in the choice controls.
/// The names are what the user sees in the wxChoice.
/// The corresponding labels are what gets stored.
void AudioIOPrefs::GetNamesAndLabels()
{
   // Get lists of devices both for play and record.
   int j;
   wxString Name;
   wxString Label;

#if USE_PORTAUDIO_V19
   int nDevices = Pa_GetDeviceCount();
#else
   int nDevices = Pa_CountDevices();
#endif

   for(j=0; j<nDevices; j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      Name = DeviceName(info);
      Label = Name;
      if (info->maxOutputChannels > 0) {
         mmPlayNames.Add( Name );
         mmPlayLabels.Add( Label );
      }
      if (info->maxInputChannels > 0) {
         mmRecordNames.Add( Name );
         mmRecordLabels.Add( Label );
      }
   }

   // Channel counts, mono, stereo etc...
   const int numChannels = 16;
   for(int c=0; c<numChannels; c++)
   {
      mmChannelNames.Add(  wxString::Format(wxT("%d"), c+1));
      mmChannelLabels.Add( c+1 );
   }
   mmChannelNames[0] = wxString::Format(_("1 (Mono)"));
   mmChannelNames[1] = wxString::Format(_("2 (Stereo)"));
}

void AudioIOPrefs::PopulateOrExchange( ShuttleGui & S )
{
   /// \todo
   /// JKC: I think setting paths in gPrefs is bad practice.
   /// Suppose we are using gPrefs from elsewhere at the same time?
   /// Change these all to full paths?
   gPrefs->SetPath(wxT("/AudioIO"));

   S.SetBorder( 2 );

   S.StartHorizontalLay(wxEXPAND, 0 );
   S.StartStatic( _("Playback"),1 );
   {
      S.StartMultiColumn(2, wxEXPAND);
      S.SetStretchyCol(1);
      S.TieChoice( _("Device") + wxString(wxT(":")), wxT("PlaybackDevice"), 
         wxT(""), mmPlayNames, mmPlayLabels );

      S.AddPrompt( _("Using:") );
      wxString ver = _("Portaudio v");
#if USE_PORTAUDIO_V19
      ver += wxT("19");
#else
      ver += wxT("18");
#endif
      S.AddFixedText( ver );
      S.EndMultiColumn();
   }                              
   S.EndStatic();
   S.StartStatic( _("Recording"), 1 );
   {
      S.StartMultiColumn(2, wxEXPAND);
      S.SetStretchyCol(1);
      S.TieChoice( _("Device") + wxString(wxT(":")), wxT("RecordingDevice"), 
         wxT(""), mmRecordNames, mmRecordLabels );
      S.TieChoice( _("Channels") + wxString(wxT(":")), wxT("RecordChannels"), 
         2, mmChannelNames, mmChannelLabels );
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.EndHorizontalLay();
   S.StartStatic( _("Playthrough") );
   {
      S.TieCheckBox( _("&Play other tracks while recording new one"),
         wxT("Duplex"),true);
#ifdef __MACOSX__
      S.TieCheckBox( _("&Hardware Playthrough: Play new track while recording it"),
         wxT("Playthrough"),false);
      S.TieCheckBox( _("&Software Playthrough: Play new track while recording it"),
         wxT("SWPlaythrough"),false);
#else
      S.TieCheckBox( _("&Software Playthrough: Play new track while recording it (uncheck when recording \"stereo mix\")"),
         wxT("SWPlaythrough"),false);
#endif

   }
   S.EndStatic();
   S.StartHorizontalLay( wxEXPAND, 0 );
   S.StartStatic( _("Effects Preview"),1 );
   {
      S.StartThreeColumn();
      S.TieTextBox( _("Length of preview:"), wxT("EffectsPreviewLen"), 3.0,9);
      S.AddUnits( _("seconds") );
      S.EndThreeColumn();
   }
   S.EndStatic();
   S.StartStatic( _("Cut Preview"),1 );
   {
      S.StartThreeColumn();
      S.TieTextBox( _("Preview before cut region:"), wxT("CutPreviewBeforeLen"),1.0,9);
      S.AddUnits(  _("seconds") );
      S.TieTextBox( _("Preview after cut region:"),wxT("CutPreviewAfterLen"), 1.0,9);
      S.AddUnits(  _("seconds") );
      S.EndThreeColumn();
   }
   S.EndStatic();
   S.EndHorizontalLay();
   S.StartHorizontalLay( wxEXPAND, 0 );

   S.StartStatic( _("Latency"),1 );
   {
      S.StartThreeColumn();
#if USE_PORTAUDIO_V19
      // only show the following controls if we use Portaudio v19, because
      // for Portaudio v18 we always use default buffer sizes
      S.TieTextBox( _("Audio to buffer:\n(higher = more latency)"),wxT("LatencyDuration"),
         DEFAULT_LATENCY_DURATION,9);
      S.AddUnits(  _("milliseconds") );
#endif
      S.TieTextBox( _("Latency correction:\n(negative = backwards)"),wxT("LatencyCorrection"),
         DEFAULT_LATENCY_CORRECTION,9);
      S.AddUnits(  _("milliseconds") );
      S.EndThreeColumn();
   }
   S.EndStatic();
   S.StartStatic( _("Seek Time when playing"),1 );
   {
      S.StartThreeColumn();
      S.TieTextBox( _("Short period:"), wxT("SeekShortPeriod"),1.0,9);
      S.AddUnits(  _("seconds") );
      S.TieTextBox( _("Long period:"),wxT("SeekLongPeriod"), 15.0,9);
      S.AddUnits(  _("seconds") );
      S.EndThreeColumn();
   }
   S.EndStatic();

   gPrefs->SetPath(wxT("/"));
}


// JKC: This is some old code that was sizing control labels all the same, 
// even if in different static controls.  It made for a nicer layout.
// We might want to do something like that again in future.
#if 0

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
#endif


AudioIOPrefs::~AudioIOPrefs()
{
}

bool AudioIOPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

#if USE_PORTAUDIO_V19
   double latencyDuration = DEFAULT_LATENCY_DURATION;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);
   if (latencyDuration < 0)
      gPrefs->Write(wxT("/AudioIO/LatencyDuration"), DEFAULT_LATENCY_DURATION);
#endif

#if USE_PORTMIXER
   if (gAudioIO)
      gAudioIO->HandleDeviceChange();
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


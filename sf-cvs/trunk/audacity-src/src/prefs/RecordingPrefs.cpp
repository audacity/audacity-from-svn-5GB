/**********************************************************************

  Audacity: A Digital Audio Editor

  RecordingPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class RecordingPrefs
\brief A PrefsPanel used to select recording options.

  Presents interface for user to update the various recording options
  like playthrough, latency correction, and others.

*//********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>

#include "../AudioIO.h"
#include "../Envelope.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "RecordingPrefs.h"

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

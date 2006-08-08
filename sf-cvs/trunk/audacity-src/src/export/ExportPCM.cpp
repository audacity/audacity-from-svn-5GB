/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/textctrl.h>
#include <wx/string.h>
#include <wx/window.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>

#include "sndfile.h"

#include "../Audacity.h"
#include "../FileFormats.h"
#include "../Internat.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Track.h"
#include "../WaveTrack.h"

#include "ExportPCM.h"

#ifdef __WXMAC__
#define __MOVIES__   /* Apple's Movies.h not compatible with Audacity */
/* #define __MACHELP__ */

#include <wx/mac/private.h>
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
# endif
#endif

bool ExportPCM(AudacityProject *project,
               int numChannels, wxString fName,
               bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
{
   double       rate = project->GetRate();
   wxWindow    *parent = project;
   TrackList   *tracks = project->GetTracks();
   int          sf_format = ReadExportFormatPref();
   wxString     formatStr;
   SF_INFO      info;
   SNDFILE     *sf = NULL;
   int          err;

   formatStr = sf_header_name(sf_format & SF_FORMAT_TYPEMASK);

   // Use libsndfile to export file

   info.samplerate = (unsigned int)(rate + 0.5);
   info.frames = (unsigned int)((t1 - t0)*rate + 0.5);
   info.channels = numChannels;
   info.format = sf_format;
   info.sections = 1;
   info.seekable = 0;

   // If we can't export exactly the format they requested,
   // try the default format for that header type...
   if (!sf_format_check(&info))
      info.format = (info.format & SF_FORMAT_TYPEMASK);
   if (!sf_format_check(&info)) {
      wxMessageBox(_("Cannot export audio in this format."));
      return false;
   }

   #ifdef _UNICODE
      /* sf_open doesn't handle fn_Str() in Unicode build. May or may not actually work. */
      sf = sf_open(FILENAME(fName).mb_str(), SFM_WRITE, &info);
   #else // ANSI
      sf = sf_open(FILENAME(fName).fn_str(), SFM_WRITE, &info);
   #endif // Unicode/ANSI

   if (!sf) {
      wxMessageBox(wxString::Format(_("Cannot export audio to %s"),
                                    fName.c_str()));
      return false;
   }

   sampleFormat format;
   if (sf_subtype_more_than_16_bits(info.format))
      format = floatSample;
   else
      format = int16Sample;

   int maxBlockLen = 44100 * 5;

   bool cancelling = false;

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            info.channels, maxBlockLen, true,
                            rate, format, true, mixerSpec);

   GetActiveProject()->ProgressShow(_("Export"),
      selectionOnly ?
      wxString::Format(_("Exporting the selected audio as a %s file"),
                       formatStr.c_str()) :
      wxString::Format(_("Exporting the entire project as a %s file"),
                       formatStr.c_str()));

   while(!cancelling) {
      sampleCount numSamples = mixer->Process(maxBlockLen);

      if (numSamples == 0)
         break;
      
      samplePtr mixed = mixer->GetBuffer();

      if (format == int16Sample)
         sf_writef_short(sf, (short *)mixed, numSamples);
      else
         sf_writef_float(sf, (float *)mixed, numSamples);

      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                       (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   delete mixer;

   delete[] waveTracks;                            

   err = sf_close(sf);

   if (err) {
      char buffer[1000];
      sf_error_str(sf, buffer, 1000);
      wxMessageBox(wxString::Format
                   (_("Error (file may not have been written): %hs"),
                    buffer));
   }

#ifdef __WXMAC__

   FSSpec spec;

   wxMacFilename2FSSpec(FILENAME(fName), &spec);

   FInfo finfo;
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      finfo.fdType = sf_header_mactype(sf_format & SF_FORMAT_TYPEMASK);
      finfo.fdCreator = AUDACITY_CREATOR;

      FSpSetFInfo(&spec, &finfo);
   }
#endif

   return !cancelling;
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
// arch-tag: c1f32472-520f-4864-8086-3dba0d593e84


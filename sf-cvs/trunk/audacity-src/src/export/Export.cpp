/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/file.h>
#include <wx/timer.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>

#include "Export.h"
#include "ExportPCM.h"
#include "ExportMP3.h"

#include "sndfile.h"

#include "../Audacity.h"
#include "../DirManager.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Track.h"
#include "../WaveTrack.h"

/*
 * This first function contains the code common to both
 * Export() and ExportLossy():
 */

wxString ExportCommon(AudacityProject *project,
                      wxString format, wxString extension,
                      bool selectionOnly, double t0, double t1,
                      bool *isStereo)
{
   TrackList *tracks = project->GetTracks();

   /* First analyze the selected audio, perform sanity checks, and provide
    * information as appropriate. */

   /* Tally how many are right, left, mono, and make sure at
      least one track is selected (if selectionOnly==true) */

   int numSelected = 0, numLeft = 0, numRight = 0, numMono = 0;

   TrackListIterator iter1(tracks);
   VTrack *tr = iter1.First();

   while (tr) {
      if (tr->GetKind() == VTrack::Wave) {
         if (tr->GetSelected() || !selectionOnly) {

            numSelected++;

            if (tr->GetChannel() == VTrack::LeftChannel)
               numLeft++;
            else if (tr->GetChannel() == VTrack::RightChannel)
               numRight++;
            else if (tr->GetChannel() == VTrack::MonoChannel)
               numMono++;
         }
      }

      tr = iter1.Next();
   }

   if (numSelected == 0 && selectionOnly) {
      wxMessageBox("No tracks are selected!\n"
                   "Choose Export... to export all tracks.");
      return "";
   }

   /* Detemine if exported file will be stereo or mono,
      and if mixing will occur */

   bool stereo = false;
   if (numRight > 0 || numLeft > 0)
      stereo = true;

   numRight += numMono;
   numLeft += numMono;

   if (numLeft > 1 || numRight > 1)
      if (stereo)
         wxMessageBox
             ("Your tracks will be mixed down to two stereo channels "
              "in the exported file.");
      else
         wxMessageBox
             ("Your tracks will be mixed down to a single mono channel "
              "in the exported file.");

   /* Prepare and display the filename selection dialog */

   wxString path = gPrefs->Read("/DefaultExportPath",::wxGetCwd());

   wxString fName = wxFileSelector(wxString::Format("Save %s File As:",
                                                    (const char *) format),
                                   path,
                                   extension,   // default file name
                                   extension,   // extension
                                   "*.*",
                                   wxSAVE | wxOVERWRITE_PROMPT);

   if (fName.Length() >= 256) {
      wxMessageBox
          ("Sorry, pathnames longer than 256 characters not supported.");
      return "";
   }

   if (fName == "")
      return fName;

   path =::wxPathOnly(fName);
   gPrefs->Write("/DefaultExportPath", path);

   *isStereo = stereo;

   return fName;
}

bool Export(AudacityProject *project,
            bool selectionOnly, double t0, double t1)
{
   wxString fName;
   wxString formatStr;
   wxString extension;
   int      format;
   bool     stereo;
   
   format = ReadExportFormatPref();
                         
   formatStr = sf_header_name(format & SF_FORMAT_TYPEMASK);
   extension = "." + sf_header_extension(format & SF_FORMAT_TYPEMASK);

   fName = ExportCommon(project, formatStr, extension,
                        selectionOnly, t0, t1, &stereo);

   if (fName == "")
      return false;

   return ::ExportPCM(project, stereo, fName,
                      selectionOnly, t0, t1);
}

bool ExportLossy(AudacityProject *project,
                 bool selectionOnly, double t0, double t1)
{
   /* Until we add full Ogg Vorbis support, MP3 is the
      only Lossy format we can export */

   wxString fName;
   bool stereo;

   fName = ExportCommon(project, "MP3", ".mp3",
                        selectionOnly, t0, t1, &stereo);

   if (fName == "")
      return false;

   return ::ExportMP3(project, stereo, fName,
                      selectionOnly, t0, t1);   
}

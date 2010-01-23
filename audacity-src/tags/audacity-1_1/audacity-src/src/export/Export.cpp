/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/textctrl.h>
#include <wx/file.h>
#include <wx/timer.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>

#include "Export.h"
#include "ExportPCM.h"
#include "ExportMP3.h"
#include "ExportOGG.h"
#include "ExportCL.h"

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
                      bool selectionOnly, double *t0, double *t1,
                      bool *isStereo)
{
   TrackList *tracks = project->GetTracks();

   /* First analyze the selected audio, perform sanity checks, and provide
    * information as appropriate. */

   /* Tally how many are right, left, mono, and make sure at
      least one track is selected (if selectionOnly==true) */

   int numSelected = 0, numLeft = 0, numRight = 0, numMono = 0;
   float earliestBegin = *t1, latestEnd = *t0;

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
            
            if(tr->GetOffset() < earliestBegin)
               earliestBegin = tr->GetOffset();

            if(tr->GetMaxLen() > latestEnd)
               latestEnd = tr->GetMaxLen();

         }
      }

      tr = iter1.Next();
   }

   if(*t0 < earliestBegin)
      *t0 = earliestBegin;
   
   if(*t1 > latestEnd)
      *t1 = latestEnd;

   if (numSelected == 0 && selectionOnly) {
      wxMessageBox(_("No tracks are selected!\n"
                     "Choose Export... to export all tracks."));
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
             (_("Your tracks will be mixed down to two stereo channels "
                "in the exported file."));
      else
         wxMessageBox
             (_("Your tracks will be mixed down to a single mono channel "
                "in the exported file."));

   /* Prepare and display the filename selection dialog */

   wxString path = gPrefs->Read("/DefaultExportPath",::wxGetCwd());

   wxString fName = project->GetName() + extension;
   fName = wxFileSelector(wxString::Format(_("Save %s File As:"),
                                           (const char *) format),
                          path,
                          fName,       // default file name
                          extension,   // extension
                          "*.*",
                          wxSAVE | wxOVERWRITE_PROMPT);

   if (fName.Length() >= 256) {
      wxMessageBox
          (_("Sorry, pathnames longer than 256 characters not supported."));
      return "";
   }

   if (fName == "")
      return fName;

   /*
    * Ensure that exporting a file by this name doesn't overwrite
    * one of the existing files in the project.  (If it would
    * overwrite an existing file, DirManager tries to rename the
    * existing file.)
    */

   if (!project->GetDirManager()->EnsureSafeFilename(fName))
      return "";

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
                        selectionOnly, &t0, &t1, &stereo);

   if (fName == "")
      return false;

   return ::ExportPCM(project, stereo, fName,
                      selectionOnly, t0, t1);
}

bool ExportLossy(AudacityProject *project,
                 bool selectionOnly, double t0, double t1)
{
   wxString fName;
   bool stereo;

   wxString format = gPrefs->Read("/FileFormats/LossyExportFormat", "MP3");

   if( format == "MP3" ) {
      fName = ExportCommon(project, "MP3", ".mp3",
                        selectionOnly, &t0, &t1, &stereo);

      if (fName == "")
         return false;

      return ::ExportMP3(project, stereo, fName,
                      selectionOnly, t0, t1);
   }
   else if( format == "OGG" ) {
#ifdef USE_LIBVORBIS
      fName = ExportCommon(project, "OGG", ".ogg",
                        selectionOnly, &t0, &t1, &stereo);

      if (fName == "")
         return false;

      return ::ExportOGG(project, stereo, fName,
                      selectionOnly, t0, t1);
#else
      wxMessageBox(_("Ogg Vorbis support is not included in this build of Audacity"));
#endif
   }
   else if( format == "External Program" ) {
#ifdef __WXGTK__
      wxString extension = gPrefs->Read( "/FileFormats/ExternalProgramExportExtension", "" );
      fName = ExportCommon(project, "External Program", "." + extension,
                        selectionOnly, &t0, &t1, &stereo);

      if (fName == "")
         return false;

      return ::ExportCL(project, stereo, fName,
                      selectionOnly, t0, t1);
#else
      wxMessageBox(_("Command-line exporting is only supported on UNIX"));
#endif
   }
   return false;
}


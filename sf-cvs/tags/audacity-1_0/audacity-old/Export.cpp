/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/choice.h>
#include <wx/file.h>
#include <wx/thread.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/timer.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/textfile.h>

#include "Export.h"
#include "ExportPCM.h"
#include "ExportMP3.h"
#include "ExportOGG.h"

#include "Audacity.h"
#include "DirManager.h"
#include "LabelTrack.h"
#include "Mix.h"
#include "Prefs.h"
#include "Project.h"
#include "Track.h"
#include "WaveTrack.h"

bool Export(AudacityProject *project,
            wxString format,
            bool selectionOnly, double t0, double t1)
{
   TrackList *tracks = project->GetTracks();

   /* Test to see if the format is supported. This could more easily be done
    * later when we dispatch to a specific function, but I think it's best to
    * let the user know up front if the format simply isn't supported */

   if (format != "WAV" &&
       format != "AIFF" && format != "IRCAM" && format != "AU" &&
#ifdef __WXMAC__
       format != "AIFF with track markers" &&
#endif
       format != "OGG" &&
       format != "MP3") {
      wxMessageBox(wxString::Format("Sorry, cannot export %s data (yet)."
                                    "Change the default export format in your preferences",
                                    (const char *) format));
      return false;
   }

#ifndef USE_LIBVORBIS
   if (format == "OGG") {
      wxMessageBox("Sorry, this build of Audacity does not include Ogg Vorbis support");
      return false;
   }
#endif

   /* First analyze the selected audio, perform sanity checks, and provide
    * information as appropriate. */

   /* make sure all selected tracks are at the same rate, tally how many are
    * right, left, mono, and make sure at least one track is selected */

   int numSelected = 0, numLeft = 0, numRight = 0, numMono = 0;
   float earliestBegin = t1, latestEnd = t0;
   double rate = 0;

   TrackListIterator iter1(tracks);
   VTrack *tr = iter1.First();

   while (tr) {
      if (tr->GetKind() == VTrack::Wave) {
         if (tr->selected || !selectionOnly) {

            if (rate == 0)
               rate = ((WaveTrack *) tr)->GetRate();

            if (rate != ((WaveTrack *) tr)->GetRate()) {
               wxMessageBox( "This version of Audacity requires that all tracks\n"
                             "match the project sample rate to play or export.\n"
                             "To change the sample rate of a track, click on its title.");
               return false;
            }

            numSelected++;

            if (tr->channel == VTrack::LeftChannel)
               numLeft++;
            if (tr->channel == VTrack::RightChannel)
               numRight++;
            if (tr->channel == VTrack::MonoChannel)
               numMono++;
            
            if(tr->tOffset < earliestBegin)
               earliestBegin = tr->tOffset;
            if(tr->GetMaxLen() > latestEnd)
               latestEnd = tr->GetMaxLen();
         }
      }

      tr = iter1.Next();
   }

   if(earliestBegin > t0)
      t0 = earliestBegin;
   if(latestEnd < t1)
      t1 = latestEnd;

   if (numSelected == 0 && selectionOnly) {
      wxMessageBox("No tracks are selected!\n"
                   "Choose Export... to export all tracks.");
      return false;
   }

   /* Detemine if exported file will be stereo or mono, and if mixing will occur */

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

   // account for "AIFF with track markers"
   wxString dlgExt = "." + format.BeforeFirst(' ').Lower();
   wxString defaultName;
   wxString projectName = project->GetName();
   if (projectName.Length() > 4 && projectName.Right(4).Upper()==".AUP")
      defaultName = projectName.Left(projectName.Length()-4)+dlgExt;
   else
      defaultName = projectName + dlgExt;

   wxString path = gPrefs->Read("/DefaultExportPath",::wxGetCwd());

   wxString fName;
   wxString nameOnly;
   wxString pathOnly;
   wxString extension;
   bool fileOkay;

   do {
      fileOkay = true;

      fName = wxFileSelector(wxString::Format("Save %s File As:",
                                              (const char *) format),
                             path,
                             defaultName,   // default file name
                             dlgExt,   // extension
                             "*.*",
                             wxSAVE | wxOVERWRITE_PROMPT);

      if (fName.Length() >= 256) {
         wxMessageBox
            ("Sorry, pathnames longer than 256 characters not supported.");
         return false;
      }
      
      if (fName == "")
         return false;

      ::wxSplitPath(fName, &pathOnly, &nameOnly, &extension);
      gPrefs->Write("/DefaultExportPath", pathOnly);

      if ((nameOnly.Left(1)=="." && extension=="") ||
          (nameOnly=="" && extension!="")) {
         wxString prompt =
            "Are you sure you want to save the file as \""+
            ::wxFileNameFromPath(fName)+"\"?\n";

         int action = wxMessageBox(prompt,
                                   "Warning",
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   project);

         fileOkay = (action == wxYES);
         continue;
      }
      
      /*
       * Check the extension - add the default if it's not there,
       * and warn user if it's abnormal.
       */
      
      wxString defExt = format.BeforeFirst(' ');
      wxString defExt3 = defExt;
      if (defExt.Length() > 3)
         defExt3 = defExt.Left(3);
      
      if (extension == "") {
         #ifdef __WXMSW__
         // Windows prefers 3-char uppercase extensions
         extension = defExt3;
         #else
         // Linux and Mac prefer lowercase extensions
         extension = defExt.Lower();
         #endif
      }
      else if (extension.Upper() != defExt.Upper() &&
               extension.Upper() != defExt3.Upper()) {
         #ifdef __WXMSW__
         // Windows prefers 3-char extensions
         defExt = defExt3;
         #endif

         wxString prompt =
            "You are about to save a "+format+" file with the name \""+
            nameOnly+"."+extension+"\".\n"+
            "Normally these files end in \"."+defExt+"\", and\n"+
            "some programs will not open files with nonstandard "+
            "extensions.\n"
            "Are you sure you want to save the file under this name?";         

         int action = wxMessageBox(prompt,
                                   "Warning",
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   project);

         if (action == wxYES)
            fileOkay = true;
         else {
            fileOkay = false;
            path = pathOnly;
            defaultName = nameOnly + "." + extension;
         }
      }

      fName = pathOnly + wxFILE_SEP_PATH + 
         nameOnly + "." + extension;

   } while (!fileOkay);

   /*
    * Ensure that exporting a file by this name doesn't overwrite
    * one of the existing files in the project.  (If it would
    * overwrite an existing file, DirManager tries to rename the
    * existing file.)
    */

   if (!project->GetDirManager()->EnsureSafeFilename(fName))
      return false;

   bool returnValue;

   wxString tempName = fName;
   int tempIndex = 1;

   while(wxFileExists(tempName)) {
      tempName = wxString::Format("%s%d", (const char *)fName, tempIndex);
      tempIndex++;
   }

   /* Finally, dispatch to the correct procedure... 
    * These functions take too many parameters, almost to the point where I
    * am tempted to create a structure to contain this data... */
   if (format == "WAV" ||
       format == "AIFF" ||
       format == "IRCAM" ||
       format == "AU" || format == "AIFF with track markers")
      returnValue = ExportPCM(project, format, stereo, tempName,
                       selectionOnly, t0, t1);
   else if (format == "MP3")
      returnValue = ExportMP3(project, stereo, tempName,
                       selectionOnly, t0, t1);
#ifdef USE_LIBVORBIS
   else if (format == "OGG")
      returnValue = ExportOGG(project, stereo, tempName,
                              selectionOnly, t0, t1);
#endif

   if (returnValue == true && tempName != fName) {
      /* rename the file to the correct name */
      wxRemoveFile(fName);
      wxRenameFile(tempName, fName);
   }

   return returnValue;
}

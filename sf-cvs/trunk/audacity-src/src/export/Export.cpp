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

#include "../Audacity.h"
#include "../DirManager.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Track.h"
#include "../WaveTrack.h"

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
       format != "MP3") {
      wxMessageBox(wxString::Format("Sorry, cannot export %s data (yet). "
                                    "Change the default export format in your preferences.",
                                    (const char *) format));
      return false;
   }

   /* First analyze the selected audio, perform sanity checks, and provide
    * information as appropriate. */

   /* make sure all selected tracks are at the same rate, tally how many are
    * right, left, mono, and make sure at least one track is selected */

   int numSelected = 0, numLeft = 0, numRight = 0, numMono = 0;
   double rate = 0;

   TrackListIterator iter1(tracks);
   VTrack *tr = iter1.First();

   while (tr) {
      if (tr->GetKind() == VTrack::Wave) {
         if (tr->GetSelected() || !selectionOnly) {

            if (rate == 0)
               rate = ((WaveTrack *) tr)->GetRate();

            if (rate != ((WaveTrack *) tr)->GetRate()) {
               wxMessageBox("Cannot export tracks with different rates.");
               return false;
            }

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
   wxString extension = "." + format.BeforeFirst(' ').Lower();

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
      return false;
   }

   if (fName == "")
      return false;

   path =::wxPathOnly(fName);
   gPrefs->Write("/DefaultExportPath", path);

   /* Finally, dispatch to the correct procedure... 
    * These functions take too many parameters, almost to the point where I
    * am tempted to create a structure to contain this data... */
   if (format == "WAV" ||
       format == "AIFF" ||
       format == "IRCAM" ||
       format == "AU" || format == "AIFF with track markers")
      return ExportPCM(project, format, stereo, fName,
                       selectionOnly, t0, t1);
   else if (format == "MP3")
      return ExportMP3(project, stereo, fName,
                       selectionOnly, t0, t1);

   /* Execution should never reach this point...!
      Return false only so we don't get a compiler warning */
   return false;
}

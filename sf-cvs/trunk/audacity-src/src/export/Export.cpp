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
#include "../Internat.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Track.h"
#include "../WaveTrack.h"
#include "../widgets/Warning.h"

/* Declare Static functions */
static wxString ExportCommon(AudacityProject *project,
                             wxString format, wxString extension,
                             bool selectionOnly, double *t0, double *t1,
                             int *numChannels,
                             wxString &actualName, bool downMix = true);


                      
/*
 * This first function contains the code common to both
 * Export() and ExportLossy()
 *
 * For safety, if the file already exists it stores the filename
 * the user wants in actualName, and returns a temporary file name.
 * The calling function should rename the file when it's successfully
 * exported.
 */
wxString ExportCommon(AudacityProject *project,
                      wxString format, wxString defaultExtension,
                      bool selectionOnly, double *t0, double *t1,
                      int *numChannels,
                      wxString &actualName, bool downMix)
{
   TrackList *tracks = project->GetTracks();

   /* First analyze the selected audio, perform sanity checks, and provide
    * information as appropriate. */

   /* Tally how many are right, left, mono, and make sure at
      least one track is selected (if selectionOnly==true) */

   int numSelected = 0, numLeft = 0, numRight = 0, numMono = 0;
   float earliestBegin = *t1;
   float latestEnd = *t0;

   TrackListIterator iter1(tracks);
   Track *tr = iter1.First();

   while (tr) {
      if (tr->GetKind() == Track::Wave) {
         if (tr->GetSelected() || !selectionOnly) {

            numSelected++;

            if (tr->GetChannel() == Track::LeftChannel)
               numLeft++;
            else if (tr->GetChannel() == Track::RightChannel)
               numRight++;
            else if (tr->GetChannel() == Track::MonoChannel) {
               // It's a mono channel, but it may be panned
               float pan = ((WaveTrack*)tr)->GetPan();
               
               if (pan == -1.0)
                  numLeft++;
               else if (pan == 1.0)
                  numRight++;
               else if (pan == 0)
                  numMono++;
               else {
                  // Panned partially off-center. Mix as stereo.
                  numLeft++;
                  numRight++;
               }
            }
            if(tr->GetOffset() < earliestBegin)
               earliestBegin = tr->GetOffset();

            if(tr->GetEndTime() > latestEnd)
               latestEnd = tr->GetEndTime();

         }
      }

      tr = iter1.Next();
   }

   if(*t0 < earliestBegin)
      *t0 = earliestBegin;
   
   if(*t1 > latestEnd)
      *t1 = latestEnd;

   if (numSelected == 0 && selectionOnly) {
      wxMessageBox(_("No tracks are selected! Use Ctrl-A (Select All)\nChoose Export... to export all tracks."),
                     _("Unable to export"),
                     wxOK | wxICON_INFORMATION);

      return wxT("");
   }
   
   /* Detemine if exported file will be stereo or mono or multichannel,
      and if mixing will occur */

   int channels = numSelected;
   if( downMix || ( channels <= 2 ) )
   {
      if (numRight > 0 || numLeft > 0)
         channels = 2;
      else
         channels = 1;

      numRight += numMono;
      numLeft += numMono;
   
      if (numLeft > 1 || numRight > 1)
         if (channels == 2) {
            ShowWarningDialog(project, wxT("MixStereo"),
                              _("Your tracks will be mixed down to two stereo channels in the exported file."));
         }
         else {
            ShowWarningDialog(project, wxT("MixMono"),
                              _("Your tracks will be mixed down to a single mono channel in the exported file."));
         }
   }
   else
   {
      ShowWarningDialog(project, wxT("MixMulti"), _("Each of your tracks will be "
               "saved as separate channels in the exported file."));
   }

   /* Prepare and display the filename selection dialog */

   wxString path = gPrefs->Read(wxT("/DefaultExportPath"),
                                FROMFILENAME(::wxGetCwd()));
   wxString nameOnly;
   wxString extension;
   wxString defaultName = project->GetName();
   wxString fName;
   wxString maskString;
   wxString endOfPathSep;

   #if 0 // this code shouldn't be here --dmazzoni
   //MERGE exercise exception
   if (defaultName == wxT("ThrowExceptionOnExport")) {  //lda
      throw("Exercise exception");
   }
   #endif

   if (defaultExtension.Left(1) == wxT("."))
      defaultExtension =
         defaultExtension.Right(defaultExtension.Length()-1);

   maskString.Printf(wxT("%s files (*.%s)|*.%s|All files (*.*)|*.*"), format.c_str(),
                     defaultExtension.c_str(), defaultExtension.c_str());

   bool fileOkay;

   do {
      fileOkay = true;

      fName = defaultName + wxT(".") + defaultExtension;
      fName = wxFileSelector(wxString::Format(_("Save %s File As:"),
                                              format.c_str()),
                             path,
                             fName,       // default file name
                             defaultExtension,
                             maskString,
                             wxSAVE | wxOVERWRITE_PROMPT);
      
      if (fName.Length() >= 256) {
         wxMessageBox
            (_("Sorry, pathnames longer than 256 characters not supported."));
         return wxT("");
      }
      
      if (fName == wxT(""))
         return wxT("");

      ::wxSplitPath(fName, &path, &nameOnly, &extension);

      //
      // Make sure the user doesn't accidentally save the file
      // as an extension with no name, like just plain ".wav".
      //

      if ((nameOnly.Left(1)==wxT(".") && extension==wxT("")) ||
          (nameOnly==wxT("") && extension!=wxT(""))) {
         wxString prompt =
            _("Are you sure you want to save the file as \"")+
            ::wxFileNameFromPath(fName)+wxT("\"?\n");
         
         int action = wxMessageBox(prompt,
                                   wxT("Warning"),
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   project);
         
         fileOkay = (action == wxYES);
         continue;
      }

      //
      // Check the extension - add the default if it's not there,
      // and warn user if it's abnormal.
      //

      wxString defaultExtension3 = defaultExtension;
      if (defaultExtension.Length() > 3)
         defaultExtension = defaultExtension.Left(3);
      
      if (extension == wxT("")) {
         #ifdef __WXMSW__
         // Windows prefers 3-char uppercase extensions
         extension = defaultExtension;
         #else
         // Linux and Mac prefer lowercase extensions
         extension = defaultExtension.Lower();
         #endif
      }
      else if (extension.Upper() != defaultExtension.Upper() &&
               extension.Upper() != defaultExtension3.Upper()) {
         #ifdef __WXMSW__
         // Windows prefers 3-char extensions
         defaultExtension3 = defaultExtension3;
         #endif

         wxString prompt;
         prompt.Printf(_("You are about to save a %s file with the name %s.\nNormally these files end in %s, and some programs will not open files with nonstandard extensions.\nAre you sure you want to save the file under this name?"),
                       format.c_str(),
                       (wxT("\"")+nameOnly+wxT(".")+extension+wxT("\"")).c_str(),
                       (wxT("\".")+defaultExtension+wxT("\"")).c_str());

         int action = wxMessageBox(prompt,
                                   wxT("Warning"),
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   project);

         if (action == wxYES)
            fileOkay = true;
         else {
            fileOkay = false;
            defaultName = nameOnly + wxT(".") + extension;
         }
      }

      if (path.Length() > 0 && path.Last() == wxFILE_SEP_PATH)
         endOfPathSep = wxT("");
      else
         endOfPathSep = wxFILE_SEP_PATH;

      fName = path + endOfPathSep + 
         nameOnly + wxT(".") + extension;
   } while(!fileOkay);

   /*
    * Ensure that exporting a file by this name doesn't overwrite
    * one of the existing files in the project.  (If it would
    * overwrite an existing file, DirManager tries to rename the
    * existing file.)
    */

   if (!project->GetDirManager()->EnsureSafeFilename(wxFileName(fName)))
      return wxT("");

   gPrefs->Write(wxT("/DefaultExportPath"), path);

   *numChannels = channels;

   /*
    * To be even MORE safe, return a temporary file name based
    * on this one...
    */

   actualName = fName;

   int suffix = 0;
   while(::wxFileExists(FILENAME(fName))) {
      fName = path + endOfPathSep + 
         nameOnly + wxString::Format(wxT("%d"), suffix) + wxT(".") + extension;
      suffix++;
   }

   return fName;
}

bool Export(AudacityProject *project,
            bool selectionOnly, double t0, double t1)
{
   wxString fName;
   wxString formatStr;
   wxString extension;
   wxString actualName;
   bool     success;
   int      format;
   int      numChannels;
   
	try {  //lda

   format = ReadExportFormatPref();

   formatStr = sf_header_name(format & SF_FORMAT_TYPEMASK);
   extension = wxT(".") + sf_header_extension(format & SF_FORMAT_TYPEMASK);

   bool downMix = gPrefs->Read( wxT("/FileFormats/ExportDownMix" ), true );
   fName = ExportCommon(project, formatStr, extension,
                        selectionOnly, &t0, &t1, &numChannels,
                        actualName, downMix);

   if (fName == wxT(""))
      return false;

   success = ::ExportPCM(project, numChannels, fName,
                         selectionOnly, t0, t1);

   if (success && actualName != fName)
      ::wxRenameFile(FILENAME(fName), FILENAME(actualName));

   return success;
}
   catch(...) {
      wxMessageBox(wxString::Format(_("File may be invalid or corrupted: %s"), 
                   (const wxChar *)project->GetName()), _("Error exporting file or project"),
                   wxOK | wxCENTRE);
      return false;
   }
}

bool ExportLossy(AudacityProject *project,
                 bool selectionOnly, double t0, double t1)
{
   wxString fName;
   int numChannels;
   wxString actualName;
   bool     success = false;
   wxString format = gPrefs->Read(wxT("/FileFormats/LossyExportFormat"), wxT("MP3"));

	try { //lda
   if( format == wxT("MP3") ) {
      fName = ExportCommon(project, wxT("MP3"), wxT(".mp3"),
                           selectionOnly, &t0, &t1, &numChannels,
                           actualName);

      if (fName == wxT(""))
         return false;

      success = ::ExportMP3(project, (numChannels == 2), fName,
                            selectionOnly, t0, t1);
   }
   else if( format == wxT("OGG") ) {
#ifdef USE_LIBVORBIS
      fName = ExportCommon(project, wxT("OGG"), wxT(".ogg"),
                           selectionOnly, &t0, &t1, &numChannels,
                           actualName);

      if (fName == wxT(""))
         return false;

      success = ::ExportOGG(project, (numChannels == 2), fName,
                      selectionOnly, t0, t1);
#else
      wxMessageBox(_("Ogg Vorbis support is not included in this build of Audacity"));
#endif
   }
   else if( format == wxT("External Program") ) {
#ifdef __WXGTK__
      wxString extension = gPrefs->Read( wxT("/FileFormats/ExternalProgramExportExtension"), wxT("") );
      fName = ExportCommon(project, wxT("External Program"), wxT(".") + extension,
                           selectionOnly, &t0, &t1, &numChannels,
                           actualName);

      if (fName == wxT(""))
         return false;

      success = ::ExportCL(project, (numChannels == 2), fName,
                           selectionOnly, t0, t1);
#else
      wxMessageBox(_("Command-line exporting is only supported on UNIX"));
#endif
   }

   if (success && actualName != fName)
      ::wxRenameFile(FILENAME(fName), FILENAME(actualName));

   return success;
}
   catch(...) {
      wxMessageBox(wxString::Format(_("File may be invalid or corrupted: %s"), 
                   (const wxChar *)project->GetName()), _("Error exporting file or project"),
                   wxOK | wxCENTRE);
      return false;
   }
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
// arch-tag: e6901653-9e2a-4a97-8ba8-377928b8e45a


/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.cpp

  Dominic Mazzoni

  This file contains a general function which will import almost
  any type of sampled audio file (i.e. anything except MIDI)
  and return the tracks that were imported.  This function just
  figures out which one to call; the actual importers are in
  ImportPCM, ImportMP3, ImportOGG, and ImportRawData.

**********************************************************************/

#include <wx/defs.h>

#ifdef __WXMAC__
#include <Files.h>
#include <Resources.h>
void wxMacFilename2FSSpec( const char *path , FSSpec *spec ) ;
#endif

#include "Audacity.h"

#include <wx/msgdlg.h>
#include <wx/string.h>

#include "ImportPCM.h"
#include "ImportMP3.h"
#include "ImportOGG.h"
#include "ImportRaw.h"

#include "Project.h"

// General purpose function used by importers
wxString TrackNameFromFileName(wxString fName)
{
   return fName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
}

// returns number of tracks imported
int Import(AudacityProject *project,
           wxString fName,
           WaveTrack *** tracks)
{
   bool success;
   int numTracks = 0;
   DirManager *dirManager = project->GetDirManager();
   wxWindow *parent = project;

   if (!fName.Right(3).CmpNoCase("aup")) {
      wxMessageBox("Audacity does not support importing Audacity Projects.\n"
                   "Please use the Open command instead of Import.",
                   "Import audio file", wxOK | wxCENTRE, parent);
      return 0;
   }

   if (!fName.Right(3).CmpNoCase("mid") ||
       !fName.Right(3).CmpNoCase("midi")) {
      wxMessageBox("Please use the Import MIDI command instead.",
                   "Import audio file", wxOK | wxCENTRE, parent);
      return 0;
   }
   
   bool isMP3 = false;

   if (!fName.Right(3).CmpNoCase("mp3") ||
       !fName.Right(3).CmpNoCase("mp2") ||
       !fName.Right(3).CmpNoCase("mpg") ||
       !fName.Right(4).CmpNoCase("mpeg"))
      isMP3 = true;
   
#ifdef __WXMAC__
   FSSpec spec;
   FInfo finfo;
   wxMacFilename2FSSpec(fName, &spec);
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      if (finfo.fdType == 'MP3 ' ||
          finfo.fdType == 'mp3 ' ||
          finfo.fdType == 'MPG ' ||
          finfo.fdType == 'MPG3' ||
          finfo.fdType == 'MPEG')
         isMP3 = true;
   }
#endif

   if (isMP3) {

     #ifdef MP3SUPPORT
      *tracks = new WaveTrack *[2];
      success =::ImportMP3(project, fName,
                           &(*tracks)[0], &(*tracks)[1]);
      if (!success)
         return 0;

      numTracks = 1;
      if ((*tracks)[1] != NULL)
         numTracks = 2;

      return numTracks;
     #else
   
      wxMessageBox("This version of Audacity was not compiled "
                   "with MP3 support.");
      return 0;
     #endif
   }

   if (!fName.Right(3).CmpNoCase("ogg")) {
     #ifdef USE_LIBVORBIS
      success =::ImportOGG(parent, fName, tracks, &numTracks, dirManager);
      if (!success)
         return 0;

      return numTracks;
     #else
      wxMessageBox("This version of Audacity was not compiled "
                   "with Ogg Vorbis support.", "Import Ogg Vorbis",
                   wxOK | wxCENTRE, parent);
      return 0;
     #endif
   }

   if (::IsPCM(fName)) {
      *tracks = new WaveTrack *[2];
      success =::ImportPCM(parent, fName,
                           &(*tracks)[0], &(*tracks)[1], dirManager);
      if (!success)
         return 0;

      numTracks = 1;
      if ((*tracks)[1] != NULL)
         numTracks = 2;

      return numTracks;
   }

   int action = wxMessageBox("Audacity did not recognize the type "
                             "of this file.\n"
                             "Would you like to try to import it as "
                             "raw PCM audio data?",
                             "Unknown file type",
                             wxYES_NO | wxICON_EXCLAMATION,
                             parent);

   if (action == wxYES) {
      *tracks = new WaveTrack *[2];
      success =::ImportRaw(parent, fName,
                           &(*tracks)[0], &(*tracks)[1], dirManager);
      if (!success)
         return 0;

      numTracks = 1;
      if ((*tracks)[1] != NULL)
         numTracks = 2;

      return numTracks;
   }

   return 0;
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.cpp

  Joshua Haberman

  This just acts as an interface to LAME. A Lame dynamic library must
  be present

**********************************************************************/

#include <wx/dynlib.h>
#include <wx/msgdlg.h>
#include <wx/utils.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/window.h>
#include <wx/ffile.h>
#include <wx/log.h>

#include "Mix.h"
#include "WaveTrack.h"
#include "ExportMP3.h"

#include "lame.h"

typedef void (*lame_initTYPE) (lame_global_flags *);
typedef int (*lame_encode_buffer_interleavedTYPE) (lame_global_flags *,
                                                   short int[], int,
                                                   char *, int);
typedef int (*lame_encode_bufferTYPE) (lame_global_flags *,
                                                   short int[], short int[], int,
                                                   char *, int);
typedef int (*lame_encode_finishTYPE) (lame_global_flags *, char *, int);
typedef int (*lame_versionTYPE) (lame_global_flags *, char *);

#ifdef __WXMSW__
const char *libname = "lame_enc.dll";
#elif defined(__WXGTK__)
const char *libname = "libmp3lame.so";
#elif defined(__WXMAC__)
const char *libname = "LAMELib";
#endif

bool ExportMP3(bool stereo, double rate, wxString fName, wxWindow * parent,
               TrackList * tracks, bool selectionOnly, double t0,
               double t1)
{
   wxLogNull logNo;             /* temporarily disable wxWindows error messages */

   wxDllType libHandle = NULL;

   /* Load the library and resolve all the function names. */

   if (wxFileExists(wxGetCwd() + wxFILE_SEP_PATH + libname))
      libHandle = wxDllLoader::LoadLibrary(wxGetCwd() + wxFILE_SEP_PATH + libname);

   if (!libHandle)
      libHandle = wxDllLoader::LoadLibrary(libname);

   if (!libHandle) {
      wxMessageBox
          ("Could not find mp3 encoder library. Audacity requires a library"
           +
           wxString::
           Format(" file called %s which you must download or build",
                  libname));
      return false;
   }
   
   lame_initTYPE lame_init =
       (lame_initTYPE) wxDllLoader::GetSymbol(libHandle,
                                              "lame_init");
   lame_versionTYPE lame_version =
       (lame_versionTYPE) wxDllLoader::GetSymbol(libHandle,
                                              "lame_version");
   lame_initTYPE lame_init_params =
       (lame_initTYPE) wxDllLoader::GetSymbol(libHandle,
                                              "lame_init_params");
   lame_encode_buffer_interleavedTYPE lame_encode_buffer_interleaved =
       (lame_encode_buffer_interleavedTYPE) wxDllLoader::
       GetSymbol(libHandle,
                 "lame_encode_buffer_interleaved");
   lame_encode_bufferTYPE lame_encode_buffer =
       (lame_encode_bufferTYPE) wxDllLoader::
       GetSymbol(libHandle,
                 "lame_encode_buffer");
   lame_encode_finishTYPE lame_encode_finish =
       (lame_encode_finishTYPE) wxDllLoader::GetSymbol(libHandle,
                                                       "lame_encode_finish");

   if (!lame_init ||
       !lame_init_params ||
       !(lame_encode_buffer_interleaved || lame_encode_buffer) ||
       !lame_encode_finish) {
      wxMessageBox(wxString::
                   Format("%s is not a compatible lame encoder", libname));
      return false;
   }

   // Allocate extra space for the global flags so that a new version of LAME
   // doesn't overwrite any memory!
   lame_global_flags *gf = (lame_global_flags *)new char[sizeof(lame_global_flags)+1000];

   lame_init(gf);
   
   char versionString[1000];
   if (lame_version) {
     lame_version(gf, versionString);
     // TODO: put this information in the progress dialog, maybe?
   }
   
   bool interleaved = (lame_encode_buffer_interleaved != 0);

   gf->num_channels = stereo ? 2 : 1;
   gf->in_samplerate = int (rate + 0.5);
   gf->brate = 128;              // TODO: make this configurable?
   gf->mode = stereo ? 0 : 3;    // (0, 1, 3) = (stereo, jstereo, mono)
   gf->quality = 2;              // 2 = high, 5 = medium, 9 = low

   lame_init_params(gf);

   double timeStep = 10.0;      // write in blocks of 10 secs

   sampleCount maxSamples = int (timeStep * rate + 0.5);

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;
   bool cancelling = false;
   long bytes;

   double t = t0;

   wxFFile outFile(fName, "w");
   if (!outFile.IsOpened()) {
      wxMessageBox("Unable to open target file for writing");
      return false;
   }

   int bufferSize = int (1.25 * timeStep * rate) + 7200;
   char *buffer = new char[bufferSize];
   wxASSERT(buffer);

   while (t < t1 && !cancelling) {

      double deltat = timeStep;
      if (t + deltat > t1)
         deltat = t1 - t;

      sampleCount numSamples = int (deltat * rate + 0.5);

      Mixer *mixer = new Mixer(stereo ? 2 : 1, numSamples, interleaved);
      wxASSERT(mixer);
      mixer->Clear();


      TrackListIterator iter(tracks);
      VTrack *tr = iter.First();
      while (tr) {
         if (tr->GetKind() == VTrack::Wave) {
            if (tr->selected || !selectionOnly) {
               if (tr->channel == VTrack::MonoChannel)
                  mixer->MixMono((WaveTrack *) tr, t, t + deltat);
               if (tr->channel == VTrack::LeftChannel)
                  mixer->MixLeft((WaveTrack *) tr, t, t + deltat);
               if (tr->channel == VTrack::RightChannel)
                  mixer->MixRight((WaveTrack *) tr, t, t + deltat);
            }
         }
         tr = iter.Next();
      }

      if (interleaved) {
        sampleType *mixed = mixer->GetBuffer();
        bytes = lame_encode_buffer_interleaved(gf, mixed, numSamples, buffer, bufferSize);       // from
      }
      else {
        sampleType *mixedLeft = mixer->GetBuffer(0);
        sampleType *mixedRight = mixer->GetBuffer(1);
        bytes = lame_encode_buffer(gf, mixedLeft, mixedRight, numSamples, buffer, bufferSize);       // from
      }

      outFile.Write(buffer, bytes);

      t += deltat;

      if (!progress && wxGetElapsedTime(false) > 500) {

         wxString message;

         if (selectionOnly)
            message =
                wxString::Format("Exporting the selected audio as an mp3");
         else
            message =
                wxString::Format("Exporting the entire project as an mp3");

         progress =
             new wxProgressDialog("Export",
                                  message,
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }

      if (progress) {
         cancelling =
             !progress->Update(int (((t - t0) * 1000) / (t1 - t0) + 0.5));
      }

      delete mixer;

   }


   bytes = lame_encode_finish(gf, buffer, 0);
   if (bytes)
      outFile.Write(buffer, bytes);

   if (progress)
      delete progress;

   delete[]buffer;
   
   delete[] (char *)gf;

   return true;
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.cpp

  Joshua Haberman

  This just acts as an interface to LAME. A Lame dynamic library must
  be present

  The difficulty in our approach is that we are attempting to use LAME
  in a way it was not designed to be used. LAME's API is reasonably
  consistant, so if we were linking directly against it we could expect
  this code to work with a variety of different LAME versions. However,
  the data structures change from version to version, and so linking
  with one version of the header and dynamically linking against a
  different version of the dynamic library will not work correctly.

  The solution is to find the lowest common denominator between versions.
  The bare minimum of functionality we must use is this:
      1. Initialize the library.
      2. Set, at minimum, the following global options:
          i.  input sample rate
          ii. input channels
      3. Encode the stream
      4. Call the finishing routine

  Just so that it's clear that we're NOT free to use whatever features
  of LAME we like, I'm not including lame.h, but instead enumerating
  here the extent of functions and structures that we can rely on being
  able to import and use from a dynamic library.

  For the record, we aim to support LAME 3.70 on. Since LAME 3.70 was
  released in April of 2000, that should be plenty.

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

/* ---------------------------------------------------------------------------*/

/* What follows is the subset of LAME functionality we can count on. */

typedef struct {
   unsigned long num_samples;
   int num_channels;
   int in_samplerate;

   /* The above are the ONLY members of this structure we can reliably read or
    * write to. */

   int space[1000];  /* to liberally accomadate for the real size of the struct */
} lame_global_flags;

/* All functions types are suffexed with _t because gcc won't let you have a type
 * and a variable of the same name */

/* NOTE: Lame >= 3.88 renames this to lame_init_old, depricating it in favor of a
 * lame_global_flags *lame_init(void). However, we'll still call the old one for
 * consistancy's sake. Please don't break this again, LAME authors... */
typedef void lame_init_t(lame_global_flags *);

/* NOTE: Same deal with this one: >= 3.88 changes it to:
 * const char *get_lame_version(), but this time they don't even leave us a
 * compatibility version! aggh! */
typedef void lame_version_t(lame_global_flags *, char *);
typedef const char *get_lame_version_t();

typedef void lame_init_params_t(lame_global_flags*);

typedef int lame_encode_buffer_t (
      lame_global_flags* gf,
      const short int    buffer_l [],
      const short int    buffer_r [],
      const int          nsamples,
      unsigned char *    mp3buf,
      const int          mp3buf_size );

typedef int lame_encode_buffer_interleaved_t(
      lame_global_flags* gf,
      short int          pcm[],
      int                num_samples,   /* per channel */
      unsigned char*     mp3buf,
      int                mp3buf_size );

typedef int lame_encode_finish_t(
      lame_global_flags *gf,
      unsigned char*     mp3buf,
      int                size );

/* ---------------------------------------------------------------------------*/

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
   
   lame_init_t* lame_init = (lame_init_t *)wxDllLoader::GetSymbol(libHandle, "lame_init_old");

   if(!lame_init)
      lame_init = (lame_init_t *) wxDllLoader::GetSymbol(libHandle, "lame_init");

   lame_version_t* lame_version = (lame_version_t *) wxDllLoader::GetSymbol(libHandle, 
                                                                        "lame_version");
   
   get_lame_version_t* get_lame_version = (get_lame_version_t *) wxDllLoader::GetSymbol(
                                                                           libHandle,
                                                                        "get_lame_version");

   lame_init_params_t *lame_init_params = (lame_init_params_t *) wxDllLoader::GetSymbol(
                                                                         libHandle,
                                                                         "lame_init_params");

   lame_encode_buffer_interleaved_t *lame_encode_buffer_interleaved =
       (lame_encode_buffer_interleaved_t *) wxDllLoader::GetSymbol(libHandle,
                                                             "lame_encode_buffer_interleaved");

   lame_encode_buffer_t *lame_encode_buffer = (lame_encode_buffer_t *) wxDllLoader::GetSymbol(
                                                                          libHandle,
                                                                          "lame_encode_buffer");
   lame_encode_finish_t *lame_encode_finish =
       (lame_encode_finish_t *) wxDllLoader::GetSymbol(libHandle, "lame_encode_finish");

   if (!lame_init ||
       !lame_init_params ||
       !(lame_encode_buffer_interleaved || lame_encode_buffer) ||
       !(lame_version || get_lame_version) ||
       !lame_encode_finish) {
      wxMessageBox(wxString::
                   Format("%s is not a compatible lame encoder", libname));
      return false;
   }

   // Allocate extra space for the global flags so that a new version of LAME
   // doesn't overwrite any memory!
   lame_global_flags *gf = (lame_global_flags *)new char[100000];

   lame_init(gf);
   
   char versionString[1000];
   if (lame_version) {
     lame_version(gf, versionString);
     // TODO: put this information in the progress dialog, maybe?
   }
   
   bool interleaved = (lame_encode_buffer_interleaved != 0);

   gf->num_channels = stereo ? 2 : 1;
   gf->in_samplerate = int (rate + 0.5);

   /* It's impossible to set these and maintain compatibility with different versions
    * of LAME (without customizing it for each different version, which would be insane.
    * We just have to rely on sane defaults. */
   /*gf->brate = 128;              // TODO: make this configurable?
   gf->mode = stereo ? 0 : 3;    // (0, 1, 3) = (stereo, jstereo, mono)
   gf->quality = 2;              // 2 = high, 5 = medium, 9 = low */

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
   unsigned char *buffer = new unsigned char[bufferSize];
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

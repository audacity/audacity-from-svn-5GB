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

#ifdef __WXGTK__

   /* --------------------------------------------------------------------------*/

   /* What follows is the subset of LAME functionality we can count on. */

   typedef struct {
      unsigned long num_samples;
      int num_channels;
      int in_samplerate;

      /* The above are the ONLY members of this structure we can reliably read
       * or write to. */

      int space[1000];  /* to liberally accomadate for the real size of the struct */
   } lame_global_flags;

   /* All functions types are suffexed with _t because gcc won't let you have a
    * type and a variable of the same name */

   /* NOTE: Lame >= 3.88 renames this to lame_init_old, depricating it in favor
    * of a lame_global_flags *lame_init(void). However, we'll still call the
    * old one for consistancy's sake. Please don't break this again, LAME
    * authors... */
   typedef void lame_init_t(lame_global_flags *);

   /* NOTE: Same deal with this one: >= 3.88 changes it to: const char
    * *get_lame_version(), but this time they don't even leave us a
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

   /* --------------------------------------------------------------------------*/

   class LinuxLAMEExporter : public MP3Exporter {
      private:
         lame_init_t* lame_init;
         lame_version_t* lame_version;
         get_lame_version_t* get_lame_version;
         lame_init_params_t* lame_init_params;
         lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
         lame_encode_finish_t* lame_encode_finish;

         lame_global_flags *mGF;
         
         bool mLibraryLoaded, mEncoding;
         char mVersion[20];

         static const int mSamplesPerChunk = 220500;
         static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);
      public:
         
         LinuxLAMEExporter() {
            mLibraryLoaded = false;
            mEncoding = false;
            mGF = NULL;
         }

         bool  LoadLibrary(wxString fileName = "") {
            if(fileName == "") fileName = "libmp3lame.so";

            wxDllType libHandle = NULL;

            if (wxFileExists(wxGetCwd() + wxFILE_SEP_PATH + fileName))
               libHandle = wxDllLoader::LoadLibrary(
                           wxGetCwd() + wxFILE_SEP_PATH + fileName);
            else
               return false;

            lame_init = (lame_init_t *)wxDllLoader::GetSymbol(libHandle, "lame_init_old");
            if(!lame_init)
               lame_init = (lame_init_t *) wxDllLoader::GetSymbol(libHandle, "lame_init");

            lame_version = (lame_version_t *) wxDllLoader::GetSymbol(libHandle, "lame_version");
            
            get_lame_version =
               (get_lame_version_t *) wxDllLoader::GetSymbol(libHandle, "get_lame_version");

            lame_init_params = 
               (lame_init_params_t *) wxDllLoader::GetSymbol(libHandle, "lame_init_params");

            lame_encode_buffer_interleaved =
                (lame_encode_buffer_interleaved_t *) wxDllLoader::GetSymbol(libHandle,
                                                                      "lame_encode_buffer_interleaved");

            lame_encode_finish =
                (lame_encode_finish_t *) wxDllLoader::GetSymbol(libHandle, "lame_encode_finish");

            if (!lame_init ||
                !lame_init_params ||
                !lame_encode_buffer_interleaved ||
                !(lame_version || get_lame_version) ||
                !lame_encode_finish) {
               return false;
            }

            mGF = new lame_global_flags;
            lame_init(mGF);
            mLibraryLoaded = true;
            return true;
         }

      bool ValidLibraryLoaded() { return mLibraryLoaded; }

      const char *GetLibraryVersion() {
         if(!mLibraryLoaded) return "";

         if(get_lame_version)
            return get_lame_version();
         else {
            lame_version(mGF, mVersion);
            return mVersion;
         }
      }

      int InitializeStream(int channels, int sampleRate) {
         if(!mLibraryLoaded) return -1;

         mGF->num_channels = channels;
         mGF->in_samplerate = sampleRate;

         lame_init_params(mGF);

         mEncoding = true;
         return mSamplesPerChunk;
      }

      int GetOutBufferSize() {
         return mOutBufferSize;
      }

      int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
         if(!mEncoding) return -1;

         return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
            outbuffer, mOutBufferSize);
      }

      int EncodeRemainder(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[]) {
         return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
            mOutBufferSize);
      }

      int FinishStream(unsigned char outbuffer[]) {
         mEncoding = false;
         return lame_encode_finish(mGF, outbuffer, mOutBufferSize);
      }

      void CancelEncoding() { mEncoding = false; }

      int GetQualityVariance() { return -1; }

      void SetBitrate(int rate) { }
      int GetBitrate() { return -1; }

      void SetQuality(int quality) { }
      int GetQuality() { return -1; }
   };

LinuxLAMEExporter gLinuxLAMEExporter;
MP3Exporter *gMP3Exporter = &gLinuxLAMEExporter;

#elif defined(__WXMSW__)

#include "BladeMP3EncDLL.h"

class BladeEncExporter : public MP3Exporter {
private:
   BE_CONFIG mConf;
   BE_VERSION mVersion;
   HBE_STREAM mStreamHandle;
   bool mLibraryLoaded, mEncoding,mStereo;
   unsigned long mOutBufferSize, mInSampleNum;


   BEINITSTREAM beInitStream;
   BEENCODECHUNK beEncodeChunk;
   BEDEINITSTREAM beDeinitStream;
   BECLOSESTREAM beCloseStream;
   BEVERSION beVersion;

public:
   BladeEncExporter() {
      mLibraryLoaded = false;
      mEncoding = false;

      /* Set all the config defaults to sane values */

      memset(&mConf, 0, sizeof(BE_CONFIG));
      mConf.dwConfig = BE_CONFIG_LAME;
      mConf.format.LHV1.dwStructVersion = 1;
      mConf.format.LHV1.dwStructSize = sizeof(BE_CONFIG);
      mConf.format.LHV1.dwReSampleRate = 0;
      mConf.format.LHV1.dwBitrate = 128;
      //mConf.format.LHV1.dwMaxBitrate = 128;
      mConf.format.LHV1.nPreset = LQP_HIGH_QUALITY;
	  mConf.format.LHV1.dwMpegVersion = MPEG1;
//      mConf.format.LHV1.bCopyright = false;
//      mConf.format.LHV1.bCRC = true;
//      mConf.format.LHV1.bOriginal = false;
//      mConf.format.LHV1.bPrivate = false;
//      mConf.format.LHV1.bWriteVBRHeader = false;
//      mConf.format.LHV1.bEnableVBR = true;
//      mConf.format.LHV1.nVBRQuality = 2;
//      mConf.format.LHV1.dwVbrAbr_bps = -1;
//      mConf.format.LHV1.bNoRes = true;
   }

   bool LoadLibrary(wxString fileName) {
      wxDllType libHandle = NULL;

      if(fileName == "") fileName = "lame_enc.dll";

      if (wxFileExists(wxGetCwd() + wxFILE_SEP_PATH + fileName))
         libHandle = wxDllLoader::LoadLibrary(
                     wxGetCwd() + wxFILE_SEP_PATH + fileName);
      else
         return false;


      beInitStream = (BEINITSTREAM)wxDllLoader::GetSymbol(libHandle, "beInitStream");
      beEncodeChunk = (BEENCODECHUNK)wxDllLoader::GetSymbol(libHandle, "beEncodeChunk");
      beDeinitStream = (BEDEINITSTREAM)wxDllLoader::GetSymbol(libHandle, "beDeinitStream");
      beCloseStream = (BECLOSESTREAM)wxDllLoader::GetSymbol(libHandle, "beCloseStream");
      beVersion = (BEVERSION)wxDllLoader::GetSymbol(libHandle, "beVersion");

      if(!beInitStream ||
         !beEncodeChunk ||
         !beDeinitStream ||
         !beCloseStream ||
         !beVersion)
         return false;

      beVersion(&mVersion);
      mLibraryLoaded = true;
      return true;
   }

   bool ValidLibraryLoaded() { return mLibraryLoaded; }

   const char *GetLibraryVersion() { return NULL; }

   int InitializeStream(int channels, int sampleRate) {

      if(!mLibraryLoaded)
         return -1;

	  int modes[] = { 0, BE_MP3_MODE_MONO, BE_MP3_MODE_STEREO };
	  mConf.format.LHV1.dwSampleRate = sampleRate;
	  mConf.format.LHV1.nMode = modes[channels];

      beInitStream(&mConf, &mInSampleNum, &mOutBufferSize, &mStreamHandle);

	  if(channels == 2)
		  mStereo = true;
	  else
		  mStereo = false;

      mEncoding = true;

      return mInSampleNum;
   }

   int GetOutBufferSize() {
      if (!mEncoding)
         return -1;

	  if(mStereo)
		 return mOutBufferSize * 2;
	  else
		 return mOutBufferSize;
   }

   int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
      if(!mEncoding)
         return -1;
      
      unsigned long bytes;
      beEncodeChunk(mStreamHandle, mInSampleNum, inbuffer, outbuffer, &bytes);

      return bytes;
   }

   int EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[]) {
      if(!mEncoding)
         return -1;

      unsigned long bytes;
      beEncodeChunk(mStreamHandle, nSamples, inbuffer, outbuffer, &bytes);

      return bytes;
   }

   int FinishStream(unsigned char outbuffer[]) {
      if(!mEncoding)
         return -1;

      unsigned long bytes;
      beDeinitStream(mStreamHandle, outbuffer, &bytes);
      beCloseStream(mStreamHandle);

      mEncoding = false;
      return bytes;
   }

   void CancelEncoding() {
      beCloseStream(mStreamHandle);
   }

   int GetQualityVariance() { return -1; }
   
   void SetBitrate(int rate) { }
   int GetBitrate() { return -1; }
   void SetQuality(int quality) { }
   int GetQuality() { return -1; }

};

BladeEncExporter gBladeEncExporter;   
MP3Exporter *gMP3Exporter = &gBladeEncExporter;

#endif      


#ifdef __WXMSW__
const char *libname = "lame_enc.dll";
#elif defined(__WXMAC__)
const char *libname = "LAMELib";
#endif

bool ExportMP3(bool stereo, double rate, wxString fName, wxWindow * parent,
               TrackList * tracks, bool selectionOnly, double t0,
               double t1)
{
   wxLogNull logNo;             /* temporarily disable wxWindows error messages */

   gMP3Exporter->LoadLibrary();
   if(!gMP3Exporter->ValidLibraryLoaded()) {
      wxMessageBox("No MP3 encoding library found!");
      return false;
   }

   wxFFile outFile(fName, "w");
   if (!outFile.IsOpened()) {
      wxMessageBox("Unable to open target file for writing");
      return false;
   }

   int iRate = int (rate + 0.5);

   sampleCount inSamples = gMP3Exporter->InitializeStream(stereo ? 2 : 1, int(rate + 0.5));
   double timeStep =  (double)inSamples / rate;
   double t = t0;

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;
   bool cancelling = false;
   long bytes;


   int bufferSize = gMP3Exporter->GetOutBufferSize();
   unsigned char *buffer = new unsigned char[bufferSize];
   wxASSERT(buffer);

   while (t < t1 && !cancelling) {

      double deltat = timeStep;
      bool lastFrame;
      sampleCount numSamples = inSamples;

      if (t + deltat > t1) {
         lastFrame = true;
         deltat = t1 - t;
         numSamples = int(deltat * rate + 0.5);
      }


      Mixer *mixer = new Mixer(stereo ? 2 : 1, numSamples, true);
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
      
      sampleType *mixed = mixer->GetBuffer();

      if(lastFrame)
         bytes = gMP3Exporter->EncodeRemainder(mixed, numSamples, buffer);
      else
         bytes = gMP3Exporter->EncodeBuffer(mixed, buffer);

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


   bytes = gMP3Exporter->FinishStream(buffer);

   if (bytes)
      outFile.Write(buffer, bytes);

   if (progress)
      delete progress;

   delete[]buffer;
   
   return true;
}

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
#include <wx/filedlg.h>

#include "ExportMP3.h"
#include "Mix.h"
#include "Prefs.h"
#include "Project.h"
#include "Tags.h"
#include "WaveTrack.h"


MP3Exporter::MP3Exporter()
{
   if (gPrefs)
      mLibPath = gPrefs->Read("/MP3/MP3LibPath", "");
}

bool MP3Exporter::FindLibrary(wxWindow *parent)
{
   mLibPath = gPrefs->Read("/MP3/MP3LibPath", "");

   if (mLibPath=="" || !::wxFileExists(mLibPath)) {
   
      int action = wxMessageBox(GetLibraryMessage(),
                                "Export MP3",
                                wxYES_NO | wxICON_EXCLAMATION,
                                parent);

      if (action == wxYES) {
         wxString question;
         question.Printf("Where is %s?", (const char *)GetLibraryName());
         mLibPath = wxFileSelector(question, NULL,
                                   GetLibraryName(),        // Name
                                   "",      // Extension
                                   GetLibraryTypeString(),
                                   wxOPEN, parent);
         
         if (mLibPath == "") {
            mLibPath = "";
            gPrefs->Write("/MP3/MP3LibPath", mLibPath);
         
            return false;
         }
         
         wxString path, baseName, extension;
         ::wxSplitPath(mLibPath, &path, &baseName, &extension);
         
         if (extension != "")
            baseName += "." + extension;
         
         if (baseName != GetLibraryName()) {
         
            wxString question;
            question.Printf("Audacity was expecting a library named \"%s\".  "
                            "Are you sure you want to attempt to export MP3 "
                            "files using \"%s\"?",
                            (const char *)GetLibraryName(),
                            (const char *)baseName);

            int action = wxMessageBox(question,
                                      "Export MP3",
                                      wxYES_NO | wxICON_EXCLAMATION,
                                      parent);
            
            if (action != wxYES) {
               mLibPath = "";
               gPrefs->Write("/MP3/MP3LibPath", mLibPath);
            
               return false;
            }
         }
      }
      else {
         mLibPath = "";
         gPrefs->Write("/MP3/MP3LibPath", mLibPath);
            
         return false;
      }
      
      gPrefs->Write("/MP3/MP3LibPath", mLibPath);
   }
   
   return true;
}

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
         
         wxString GetLibraryName()
         {
            return "libmp3lame.so";
         }
         
         wxString GetLibraryTypeString()
         {
            return wxString("Shared Object files (*.so)|*.so");
         }
         
         wxString GetLibraryMessage()
         {
            return "Audacity does not export MP3 files directly, but instead uses the \n"
                   "freely available LAME library to handle MP3 file encoding.  You must \n"
                   "obtain libmp3lame.so separately, either by downloading it or building \n"
                   "it from the sources, and then locate the file for Audacity.  You only \n"
                   "need to do this once.\n\n"
                   "Would you like to locate libmp3lame.so now?";
         }

         bool  LoadLibrary() {
            wxLogNull logNo;

            wxDllType libHandle = NULL;

            if (wxFileExists(mLibPath))
               libHandle = wxDllLoader::LoadLibrary(mLibPath);
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
            return wxString::Format("LAME %s", get_lame_version()).c_str();
         else {
            lame_version(mGF, mVersion);
            return wxString::Format("LAME %s", mVersion).c_str();
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

      int GetConfigurationCaps() { return 0; }

      int GetQualityVariance() { return -1; }

      void SetBitrate(int rate) { }
      int GetBitrate() { return -1; }

      void SetQuality(int quality) { }
      int GetQuality() { return -1; }
   };

MP3Exporter *gMP3Exporter = NULL;

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new LinuxLAMEExporter();
   
   return gMP3Exporter;
}

#elif defined(__WXMAC__)

   /* --------------------------------------------------------------------------*/

   /* The following code is intended to work with LAMELib, roughly LAME
      verson 3.87, as distributed by N2MP3. */

   typedef struct {
      /* input file description */
      unsigned long num_samples;  /* number of samples. default=2^32-1    */
      int num_channels;           /* input number of channels. default=2  */
      int in_samplerate;          /* input_samp_rate. default=44.1kHz     */
      int out_samplerate;         /* output_samp_rate. (usually determined automatically)   */ 
      float scale;                /* scale input by this amount */

      /* general control params */
      int gtkflag;                /* run frame analyzer?       */
      int bWriteVbrTag;           /* add Xing VBR tag?         */
      int disable_waveheader;     /* disable writing of .wav header, when *decoding* */
      int decode_only;            /* use lame/mpglib to convert mp3 to wav */
      int ogg;                    /* encode to Vorbis .ogg file */

      int quality;                /* quality setting 0=best,  9=worst  */
      int silent;                 /* disable some status output */
      float update_interval;      /* to use Frank's time status display */
      int brhist_disp;            /* enable VBR bitrate histogram display */
      int mode;                       /* 0,1,2,3 stereo,jstereo,dual channel,mono */
      int mode_fixed;                 /* use specified the mode, do not use lame's opinion of the best mode */
      int force_ms;                   /* force M/S mode.  requires mode=1 */
      int brate;                      /* bitrate */
      float compression_ratio;          /* user specified compression ratio, instead of brate */
      int free_format;                /* use free format? */

      int space[10000];  /* to liberally accomadate for the real size of the struct */
   } lame_global_flags;

   /* All functions types are suffexed with _t because gcc won't let you have a
    * type and a variable of the same name */

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

   class MacLAMEExporter : public MP3Exporter {
      private:
         lame_init_t* lame_init;
         lame_version_t* lame_version;
         get_lame_version_t* get_lame_version;
         lame_init_params_t* lame_init_params;
         lame_encode_buffer_t* lame_encode_buffer;
         lame_encode_finish_t* lame_encode_finish;

         lame_global_flags *mGF;
         
         bool mLibraryLoaded, mEncoding;
         char mVersion[20];

         static const int mSamplesPerChunk = 220500;
         static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);

         short int *mLeftBuffer;
         short int *mRightBuffer;
         
      public:
         
         MacLAMEExporter() {
            mLibraryLoaded = false;
            mEncoding = false;
            mGF = NULL;
         }

         wxString GetLibraryName()
         {
            return "LAMELib";
         }
         
         wxString GetLibraryTypeString()
         {
            return "Shared Libraries|*";
         }
         
         wxString GetLibraryMessage()
         {
            // Must be <= 255 characters on Mac
            return "Audacity does not export MP3 files directly, but instead uses LAME, "
                   "an MP3 exporting library available separately.  See the documentation "
                   "for more information.\n\n"
                   "Would you like to locate LameLib now?";
         }

         bool  LoadLibrary() {
            wxLogNull logNo;

            wxDllType libHandle = NULL;

            if (wxFileExists(mLibPath))
               libHandle = wxDllLoader::LoadLibrary(mLibPath);
            else
               return false;

            lame_init = (lame_init_t *) wxDllLoader::GetSymbol(libHandle, "lame_init");

            lame_version = (lame_version_t *) wxDllLoader::GetSymbol(libHandle, "lame_version");
            
            get_lame_version =
               (get_lame_version_t *) wxDllLoader::GetSymbol(libHandle, "get_lame_version");

            lame_init_params = 
               (lame_init_params_t *) wxDllLoader::GetSymbol(libHandle, "lame_init_params");

            lame_encode_buffer =
                (lame_encode_buffer_t *) wxDllLoader::GetSymbol(libHandle,
                                                                "lame_encode_buffer");
            lame_encode_finish =
                (lame_encode_finish_t *) wxDllLoader::GetSymbol(libHandle, "lame_encode_finish");

            if (!lame_init ||
                !lame_init_params ||
                !lame_encode_buffer ||
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
         mGF->out_samplerate = sampleRate;
         mGF->num_samples = 0;
         
         if (channels == 1)
            mGF->mode = 3;  // mono
         else
            mGF->mode = 1;  // joint stereo

         lame_init_params(mGF);
         
         mLeftBuffer = new short[mSamplesPerChunk];
         mRightBuffer = new short[mSamplesPerChunk];

         mEncoding = true;
         return mSamplesPerChunk;
      }

      int GetOutBufferSize() {
         return mOutBufferSize;
      }
      
      int GetConfigurationCaps() {
         return MP3CONFIG_BITRATE;
      }

      int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
         if(!mEncoding) return -1;
         
         if (mGF->num_channels == 2) {
            for(int i=0; i<mSamplesPerChunk; i++) {
               mLeftBuffer[i] = inbuffer[2*i];
               mRightBuffer[i] = inbuffer[2*i+1];
            }

            return lame_encode_buffer(mGF, mLeftBuffer, mRightBuffer, mSamplesPerChunk,
                                      outbuffer, mOutBufferSize);
         }
         else {
            return lame_encode_buffer(mGF, inbuffer, inbuffer, mSamplesPerChunk,
                       outbuffer, mOutBufferSize);
         }
      }

      int EncodeRemainder(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[]) {

         if (mGF->num_channels == 2) {
            for(int i=0; i<nSamples; i++) {
               mLeftBuffer[i] = inbuffer[2*i];
               mRightBuffer[i] = inbuffer[2*i+1];
            }

            return lame_encode_buffer(mGF, mLeftBuffer, mRightBuffer, nSamples, outbuffer,
               mOutBufferSize);
         }
         else {
            return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples,
                       outbuffer, mOutBufferSize);
         }
      }

      int FinishStream(unsigned char outbuffer[]) {
         mEncoding = false;
         int result = lame_encode_finish(mGF, outbuffer, mOutBufferSize);
         
         delete[] mLeftBuffer;
         delete[] mRightBuffer;
         
         return result;
      }

      void CancelEncoding() { mEncoding = false; }

      int GetQualityVariance() { return -1; }

      void SetBitrate(int rate) { }
      int GetBitrate() { return -1; }

      void SetQuality(int quality) { }
      int GetQuality() { return -1; }
   };

MP3Exporter *gMP3Exporter = NULL;

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new MacLAMEExporter();
   
   return gMP3Exporter;
}

#elif defined(__WXMSW__)

#include "BladeMP3EncDLL.h"

class BladeEncExporter : public MP3Exporter {
private:
   BE_CONFIG mConf;
   BE_VERSION mVersion;
   HBE_STREAM mStreamHandle;
   bool mLibraryLoaded, mEncoding,mStereo;
   unsigned long mOutBufferSize, mInSampleNum;
   int mDefaultRate;


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
//      mConf.dwConfig = BE_CONFIG_LAME;
//      mConf.format.LHV1.dwStructVersion = 1;
//      mConf.format.LHV1.dwStructSize = sizeof(BE_CONFIG);
//      mConf.format.LHV1.dwReSampleRate = 0;
//      mConf.format.LHV1.dwBitrate = 128;
      //mConf.format.LHV1.dwMaxBitrate = 128;
//      mConf.format.LHV1.nPreset = LQP_HIGH_QUALITY;
//	  mConf.format.LHV1.dwMpegVersion = MPEG1;
//      mConf.format.LHV1.bCopyright = false;
//      mConf.format.LHV1.bCRC = true;
//      mConf.format.LHV1.bOriginal = false;
//      mConf.format.LHV1.bPrivate = false;
//      mConf.format.LHV1.bWriteVBRHeader = false;
//      mConf.format.LHV1.bEnableVBR = true;
//      mConf.format.LHV1.nVBRQuality = 2;
//      mConf.format.LHV1.dwVbrAbr_bps = -1;
//      mConf.format.LHV1.bNoRes = true;
      mConf.dwConfig = BE_CONFIG_MP3;
      mConf.format.mp3.wBitrate = 128;
      mConf.format.mp3.bCopyright = false;
      mConf.format.mp3.bCRC = true;
      mConf.format.mp3.bOriginal = false;
      mConf.format.mp3.bPrivate = false;
      
      mDefaultRate = 128;
   }

   wxString GetLibraryName()
   {
      return "lame_enc.dll";
   }
   
   wxString GetLibraryTypeString()
   {
      return "Dynamically Linked Libraries (*.dll)|*.dll";
   }
   
   wxString GetLibraryMessage()
   {
      return "Audacity does not export MP3 files directly, but instead uses the\n"
             "freely available LAME library to handle MP3 file encoding.  You must\n"
             "obtain lame_enc.dll separately, by downloading the LAME MP3 encoder,"
             "and then locate this file for Audacity.  You only need to do this once.\n\n"
             "Would you like to locate lame_enc.dll now?";
   }


   bool  LoadLibrary() {
      wxLogNull logNo;

      wxDllType libHandle = NULL;

      if (wxFileExists(mLibPath))
         libHandle = wxDllLoader::LoadLibrary(mLibPath);
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

   const char *GetLibraryVersion() {
      BE_VERSION ver;

      if(!mLibraryLoaded)
         return NULL;

      beVersion(&ver);

      return wxString::Format("LAME v%d.%d", ver.byMajorVersion, ver.byMinorVersion).c_str();
   }

   int InitializeStream(int channels, int sampleRate) {

      if(!mLibraryLoaded)
         return -1;

	  //int modes[] = { 0, BE_MP3_MODE_MONO, BE_MP3_MODE_STEREO };
	  //mConf.format.LHV1.dwSampleRate = sampleRate;
	  //mConf.format.LHV1.nMode = modes[channels];
      int modes[] = { 0, BE_MP3_MODE_MONO, BE_MP3_MODE_STEREO };
      mConf.format.mp3.byMode = modes[channels];
      mConf.format.mp3.dwSampleRate = sampleRate;
      mConf.format.mp3.wBitrate = mDefaultRate;

      beInitStream(&mConf, &mInSampleNum, &mOutBufferSize, &mStreamHandle);

      mEncoding = true;

      if(channels == 2) {
         mStereo = true;
         return(mInSampleNum / 2); /* convert samples_total into samples_per_channel */
      }
      else {
         mStereo = false;
         return (mInSampleNum);
      }

   }

   int GetOutBufferSize() {
      if (!mEncoding)
         return -1;

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

   int GetConfigurationCaps() {
      return MP3CONFIG_BITRATE;
   }
   
   void SetBitrate(int rate) { 
      mDefaultRate = rate;
   }

   int GetBitrate() {
      return mDefaultRate;
   }

   void SetQuality(int quality) { }
   int GetQuality() { return -1; }

};

MP3Exporter *gMP3Exporter = NULL;

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new BladeEncExporter();
   
   return gMP3Exporter;
}

#endif      

bool ExportMP3(AudacityProject *project,
               bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1)
{
   double rate = project->GetRate();
   wxWindow *parent = project;
   TrackList *tracks = project->GetTracks();

   wxLogNull logNo;             /* temporarily disable wxWindows error messages */

   bool success = GetMP3Exporter()->FindLibrary(parent);
   
   if (!success)
      return false;

   success = GetMP3Exporter()->LoadLibrary();
   if (!success) {
      wxMessageBox("Could not open MP3 encoding library!");
      gPrefs->Write("/MP3/MP3LibPath", wxString(""));

      return false;
   }

   if(!GetMP3Exporter()->ValidLibraryLoaded()) {
      wxMessageBox("Not a valid or supported MP3 encoding library!");      
      gPrefs->Write("/MP3/MP3LibPath", wxString(""));
      
      return false;
   }
   
   /* Open file for writing */

   wxFFile outFile(fName, "wb");
   if (!outFile.IsOpened()) {
      wxMessageBox("Unable to open target file for writing");
      return false;
   }
   
   /* Put ID3 tags at beginning of file */
   
   Tags *tags = project->GetTags();
   if (!tags->ShowEditDialog(project, "Edit the ID3 tags for the MP3 file"))
      return false;  // used selected "cancel"

   char *id3buffer;
   int id3len;
   bool endOfFile;
   id3len = tags->ExportID3(&id3buffer, &endOfFile);
   if (!endOfFile)
     outFile.Write(id3buffer, id3len);

   /* Export MP3 using DLL */

   long bitrate = gPrefs->Read("/FileFormats/MP3Bitrate", 128);
   GetMP3Exporter()->SetBitrate(bitrate);

   sampleCount inSamples = GetMP3Exporter()->InitializeStream(stereo ? 2 : 1, int(rate + 0.5));
   double timeStep =  (double)inSamples / rate;
   double t = t0;

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;
   bool cancelling = false;
   long bytes;


   int bufferSize = GetMP3Exporter()->GetOutBufferSize();
   unsigned char *buffer = new unsigned char[bufferSize];
   wxASSERT(buffer);

   while (t < t1 && !cancelling) {

      double deltat = timeStep;
      bool lastFrame = false;
      sampleCount numSamples = inSamples;

      if (t + deltat > t1) {
         lastFrame = true;
         deltat = t1 - t;
         numSamples = int(deltat * rate + 0.5);
      }


      Mixer *mixer = new Mixer(stereo ? 2 : 1, numSamples, true, rate);
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
         bytes = GetMP3Exporter()->EncodeRemainder(mixed, numSamples, buffer);
      else
         bytes = GetMP3Exporter()->EncodeBuffer(mixed, buffer);

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

   bytes = GetMP3Exporter()->FinishStream(buffer);

   if (bytes)
      outFile.Write(buffer, bytes);
   
   /* Write ID3 tag if it was supposed to be at the end of the file */
   
   if (endOfFile)
      outFile.Write(id3buffer, id3len);
   delete[] id3buffer;

   /* Close file */
   
   outFile.Close();
      
   /* MacOS: set the file type/creator so that the OS knows it's an MP3
      file which was created by Audacity */
      
#ifdef __WXMAC__
   FSSpec spec;
   wxMacFilename2FSSpec(fName, &spec);
   FInfo finfo;
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      finfo.fdType = 'MP3 ';
      finfo.fdCreator = AUDACITY_CREATOR;

      FSpSetFInfo(&spec, &finfo);
   }
#endif

   if (progress)
      delete progress;

   delete[]buffer;
   
   return true;
}

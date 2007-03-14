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


  Copyright 2002, 2003 Joshua Haberman.
  Some portions may be Copyright 2003 Paolo Patruno.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*******************************************************************//**

\class MP3Exporter
\brief Class used to export MP3 files

*//*****************************************************************//**

\class MP3ExporterCleanup
\brief Tiny class that is used to clean up any allocated resources 
when the program exits.  A global instance of it is created, and its
destructor does the cleanup.

*//********************************************************************/

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <wx/dynlib.h>
#include <wx/msgdlg.h>
#include <wx/utils.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/window.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/filedlg.h>
#include <wx/intl.h>

#include "../Audacity.h"
#include "ExportMP3.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../WaveTrack.h"

#ifdef __WXMAC__
#define __MOVIES__  /* Apple's Movies.h not compatible with Audacity */
/* #define __MACHELP__ */

#include <wx/mac/private.h>
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
#  include <Files.h>
# endif
#endif


MP3Exporter *gMP3Exporter = NULL;

MP3Exporter::MP3Exporter()
{
   if (gPrefs) {
      mLibPath = gPrefs->Read(wxT("/MP3/MP3LibPath"), wxT(""));
   }
}

bool MP3Exporter::FindLibrary(wxWindow *parent)
{
   mLibPath = gPrefs->Read(wxT("/MP3/MP3LibPath"), wxT(""));

   if (mLibPath==wxT("") || !::wxFileExists(mLibPath)) {
   
      int action = wxMessageBox(GetLibraryMessage(),
                                _("Export MP3"),
                                wxYES_NO | wxICON_EXCLAMATION,
                                parent);

      if (action == wxYES) {
         wxString question;
         /* i18n-hint: It's asking for the location of a file, for
            example, "Where is lame_enc.dll?" - you could translate
            "Where would I find the file %s" instead if you want. */
         question.Printf(_("Where is %s?"), GetLibraryName().c_str());
         mLibPath = wxFileSelector(question, 
                                   GetLibraryPath(),        // Path
                                   GetLibraryName(),        // Name
                                   wxT(""),      // Extension
                                   GetLibraryTypeString(),
                                   wxOPEN, parent);
         
         if (mLibPath == wxT("")) {
            mLibPath = wxT("");
            gPrefs->Write(wxT("/MP3/MP3LibPath"), mLibPath);
         
            return false;
         }
         
         wxString path, baseName, extension;
         ::wxSplitPath(mLibPath, &path, &baseName, &extension);
         
         if (extension != wxT("")) {
            baseName += wxT(".") + extension;
         }
         
         if (baseName.CmpNoCase(GetLibraryName())) {
         
            wxString question;
            question.Printf(_("Audacity was expecting a library named \"%s\". Are you sure you want to attempt to export MP3 files using \"%s\"?"),
                            GetLibraryName().c_str(),
                            baseName.c_str());

            int action = wxMessageBox(question,
                                      _("Export MP3"),
                                      wxYES_NO | wxICON_EXCLAMATION,
                                      parent);
            
            if (action != wxYES) {
               mLibPath = wxT("");
               gPrefs->Write(wxT("/MP3/MP3LibPath"), mLibPath);
            
               return false;
            }
         }
      }
      else {
         mLibPath = wxT("");
         gPrefs->Write(wxT("/MP3/MP3LibPath"), mLibPath);
            
         return false;
      }
      
      gPrefs->Write(wxT("/MP3/MP3LibPath"), mLibPath);
   }
   
   return true;
}

#if defined(__WXMSW__)

#include "BladeMP3EncDLL.h"

class BladeEncExporter : public MP3Exporter
{
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
   BladeEncExporter()
   {
      mLibraryLoaded = false;
      mEncoding = false;

      /* Set all the config defaults to sane values */

      memset(&mConf, 0, sizeof(BE_CONFIG));
//      mConf.dwConfig = BE_CONFIG_LAME;
//      mConf.format.LHV1.dwStructVersion = 1;
//      mConf.format.LHV1.dwStructSize = sizeof(BE_CONFIG);
//      mConf.format.LHV1.dwReSampleRate = 0;
//      mConf.format.LHV1.dwBitrate = 128;
//      mConf.format.LHV1.dwMaxBitrate = 128;
//      mConf.format.LHV1.nPreset = LQP_HIGH_QUALITY;
//	     mConf.format.LHV1.dwMpegVersion = MPEG1;
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


   wxString GetLibraryPath()
   {
      return wxT("");
   }

   wxString GetLibraryName()
   {
      return wxT("lame_enc.dll");
   }
   
   wxString GetLibraryTypeString()
   {
      return _("Only lame_enc.dll|lame_enc.dll|Dynamically Linked Libraries (*.dll)|*.dll|All Files (*.*)|*");
   }
   
   wxString GetLibraryMessage()
   {
      /* i18n-hint: This message is used on Windows. */
      return _("Audacity does not export MP3 files directly, but instead uses the\nfreely available LAME library to handle MP3 file encoding.  You must\nobtain lame_enc.dll separately, by downloading the LAME MP3 encoder,and then locate this file for Audacity.  You only need to do this once.\n\nWould you like to locate lame_enc.dll now?");
   }


   bool  LoadLibrary() {
      wxLogNull logNo;

      if (wxFileExists(mLibPath))
      {
         if(lame_enc_lib.IsLoaded())
         {
            lame_enc_lib.Unload();
         }

         if(!lame_enc_lib.Load(mLibPath))
         {
            return false;
         }
      }
      else
         return false;

      beInitStream = (BEINITSTREAM)lame_enc_lib.GetSymbol(wxT("beInitStream"));
      beEncodeChunk = (BEENCODECHUNK)lame_enc_lib.GetSymbol(wxT("beEncodeChunk"));
      beDeinitStream = (BEDEINITSTREAM)lame_enc_lib.GetSymbol(wxT("beDeinitStream"));
      beCloseStream = (BECLOSESTREAM)lame_enc_lib.GetSymbol(wxT("beCloseStream"));
      beVersion = (BEVERSION)lame_enc_lib.GetSymbol(wxT("beVersion"));

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

   wxString GetLibraryVersion() {
      BE_VERSION ver;

      if(!mLibraryLoaded)
         return wxT("");

      beVersion(&ver);

	  return wxString::Format(wxT("LAME v%d.%d"), ver.byMajorVersion, ver.byMinorVersion);
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

   int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[]) {
      return EncodeBuffer(inbuffer, outbuffer);
   }

   int EncodeRemainderMono(short int inbuffer[], int nSamples,
                     unsigned char outbuffer[]) {
      return EncodeRemainder(inbuffer, nSamples, outbuffer);
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

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new BladeEncExporter();
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if( gMP3Exporter )
      delete gMP3Exporter;
   gMP3Exporter = NULL;
}

#else

/* --------------------------------------------------------------------------*/

struct lame_global_flags;
typedef lame_global_flags *lame_init_t(void);
typedef int lame_init_params_t(lame_global_flags*);
typedef const char* get_lame_version_t(void);

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

typedef int lame_encode_flush_t(
      lame_global_flags *gf,
      unsigned char*     mp3buf,
      int                size );

typedef int lame_close_t(lame_global_flags*);

typedef int lame_set_in_samplerate_t(lame_global_flags*, int);
typedef int lame_set_out_samplerate_t(lame_global_flags*, int);
typedef int lame_set_num_channels_t(lame_global_flags*, int );
typedef int lame_set_quality_t(lame_global_flags*, int);
typedef int lame_get_quality_t(lame_global_flags*);
typedef int lame_set_brate_t(lame_global_flags*, int);
typedef int lame_get_brate_t(lame_global_flags*);

/* --------------------------------------------------------------------------*/

class LameExporter : public MP3Exporter
{
   private:

      /* function pointers to the symbols we get from the library */
      lame_init_t* lame_init;
      lame_init_params_t* lame_init_params;
      lame_encode_buffer_t* lame_encode_buffer;
      lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
      lame_encode_flush_t* lame_encode_flush;
      lame_close_t* lame_close;
      get_lame_version_t* get_lame_version;
      
      lame_set_in_samplerate_t* lame_set_in_samplerate;
      lame_set_out_samplerate_t* lame_set_out_samplerate;
      lame_set_num_channels_t* lame_set_num_channels;
      lame_set_quality_t* lame_set_quality;
      lame_get_quality_t* lame_get_quality;
      lame_set_brate_t* lame_set_brate;
      lame_get_brate_t* lame_get_brate;

      lame_global_flags *mGF;
      
      bool mLibraryLoaded, mEncoding;
      char mVersion[20];

      static const int mSamplesPerChunk = 220500;
      static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);

   public:
      
      LameExporter()
      {
         mLibraryLoaded = false;
         mEncoding = false;
         mGF = NULL;
      }

      bool LoadLibrary()
      {
         wxLogNull logNo;

         if (!wxFileExists(mLibPath)) {
            return false;
         }

         if (lame_enc_lib.IsLoaded()) {
            lame_enc_lib.Unload();
         }

         if (!lame_enc_lib.Load(mLibPath, wxDL_LAZY)) {
            return false;
         }

         /* get function pointers from the shared library */

         lame_init = (lame_init_t *)lame_enc_lib.GetSymbol(wxT("lame_init"));
         get_lame_version = (get_lame_version_t *)lame_enc_lib.GetSymbol(wxT("get_lame_version"));
         lame_init_params = 
            (lame_init_params_t *) lame_enc_lib.GetSymbol(wxT("lame_init_params"));
         lame_encode_buffer =
             (lame_encode_buffer_t *) lame_enc_lib.GetSymbol(wxT("lame_encode_buffer"));
         lame_encode_buffer_interleaved =
             (lame_encode_buffer_interleaved_t *) lame_enc_lib.GetSymbol(wxT("lame_encode_buffer_interleaved"));
         lame_encode_flush =
             (lame_encode_flush_t *) lame_enc_lib.GetSymbol(wxT("lame_encode_flush"));
         lame_close =
             (lame_close_t *) lame_enc_lib.GetSymbol(wxT("lame_close"));

         lame_close =
             (lame_close_t *) lame_enc_lib.GetSymbol(wxT("lame_close"));

         lame_set_in_samplerate =
             (lame_set_in_samplerate_t *) lame_enc_lib.GetSymbol(wxT("lame_set_in_samplerate"));
         lame_set_out_samplerate =
             (lame_set_out_samplerate_t *) lame_enc_lib.GetSymbol(wxT("lame_set_out_samplerate"));
         lame_set_num_channels =
             (lame_set_num_channels_t *) lame_enc_lib.GetSymbol(wxT("lame_set_num_channels"));
         lame_set_quality =
             (lame_set_quality_t *) lame_enc_lib.GetSymbol(wxT("lame_set_quality"));
         lame_get_quality =
             (lame_get_quality_t *) lame_enc_lib.GetSymbol(wxT("lame_get_quality"));
         lame_set_brate =
             (lame_set_brate_t *) lame_enc_lib.GetSymbol(wxT("lame_set_brate"));
         lame_get_brate =
             (lame_get_brate_t *) lame_enc_lib.GetSymbol(wxT("lame_get_brate"));

         /* we assume that if all the symbols are found, it's a valid library */

         if (!lame_init ||
             !get_lame_version ||
             !lame_init_params ||
             !lame_encode_buffer ||
             !lame_encode_buffer_interleaved ||
             !lame_encode_flush ||
             !lame_close ||
             !lame_set_in_samplerate ||
             !lame_set_out_samplerate ||
             !lame_set_num_channels ||
             !lame_set_quality ||
             !lame_set_brate) {
            return false;
         }

         mGF = lame_init();
         mLibraryLoaded = true;
         return true;
      }

      bool ValidLibraryLoaded()
      {
         return mLibraryLoaded;
      }

      wxString GetLibraryVersion()
      {
         if (!mLibraryLoaded) {
            return wxT("");
         }

         return wxString::Format(wxT("LAME %hs"), get_lame_version());
      }

      int InitializeStream(int channels, int sampleRate)
      {
         if (!mLibraryLoaded) {
            return -1;
         }

         lame_set_num_channels(mGF, channels);
         lame_set_in_samplerate(mGF, sampleRate);
         lame_set_out_samplerate(mGF, sampleRate);

         lame_init_params(mGF);

         mEncoding = true;
         return mSamplesPerChunk;
      }

      int GetOutBufferSize()
      {
         return mOutBufferSize;
      }

      int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[])
      {
         if(!mEncoding) return -1;

         return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
            outbuffer, mOutBufferSize);
      }

      int EncodeRemainder(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[])
      {
         return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
            mOutBufferSize);
      }

      int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[])
      {
         if (!mEncoding) {
            return -1;
         }

         return lame_encode_buffer(mGF, inbuffer,inbuffer, mSamplesPerChunk,
            outbuffer, mOutBufferSize);
      }

      int EncodeRemainderMono(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[])
      {
         return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples, outbuffer,
            mOutBufferSize);
      }

      int FinishStream(unsigned char outbuffer[])
      {
         mEncoding = false;
         int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);
         lame_close(mGF);
         return result;
      }

      void CancelEncoding()
      {
         mEncoding = false;
      }

      int GetConfigurationCaps()
      {
         return MP3CONFIG_BITRATE|MP3CONFIG_QUALITY;
      }

      int GetQualityVariance()
      {
         return 10;
      }

      void SetBitrate(int rate)
      {
         lame_set_brate(mGF, rate);
      }

      int GetBitrate()
      {
         return lame_get_quality(mGF);
      }

      void SetQuality(int quality)
      {
         lame_set_quality(mGF, quality);
      }

      int GetQuality()
      {
         return lame_get_quality(mGF);
      }
};

#if !defined(__WXMAC__)

class LinuxLameExporter : public LameExporter
{
   public:
      
      wxString GetLibraryPath()
      {
         return wxT("/usr/lib");
      }

      wxString GetLibraryName()
      {
         return wxT("libmp3lame.so");
      }
      
      wxString GetLibraryTypeString()
      {
         return wxString(_("Only libmp3lame.so|libmp3lame.so|Primary Shared Object files (*.so)|*.so|Extended Libraries (*.so*)|*.so*|All Files (*)|*"));
      }
      
      wxString GetLibraryMessage()
      {
         /* i18n-hint: This message is used on Unix/Linux */
         return _("Audacity does not export MP3 files directly, but instead uses the \n"
                "freely available LAME library to handle MP3 file encoding.  You must \n"
                "obtain libmp3lame.so separately, either by downloading it or building \n"
                "it from the sources, and then locate the file for Audacity.  You only \n"
                "need to do this once.\n\n"
                "Would you like to locate libmp3lame.so now?");
      }
};

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new LinuxLameExporter();
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if( gMP3Exporter )
      delete gMP3Exporter;
   gMP3Exporter = NULL;
}

#else

class MacLameExporter : public LameExporter
{
   public:
      
      wxString GetLibraryPath()
      {
         return wxT("/usr/local/lib");
      }

      wxString GetLibraryName()
      {
         return wxT("libmp3lame.dylib");
      }
      
      wxString GetLibraryTypeString()
      {
         return wxString(_("Only libmp3lame.dylib|libmp3lame.dylib|Dynamic Libraries (*.dylib)|*.dylib|All Files (*)|*"));
      }
      
      wxString GetLibraryMessage()
      {
         // Must be <= 255 characters on Mac
         /* i18n-hint: This message is used on Mac OS X.  This particular
          message must be <= 255 characters.  Be brief. */
         return _("Audacity does not export MP3 files directly, but instead uses LAME, "
                "an MP3 exporting library available separately.  See the documentation "
                "for more information.\n\n"
                "Would you like to locate libmp3lame.dylib now?");
      }
};

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new MacLameExporter();
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if( gMP3Exporter )
      delete gMP3Exporter;
   gMP3Exporter = NULL;
}

#endif

#endif  

class MP3ExporterCleanup
{
public:
   MP3ExporterCleanup(){};
   ~MP3ExporterCleanup(){ ReleaseMP3Exporter();}
};

// Create instance of this cleanup class purely to clean up 
// the exporter.
// No one will reference this variable, but when the program
// exits it will clean up any allocated MP3 exporter.
MP3ExporterCleanup gMP3ExporterCleanup;


bool ExportMP3(AudacityProject *project,
               bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
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
      wxMessageBox(_("Could not open MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));

      return false;
   }

   if(!GetMP3Exporter()->ValidLibraryLoaded()) {
      wxMessageBox(_("Not a valid or supported MP3 encoding library!"));      
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
      
      return false;
   }
   
   /* Open file for writing */

   wxFFile outFile(fName, wxT("wb"));
   if (!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
      return false;
   }
   
   /* Put ID3 tags at beginning of file */
   /*lda Check ShowId3Dialog flag for CleanSpeech */
   bool showId3Dialog = project->GetShowId3Dialog();
   Tags *tags = project->GetTags();
   bool emptyTags = tags->IsEmpty();
   if (showId3Dialog && emptyTags) {
      if (!tags->ShowEditDialog(project,
                                _("Edit the ID3 tags for the MP3 file"),
                                true)) {
         return false;  // used selected "cancel"
      }
   }

   char *id3buffer = NULL;
   int id3len;
   bool endOfFile;
   id3len = tags->ExportID3(&id3buffer, &endOfFile);
   if (!endOfFile)
     outFile.Write(id3buffer, id3len);

   /* Export MP3 using DLL */

   long bitrate = gPrefs->Read(wxT("/FileFormats/MP3Bitrate"), 128);
   GetMP3Exporter()->SetBitrate(bitrate);

   sampleCount inSamples = GetMP3Exporter()->InitializeStream(stereo ? 2 : 1, int(rate + 0.5));

   bool cancelling = false;
   long bytes;

   int bufferSize = GetMP3Exporter()->GetOutBufferSize();
   unsigned char *buffer = new unsigned char[bufferSize];
   wxASSERT(buffer);

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            stereo? 2: 1, inSamples, true,
                            rate, int16Sample, true, mixerSpec);

   GetActiveProject()->ProgressShow(selectionOnly ?
      wxString::Format(_("Exporting selected audio at %d kbps"), bitrate) :
      wxString::Format(_("Exporting entire file at %d kbps"), bitrate),
      wxFileName(fName).GetName());

   while(!cancelling) {
      sampleCount blockLen = mixer->Process(inSamples);

      if (blockLen == 0)
         break;
      
      short *mixed = (short *)mixer->GetBuffer();

      if(blockLen < inSamples) {
         if (stereo)
            bytes = GetMP3Exporter()->EncodeRemainder(mixed,  blockLen , buffer);
         else
            bytes = GetMP3Exporter()->EncodeRemainderMono(mixed,  blockLen , buffer);
      }
      else {
         if (stereo)
            bytes = GetMP3Exporter()->EncodeBuffer(mixed, buffer);
         else
            bytes = GetMP3Exporter()->EncodeBufferMono(mixed, buffer);
      }

      outFile.Write(buffer, bytes);

      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                          (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   delete mixer;

   bytes = GetMP3Exporter()->FinishStream(buffer);

   if (bytes)
      outFile.Write(buffer, bytes);
   
   /* Write ID3 tag if it was supposed to be at the end of the file */
   
   if (endOfFile)
      outFile.Write(id3buffer, id3len);
   free(id3buffer);

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

   delete[]buffer;
   
   return !cancelling;
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
// arch-tag: c6af56b1-37fa-4d95-b982-0a24b3a49c00


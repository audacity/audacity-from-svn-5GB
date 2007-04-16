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

#include <wx/choice.h>
#include <wx/dynlib.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/mimetype.h>
#include <wx/msgdlg.h>
#include <wx/radiobut.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/window.h>

#include "../Audacity.h"
#include "../FileIO.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../widgets/LinkingHtmlWindow.h"

#include "FileDialog.h"

#include "ExportMP3.h"

MP3Exporter *gMP3Exporter = NULL;

#if defined(__WXMSW__)

#include "BladeMP3EncDLL.h"

class LameExporter : public MP3Exporter
{
public:
   LameExporter() : MP3Exporter()
   {
      /* Set config defaults to sane values */
      memset(&mConf, 0, CURRENT_STRUCT_SIZE);
      mConf.dwConfig = BE_CONFIG_LAME;
      mConf.format.LHV1.dwStructVersion = CURRENT_STRUCT_VERSION;
      mConf.format.LHV1.dwStructSize = CURRENT_STRUCT_SIZE;
      mConf.format.LHV1.nPreset = LQP_HIGH_QUALITY;
      mConf.format.LHV1.dwMpegVersion = MPEG1;
      mConf.format.LHV1.bCRC = true;
      mConf.format.LHV1.bNoRes = true;
   }

   ~LameExporter()
   {
      FreeLibrary();
   }

   bool InitLibrary(wxString libpath)
   {
      if (!lame_lib.Load(libpath, wxDL_LAZY)) {
         return false;
      }

      beInitStream = (BEINITSTREAM) lame_lib.GetSymbol(wxT("beInitStream"));
      beEncodeChunk = (BEENCODECHUNK) lame_lib.GetSymbol(wxT("beEncodeChunk"));
      beDeinitStream = (BEDEINITSTREAM) lame_lib.GetSymbol(wxT("beDeinitStream"));
      beCloseStream = (BECLOSESTREAM) lame_lib.GetSymbol(wxT("beCloseStream"));
      beVersion = (BEVERSION) lame_lib.GetSymbol(wxT("beVersion"));

      if (!beInitStream ||
         !beEncodeChunk ||
         !beDeinitStream ||
         !beCloseStream ||
         !beVersion) {
         return false;
      }

      return true;
   }

   void FreeLibrary()
   {
      lame_lib.Unload();

      return;
   }

   wxString GetLibraryVersion()
   {
      BE_VERSION ver;

      if (!mLibraryLoaded) {
         return wxT("");
      }

      beVersion(&ver);

      return wxString::Format(wxT("LAME v%d.%d"), ver.byMajorVersion, ver.byMinorVersion);
   }

   int InitializeStream(int channels, int sampleRate)
   {
      if (!mLibraryLoaded) {
         return -1;
      }

      if (channels > 2) {
         return -1;
      }

      mConf.format.LHV1.dwSampleRate = sampleRate;
      mConf.format.LHV1.nMode = (channels == 1 ? BE_MP3_MODE_MONO : mMode);
      mConf.format.LHV1.dwBitrate = mBitrate;
      if (mVBR) {
         mConf.format.LHV1.bEnableVBR = true;
         mConf.format.LHV1.nVBRQuality = mVBRQuality;
      }
      else {
         mConf.format.LHV1.bEnableVBR = false;
      }

      if (beInitStream(&mConf, &mInSampleNum, &mOutBufferSize, &mStreamHandle)) {
         return -1;
      }

      mEncoding = true;

      return (mInSampleNum / channels); /* convert samples_total into samples_per_channel */
   }

   int GetOutBufferSize()
   {
      if (!mEncoding)
         return -1;

      return mOutBufferSize;
   }

   int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }
      
      unsigned long bytes;
      if (beEncodeChunk(mStreamHandle, mInSampleNum, inbuffer, outbuffer, &bytes)) {
         return -1;
      }

      return bytes;
   }

   int EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      unsigned long bytes;
      if (beEncodeChunk(mStreamHandle, nSamples, inbuffer, outbuffer, &bytes)) {
         return -1;
      }

      return bytes;
   }

   int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[])
   {
      return EncodeBuffer(inbuffer, outbuffer);
   }

   int EncodeRemainderMono(short int inbuffer[], int nSamples,
                     unsigned char outbuffer[])
   {
      return EncodeRemainder(inbuffer, nSamples, outbuffer);
   }

   int FinishStream(unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      unsigned long bytes;
      if (beDeinitStream(mStreamHandle, outbuffer, &bytes)) {
         return -1;
      }

      if (beCloseStream(mStreamHandle)) {
         return -1;
      }

      mEncoding = false;

      return bytes;
   }

   void CancelEncoding()
   {
      if (!mEncoding) {
         return;
      }

      beCloseStream(mStreamHandle);

      return;
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
   
private:
   wxDynamicLibrary lame_lib;

   BE_CONFIG mConf;
   HBE_STREAM mStreamHandle;
   unsigned long mOutBufferSize;
   unsigned long mInSampleNum;

   /* function pointers to the symbols we get from the library */
   BEINITSTREAM beInitStream;
   BEENCODECHUNK beEncodeChunk;
   BEDEINITSTREAM beDeinitStream;
   BECLOSESTREAM beCloseStream;
   BEVERSION beVersion;
};

#else

#include <dlfcn.h>
#include "lame.h"

/* --------------------------------------------------------------------------*/

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
typedef int lame_set_brate_t(lame_global_flags*, int);
typedef int lame_set_VBR_t(lame_global_flags *, vbr_mode);
typedef int lame_set_VBR_q_t(lame_global_flags *, int);
typedef int lame_set_VBR_min_bitrate_kbps_t(lame_global_flags *, int);
typedef int lame_set_mode_t(lame_global_flags *, MPEG_mode);
typedef int lame_set_error_protection_t(lame_global_flags *, int);

/* --------------------------------------------------------------------------*/

class LameExporter : public MP3Exporter
{
public:
   LameExporter() : MP3Exporter()
   {
      mLib = NULL;
      mGF = NULL;
   }

   ~LameExporter()
   {
      FreeLibrary();
   }

   bool InitLibrary(wxString libpath)
   {
      // Until wxWidgets supports Dynamic Libraries (dylib) on the Mac, we just use
      // dlopen() and friends to support nixes.

      mLib = dlopen(OSFILENAME(libpath), RTLD_LAZY);
      if (mLib == NULL) {
         return false;
      }

      // These strings should not be translated or unicode enabled

      lame_init = (lame_init_t *)
         dlsym(mLib, "lame_init");
      get_lame_version = (get_lame_version_t *)
         dlsym(mLib, "get_lame_version");
      lame_init_params = (lame_init_params_t *)
         dlsym(mLib, "lame_init_params");
      lame_encode_buffer = (lame_encode_buffer_t *)
         dlsym(mLib, "lame_encode_buffer");
      lame_encode_buffer_interleaved = (lame_encode_buffer_interleaved_t *)
         dlsym(mLib, "lame_encode_buffer_interleaved");
      lame_encode_flush = (lame_encode_flush_t *)
         dlsym(mLib, "lame_encode_flush");
      lame_close = (lame_close_t *)
         dlsym(mLib, "lame_close");

      lame_set_in_samplerate = (lame_set_in_samplerate_t *)
          dlsym(mLib, "lame_set_in_samplerate");
      lame_set_out_samplerate = (lame_set_out_samplerate_t *)
          dlsym(mLib, "lame_set_out_samplerate");
      lame_set_num_channels = (lame_set_num_channels_t *)
          dlsym(mLib, "lame_set_num_channels");
      lame_set_quality = (lame_set_quality_t *)
          dlsym(mLib, "lame_set_quality");
      lame_set_brate = (lame_set_brate_t *)
          dlsym(mLib, "lame_set_brate");
      lame_set_VBR = (lame_set_VBR_t *)
          dlsym(mLib, "lame_set_VBR");
      lame_set_VBR_q = (lame_set_VBR_q_t *)
          dlsym(mLib, "lame_set_VBR_q");
      lame_set_VBR_min_bitrate_kbps = (lame_set_VBR_min_bitrate_kbps_t *)
          dlsym(mLib, "lame_set_VBR_min_bitrate_kbps");
      lame_set_mode = (lame_set_mode_t *) 
          dlsym(mLib, "lame_set_mode");
      lame_set_error_protection = (lame_set_error_protection_t *)
          dlsym(mLib, "lame_set_error_protection");

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
         !lame_set_brate ||
         !lame_set_VBR ||
         !lame_set_VBR_q ||
         !lame_set_mode ||
         !lame_set_error_protection) {
         return false;
      }

      mGF = lame_init();

      return true;
   }

   void FreeLibrary()
   {
      if (mLib) {
         dlclose(mLib);
         mLib = NULL;
      }

      return;
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

      lame_set_error_protection(mGF, true);
      lame_set_num_channels(mGF, channels);
      lame_set_in_samplerate(mGF, sampleRate);
      lame_set_out_samplerate(mGF, sampleRate);
      lame_set_brate(mGF, mBitrate);

      if (mVBR) {
         lame_set_VBR(mGF, vbr_mtrh);
         lame_set_VBR_q(mGF, mVBRQuality);
         lame_set_VBR_min_bitrate_kbps(mGF, mBitrate);
      }
      else {
         lame_set_VBR(mGF, vbr_off);
      }

      int mode = (channels == 1 ? MP3_MODE_MONO : mMode);
      switch (mode) {
         case MP3_MODE_STEREO:
            lame_set_mode(mGF, STEREO);
         break;

         case MP3_MODE_JOINT:
            lame_set_mode(mGF, JOINT_STEREO);
         break;

         case MP3_MODE_DUAL:
            lame_set_mode(mGF, DUAL_CHANNEL);
         break;

         case MP3_MODE_MONO:
            lame_set_mode(mGF, MONO);
         break;
      }

      lame_init_params(mGF);

      mEncoding = true;

      return mSamplesPerChunk;
   }

   int GetOutBufferSize()
   {
      if (!mEncoding)
         return -1;

      return mOutBufferSize;
   }

   int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
         outbuffer, mOutBufferSize);
   }

   int EncodeRemainder(short int inbuffer[], int nSamples,
                     unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

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
      if (!mEncoding) {
         return -1;
      }

      return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples, outbuffer,
         mOutBufferSize);
   }

   int FinishStream(unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      mEncoding = false;
      int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);
      lame_close(mGF);
      return result;
   }

   void CancelEncoding()
   {
      mEncoding = false;
   }

#if defined(__WXMAC__)

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

#else

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

#endif

private:

   void *mLib;

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
   lame_set_brate_t* lame_set_brate;
   lame_set_VBR_t* lame_set_VBR;
   lame_set_VBR_q_t* lame_set_VBR_q;
   lame_set_VBR_min_bitrate_kbps_t* lame_set_VBR_min_bitrate_kbps;
   lame_set_mode_t* lame_set_mode;
   lame_set_error_protection_t* lame_set_error_protection;

   lame_global_flags *mGF;

   static const int mSamplesPerChunk = 220500;
   static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);
};
#endif

#define ID_BROWSE 5000
#define ID_DLOAD  5001

class FindDialog : public wxDialog
{
public:

   FindDialog(wxWindow *parent, wxString path, wxString name, wxString type)
   : wxDialog(parent, wxID_ANY, wxString(_("Locate Lame")))
   {
      ShuttleGui S(this, eIsCreating);

      mPath = path;
      mName = name;
      mType = type;

      mLibPath.Assign(mPath, mName);

      PopulateOrExchange(S);
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      wxString text;

      S.SetBorder(10);
      S.StartVerticalLay(true);
      {
         text.Printf(_("Audacity needs the file %s to create MP3s."), mName.c_str());
         S.AddTitle(text);

         S.SetBorder(3);
         S.StartHorizontalLay(wxALIGN_LEFT, true);
         {
            text.Printf(_("Location of %s:"), mName.c_str());
            S.AddTitle(text);
         }
         S.EndHorizontalLay();

         S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(0);
         {
            if (mLibPath.GetFullPath().IsEmpty()) {
               text.Printf(_("To find %s, click here -->"), mName.c_str());
               mPathText = S.AddTextBox(wxT(""), text, 0);
            }
            else {
               mPathText = S.AddTextBox(wxT(""), mLibPath.GetFullPath(), 0);
            }
            S.Id(ID_BROWSE).AddButton(_("Browse..."), wxALIGN_RIGHT);
            S.AddVariableText(_("To get a free copy of Lame, click here -->"), true);
            S.Id(ID_DLOAD).AddButton(_("Download..."), wxALIGN_RIGHT);
         }
         S.EndMultiColumn();
         S.SetBorder(5);
         S.StartHorizontalLay(wxALIGN_BOTTOM | wxALIGN_CENTER, false);
         {
            S.StartHorizontalLay(wxALIGN_CENTER, false);
            {
#if defined(__WXGTK20__) || defined(__WXMAC__)
               S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
               S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
#else
               S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
               S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
            }
         }
         S.EndHorizontalLay();
      }
      S.EndVerticalLay();
      GetSizer()->AddSpacer(5);
      Layout();
      Fit();
      SetMinSize(GetSize());
      Center();

      return;
   }

   void OnBrowse(wxCommandEvent & event)
   {
      wxString question;
      /* i18n-hint: It's asking for the location of a file, for
         example, "Where is lame_enc.dll?" - you could translate
         "Where would I find the file %s" instead if you want. */
      question.Printf(_("Where is %s?"), mName.c_str());

      wxString path = FileSelector(question, 
                                   mLibPath.GetPath(),
                                   mLibPath.GetName(),
                                   wxT(""),
                                   mType,
                                   wxOPEN,
                                   this);
      if (!path.IsEmpty()) {
         mLibPath = path;
         mPathText->SetValue(path);
      }
   }

   void OnDownload(wxCommandEvent & event)
   {
      wxString page = wxT("http://audacity.sourceforge.net/lame");
      ::OpenInDefaultBrowser(page);
   }

   wxString GetLibPath()
   {
      return mLibPath.GetFullPath();
   }

private:

   wxFileName mLibPath;

   wxString mPath;
   wxString mName;
   wxString mType;

   wxTextCtrl *mPathText;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(FindDialog, wxDialog)
   EVT_BUTTON(ID_BROWSE, FindDialog::OnBrowse)
   EVT_BUTTON(ID_DLOAD,  FindDialog::OnDownload)
END_EVENT_TABLE()

// --------------------------------------------------------------------

MP3Exporter::MP3Exporter()
{
   mLibraryLoaded = false;
   mEncoding = false;

   if (gPrefs) {
      mLibPath = gPrefs->Read(wxT("/MP3/MP3LibPath"), wxT(""));
   }

   mBitrate = 128;
   mVBRQuality = 5;
   mMode = JOINT_STEREO;
   mVBR = false;
}

MP3Exporter::~MP3Exporter()
{
}

bool MP3Exporter::FindLibrary(wxWindow *parent, bool showdialog)
{
   wxString path;
   wxString name;

   if (!mLibPath.IsEmpty()) {
      wxFileName fn = mLibPath;
      path = fn.GetPath();
      name = fn.GetFullName();
   }
   else {
      path = GetLibraryPath();
      name = GetLibraryName();
   }

   FindDialog fd(parent,
                 path,
                 name,
                 GetLibraryTypeString());

   if (fd.ShowModal() == wxID_CANCEL) {
      return false;
   }

   path = fd.GetLibPath();
   
   if (!::wxFileExists(path)) {
      return false;
   }

   mLibPath = path;
   gPrefs->Write(wxT("/MP3/MP3LibPath"), mLibPath);

   return true;
}

bool MP3Exporter::LoadLibrary(wxWindow *parent, bool askuser)
{
   wxLogNull logNo;

   if (ValidLibraryLoaded()) {
      FreeLibrary();
      mLibraryLoaded = false;
   }

   // First try loading it from a previously located path
   if (!mLibPath.IsEmpty()) {
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, try loading using system search paths
   if (!ValidLibraryLoaded()) {
      mLibPath = GetLibraryName();
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, try loading using compiled in path
   if (!ValidLibraryLoaded()) {
      wxFileName fn(GetLibraryPath(), GetLibraryName());
      mLibPath = fn.GetFullPath();
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, must ask the user
   if (!ValidLibraryLoaded()) {
      if (askuser && FindLibrary(parent, true)) {
         mLibraryLoaded = InitLibrary(mLibPath);
      }
   }

   // Oh well, just give up
   if (!ValidLibraryLoaded()) {
      return false;
   }

   return true;
}

bool MP3Exporter::ValidLibraryLoaded()
{
   return mLibraryLoaded;
}

void MP3Exporter::SetBitrate(int rate)
{
   mVBR = false;
   mBitrate = rate;
}

int MP3Exporter::GetBitrate()
{
   return mBitrate;
}

void MP3Exporter::SetVBRQuality(int quality)
{
   mVBR = true;
   mVBRQuality = quality;
}

int MP3Exporter::GetVBRQuality()
{
   return mVBRQuality;
}

void MP3Exporter::SetMode(int mode)
{
   if (mode < MP3_MODE_STEREO || mode > MP3_MODE_MONO) {
      return;
   }

   mMode = mode;
}

int MP3Exporter::GetMode()
{
   return mMode;
}

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter) {
      gMP3Exporter = new LameExporter();
   }
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if (gMP3Exporter) {
      delete gMP3Exporter;
   }

   gMP3Exporter = NULL;
}

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
               int channels, wxString fName,
               bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
{
   bool stereo = (channels == 2);
   double rate = project->GetRate();
   wxWindow *parent = project;
   TrackList *tracks = project->GetTracks();
   MP3Exporter *exporter = GetMP3Exporter();

   if (!exporter->LoadLibrary(parent, true)) {
      wxMessageBox(_("Could not open MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));

      return false;
   }

   if (!exporter->ValidLibraryLoaded()) {
      wxMessageBox(_("Not a valid or supported MP3 encoding library!"));      
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
      
      return false;
   }
   
   /* Open file for writing */
   FileIO outFile(fName, FileIO::Output);
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
   if (!endOfFile) {
     outFile.Write(id3buffer, id3len);
   }

   /* Export MP3 using DLL */

   long bitrate = gPrefs->Read(wxT("/FileFormats/MP3Bitrate"), 128);
   wxString rmode = gPrefs->Read(wxT("/FileFormats/MP3RateMode"), wxT("cbr"));
   if (rmode == wxT("cbr")) {
      exporter->SetBitrate(bitrate);
   }
   else {
      exporter->SetVBRQuality(bitrate);
   }

   wxString cmode = gPrefs->Read(wxT("/FileFormats/MP3ChannelMode"), wxT("joint"));
   exporter->SetMode( cmode == wxT("joint") ? JOINT_STEREO : STEREO );

   sampleCount inSamples = exporter->InitializeStream(stereo ? 2 : 1, int(rate + 0.5));

   bool cancelling = false;
   long bytes;

   int bufferSize = exporter->GetOutBufferSize();
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

   if (rmode == wxT("cbr"))
      GetActiveProject()->ProgressShow(selectionOnly ?
         wxString::Format(_("Exporting selected audio at %d kbps"), bitrate) :
         wxString::Format(_("Exporting entire file at %d kbps"), bitrate),
         wxFileName(fName).GetName());
   else
      GetActiveProject()->ProgressShow(selectionOnly ?
         wxString::Format(_("Exporting selected audio at quality %d"), bitrate) :
         wxString::Format(_("Exporting entire file at quality %d"), bitrate),
         wxFileName(fName).GetName());

   while (!cancelling) {
      sampleCount blockLen = mixer->Process(inSamples);

      if (blockLen == 0) {
         break;
      }
      
      short *mixed = (short *)mixer->GetBuffer();

      if (blockLen < inSamples) {
         if (stereo) {
            bytes = exporter->EncodeRemainder(mixed,  blockLen , buffer);
         }
         else {
            bytes = exporter->EncodeRemainderMono(mixed,  blockLen , buffer);
         }
      }
      else {
         if (stereo) {
            bytes = exporter->EncodeBuffer(mixed, buffer);
         }
         else {
            bytes = exporter->EncodeBufferMono(mixed, buffer);
         }
      }

      outFile.Write(buffer, bytes);

      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                          (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   delete mixer;

   bytes = exporter->FinishStream(buffer);

   if (bytes) {
      outFile.Write(buffer, bytes);
   }
   
   /* Write ID3 tag if it was supposed to be at the end of the file */
   
   if (endOfFile) {
      outFile.Write(id3buffer, id3len);
   }

   free(id3buffer);

   /* Close file */
   outFile.Close();
      
   delete[]buffer;
   
   return !cancelling;
}

#define ID_MP3_VBR 7000
#define ID_MP3_CBR 7001

static int iBitrates[] = {
   16, 24, 32, 40, 48, 56, 64,
   80, 96, 112, 128, 160,
   192, 224, 256, 320 
};

class MP3OptionsDialog : public wxDialog
{
public:

   /// 
   /// 
   MP3OptionsDialog(wxWindow *parent)
   : wxDialog(NULL, wxID_ANY, wxString(_("Specify MP3 Options")),
      wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
   {
      ShuttleGui S(this, eIsCreatingFromPrefs);

      PopulateOrExchange(S);
   }

   /// 
   /// 
   void PopulateOrExchange(ShuttleGui & S)
   {
      S.StartHorizontalLay(wxEXPAND, 0);
      {
         S.StartStatic(_("MP3 Export Setup"), 0);
         {
            S.StartMultiColumn(2,wxEXPAND);
            {
               S.SetStretchyCol(1);
               S.StartTwoColumn();
               {
                  S.AddPrompt(_("Bit Rate:"));
                  S.StartTwoColumn();
                  {
                     S.StartRadioButtonGroup(wxT("/FileFormats/MP3RateMode"), wxT("cbr"));
                     {
                        mMP3VBR = S.Id(ID_MP3_VBR).TieRadioButton(_("Variable"), wxT("vbr"));
                        mMP3CBR = S.Id(ID_MP3_CBR).TieRadioButton(_("Constant"), wxT("cbr"));
                     }
                     S.EndRadioButtonGroup();
                  }
                  S.EndTwoColumn();

                  if (mMP3VBR->GetValue())
                  {
                     mMP3RateNames.Add(_("0 (Best quality), 220-260 kbps"));
                     mMP3RateLabels.Add(0);
                     mMP3RateNames.Add(_("1, 200-250 kbps"));
                     mMP3RateLabels.Add(1);
                     mMP3RateNames.Add(_("2, 170-210 kbps"));
                     mMP3RateLabels.Add(2);
                     mMP3RateNames.Add(_("3, 155-195 kbps"));
                     mMP3RateLabels.Add(3);
                     mMP3RateNames.Add(_("4, 145-185 kbps"));
                     mMP3RateLabels.Add(4);
                     mMP3RateNames.Add(_("5, 110-150 kbps"));
                     mMP3RateLabels.Add(5);
                     mMP3RateNames.Add(_("6, 95-135 kbps"));
                     mMP3RateLabels.Add(6);
                     mMP3RateNames.Add(_("7, 80-120 kbps"));
                     mMP3RateLabels.Add(7);
                     mMP3RateNames.Add(_("8, 65-105 kbps"));
                     mMP3RateLabels.Add(8);
                     mMP3RateNames.Add(_("9 (Smaller files), 45-85 kbps"));
                     mMP3RateLabels.Add(9);
                  }
                  else
                  {
                     for(unsigned int i=0; i < (sizeof(iBitrates)/sizeof(int)); i++)
                     {
                        mMP3RateNames.Add(wxString::Format(wxT("%i kbps"),iBitrates[i]));
                        mMP3RateLabels.Add(iBitrates[i]);
                     }
                  }

                  mMP3Bitrate = S.TieChoice(_("Quality:"),
                                            wxT("/FileFormats/MP3Bitrate"), 
                                            128,
                                            mMP3RateNames,
                                            mMP3RateLabels);
                  S.AddPrompt(_("Mode:"));
                  S.StartTwoColumn();
                  {
                     S.StartRadioButtonGroup(wxT("/FileFormats/MP3ChannelMode"), wxT("joint"));
                     {
                        mMP3Joint = S.TieRadioButton(_("Joint Stereo"), wxT("joint"));
                        mMP3Stereo = S.TieRadioButton(_("Stereo"), wxT("stereo"));
                     }
                     S.EndRadioButtonGroup();
                  }
                  S.EndTwoColumn();
               }
               S.EndTwoColumn();
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
      }
      S.EndHorizontalLay();
      S.StartHorizontalLay(wxALIGN_CENTER, false);
      {
#if defined(__WXGTK20__) || defined(__WXMAC__)
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
         S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
#else
         S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
      }
      GetSizer()->AddSpacer(5);
      Layout();
      Fit();
      SetMinSize(GetSize());
      Center();

      return;
   }

   /// 
   /// 
   void OnOK(wxCommandEvent& event)
   {
      ShuttleGui S(this, eIsSavingToPrefs);
      PopulateOrExchange(S);

      EndModal(wxID_OK);

      return;
   }

   /// 
   /// 
   void OnMP3VBR(wxCommandEvent& evt)
   {
      mMP3Bitrate->Clear();
      mMP3RateNames.Clear();
      mMP3RateLabels.Clear();

      mMP3RateNames.Add(_("0 (Best quality), 220-260 kbps"));
      mMP3RateLabels.Add(0);
      mMP3RateNames.Add(_("1, 200-250 kbps"));
      mMP3RateLabels.Add(1);
      mMP3RateNames.Add(_("2, 170-210 kbps"));
      mMP3RateLabels.Add(2);
      mMP3RateNames.Add(_("3, 155-195 kbps"));
      mMP3RateLabels.Add(3);
      mMP3RateNames.Add(_("4, 145-185 kbps"));
      mMP3RateLabels.Add(4);
      mMP3RateNames.Add(_("5, 110-150 kbps"));
      mMP3RateLabels.Add(5);
      mMP3RateNames.Add(_("6, 95-135 kbps"));
      mMP3RateLabels.Add(6);
      mMP3RateNames.Add(_("7, 80-120 kbps"));
      mMP3RateLabels.Add(7);
      mMP3RateNames.Add(_("8, 65-105 kbps"));
      mMP3RateLabels.Add(8);
      mMP3RateNames.Add(_("9 (Smaller files), 45-85 kbps"));
      mMP3RateLabels.Add(9);

      mMP3Bitrate->Append(mMP3RateNames);
      mMP3Bitrate->SetSelection(5);
      mMP3Bitrate->Refresh();
   }

   /// 
   /// 
   void OnMP3CBR(wxCommandEvent& evt)
   {
      mMP3Bitrate->Clear();
      mMP3RateNames.Clear();
      mMP3RateLabels.Clear();

      for(unsigned int i=0;i<(sizeof(iBitrates)/sizeof(int));i++)
      {
         mMP3RateNames.Add( wxString::Format(wxT("%i kbps"),iBitrates[i] ));
         mMP3RateLabels.Add( iBitrates[i] );
      }

      mMP3Bitrate->Append(mMP3RateNames);
      mMP3Bitrate->SetSelection(10);
      mMP3Bitrate->Refresh();
   }

private:
   wxArrayString mMP3RateNames;
   wxArrayInt    mMP3RateLabels;

   wxRadioButton *mMP3Stereo;
   wxRadioButton *mMP3Joint;
   wxRadioButton *mMP3VBR;
   wxRadioButton *mMP3CBR;
   wxChoice *mMP3Bitrate;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(MP3OptionsDialog, wxDialog)
   EVT_RADIOBUTTON(ID_MP3_VBR,   MP3OptionsDialog::OnMP3VBR)
   EVT_RADIOBUTTON(ID_MP3_CBR,   MP3OptionsDialog::OnMP3CBR)
   EVT_BUTTON(wxID_OK,           MP3OptionsDialog::OnOK)
END_EVENT_TABLE()

bool ExportMP3Options(AudacityProject *project)
{
   MP3OptionsDialog od(project);

   od.ShowModal();

   return true;
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

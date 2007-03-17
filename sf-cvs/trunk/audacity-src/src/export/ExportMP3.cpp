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
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "ExportMP3.h"

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

#if defined(__WXMSW__)

static wxString GetLibraryPath()
{
   return wxT("");
}

static wxString GetLibraryName()
{
   return wxT("lame_enc.dll");
}
      
static wxString GetLibraryTypeString()
{
   return _("Only lame_enc.dll|lame_enc.dll|Dynamically Linked Libraries (*.dll)|*.dll|All Files (*.*)|*");
}
      
static wxString GetLibraryMessage()
{
   /* i18n-hint: This message is used on Windows. */
   return _("Audacity does not export MP3 files directly, but instead uses the\nfreely available LAME library to handle MP3 file encoding.  You must\nobtain lame_enc.dll separately, by downloading the LAME MP3 encoder,and then locate this file for Audacity.  You only need to do this once.\n\nWould you like to locate lame_enc.dll now?");
}

#else
#if defined(__WXMAC__)

static wxString GetLibraryPath()
{
   return wxT("/usr/local/lib");
}

static wxString GetLibraryName()
{
   return wxT("libmp3lame.so");
}

static wxString GetLibraryTypeString()
{
   return wxString(_("Only libmp3lame.so|libmp3lame.so|Bundles (*.so)|*.so|All Files (*)|*"));
}

static wxString GetLibraryMessage()
{
   // Must be <= 255 characters on Mac
   /* i18n-hint: This message is used on Mac OS X.  This particular
    message must be <= 255 characters.  Be brief. */
   return _("Audacity does not export MP3 files directly, but instead uses LAME, "
          "an MP3 exporting library available separately.  See the documentation "
          "for more information.\n\n"
          "Would you like to locate libmp3lame.so now?");
}

#else

static wxString GetLibraryPath()
{
   return wxT("/usr/lib");
}

static wxString GetLibraryName()
{
   return wxT("libmp3lame.so");
}

static wxString GetLibraryTypeString()
{
   return wxString(_("Only libmp3lame.so|libmp3lame.so|Primary Shared Object files (*.so)|*.so|Extended Libraries (*.so*)|*.so*|All Files (*)|*"));
}

static wxString GetLibraryMessage()
{
   /* i18n-hint: This message is used on Unix/Linux */
   return _("Audacity does not export MP3 files directly, but instead uses the \n"
          "freely available LAME library to handle MP3 file encoding.  You must \n"
          "obtain libmp3lame.so separately, either by downloading it or building \n"
          "it from the sources, and then locate the file for Audacity.  You only \n"
          "need to do this once.\n\n"
          "Would you like to locate libmp3lame.so now?");
}
#endif
#endif

MP3Exporter *gMP3Exporter = NULL;

MP3Exporter::MP3Exporter()
{
   mLibraryLoaded = false;
   mEncoding = false;
   mGF = NULL;
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

bool MP3Exporter::LoadLibrary()
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
   lame_set_VBR =
       (lame_set_VBR_t *) lame_enc_lib.GetSymbol(wxT("lame_set_VBR"));
   lame_get_VBR =
       (lame_get_VBR_t *) lame_enc_lib.GetSymbol(wxT("lame_get_VBR"));
   lame_set_VBR_q =
       (lame_set_VBR_q_t *) lame_enc_lib.GetSymbol(wxT("lame_set_VBR_q"));
   lame_get_VBR_q =
       (lame_get_VBR_q_t *) lame_enc_lib.GetSymbol(wxT("lame_get_VBR_q"));
   lame_set_mode =
       (lame_set_mode_t *) lame_enc_lib.GetSymbol(wxT("lame_set_mode"));

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
       !lame_set_mode) {
      return false;
   }

   mGF = lame_init();
   mLibraryLoaded = true;
   return true;
}

bool MP3Exporter::ValidLibraryLoaded()
{
   return mLibraryLoaded;
}

wxString MP3Exporter::GetLibraryVersion()
{
   if (!mLibraryLoaded) {
      return wxT("");
   }

   return wxString::Format(wxT("LAME %hs"), get_lame_version());
}

int MP3Exporter::InitializeStream(int channels, int sampleRate)
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

int MP3Exporter::GetOutBufferSize()
{
   return mOutBufferSize;
}

int MP3Exporter::EncodeBuffer(short int inbuffer[], unsigned char outbuffer[])
{
   if(!mEncoding) return -1;

   return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
      outbuffer, mOutBufferSize);
}

int MP3Exporter::EncodeRemainder(short int inbuffer[], int nSamples,
                  unsigned char outbuffer[])
{
   return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
      mOutBufferSize);
}

int MP3Exporter::EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[])
{
   if (!mEncoding) {
      return -1;
   }

   return lame_encode_buffer(mGF, inbuffer,inbuffer, mSamplesPerChunk,
      outbuffer, mOutBufferSize);
}

int MP3Exporter::EncodeRemainderMono(short int inbuffer[], int nSamples,
                  unsigned char outbuffer[])
{
   return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples, outbuffer,
      mOutBufferSize);
}

int MP3Exporter::FinishStream(unsigned char outbuffer[])
{
   mEncoding = false;
   int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);
   lame_close(mGF);
   return result;
}

void MP3Exporter::CancelEncoding()
{
   mEncoding = false;
}

int MP3Exporter::GetConfigurationCaps()
{
   return MP3CONFIG_BITRATE|MP3CONFIG_QUALITY;
}

int MP3Exporter::GetQualityVariance()
{
   return 10;
}

void MP3Exporter::SetBitrate(int rate)
{
   lame_set_VBR(mGF, vbr_off);
   lame_set_brate(mGF, rate);
}

int MP3Exporter::GetBitrate()
{
   return lame_get_brate(mGF);
}

void MP3Exporter::SetVBRQuality(int quality)
{
   lame_set_VBR(mGF, vbr_mtrh);
   lame_set_VBR_q(mGF, quality);
}

int MP3Exporter::GetVBRQuality()
{
   return lame_get_VBR_q(mGF);
}

void MP3Exporter::SetMode(MPEG_mode mode)
{
   lame_set_mode(mGF, mode);
}

MPEG_mode MP3Exporter::GetMode()
{
   return lame_get_mode(mGF);
}

void MP3Exporter::SetQuality(int quality)
{
   lame_set_quality(mGF, quality);
}

int MP3Exporter::GetQuality()
{
   return lame_get_quality(mGF);
}

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new MP3Exporter();
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if( gMP3Exporter )
      delete gMP3Exporter;
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
               bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
{
   double rate = project->GetRate();
   wxWindow *parent = project;
   TrackList *tracks = project->GetTracks();
   MP3Exporter *exporter = GetMP3Exporter();

   bool success = exporter->FindLibrary(parent);
   
   if (!success)
      return false;

   success = exporter->LoadLibrary();
   if (!success) {
      wxMessageBox(_("Could not open MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));

      return false;
   }

   if(!exporter->ValidLibraryLoaded()) {
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
            bytes = exporter->EncodeRemainder(mixed,  blockLen , buffer);
         else
            bytes = exporter->EncodeRemainderMono(mixed,  blockLen , buffer);
      }
      else {
         if (stereo)
            bytes = exporter->EncodeBuffer(mixed, buffer);
         else
            bytes = exporter->EncodeBufferMono(mixed, buffer);
      }

      outFile.Write(buffer, bytes);

      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                          (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   delete mixer;

   bytes = exporter->FinishStream(buffer);

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

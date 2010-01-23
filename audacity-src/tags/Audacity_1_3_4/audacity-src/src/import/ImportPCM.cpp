/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.cpp

  Dominic Mazzoni

*//****************************************************************//**

\class PCMImportFileHandle
\brief An ImportFileHandle for PCM data

*//****************************************************************//**

\class PCMImportPlugin
\brief An ImportPlugin for PCM data

*//*******************************************************************/

#include "../Audacity.h"
#include "../AudacityApp.h"
#include "../Internat.h"
#include "../Tags.h"
#include "ImportPCM.h"

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
#include <wx/ffile.h>
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/stattext.h>

#include "sndfile.h"

#ifndef SNDFILE_1
#error Requires libsndfile 1.0 or higher
#endif

#include "../FileFormats.h"
#include "../Prefs.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"

#define DESC _("WAV, AIFF, and other uncompressed types")

class PCMImportPlugin : public ImportPlugin
{
public:
   PCMImportPlugin():
      ImportPlugin(wxArrayString())
   {
      mExtensions = sf_get_all_extensions();
   }

   ~PCMImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};


class PCMImportFileHandle : public ImportFileHandle
{
public:
   PCMImportFileHandle(wxString name, SNDFILE *file, SF_INFO info);
   ~PCMImportFileHandle();

   void SetProgressCallback(progress_callback_t function,
                            void *userData);
   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   bool Import(TrackFactory *trackFactory, Track ***outTracks,
               int *outNumTracks, Tags *tags);
private:
   wxString              mName;
   SNDFILE              *mFile;
   SF_INFO               mInfo;
   sampleFormat          mFormat;
   void                 *mUserData;
   progress_callback_t  mProgressCallback;
};

void GetPCMImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new PCMImportPlugin);
}

wxString PCMImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

ImportFileHandle *PCMImportPlugin::Open(wxString filename)
{
   SF_INFO info;
   SNDFILE *file;

   memset(&info, 0, sizeof(info));

   file = sf_open(OSFILENAME(filename), SFM_READ, &info);
   if (!file) {
      // TODO: Handle error
      //char str[1000];
      //sf_error_str((SNDFILE *)NULL, str, 1000);

      return NULL;
   }

   return new PCMImportFileHandle(filename, file, info);
}

PCMImportFileHandle::PCMImportFileHandle(wxString name,
                                         SNDFILE *file, SF_INFO info):
   mName(name),
   mFile(file),
   mInfo(info),
   mUserData(NULL),
   mProgressCallback(NULL)
{
   //
   // Figure out the format to use.
   //
   // In general, go with the user's preferences.  However, if
   // the file is higher-quality, go with a format which preserves
   // the quality of the original file.
   //
   
   mFormat = (sampleFormat)
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);

   if (mFormat != floatSample &&
       sf_subtype_more_than_16_bits(mInfo.format))
      mFormat = floatSample;
}

void PCMImportFileHandle::SetProgressCallback(progress_callback_t progressCallback,
                                      void *userData)
{
   mProgressCallback = progressCallback;
   mUserData = userData;
}

wxString PCMImportFileHandle::GetFileDescription()
{
   return sf_header_name(mInfo.format);
}

int PCMImportFileHandle::GetFileUncompressedBytes()
{
   return mInfo.frames * mInfo.channels * SAMPLE_SIZE(mFormat);
}

bool PCMImportFileHandle::Import(TrackFactory *trackFactory,
                                 Track ***outTracks,
                                 int *outNumTracks,
                                 Tags *tags)
{
   wxASSERT(mFile);

   WaveTrack **channels = new WaveTrack *[mInfo.channels];

   int c;
   for (c = 0; c < mInfo.channels; c++) {
      channels[c] = trackFactory->NewWaveTrack(mFormat, mInfo.samplerate);

      if (mInfo.channels > 1)
         switch (c) {
         case 0:
            channels[c]->SetChannel(Track::LeftChannel);
            break;
         case 1:
            channels[c]->SetChannel(Track::RightChannel);
            break;
         default:
            channels[c]->SetChannel(Track::MonoChannel);
         }
   }

   if (mInfo.channels == 2) {
      channels[0]->SetLinked(true);
      channels[1]->SetTeamed(true);
   }

   sampleCount fileTotalFrames = (sampleCount)mInfo.frames;
   sampleCount maxBlockSize = channels[0]->GetMaxBlockSize();
   bool cancelled = false;
   
   wxString copyEdit =
       gPrefs->Read(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("edit"));

   // Fall back to "edit" if it doesn't match anything else
   bool doEdit = true;          
   if (copyEdit.IsSameAs(wxT("copy"), false))
      doEdit = false;
      
   // If the format is not seekable, we must use 'copy' mode,
   // because 'edit' mode depends on the ability to seek to an
   // arbitrary location in the file.
   if (!mInfo.seekable)
      doEdit = false;

   if (doEdit) {

      // If this mode has been selected, we form the tracks as
      // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
      // instead of actually making fresh copies of the samples.
      
      for (sampleCount i = 0; i < fileTotalFrames; i += maxBlockSize) {
         sampleCount blockLen = maxBlockSize;
         if (i + blockLen > fileTotalFrames)
            blockLen = fileTotalFrames - i;

         for (c = 0; c < mInfo.channels; c++)
            channels[c]->AppendAlias(mName, i, blockLen, c);

         if( mProgressCallback )
            cancelled = mProgressCallback(mUserData,
                                          i*1.0 / fileTotalFrames);
         if (cancelled)
            break;
      }
   }
   else {
      // Otherwise, we're in the "copy" mode, where we read in the actual
      // samples from the file and store our own local copy of the
      // samples in the tracks.
      
      samplePtr srcbuffer = NewSamples(maxBlockSize * mInfo.channels,
                                       mFormat);
      samplePtr buffer = NewSamples(maxBlockSize, mFormat);

      unsigned long framescompleted = 0;
      
      long block;
      do {
         block = maxBlockSize;
         
         if (mFormat == int16Sample)
            block = sf_readf_short(mFile, (short *)srcbuffer, block);
         else
            block = sf_readf_float(mFile, (float *)srcbuffer, block);
         
         if (block) {
            for(c=0; c<mInfo.channels; c++) {
               if (mFormat==int16Sample) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[mInfo.channels*j+c];
               }
               else {
                  for(int j=0; j<block; j++)
                     ((float *)buffer)[j] =
                        ((float *)srcbuffer)[mInfo.channels*j+c];
               }
               
               channels[c]->Append(buffer, mFormat, block);
            }
            framescompleted += block;
         }

         if( mProgressCallback )
            cancelled = mProgressCallback(mUserData,
                                          framescompleted*1.0 /
                                          fileTotalFrames);
         if (cancelled)
            break;

      } while (block > 0);
   }

   if (cancelled) {
      for (c = 0; c < mInfo.channels; c++)
         delete channels[c];
      delete[] channels;

      return false;
   }

   *outNumTracks = mInfo.channels;
   *outTracks = new Track *[mInfo.channels];
   for(c = 0; c < mInfo.channels; c++) {
         channels[c]->Flush();
         (*outTracks)[c] = channels[c];
      }
      delete[] channels;

   const char *str;

   str = sf_get_string(mFile, SF_STR_TITLE);
   if (str) {
      tags->SetTag(TAG_TITLE, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_ARTIST);
   if (str) {
      tags->SetTag(TAG_ARTIST, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_COMMENT);
   if (str) {
      tags->SetTag(TAG_COMMENTS, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_DATE);
   if (str) {
      tags->SetTag(TAG_YEAR, UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_COPYRIGHT);
   if (str) {
      tags->SetTag(wxT("Copyright"), UTF8CTOWX(str));
   }

   str = sf_get_string(mFile, SF_STR_SOFTWARE);
   if (str) {
      tags->SetTag(wxT("Software"), UTF8CTOWX(str));
   }

   return true;
}

PCMImportFileHandle::~PCMImportFileHandle()
{
   sf_close(mFile);
}


#if 0

#include <wx/file.h>
#include <wx/string.h>
#include <wx/thread.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/intl.h>

#include "Import.h"
#include "ImportPCM.h"

#include "../FileFormats.h"
#include "../WaveTrack.h"
#include "../DirManager.h"
#include "../Prefs.h"

#include "sndfile.h"

bool IsPCM(wxString fName)
{
   wxFile testFile;
   testFile.Open(fName);
   if (!testFile.IsOpened())
      return false;
   testFile.Close();

   SF_INFO    info;
   SNDFILE   *fp;

   fp = sf_open_read(OSFILENAME(fName), &info);

   if (fp) {
      sf_close(fp);
      return true;
   }

   return false;
}


bool ImportPCM(wxWindow * parent,
               wxString fName, 
               WaveTrack ** channels[],
               int *numChannels,
               DirManager * dirManager)
{
   SF_INFO       info;
   SNDFILE      *fp;
   sampleFormat  format;

   fp = sf_open_read(OSFILENAME(fName), &info);

   if (!fp) {
      char str[1000];
      sf_error_str((SNDFILE *)NULL, str, 1000);
      wxMessageBox(LAT1CTOWX(str));

      return false;
   }

   wxString progressStr;
   wxString formatName = sf_header_name(info.format & SF_FORMAT_TYPEMASK);
   progressStr.Printf(_("Importing %s File..."),
                      formatName.c_str());

   *numChannels = info.channels;
   *channels = new WaveTrack*[*numChannels];

   if (info.pcmbitwidth > 16)
      format = floatSample;
   else
      format = int16Sample;

   int c;
   for(c=0; c<*numChannels; c++) {
      (*channels)[c] = new WaveTrack(dirManager, format, info.samplerate);
      (*channels)[c]->SetName(TrackNameFromFileName(fName));
      (*channels)[c]->SetChannel(Track::MonoChannel);
   }

   if (*numChannels == 2) {
      (*channels)[0]->SetChannel(Track::LeftChannel);
      (*channels)[1]->SetChannel(Track::RightChannel);
      (*channels)[0]->SetLinked(true);
      (*channels)[1]->SetTeamed(true);
   }

   sampleCount fileTotalFrames = (sampleCount)info.frames;
   sampleCount maxBlockSize = (*channels)[0]->GetMaxBlockSize();

   wxString copyEdit =
       gPrefs->Read(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("edit"));

   // Fall back to "edit" if it doesn't match anything else
   bool doEdit = true;          
   if (copyEdit.IsSameAs(wxT("copy"), false))
      doEdit = false;

   if (doEdit) {

      // If this mode has been selected, we form the tracks as
      // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
      // instead of actually making fresh copies of the samples.

      bool cancelling = false;

      GetActiveProject()->ProgressShow(_("Import"), progressStr);

      for (sampleCount i = 0; i < fileTotalFrames; i += maxBlockSize) {
         sampleCount blockLen = maxBlockSize;
         if (i + blockLen > fileTotalFrames)
            blockLen = fileTotalFrames - i;

         for(c=0; c<*numChannels; c++)
            (*channels)[c]->AppendAlias(fName, i, blockLen, c);

         cancelling = !GetActiveProject()->ProgressUpdate((int)((i*1000.0)/fileTotalFrames));

         if (cancelling)
            i = fileTotalFrames;
      }

      GetActiveProject()->ProgressHide();

      //printf(_("Time elapsed: %d\n"), wxGetElapsedTime());

      if (cancelling) {
         for(c=0; c<*numChannels; c++)
            delete (*channels)[c];
         delete[] (*channels);
         *channels = NULL;

         return false;
      }

      return true;
   }

   // Otherwise, we're in the "copy" mode, where we read in the actual
   // samples from the file and store our own local copy of the
   // samples in the tracks.

   samplePtr srcbuffer = NewSamples(maxBlockSize * (*numChannels),
                                    format);
   samplePtr buffer = NewSamples(maxBlockSize, format);

   sampleCount framescompleted = 0;

   bool cancelling = false;

   GetActiveProject->ProgressShow(_("Import"), progressStr);

   long block;
   do {
      block = maxBlockSize;

      if (format == int16Sample)
         block = sf_readf_short(fp, (short *)srcbuffer, block);
      else
         block = sf_readf_float(fp, (float *)srcbuffer, block);

      if (block) {
         for(c=0; c<(*numChannels); c++) {

            if (format==int16Sample) {
               if (info.pcmbitwidth == 8) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*numChannels)*j+c] << 8;
               }
               else {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*numChannels)*j+c];
               }
            }
            else
               for(int j=0; j<block; j++)
                  ((float *)buffer)[j] =
                     ((float *)srcbuffer)[(*numChannels)*j+c];

            (*channels)[c]->Append(buffer, format, block);
         }

         framescompleted += block;
      }

      int progressvalue = (framescompleted > fileTotalFrames) ?
          fileTotalFrames : framescompleted;

      cancelling =
         !GetActiveProject()->ProgressUpdate((int)((progressvalue*1000.0)/fileTotalFrames));

      if (cancelling)
         block = 0;
   } while (block > 0);

   GetActiveProject()->ProgressHide();

   sf_close(fp);

   //printf("Time elapsed: %d\n", wxGetElapsedTime());

   DeleteSamples(srcbuffer);
   DeleteSamples(buffer);

   if (cancelling) {
      for(c=0; c<*numChannels; c++)
         delete (*channels)[c];
      delete[] (*channels);
      *channels = NULL;

      return false;
   }

   return true;
}

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 2e9db06a-fd0b-4af3-badd-eeb8437067e7


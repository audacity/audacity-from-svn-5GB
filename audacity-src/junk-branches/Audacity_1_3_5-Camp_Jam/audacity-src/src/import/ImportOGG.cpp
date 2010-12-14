/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportOGG.cpp

  Joshua Haberman

*//****************************************************************//**

\class ImportFileHandle
\brief An ImportFileHandle for data

  The Ogg format supports multiple logical bitstreams that can be chained
  within the physical bitstream. The sampling rate and number of channels
  can vary between these logical bitstreams. For the moment, we'll ignore
  all but the first logical bitstream.

  Ogg also allows for an arbitrary number of channels. Luckily, so does
  Audacity. We'll call the first channel LeftChannel, the second
  RightChannel, and all others after it MonoChannel.

*//****************************************************************//**

\class OGGImportPlugin
\brief An ImportPlugin for OGG data

*//*******************************************************************/

#include <wx/intl.h>
#include "../Audacity.h"
#include "ImportOGG.h"
#include "../Internat.h"
#include "../Tags.h"

#define DESC _("Ogg Vorbis files")

static const wxChar *exts[] =
{
   wxT("ogg")
};

#ifndef USE_LIBVORBIS
/* BPF There is no real reason to compile without LIBVORBIS, but if you do, you will needs this header */
#include "ImportPlugin.h"  

void GetOGGImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   UnusableImportPlugin* oggIsUnsupported =
      new UnusableImportPlugin(DESC, wxArrayString(WXSIZEOF(exts), exts));

   unusableImportPluginList->Append(oggIsUnsupported);
}

#else /* USE_LIBVORBIS */

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
/* ffile.h must be included AFTER at least one other wx header that includes
 * wx/setup.h, otherwise #ifdefs erronously collapse it to nothing. This is
 * a bug in wxWindows (ffile.h should itself include wx/setup.h), and it
 * was a bitch to track down. */
#include <wx/ffile.h>

#include <vorbis/vorbisfile.h>

#include "../WaveTrack.h"
#include "ImportPlugin.h"

class OggImportPlugin : public ImportPlugin
{
public:
   OggImportPlugin():
      ImportPlugin(wxArrayString(WXSIZEOF(exts), exts))
   {
   }

   ~OggImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};


class OggImportFileHandle : public ImportFileHandle
{
public:
   OggImportFileHandle(wxFFile *file, OggVorbis_File *vorbisFile):
      mFile(file),
      mVorbisFile(vorbisFile),
      mProgressCallback(NULL),
      mUserData(NULL)
   {
   }
   ~OggImportFileHandle();

   void SetProgressCallback(progress_callback_t function,
                            void *userData);
   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   bool Import(TrackFactory *trackFactory, Track ***outTracks,
               int *outNumTracks, Tags *tags);
private:
   wxFFile *mFile;
   OggVorbis_File *mVorbisFile;
   progress_callback_t mProgressCallback;
   void *mUserData;
};

void GetOGGImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new OggImportPlugin);
}

wxString OggImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

ImportFileHandle *OggImportPlugin::Open(wxString filename)
{
   OggVorbis_File *vorbisFile = new OggVorbis_File;
   wxFFile *file = new wxFFile(filename, wxT("rb"));

   if (!file->IsOpened()) {
      // No need for a message box, it's done automatically (but how?)
      delete vorbisFile;
      delete file;
      return NULL;
   }

   int err = ov_open(file->fp(), vorbisFile, NULL, 0);

   if (err < 0) {
      wxString message;

      switch (err) {
         case OV_EREAD:
            message = _("Media read error");
            break;
         case OV_ENOTVORBIS:
            message = _("Not an Ogg Vorbis file");
            break;
         case OV_EVERSION:
            message = _("Vorbis version mismatch");
            break;
         case OV_EBADHEADER:
            message = _("Invalid Vorbis bitstream header");
            break;
         case OV_EFAULT:
            message = _("Internal logic fault");
            break;
      }

      // what to do with message?
      file->Close();
      delete vorbisFile;
      delete file;
      return NULL;
   }

   return new OggImportFileHandle(file, vorbisFile);
}

void OggImportFileHandle::SetProgressCallback(progress_callback_t progressCallback,
                                      void *userData)
{
   mProgressCallback = progressCallback;
   mUserData = userData;
}

wxString OggImportFileHandle::GetFileDescription()
{
   return DESC;
}

int OggImportFileHandle::GetFileUncompressedBytes()
{
   // TODO:
   return 0;
}

bool OggImportFileHandle::Import(TrackFactory *trackFactory, Track ***outTracks,
                                 int *outNumTracks, Tags *tags)
{
   wxASSERT(mFile->IsOpened());

   /* -1 is for the current logical bitstream */
   vorbis_info *vi = ov_info(mVorbisFile, -1);
   vorbis_comment *vc = ov_comment(mVorbisFile, -1);

   WaveTrack **channels = new WaveTrack *[vi->channels];

   int c;
   for (c = 0; c < vi->channels; c++) {
      channels[c] = trackFactory->NewWaveTrack(int16Sample, vi->rate);

      if (vi->channels == 2) {
         switch (c) {
         case 0:
            channels[c]->SetChannel(Track::LeftChannel);
            channels[c]->SetLinked(true);
            break;
         case 1:
            channels[c]->SetChannel(Track::RightChannel);
            channels[c]->SetTeamed(true);
            break;
         }
   }
      else {
         channels[c]->SetChannel(Track::MonoChannel);
      }
   }

/* The number of bytes to get from the codec in each run */
#define CODEC_TRANSFER_SIZE 4096

/* The number of samples to read between calls to the callback.
 * Balance between responsiveness of the GUI and throughput of import. */
#define SAMPLES_PER_CALLBACK 100000

   short *mainBuffer = new short[CODEC_TRANSFER_SIZE];

   /* determine endianness (clever trick courtesy of Nicholas Devillard,
    * (http://www.eso.org/~ndevilla/endian/) */
   int testvar = 1, endian;
   if(*(char *)&testvar)
      endian = 0;  // little endian
   else
      endian = 1;  // big endian

   /* number of samples currently in each channel's buffer */
   bool cancelled = false;
   long bytesRead = 0;
   long samplesRead = 0;
   int bitstream = 0;
   int samplesSinceLastCallback = 0;

   // You would think that the stream would already be seeked to 0, and
   // indeed it is if the file is legit.  But I had several ogg files on
   // my hard drive that have malformed headers, and this added call
   // causes them to be read correctly.  Otherwise they have lots of
   // zeros inserted at the beginning
   ov_pcm_seek(mVorbisFile, 0);
   
   do {
      /* get data from the decoder */
      bytesRead = ov_read(mVorbisFile, (char *) mainBuffer,
                          CODEC_TRANSFER_SIZE,
                          endian,
                          2,    // word length (2 for 16 bit samples)
                          1,    // signed
                          &bitstream);

      if (bytesRead < 0) {
         /* Malformed Ogg Vorbis file. */
         /* TODO: Return some sort of meaningful error. */
         break;
      }

      samplesRead = bytesRead / vi->channels / sizeof(short);

      /* give the data to the wavetracks */
      for (c = 0; c < vi->channels; c++)
          channels[c]->Append((char *)(mainBuffer + c),
                              int16Sample,
                              samplesRead,
                              vi->channels);

      samplesSinceLastCallback += samplesRead;
      if (samplesSinceLastCallback > SAMPLES_PER_CALLBACK) {
          if( mProgressCallback )
             cancelled = mProgressCallback(mUserData,
                                           ov_time_tell(mVorbisFile) /
                                           ov_time_total(mVorbisFile, bitstream));
          samplesSinceLastCallback -= SAMPLES_PER_CALLBACK;
      }

   } while (!cancelled && bytesRead != 0 && bitstream == 0);

   delete[]mainBuffer;

   bool res = (!cancelled && bytesRead >= 0);

   if (!res) {
      for(c = 0; c < vi->channels; c++) {
         delete channels[c];
      }
      delete[] channels;

      return false;
   }

   *outNumTracks = vi->channels;
   *outTracks = new Track *[vi->channels];
   for (c = 0; c < vi->channels; c++) {
      channels[c]->Flush();
         (*outTracks)[c] = channels[c];
   }
      delete[] channels;

   if (vc) {
      tags->Clear();
      for (c = 0; c < vc->comments; c++) {
         wxString comment = UTF8CTOWX(vc->user_comments[c]);
         tags->SetTag(comment.BeforeFirst(wxT('=')),
                      comment.AfterFirst(wxT('=')));
      }
   }

   return true;
}

OggImportFileHandle::~OggImportFileHandle()
{
   ov_clear(mVorbisFile);
   mFile->Detach();    // so that it doesn't try to close the file (ov_clear()
                       // did that already)

   delete mVorbisFile;
   delete mFile;
}

#endif                          /* USE_LIBVORBIS */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 1436952e-a888-41d9-922a-8ebf7413991d


/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportOGG.cpp

  Joshua Haberman

  The Ogg format supports multiple logical bitstreams that can be chained
  within the physical bitstream. The sampling rate and number of channels
  can vary between these logical bitstreams. For the moment, we'll ignore
  all but the first logical bitstream.

  Ogg also allows for an arbitrary number of channels. Luckily, so does
  Audacity. We'll call the first channel LeftChannel, the second
  RightChannel, and all others after it MonoChannel.

**********************************************************************/

#include "Audacity.h"

#ifdef USE_LIBVORBIS

/* MSVC does not know how to scope for loops */
#ifdef __WXWIN__
#define for if(1) for
#endif

#include <wx/log.h>
#include <wx/string.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
/* ffile.h must be included AFTER at least one other wx header that includes
 * wx/setup.h, otherwise #ifdefs erronously collapse it to nothing. This is
 * a bug in wxWindows (ffile.h should itself include wx/setup.h), and it 
 * was a bitch to track down. */
#include <wx/ffile.h>

#include <vorbis/vorbisfile.h>

#include "ImportOGG.h"

#include "WaveTrack.h"
#include "DirManager.h"

bool ImportOGG(wxWindow * parent,
               wxString Filename, WaveTrack ** channels[],
               int *numChannels, DirManager * dirManager)
{
   int c;
   wxLogNull logNo;   /* disable automatic dialog generation */
   FILE *file = fopen((const char *)Filename, "rb");

   if (!file || ferror(file)) {
      wxMessageBox("Unable to open file " + Filename, "Error",
                   wxICON_ERROR|wxOK|wxCENTRE);
      return false;
   }

   OggVorbis_File vf;
   int err = ov_open(file, &vf, NULL, 0);

   if (err < 0) {
      wxString message;

      switch (err) {
         case OV_EREAD:
            message = "Media read error";
            break;
         case OV_ENOTVORBIS:
            message = "Not an Ogg Vorbis file";
            break;
         case OV_EVERSION:
            message = "Vorbis version mismatch";
            break;
         case OV_EBADHEADER:
            message = "Invalid Vorbis bitstream header";
            break;
         case OV_EFAULT:
            message = "Internal logic fault";
            break;
      }

      wxMessageBox(message);
      fclose(file);
      return false;
   }

   /* -1 is for the current logical bitstream */
   vorbis_info *vi = ov_info(&vf, -1);

   *numChannels = vi->channels;
   *channels = new WaveTrack *[*numChannels];

   for (c = 0; c < *numChannels; c++) {
      (*channels)[c] = new WaveTrack(dirManager);
      (*channels)[c]->rate = vi->rate;

      /* link all channels together (so set 'linked' flag on all but the last) */

      if( c < *numChannels - 1 )
         (*channels)[c]->linked = true;

      switch (c) {
         case 0:
            (*channels)[c]->channel = VTrack::LeftChannel;
            break;
         case 1:
            (*channels)[c]->channel = VTrack::RightChannel;
            break;
         default:
            (*channels)[c]->channel = VTrack::MonoChannel;
      }
   }

   wxProgressDialog *progress = NULL;

   wxYield();
   wxStartTimer();

/* The number of bytes to get from the codec in each run */
#define CODEC_TRANSFER_SIZE 4196
   
   const int bufferSize = WaveTrack::GetIdealBlockSize();
   sampleType *mainBuffer = new sampleType[CODEC_TRANSFER_SIZE];

   sampleType **buffers = new sampleType *[*numChannels];
   for (int i = 0; i < *numChannels; i++) {
      buffers[i] = new sampleType[bufferSize];
   }

   /* determine endianness (clever trick courtesy of Nicholas Devillard,
    * (http://www.eso.org/~ndevilla/endian/) */
   int testvar = 1, endian;
   if(*(char *)&testvar)
      endian = 0;  // little endian
   else
      endian = 1;  // big endian

   /* number of samples currently in each channel's buffer */
   int bufferCount = 0;
   bool cancelled = false;
   long bytesRead = 0;
   long samplesRead = 0;
   int bitstream = 0;

   do {
      bytesRead = ov_read(&vf, (char *) mainBuffer, CODEC_TRANSFER_SIZE,
                          endian,
                          2,    // word length (2 for 16 bit samples)
                          1,    // signed
                          &bitstream);
      samplesRead = bytesRead / *numChannels / sizeof(sampleType);

      if (samplesRead + bufferCount > bufferSize) {
         for (int c = 0; c < *numChannels; c++)
            (*channels)[c]->Append(buffers[c], bufferCount);
         bufferCount = 0;
      }

      /* Un-interleave */
      for (int s = 0; s < samplesRead; s++)
         for (int c = 0; c < *numChannels; c++)
            buffers[c][s + bufferCount] =
                mainBuffer[s * (*numChannels) + c];

      bufferCount += samplesRead;


      if (!progress && wxGetElapsedTime(false) > 500)
         progress = new wxProgressDialog("Import",
                                         "Importing Ogg Vorbis File...",
                                         1000,
                                         parent,
                                         wxPD_CAN_ABORT |
                                         wxPD_REMAINING_TIME |
                                         wxPD_AUTO_HIDE);

      if (progress)
         cancelled = !progress->Update(ov_time_tell(&vf) * 1000 /
                                       ov_time_total(&vf, bitstream));

   } while (!cancelled && bytesRead != 0 && bitstream == 0);

   /* ...the rest is de-allocation */
   ov_clear(&vf);

   delete[]mainBuffer;

   for (c = 0; c < *numChannels; c++)
      delete[]buffers[c];
   delete[]buffers;

   if (progress)
      delete progress;

   if (cancelled) {
      for (int c = 0; c < *numChannels; c++)
         delete(*channels)[c];
      delete[] * channels;

      return false;
   }

   return true;

}

#endif                          /* USE_LIBVORBIS */

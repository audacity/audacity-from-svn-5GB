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

#include "../Audacity.h"

#ifdef USE_LIBVORBIS

#include <wx/string.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/intl.h>
/* ffile.h must be included AFTER at least one other wx header that includes
 * wx/setup.h, otherwise #ifdefs erronously collapse it to nothing. This is
 * a bug in wxWindows (ffile.h should itself include wx/setup.h), and it 
 * was a bitch to track down. */
#include <wx/ffile.h>

#include <vorbis/vorbisfile.h>

#include "ImportOGG.h"

#include "../WaveTrack.h"
#include "../DirManager.h"

bool ImportOGG(wxWindow * parent,
               wxString Filename, WaveTrack ** channels[],
               int *numChannels, DirManager * dirManager)
{

   wxFFile file(Filename, "rb");

   if (!file.IsOpened()) {
      // No need for a message box, it's done automatically (but how?)
      return false;
   }

   OggVorbis_File vf;
   int err = ov_open(file.fp(), &vf, NULL, 0);

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

      wxMessageBox(message);
      file.Close();
      return false;
   }

   /* -1 is for the current logical bitstream */
   vorbis_info *vi = ov_info(&vf, -1);

   *numChannels = vi->channels;
   *channels = new WaveTrack *[*numChannels];

   int c;
   for (c = 0; c < *numChannels; c++) {
      (*channels)[c] = new WaveTrack(dirManager);
      (*channels)[c]->SetRate(vi->rate);
      (*channels)[c]->SetSampleFormat(int16Sample);

      switch (c) {
         case 0:
            (*channels)[c]->SetChannel(VTrack::LeftChannel);
            break;
         case 1:
            (*channels)[c]->SetChannel(VTrack::RightChannel);
            break;
         default:
            (*channels)[c]->SetChannel(VTrack::MonoChannel);
      }
   }

   if (*numChannels == 2)
      (*channels)[0]->SetLinked(true);

   wxProgressDialog *progress = NULL;

   wxYield();
   wxStartTimer();

/* The number of bytes to get from the codec in each run */
#define CODEC_TRANSFER_SIZE 4096
   
   const int bufferSize = 1048576;
   short *mainBuffer = new short[CODEC_TRANSFER_SIZE];

   short **buffers = new short *[*numChannels];
   for (int i = 0; i < *numChannels; i++) {
      buffers[i] = new short[bufferSize];
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
      samplesRead = bytesRead / *numChannels / sizeof(short);

      if (samplesRead + bufferCount > bufferSize) {
         for (c = 0; c < *numChannels; c++)
            (*channels)[c]->Append((samplePtr)buffers[c],
                                   int16Sample,
                                   bufferCount);
         bufferCount = 0;
      }

      /* Un-interleave */
      for (int s = 0; s < samplesRead; s++)
         for (c = 0; c < *numChannels; c++)
            buffers[c][s + bufferCount] =
                mainBuffer[s * (*numChannels) + c];

      bufferCount += samplesRead;


      if (!progress && wxGetElapsedTime(false) > 500)
         progress = new wxProgressDialog(_("Import"),
                                         _("Importing Ogg Vorbis File..."),
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
   file.Detach();    // so that it doesn't try to close the file (ov_clear()
                     // did that already)

   delete[]mainBuffer;

   for (c = 0; c < *numChannels; c++)
      delete[]buffers[c];
   delete[]buffers;

   if (progress)
      delete progress;

   if (cancelled) {
      for (c = 0; c < *numChannels; c++)
         delete(*channels)[c];
      delete[] * channels;

      return false;
   }

   return true;

}

#endif                          /* USE_LIBVORBIS */

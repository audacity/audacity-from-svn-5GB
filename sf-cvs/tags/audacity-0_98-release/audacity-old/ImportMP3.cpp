/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.cpp

  Dominic Mazzoni
  Joshua Haberman

  Note: Audacity can be compiled to use one of two mp3 importing
  libraries:

  xaudio: a commercial mp3 decoder that we have obtained a free
  unlimited license to.  The library is available in binary form
  only from www.xaudio.com.  It is very fast, robust, and stable.

  libmad: a free as in speech and beer mp3 decoding library.

  There is a master symbol MP3SUPPORT -- if it is not defined, there
  be no MP3 support at all. If MP3SUPPORT is defined, another symbol
  must be defined as well to indicate which library -- at the moment,
  either USE_XAUDIO or USE_LIBMAD
  
**********************************************************************/

#include "Audacity.h"

#ifdef MP3SUPPORT

#include <wx/file.h>
#include <wx/thread.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/timer.h>

#include "Import.h"
#include "ImportMP3.h"

#include "DirManager.h"
#include "WaveTrack.h"
#include "Project.h"
#include "Tags.h"

#ifdef USE_LIBMAD

extern "C" {
#include "mad.h"
}

/** here are all of the callbacks and data structures for libmad ************/

#define INPUT_BUFFER_SIZE 65535
#define PROGRESS_SCALING_FACTOR 100000

/* this is a private structure we can use for whatever we like, and it will get
 * passed to each of the callback routines, allowing us to keep track of
 * things. */
struct priv_data {
   wxFile *file;   /* the file containing the mp3 data we're feeding the encoder */
   unsigned char *inputBuffer;
   WaveTrack **leftTrack, **rightTrack;
   sampleType *leftBuffer, *rightBuffer;
   sampleCount bufferSize;  /* how big each of the above buffers is */
   sampleCount numDecoded;  /* how many decoded samples are sitting in each buffer */
   wxString name;           /* what to name the created tracks */ 
   wxProgressDialog *progress;
   bool cancelled;
   DirManager *dirManager;  /* Grr. don't ask. */
   wxWindow *parent;        /* Likewise. */
};



/* convert libmad's fixed point representation to 16 bit signed integers. This
 * code is taken verbatim from minimad.c. */

inline sampleType scale(mad_fixed_t sample)
{
  /* round */
  sample += (1L << (MAD_F_FRACBITS - 16));

  /* clip */
  if (sample >= MAD_F_ONE)
    sample = MAD_F_ONE - 1;
  else if (sample < -MAD_F_ONE)
    sample = -MAD_F_ONE;

  /* quantize */
  return sample >> (MAD_F_FRACBITS + 1 - 16);
}



enum mad_flow input_cb(void *_data, struct mad_stream *stream)
{
   struct priv_data *data = (struct priv_data *)_data;

   if(data->file->Eof()) {
      data->cancelled = false;
      return MAD_FLOW_STOP;
   }

   if(!data->progress)
      data->progress =
         new wxProgressDialog("Import",
                              "Importing MP3 file...",
                              1000,
                              data->parent,
                              wxPD_CAN_ABORT | wxPD_ELAPSED_TIME |
                              wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);

   if( data->progress &&
      !data->progress->Update( int(1000.0 * data->file->Tell() /
                                   data->file->Length()) ) ) {
      
      /* user cancelled */

      data->cancelled = true;
      return MAD_FLOW_STOP;
   }
   
   /* "Each time you refill your buffer, you need to preserve the data in
    *  your existing buffer from stream.next_frame to the end.
    *
    *  This usually amounts to calling memmove() on this unconsumed portion
    *  of the buffer and appending new data after it, before calling
    *  mad_stream_buffer()"
    *           -- Rob Leslie, on the mad-dev mailing list */

   unsigned int unconsumedBytes;
   if(stream->next_frame) {
      unconsumedBytes = data->inputBuffer + INPUT_BUFFER_SIZE - stream->next_frame;
      memmove(data->inputBuffer, stream->next_frame, unconsumedBytes);
   }
   else
      unconsumedBytes = 0;

   
   size_t read = data->file->Read(data->inputBuffer + unconsumedBytes,
                                  INPUT_BUFFER_SIZE - unconsumedBytes);

   mad_stream_buffer(stream, data->inputBuffer, read + unconsumedBytes);

   return MAD_FLOW_CONTINUE;
}



enum mad_flow output_cb(void *_data,
                        struct mad_header const *header,
                        struct mad_pcm *pcm)
{
   unsigned int channels, samples, samplerate;
   mad_fixed_t const *left, *right;
   struct priv_data *data = (struct priv_data *)_data;

   samplerate= pcm->samplerate;
   channels  = pcm->channels;
   samples   = pcm->length;
   left      = pcm->samples[0];
   right     = pcm->samples[1];

   /* the left and right buffers get created on the first run */
 
   if(!data->leftBuffer) {
      data->leftBuffer = new sampleType[data->bufferSize];
      *data->leftTrack = new WaveTrack(data->dirManager);
      (*data->leftTrack)->channel = VTrack::MonoChannel;
      (*data->leftTrack)->name = data->name;
      (*data->leftTrack)->rate = samplerate;
      
      if(channels == 2) {
         data->rightBuffer = new sampleType[data->bufferSize];
         *data->rightTrack = new WaveTrack(data->dirManager);
         (*data->leftTrack)->channel = VTrack::LeftChannel;
         (*data->rightTrack)->channel = VTrack::RightChannel;
         (*data->leftTrack)->linked = true;
         (*data->rightTrack)->name = data->name;
         (*data->rightTrack)->rate = samplerate;
      }
   }

   if(data->numDecoded + samples > data->bufferSize) {
      (*data->leftTrack)->Append(data->leftBuffer, data->numDecoded);
      if(channels == 2)
         (*data->rightTrack)->Append(data->rightBuffer, data->numDecoded);

      data->numDecoded = 0;
   }

   while(samples--) {
      data->leftBuffer[data->numDecoded] = scale(*left++);
      if(channels == 2)
         data->rightBuffer[data->numDecoded] = scale(*right++);

      data->numDecoded++;
   }

   return MAD_FLOW_CONTINUE;
}

enum mad_flow error_cb(void *_data, struct mad_stream *stream, 
                       struct mad_frame *frame)
{
/* enum mad_flow {
     MAD_FLOW_CONTINUE = 0x0000,
     MAD_FLOW_STOP     = 0x0010,
     MAD_FLOW_BREAK    = 0x0011,
     MAD_FLOW_IGNORE   = 0x0020
   }; */

   return MAD_FLOW_CONTINUE;
}

#elif defined(USE_XAUDIO)
   #ifdef __WXGTK__
      #include "xaudio/linux/include/decoder.h"
      #include "xaudio/linux/include/mpeg_codec.h"
      #include "xaudio/linux/include/file_input.h"
   #endif
   #ifdef __WXMSW__
      #include "xaudio/win/include/decoder.h"
      #include "xaudio/win/include/file_input.h"
   #endif
   #ifdef __WXMAC__
      #include "xaudio/mac/include/decoder.h"
      #include "xaudio/mac/include/mpeg_codec.h"
      #include "xaudio/mac/include/file_input.h"
   #endif
#else
   #error MP3 support selected but no mp3 decoding library specified
#endif

bool ImportMP3(AudacityProject * project,
               wxString fName, WaveTrack ** left, WaveTrack ** right)
{
   DirManager *dirManager = project->GetDirManager();
   wxWindow *parent = project;

   *left = NULL;
   *right = NULL;

   Tags *tags = project->GetTags();
   tags->ImportID3(fName);

#ifdef USE_LIBMAD
   wxBusyCursor wait;

   /* Prepare decoder data, initialize decoder */

   wxFile file((char *) fName.c_str());

   if (!file.IsOpened()) {
      wxMessageBox("Could not open " + fName);
      return false;
   }

   struct priv_data data;
   struct mad_decoder decoder;

   data.file        = &file;
   data.inputBuffer = new unsigned char [INPUT_BUFFER_SIZE];
   data.leftTrack   = left;
   data.rightTrack  = right;
   data.leftBuffer  = NULL;
   data.rightBuffer = NULL;
   data.bufferSize  = WaveTrack::GetIdealBlockSize();
   data.numDecoded  = 0;
   data.name        = TrackNameFromFileName(fName);
   data.progress    = NULL;
   data.cancelled   = false;
   data.dirManager  = dirManager;
   data.parent      = parent;

   wxStartTimer();
   
   mad_decoder_init(&decoder, &data, input_cb, 0, 0, output_cb, error_cb, 0);
   
   /* and send the decoder on its way! */

   mad_decoder_run(&decoder, MAD_DECODER_MODE_SYNC);

   mad_decoder_finish(&decoder);

   /* write anything left in the buffers and clean up */

   (*data.leftTrack)->Append(data.leftBuffer, data.numDecoded);
   if(data.rightBuffer)
      (*data.rightTrack)->Append(data.rightBuffer, data.numDecoded);

   if(data.leftBuffer)
      delete [] data.leftBuffer;
   
   if(data.rightBuffer)
      delete [] data.rightBuffer;

   if(data.progress)
      delete data.progress;

   if(data.cancelled && *data.leftTrack) {
      delete *data.leftTrack;
      if(*data.rightTrack)
         delete *data.rightTrack;

      return false;
   }


   return true;

#elif defined(USE_XAUDIO)

   /* Use XAUDIO */
   wxBusyCursor wait;

   XA_DecoderInfo *decoder;
   int status;

   /* create a decoder */
   if (decoder_new(&decoder) != XA_SUCCESS) {
      wxMessageBox("Cannot Initialize MP3 Decoder 1");
      return false;
   }

   /* register the file input module */
   {
      XA_InputModule module;

      file_input_module_register(&module);
      decoder_input_module_register(decoder, &module);
   }

   /* register mpeg audio codec */
#ifndef __WXMSW__
   {
      XA_CodecModule module;

      mpeg_codec_module_register(&module);
      decoder_codec_module_register(decoder, &module);
   }
#endif

   *left = new WaveTrack(dirManager);
   *right = new WaveTrack(dirManager);

   (*left)->channel = VTrack::LeftChannel;
   (*right)->channel = VTrack::RightChannel;

   (*left)->name = TrackNameFromFileName(fName);
   (*right)->name = TrackNameFromFileName(fName);

   (*left)->linked = true;

   wxProgressDialog *progress = NULL;

   wxYield();
   wxStartTimer();

   /* create and open input object */

   const char *cFName = (const char *) fName;

   status =
       decoder_input_new(decoder, cFName, XA_DECODER_INPUT_AUTOSELECT);
   if (status != XA_SUCCESS) {
      wxMessageBox("Cannot open MP3 file");
      return false;
   }
   if (decoder_input_open(decoder) != XA_SUCCESS) {
      wxMessageBox("Cannot open MP3 file");
      return false;
   }

   /* allocate buffers for the PCM samples */

   const int bufferSize = (*left)->GetIdealBlockSize();
   int bufferCount = 0;
   sampleType *left_buffer = new sampleType[bufferSize];
   sampleType *right_buffer = new sampleType[bufferSize];
   bool stereo = true;

   bool cancelled = false;

   do {
      status = decoder_decode(decoder, NULL);

      if (status == XA_SUCCESS) {

#ifdef __WXGTK__
         int channels = decoder->output_buffer->channels;
         int samples = decoder->output_buffer->size / channels / 2;
#endif

#ifdef __WXMSW__
         int channels = decoder->output_buffer->stereo + 1;
         int samples = decoder->output_buffer->size / channels / 2;
#endif

#ifdef __WXMAC__
         int channels = decoder->output_buffer->channels;
         int samples = decoder->output_buffer->nb_samples;
#endif

         if (bufferCount + samples > bufferSize) {
            (*left)->Append(left_buffer, bufferCount);
            if (stereo)
               (*right)->Append(right_buffer, bufferCount);
            bufferCount = 0;
         }

         if (channels == 1) {
            stereo = false;

            for (int i = 0; i < samples; i++)
               left_buffer[bufferCount + i] =
                   (((sampleType *) decoder->output_buffer->
                     pcm_samples)[i]);
         } else {
            stereo = true;

            // Un-interleave the samples
            for (int i = 0; i < samples; i++) {
               left_buffer[bufferCount + i] =
                   (((sampleType *) (decoder->output_buffer->
                                     pcm_samples))[2 * i]);
               right_buffer[bufferCount + i] =
                   (((sampleType *) (decoder->output_buffer->
                                     pcm_samples))[2 * i + 1]);
            }
         }

         bufferCount += samples;

         (*left)->rate = double (decoder->output_buffer->sample_rate);
         (*right)->rate = double (decoder->output_buffer->sample_rate);
      }

      if (!progress && wxGetElapsedTime(false) > 500)
         progress =
             new wxProgressDialog("Import",
                                  "Importing MP3 file...",
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);

      if (progress) {
         cancelled =
             !progress->Update(int (decoder->status->position * 1000.0));
      }

   } while (cancelled == false &&
            (status == XA_SUCCESS ||
             status == XA_ERROR_TIMEOUT ||
             status == XA_ERROR_INVALID_FRAME));

   if (bufferCount) {
      (*left)->Append(left_buffer, bufferCount);
      if (stereo)
         (*right)->Append(right_buffer, bufferCount);
   }

   delete[]left_buffer;
   delete[]right_buffer;

   if (!stereo) {
      delete *right;
      *right = NULL;

      (*left)->channel = VTrack::MonoChannel;
      (*left)->linked = false;
   }

   if (progress)
      delete progress;

   if (cancelled) {
      if (*left) {
         delete *left;
         *left = NULL;
      }
      if (*right) {
         delete *right;
         *right = NULL;
      }

      return false;
   }

   return true;

#endif
}

#endif                          /* defined(MP3SUPPORT) */

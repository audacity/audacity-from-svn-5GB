/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.cpp

  Joshua Haberman

  Audacity has finally moved to using a single mp3 library on all
  platforms! It is the high performance, beautifully written libmad
  (mpeg audio decoder). Finally there is harmony in the mp3 universe.

  Much of this source code is based on 'minimad.c' as distributed
  with libmad.

  TODO (important!): solve the chicken and egg problem.
  
**********************************************************************/

#include "Audacity.h"

#ifdef USE_LIBMAD

#include <wx/file.h>
#include <wx/thread.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/timer.h>

extern "C" {
#include <mad.h>
}

#include "Import.h"
#include "ImportMP3.h"

#include "DirManager.h"
#include "WaveTrack.h"
#include "Project.h"
#include "Tags.h"

/* in mad.h, this struct is declared inside another struct. gcc's down with
 * that, but g++ complains of it being undeclared if you do. This isn't
 * a pretty solution, but it's really the only one. */

#define INPUT_BUFFER_SIZE 65535
#define PROGRESS_SCALING_FACTOR 100000

/* this is a private structure we can use for whatever we like, and it will get
 * passed to each of the callback routines, allowing us to keep track of
 * things. */
struct priv_data {
   wxFile *file;            /* the file containing the mp3 data we're feeding the encoder */
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

   unsigned int unconsumedBytes;
   if(stream->next_frame) {
      unconsumedBytes = data->inputBuffer + INPUT_BUFFER_SIZE - stream->next_frame;
      memmove(data->inputBuffer, stream->next_frame, unconsumedBytes);
   }
   else
      unconsumedBytes = 0;

   
   size_t read = data->file->Read(data->inputBuffer + unconsumedBytes,
                                  INPUT_BUFFER_SIZE - unconsumedBytes);

   mad_stream_buffer(stream, data->inputBuffer, read);

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
      (*data->leftTrack)->channel = VTrack::LeftChannel;
      (*data->leftTrack)->name = data->name;
      (*data->leftTrack)->rate = samplerate;
      
      if(channels == 2) {
         data->rightBuffer = new sampleType[data->bufferSize];
         *data->rightTrack = new WaveTrack(data->dirManager);
         (*data->rightTrack)->channel = VTrack::RightChannel;
         (*data->rightTrack)->name = data->name;
         (*data->rightTrack)->rate = samplerate;
         (*data->leftTrack)->linked = true;
      }
   }

   if(!data->progress && wxGetElapsedTime() > 500)
      data->progress = new wxProgressDialog("Import",
                                            "Importing MP3 file...",
                                            1000,
                                            data->parent,
                                            wxPD_CAN_ABORT | wxPD_ELAPSED_TIME |
                                            wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);

                                            
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

   if( data->progress &&
      !data->progress->Update( int(1000.0 * data->file->Tell() / data->file->Length()) ) ) {
      
      /* user cancelled */

      data->cancelled = true;
      return MAD_FLOW_STOP;
   }
   return MAD_FLOW_CONTINUE;
}



enum mad_flow error_cb(void *_data, struct mad_stream *stream, 
                       struct mad_frame *frame)
{
}



bool ImportMP3(AudacityProject * project,
               wxString fName, WaveTrack ** left, WaveTrack ** right)
{
   DirManager *dirManager = project->GetDirManager();
   wxWindow *parent = project;

   Tags *tags = project->GetTags();
   tags->ImportID3(fName);

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
}


#endif                          /* defined(USE_LIBMAD) */

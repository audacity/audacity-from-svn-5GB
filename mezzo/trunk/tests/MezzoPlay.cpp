/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  MezzoPlay.cpp

  Copyright (c) 2004 Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "portaudio/pa_common/portaudio.h"

#include "Sequence.h"
#include "ManagedFileContext.h"
#include "DiskIO.h"
#include "BufferGroup.h"
#include "XMLLoadStore.h"

#include <string>

extern "C" {
#include "srsw_queue.h"
}

using namespace Mezzo;

const int kBufferSize = 256;
const int kNumBuffers = 1024;
const int kSampleRate = 44100;

const int kMinBuffers = 32; /* Don't fill buffers unless we can do at
                               least this many */
const int kMaxBuffers = 64; /* Maximum number of playback buffers
                               in the queue at any one time */

ManagedFileContext *gContext;
BlockedSequence *gSequence;
BufferGroup gBuffers(kBufferSize, kNumBuffers);
SRSW_Queue *gQueue = srsw_queue_new();
int gDiskIOFinished = 0;
int gDiskIOPosition = 0;

void *DiskIOThreadFunc(void *arg)
{
   gDiskIO->Run();
   
   return NULL;
}

void StartDiskIOThread()
{
   pthread_t thread;

   if (pthread_create(&thread, NULL,
                      DiskIOThreadFunc, NULL)) {
      fprintf(stderr, "Fatal error: could not create Disk I/O thread!\n");
      exit(0);
   }   
}

void FillBuffers()
{
   int numBuffersInQueue = srsw_queue_get_count(gQueue);
   int numSamplesLeft = gSequence->GetLength() - gDiskIOPosition;
   int numBuffersLeft = (numSamplesLeft + (kBufferSize-1))/kBufferSize;
   int maxBuffersThisRound = kMaxBuffers - numBuffersInQueue;
   int numBuffersThisRound;
   
   numBuffersThisRound = numBuffersLeft;
   if (numBuffersThisRound > maxBuffersThisRound)
      numBuffersThisRound = maxBuffersThisRound;
   
   if (numBuffersThisRound >= kMinBuffers ||
       (numBuffersLeft > 0 &&
        numBuffersLeft < kMinBuffers &&
        numBuffersLeft <= maxBuffersThisRound)) {
      printf("Filling %d buffers\n", numBuffersThisRound);

      int len = numBuffersThisRound * kBufferSize;
      int getLen = len;
      if (getLen + gDiskIOPosition > gSequence->GetLength())
         getLen = gSequence->GetLength() - gDiskIOPosition;
      Buffer buffer = gSequence->Get(gDiskIOPosition, getLen);
      if (getLen < len)
         buffer = Buffer::Append(buffer, FloatBuffer(len-getLen));
      
      int i;
      for(i=0; i<numBuffersThisRound; i++) {
         float *floatBuffer = gBuffers.Allocate();
         buffer.AsFloat().Get(floatBuffer, i*kBufferSize, kBufferSize);
         srsw_queue_enqueue(gQueue, (void *)floatBuffer);
      }
      
      gDiskIOPosition += getLen;
      
      if (gDiskIOPosition >= gSequence->GetLength()) {
         printf("Disk I/O Finished\n");
         gDiskIOFinished = 1;
      }
   }
}

class MyDiskIOClass : public DiskIOCallable
{
   virtual void HandleDiskIOTasks()
   {
      FillBuffers();
   }
};

static int playCallback( void *inputBuffer, void *outputBuffer,
                         unsigned long framesPerBuffer,
                         PaTimestamp outTime, void *userData )
{
   if (framesPerBuffer != (unsigned long)kBufferSize) {
      fprintf(stderr, "Expected %d samples in callback, got %d\n",
              (int)kBufferSize, (int)framesPerBuffer);
      return 0;
   }

   if (!outputBuffer) {
      fprintf(stderr, "No output buffer in callback!\n");
      return 0;
   }

   if (srsw_queue_get_count(gQueue) <= 0) {
      memset(outputBuffer, 0, kBufferSize * sizeof(float));
      if (gDiskIOFinished) {
         printf("Audio I/O exiting cleanly (path 2)\n");
         return 1; /* Finished */
      }
      else {
         fprintf(stderr, "playCallback starved!\n");
         return 0;
      }
   }

   float *floatBuffer;
   srsw_queue_dequeue(gQueue, (void **)&floatBuffer);
   memcpy(outputBuffer, floatBuffer, kBufferSize * sizeof(float));
   gBuffers.Recycle(floatBuffer);

   if (srsw_queue_get_count(gQueue) <= 0 && gDiskIOFinished) {
      printf("Audio I/O exiting cleanly (path 1)\n");
      return 1; /* Finished */
   }
   else
      return 0; /* Not finished */
}

int main(int argc, char **argv)
{
   if (argc < 2) {
      printf("Usage: %s <Mezzo Project XML File>\n",
             argv[0]);
      return -1;
   }

   XMLLoader loader(argv[1]);
   loader.GetNextToken();

   gContext = new ManagedFileContext(loader);
   gSequence = new BlockedSequence(loader);

   MyDiskIOClass *diskIOClass = new MyDiskIOClass();
   PortAudioStream *stream;
   PaError    err;

   gDiskIOFinished = 0;
   gDiskIOPosition = 0;

   FillBuffers();

   StartDiskIOThread();
   gDiskIO->RegisterCallable(diskIOClass, 1);
   
   err = Pa_Initialize();
   if( err != paNoError )
      goto error;

   /* Record some audio. -------------------------------------------- */
   err = Pa_OpenStream(&stream,
                       paNoDevice,
                       0,
                       paFloat32,
                       NULL,
                       Pa_GetDefaultOutputDeviceID(),
                       1, /* one channel out */
                       paFloat32,
                       NULL,
                       kSampleRate,
                       kBufferSize,  /* frames per buffer */
                       0,    /* number of buffers, if zero use default */
                       0, //paDitherOff,    /* flags */
                       playCallback,
                       NULL );
    if( err != paNoError )
       goto error;

    err = Pa_StartStream( stream );
    if( err != paNoError )
       goto error;

    while(!gDiskIOFinished) {
       sleep(1);
    }

    printf("Finished playing %d samples!\n", (int)gSequence->GetLength());

    gDiskIO->UnregisterCallable(diskIOClass);

    Pa_StopStream( stream );

    Pa_Terminate();

    return 0;

 error:
    Pa_Terminate();
    if (err) {
       fprintf( stderr, "Error number: %d\n", err );
       fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    }

    return -1;    
}

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


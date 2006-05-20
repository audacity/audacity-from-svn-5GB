/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  MezzoRecord.cpp

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
const int kTotalSamples = kSampleRate * 10;

std::string gDirectoryName = "MezzoRecordedData";
ManagedFileContext *gContext;
BlockedSequence *gSequence;
BufferGroup gBuffers(kBufferSize, kNumBuffers);
SRSW_Queue *gQueue = srsw_queue_new();
int gAudioIOTotal;
int gDiskIOTotal;

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

class MyDiskIOClass : public DiskIOCallable
{
   virtual void HandleDiskIOTasks()
   {
      int numItems = srsw_queue_get_count(gQueue);
      int numSamples = numItems * kBufferSize;
      
      if (numSamples > kSampleRate ||
          (numSamples > 0 && gAudioIOTotal >= kTotalSamples)) {
         printf("%d samples of data to append to the Sequence\n",
                numSamples);
         
         FloatBuffer buffer(numSamples);
         int i;
         for(i=0; i<numItems; i++) {
            float *dataPtr;
            srsw_queue_dequeue(gQueue, (void **)&dataPtr);
            buffer.Set(dataPtr, i*kBufferSize, kBufferSize);
            gBuffers.Recycle(dataPtr);
         }
         gSequence->Append(buffer);
      }
   }
};

static int recordCallback( void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           PaTimestamp outTime, void *userData )
{
   float *buffer;

   if (framesPerBuffer != (unsigned long)kBufferSize) {
      fprintf(stderr, "Expected %d samples in callback, got %d\n",
              (int)kBufferSize, (int)framesPerBuffer);
      return 0;
   }

   if (!inputBuffer) {
      fprintf(stderr, "No input buffer in callback!\n");
      return 0;
   }

   buffer = gBuffers.Allocate();
   memcpy(buffer, inputBuffer, kBufferSize * sizeof(float));

   srsw_queue_enqueue(gQueue, (void *)buffer);

   gAudioIOTotal += kBufferSize;
   if (gAudioIOTotal >= kTotalSamples) {
      fprintf(stderr, "AudioIO finished\n");
      return 1; /* Finished */
   }

   return 0; /* Not finished */
}

int main(void)
{
   MyDiskIOClass *diskIOClass = new MyDiskIOClass();
   PortAudioStream *stream;
   PaError    err;

   gContext = new ManagedFileContext(gDirectoryName);
   gSequence = new BlockedSequence(gContext, Buffer::FloatSample);
   gAudioIOTotal = 0;
   gDiskIOTotal = 0;

   StartDiskIOThread();
   gDiskIO->RegisterCallable(diskIOClass, 1);
   
   err = Pa_Initialize();
   if( err != paNoError )
      goto error;

   /* Record some audio. -------------------------------------------- */
   err = Pa_OpenStream(&stream,
                       Pa_GetDefaultInputDeviceID(),
                       1, /* one channel in */
                       paFloat32,
                       NULL,
                       paNoDevice,
                       0,
                       paFloat32,
                       NULL,
                       kSampleRate,
                       kBufferSize,  /* frames per buffer */
                       0,    /* number of buffers, if zero use default */
                       0, //paDitherOff,    /* flags */
                       recordCallback,
                       NULL );
    if( err != paNoError )
       goto error;

    err = Pa_StartStream( stream );
    if( err != paNoError )
       goto error;

    while(gSequence->GetLength() < kTotalSamples) {
       sleep(1);
    }

    printf("Finished recording %d samples!\n", (int)gSequence->GetLength());

    gDiskIO->UnregisterCallable(diskIOClass);

    Pa_StopStream( stream );

    Pa_Terminate();

    if (1) {
       XMLStorer storer(gDirectoryName + "/mezzo.xml");
       AttrDict attrs;
       storer.StoreBeginNode("MezzoProject", attrs);
       gContext->Store(storer, gDirectoryName,
                       gSequence->GetManagedFilesInUse());
       gSequence->Store(storer);
       storer.StoreEndNode("MezzoProject");
    }

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


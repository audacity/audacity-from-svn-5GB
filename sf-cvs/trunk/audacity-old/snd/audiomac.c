/*
 * audiomac.c
 *
 * Written by Dominic Mazzoni
 *
 * sndlib by Roger Dannenberg
 *
 *
 * Brief description of algorithm: The Macintosh uses callback-based double-buffering
 * for realtime audio support.  We create three buffers: two that the Mac uses for
 * its double-buffering, and a third that we fill to keep ahead of it.  audio_poll
 * returns the sum of available samples in all three.  As soon as two buffers are full,
 * the Mac OS is told to start playing.  When the Mac OS has finished with one buffer,
 * it calls our callback routine.  If we have any bytes in our third buffer, we copy
 * them to the empty buffer and clear the third buffer, and then mark the newly filled
 * buffer as ready to play.
 *
 * Good reference:
 *
 * http://devworld.apple.com/samplecode/Sample_Code/Sound/SndPlayDoubleBuffer.htm
 *
 */
 
#include "memory.h"
#include "stdio.h"

#include "snd.h"
#include "audiomac.h"

#include <FixMath.h>

#ifdef __cplusplus
extern "C" {
#endif

pascal void doubleBack(SndChannelPtr channel, SndDoubleBufferPtr doubleBufferPtr)
{
  buffer_state *data = (buffer_state *)doubleBufferPtr->dbUserInfo[0];

  if (data->busy)
    return;
    
  data->busy = 1;

  // If there's data in our third buffer, copy it into the available double-buffer
  // and mark our third buffer as empty again.

  if (data->curBuffer == 2 && data->curSize>0) {
    BlockMove((Ptr)data->nextBuffer, (Ptr)doubleBufferPtr->dbSoundData, data->curSize);    
    doubleBufferPtr->dbNumFrames = data->curSize / data->frameSize;
    doubleBufferPtr->dbFlags |= dbBufferReady;
    if (data->flushing) {
      doubleBufferPtr->dbFlags |= dbLastBuffer;
      data->finished = 1;
    }
    
    data->curSize = 0;
  }
  else {
  
    // Otherwise, either we're finished playing or we're stalling
  
    if (data->flushing) {
      // Send a single final sample and tell it to stop
      ((short *)doubleBufferPtr->dbSoundData)[0] = 0;
      doubleBufferPtr->dbNumFrames = 1;
      doubleBufferPtr->dbFlags |= dbLastBuffer;
      doubleBufferPtr->dbFlags |= dbBufferReady;
      data->finished = 1;
    }
    else {
      // Send some silence through the speaker while we wait for
      // the program to catch up
      int i;
      for(i=0; i<data->bufferSize / 2; i++)
          ((short *)doubleBufferPtr->dbSoundData)[i] = 0;
      doubleBufferPtr->dbNumFrames = data->bufferSize / data->frameSize;
      doubleBufferPtr->dbFlags |= dbBufferReady;
    }
  }
  
  data->busy = 0;
}

int audio_open(snd_node *n, long *f)
{
  buffer_state *data = (buffer_state *)malloc(sizeof(buffer_state));
  n->u.audio.descriptor = (void *)data;
  unsigned short numerator;
  unsigned short denomenator;
  Fixed sampleRateFixed;

  OSErr	err;
	
  data->chan = NULL;
  err = SndNewChannel(&data->chan, sampledSynth, 0, NULL);
	
  if (err)
    return !SND_SUCCESS;
	  
  data->frameSize = snd_bytes_per_frame(n);

  data->bufferSize = (int) (0.1 * n->format.srate * (double)data->frameSize);
  if (n->u.audio.latency > 0.0)
    data->bufferSize = (int)(n->format.srate * n->u.audio.latency) * data->frameSize;

  data->buffer[0] = (SndDoubleBuffer *)malloc(sizeof(SndDoubleBuffer) + data->bufferSize);
  data->buffer[0]->dbNumFrames = data->bufferSize / data->frameSize;
  data->buffer[0]->dbFlags = 0;
  data->buffer[0]->dbUserInfo[0] = (long)data;
  data->buffer[1] = (SndDoubleBuffer *)malloc(sizeof(SndDoubleBuffer) + data->bufferSize);    
  data->buffer[1]->dbNumFrames = data->bufferSize / data->frameSize;
  data->buffer[1]->dbFlags = 0;
  data->buffer[1]->dbUserInfo[0] = (long)data;
  
  data->nextBuffer = (char *)malloc(data->bufferSize);   

  if (!data->buffer[0] || !data->buffer[1])
    return !SND_SUCCESS;

  /* Calculate sample rate as an unsigned fixed-point number */
  if (n->format.srate > 65535.0 ||
      n->format.srate < 1.0)
    sampleRateFixed = 0xAC440000; /* Fixed for 44100 */
  else {
    numerator = (unsigned short)n->format.srate;
    denomenator = (unsigned short)(65536.0*(n->format.srate - numerator));
    sampleRateFixed = (numerator << 16) | denomenator;
  }

  data->dbheader.dbhSampleRate = sampleRateFixed;
  data->dbheader.dbhNumChannels = n->format.channels;
  data->dbheader.dbhSampleSize = 16;
  data->dbheader.dbhCompressionID = 0;
  data->dbheader.dbhPacketSize = 0;
  
  data->dbheader.dbhBufferPtr[0] = data->buffer[0];
  data->dbheader.dbhBufferPtr[1] = data->buffer[1];
  
  // On the Mac, function pointers are structures with special extra fields.
  // Luckily, the Sound Manager provides us with a routine to create one
  // given a pointer to our callback function.
  data->dbheader.dbhDoubleBack = NewSndDoubleBackProc(doubleBack);
  
  data->curBuffer = 0;
  data->curSize = 0;
  data->firstTime = 1;
  data->finished = 0;
  data->busy = 0;
  data->flushing = 0;
  
  return SND_SUCCESS;
}


int audio_close(snd_node *n)
{
  buffer_state *data = (buffer_state *)n->u.audio.descriptor;
  OSErr err;
  
  data->finished = 1;
  
  err = SndDisposeChannel(data->chan,
                          true         // quiets the channel now
                          );

  DisposeRoutineDescriptor(data->dbheader.dbhDoubleBack);

  free((void *)data->buffer[0]);  
  free((void *)data->buffer[1]);  
  free((void *)data->nextBuffer);
  
  free((void *)data);

  return SND_SUCCESS;
}


int audio_flush(snd_type snd)
{
  buffer_state *data = (buffer_state *)snd->u.audio.descriptor;
  SCStatus status;
  OSErr err;

  data->flushing = 1;
  
  // Start playback if we haven't already

  if (data->firstTime) {
    data->buffer[data->curBuffer]->dbNumFrames = data->curSize / data->frameSize;
    data->buffer[0]->dbFlags |= dbBufferReady;
    if (data->curBuffer == 0) {
      data->buffer[0]->dbFlags |= dbLastBuffer;
    }
    else
      data->buffer[1]->dbFlags |= (dbBufferReady | dbLastBuffer);    
    
    SndPlayDoubleBuffer(data->chan, &data->dbheader);
    data->firstTime = 0;
  }

  do {
    err = SndChannelStatus(data->chan, sizeof(status), &status);
  } while (!err && status.scChannelBusy);

  return SND_SUCCESS;
}


long audio_read(snd_node *n, void *buffer, long length)
{
    /* audio read not implemented */
    return !SND_SUCCESS;
}


long audio_write(snd_node *n, void *buffer, long length)
{
  buffer_state *data = (buffer_state *)n->u.audio.descriptor;
  
  // Copy into the initial two buffers
  
  long written = 0;
  long block;
  
  while(data->curBuffer < 2 && length>0) {
    block = min(length, data->bufferSize - data->curSize);
    
    if (block>0) {
    
      Ptr dest = (Ptr)&(((char *)data->buffer[data->curBuffer]->dbSoundData)[data->curSize]);
      BlockMove((Ptr)buffer, dest, block);
      
      length -= block;
      written += block;
      data->curSize += block;
      buffer = &((char *)buffer)[block];
      
      if (data->curSize == data->bufferSize) {
        data->buffer[data->curBuffer]->dbFlags |= dbBufferReady;
        data->curSize = 0;
        data->curBuffer++;
      }
    }
  }
  
  // Copy into the third buffer (the one we don't pass to the Sound Manager directly)

  if (data->curBuffer == 2 && length>0) {
    block = min(length, data->bufferSize - data->curSize);
    
    if (block > 0) {
    
      Ptr dest = (Ptr)&data->nextBuffer[data->curSize];
      BlockMove((Ptr)buffer, dest, block);
      
      length -= block;
      written += block;
      data->curSize += block;
    }
  }

  // when both buffers are full for the first time, start playback:

  if (data->curBuffer==2 && data->firstTime) {
    SndPlayDoubleBuffer(data->chan, &data->dbheader);
    data->firstTime = 0;
  }

  return written;
}

int audio_reset(snd_node *n)
{
    /* audio reset not implemented */
    return !SND_SUCCESS;
}

long audio_poll(snd_type snd)
{
  buffer_state *data = (buffer_state *)snd->u.audio.descriptor;

  long avail = data->bufferSize - data->curSize;

  if (data->curBuffer < 2)
    avail += data->bufferSize;
  if (data->curBuffer < 1)
    avail += data->bufferSize;  

  // Is this a bug in snd that I have to return frames here,
  // and bytes everywhere else?
  
  return avail / data->frameSize;
}

snd_fns_node mac_dictionary = { audio_poll, audio_read, audio_write, 
                       audio_open, audio_close, audio_reset, audio_flush };


void snd_init()
{
    snd_add_device("Macintosh", "default", &mac_dictionary);
}


#ifdef __cplusplus
} // extern "C"
#endif


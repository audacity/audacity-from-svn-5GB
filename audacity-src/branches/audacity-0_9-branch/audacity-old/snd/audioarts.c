/*
  aRts (analog real-time synthesizer; comes with KDE
  on Linux) implementation of snd by Dominic Mazzoni
*/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>

#include <sys/time.h>

#include <kde/artsc/artsc.h>

/* snd includes */

#include "snd.h"

typedef struct {
   arts_stream_t    stream;

   int              buffersize;
   int              start;
   int              len;
   void            *buffer;
   pthread_t        thread;
   pthread_mutex_t  mutex;

   int              error_count;

   int              stop_flag;

} arts_info_struct, *arts_info;

void copy_from_wrap_buffer(arts_info info, void *dest, int bytes_to_copy)
{
   char *dstp;
   int block;

   if (bytes_to_copy > info->len)
      bytes_to_copy = info->len;

   dstp = (char *)dest;

   while(bytes_to_copy) {
      block = bytes_to_copy;
      if (block > info->buffersize - info->start)
         block = info->buffersize - info->start;
      
      memcpy(dstp, (char *)info->buffer + info->start, block);
      dstp += block;
      info->start = (info->start + block) % info->buffersize;
      info->len -= block;
      bytes_to_copy -= block;
   }
}

int copy_to_wrap_buffer(arts_info info, void *src, int bytes_to_copy)
{
   char *srcp;
   int block;
   int copied;
   int pos;

   if (bytes_to_copy > info->buffersize - info->len)
      bytes_to_copy = info->buffersize - info->len;

   srcp = (char *)src;
   copied = 0;
   pos = (info->start + info->len) % info->buffersize;

   while(bytes_to_copy) {
      block = bytes_to_copy;
      if (block > info->buffersize - pos)
         block = info->buffersize - pos;
      
      memcpy((char *)info->buffer + pos, srcp, block);
      srcp += block;
      pos = (pos + block) % info->buffersize;
      bytes_to_copy -= block;
      copied += block;
      info->len += block;
   }

   return bytes_to_copy;
}


arts_info get_arts_info(snd_type snd)
{
   return (arts_info) snd->u.audio.descriptor;
}


void *record_thread(void *param)
{
   arts_info dp = (arts_info)param;
   int buffer_size;
   char *buffer;
   
   buffer_size = 256;
   buffer = (char *)malloc(buffer_size);

   while(!dp->stop_flag) {
      int rval = arts_read(dp->stream, buffer, buffer_size);
      if (rval > 0) {
         int copied;

         pthread_mutex_lock(&dp->mutex);

         copied = copy_to_wrap_buffer(dp, buffer, rval);
         if (copied != rval)
            dp->error_count++;

         pthread_mutex_unlock(&dp->mutex);
      }
   }

   free(buffer);
}


void *playback_thread(void *param)
{
   arts_info dp = (arts_info)param;
   int buffer_size;
   char *buffer;
   
   buffer_size = 65536;
   buffer = (char *)malloc(buffer_size);

   while(!dp->stop_flag) {
      if (dp->len > 0) {
         int block = dp->len;
         if (block > buffer_size)
            block = buffer_size;

         pthread_mutex_lock(&dp->mutex);
         copy_from_wrap_buffer(dp, buffer, block);
         pthread_mutex_unlock(&dp->mutex);

         arts_write(dp->stream, buffer, block);
      }
      else {
         pthread_yield();
      }
   }

   free(buffer);
}


int arts_audio_open(snd_type snd, long *flags)
{
   int channels;
   int rate;
   int bits;
   arts_info dp;
   pthread_t thread;

   if (arts_init() != 0)
      return !SND_SUCCESS;

   snd->u.audio.descriptor = (arts_info) malloc(sizeof(arts_info_struct));
   dp = get_arts_info(snd);

   channels = snd->format.channels;
   rate = (int)(snd->format.srate + 0.5);
   bits = 16;

   if (snd->write_flag == SND_READ)
      dp->stream = arts_record_stream(rate, bits, channels,
                                      "Audacity In");
   else {
      dp->stream = arts_play_stream(rate, bits, channels,
                                    "Audacity Out");
   }
   
   if (!dp->stream)
      return !SND_SUCCESS;
   
   if (snd->u.audio.latency > 0.0)
      dp->buffersize = rate * snd->u.audio.latency * snd_bytes_per_frame(snd);
   else {
      if (snd->write_flag == SND_READ)
         dp->buffersize = rate * 5.0 * snd_bytes_per_frame(snd);
      else
         dp->buffersize = rate * 1.0 * snd_bytes_per_frame(snd);
   }

   dp->buffer = malloc(dp->buffersize);
   
   dp->start = 0;
   dp->len = 0;
   dp->error_count = 0;
   dp->stop_flag = 0;
   
   pthread_mutex_init (&dp->mutex, 0);
   
   if (snd->write_flag == SND_READ) {
      if (0 != pthread_create(&dp->thread, NULL,
                              record_thread, (void *)dp))
         return !SND_SUCCESS;
   }
   else {
      if (0 != pthread_create(&dp->thread, NULL,
                              playback_thread, (void *)dp))
         return !SND_SUCCESS;
   }
  
   return SND_SUCCESS;
}


long arts_audio_poll(snd_type snd)
{
   arts_info dp = get_arts_info(snd);

   /* Note that this returns frames while arts_audio_write
      returns bytes */

   if (snd->write_flag == SND_READ)
      return (dp->len / snd_bytes_per_frame(snd));
   else
      return ((dp->buffersize - dp->len) /
              snd_bytes_per_frame(snd));
}


long arts_audio_read(snd_type snd, void *buffer, long length_in_bytes)
{
   arts_info dp = get_arts_info(snd);

   pthread_mutex_lock(&dp->mutex);

   if (dp->len > length_in_bytes)
      length_in_bytes = dp->len;

   copy_from_wrap_buffer(dp, buffer, length_in_bytes);

   pthread_mutex_unlock(&dp->mutex);

   return (length_in_bytes / snd_bytes_per_frame(snd));
}


long arts_audio_write(snd_type snd, void *buffer, long length_in_bytes)
{
   arts_info dp = get_arts_info(snd);

   pthread_mutex_lock(&dp->mutex);

   if (dp->len + length_in_bytes > dp->buffersize)
      length_in_bytes = dp->buffersize - dp->len;

   copy_to_wrap_buffer(dp, buffer, length_in_bytes);

   pthread_mutex_unlock(&dp->mutex);

   return (length_in_bytes / snd_bytes_per_frame(snd));
}


/* arts_audio_flush -- finish audio output */
int arts_audio_flush(snd_type snd)
{
   arts_info dp = get_arts_info(snd);

   if (snd->write_flag = SND_WRITE) {
      /* wait until what's already queued up has
         finished playing */

      while(dp->len > 0) {
         pthread_yield();
      }
   }
   
   dp->stop_flag = 1;
   pthread_join(dp->thread, 0);
   dp->thread = 0;
   
   return SND_SUCCESS;
}


int arts_audio_reset(snd_type snd)
{
   arts_info dp = get_arts_info(snd);

   dp->stop_flag = 1;
   /* Wait for our thread to finish */
   pthread_join(dp->thread, 0);
   dp->stream = 0;
   dp->thread = 0;
   
   return SND_SUCCESS;
}


int arts_audio_close(snd_type snd)
{
   arts_info dp = get_arts_info(snd);

   if (dp->thread)
      arts_audio_reset(snd);
   
   if (dp->stream) {
      arts_close_stream(dp->stream);      
      dp->stream = 0;
   }
   
   free(dp->buffer);
   dp->buffer = 0;

   free((void *)snd->u.audio.descriptor);

   arts_free();

   return SND_SUCCESS;
}


snd_fns_node artssystem_dictionary = { arts_audio_poll, arts_audio_read,
                                       arts_audio_write, arts_audio_open,
                                       arts_audio_close, arts_audio_reset,
                                       arts_audio_flush };

void snd_init()
{
   snd_add_device("aRts", "default", &artssystem_dictionary);
}

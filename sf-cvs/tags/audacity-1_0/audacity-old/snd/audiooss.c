/*
  Open Sound System (oss) implementation of snd
  by Dominic Mazzoni

  Version 2 (Dec 2001): uses a second thread to
  actually handle the recording and playback,
  instead of polling the device.  This is
  much more compatible with the vast majority
  of soundcards.
*/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>

#include <sys/ioctl.h>
#include <sys/soundcard.h>
#include <sys/time.h>

/* snd includes */

#include "snd.h"

typedef struct {
   int              audio_fd;

   int              buffersize;
   int              start;
   int              len;
   void            *buffer;
   pthread_t        thread;
   pthread_mutex_t  mutex;

   int              error_count;

   int              stop_flag;

} oss_info_struct, *oss_info;

void copy_from_wrap_buffer(oss_info info, void *dest, int bytes_to_copy)
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

int copy_to_wrap_buffer(oss_info info, void *src, int bytes_to_copy)
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

   return copied;
}


oss_info get_oss_info(snd_type snd)
{
   return (oss_info) snd->u.audio.descriptor;
}


void *record_thread(void *param)
{
   oss_info dp = (oss_info)param;
   struct timeval timeout;
   fd_set readfds;
   fd_set writefds;
   fd_set exceptfds;
   int n;
   int buffer_size;
   char *buffer;
   
   /* start recording immediately */
   timeout.tv_sec = 0;
   timeout.tv_usec = 0;
   FD_ZERO(&readfds);
   FD_ZERO(&writefds);
   FD_ZERO(&exceptfds);
   FD_SET(dp->audio_fd, &readfds);
   
   n = dp->audio_fd + 1;
   
   select(n, &readfds, &writefds, &exceptfds, &timeout);

   buffer_size = 256;
   buffer = (char *)malloc(buffer_size);

   while(!dp->stop_flag) {
      int rval = read(dp->audio_fd, buffer, buffer_size);
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
   oss_info dp = (oss_info)param;
   int buffer_size;
   char *buffer;
   char *p;
   
   buffer_size = 65536;
   buffer = (char *)malloc(buffer_size);

   while(!dp->stop_flag) {
      if (dp->len > 0) {
         int block = dp->len;
         int written;
         if (block > buffer_size)
            block = buffer_size;

         pthread_mutex_lock(&dp->mutex);
         copy_from_wrap_buffer(dp, buffer, block);
         pthread_mutex_unlock(&dp->mutex);

         written = 0;
         while(written < block) {
            int rval = write(dp->audio_fd, buffer, block);
            if (rval < 0)
               return;
            written += rval;
         }
      }
      else {
         sched_yield();
      }
   }

   free(buffer);
}


int audio_open(snd_type snd, long *flags)
{
   int format;
   int channels;
   int rate;
   oss_info dp;
   const char *device = "/dev/dsp";
   pthread_t thread;

   snd->u.audio.descriptor = (oss_info) malloc(sizeof(oss_info_struct));
   dp = get_oss_info(snd);

   if (snd->u.audio.devicename[0] != 0)
      device = snd->u.audio.devicename;

   dp->audio_fd = open(device, O_RDWR|O_NONBLOCK, 0);
	
   if (dp->audio_fd == -1)
      return !SND_SUCCESS;

   close(dp->audio_fd);
   dp->audio_fd = open(device, O_RDWR, 0);
	
   /* Set format to signed 16-bit little-endian */
   format = AFMT_S16_LE;
   if (ioctl(dp->audio_fd, SNDCTL_DSP_SETFMT, &format) == -1)
      return !SND_SUCCESS;
   if (format != AFMT_S16_LE) /* this format is not supported */
      return !SND_SUCCESS;
  
   /* Set number of channels */
   channels = snd->format.channels;
   if (ioctl(dp->audio_fd, SNDCTL_DSP_CHANNELS, &channels) == -1)
      return !SND_SUCCESS;
   if (channels != snd->format.channels)
      return !SND_SUCCESS;
  
   /* Set sampling rate.  Must set sampling rate AFTER setting
      number of channels. */
   rate = (int)(snd->format.srate + 0.5);
   if (ioctl(dp->audio_fd, SNDCTL_DSP_SPEED, &rate) == -1)
      return !SND_SUCCESS;
   if (rate - (int)(snd->format.srate + 0.5) > 100 ||
       rate - (int)(snd->format.srate + 0.5) < -100)
      return !SND_SUCCESS;

   /* finish initialization and start the thread */

   if (snd->write_flag == SND_READ) {
      /* always buffer 10 seconds of audio for reading
         (no reason not to for this implementation */
      dp->buffersize = rate * 10.0 * snd_bytes_per_frame(snd); 
   }
   else {
      if (snd->u.audio.latency > 0.0)
         dp->buffersize = rate * snd->u.audio.latency * 
            snd_bytes_per_frame(snd);
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


long audio_poll(snd_type snd)
{
   oss_info dp = get_oss_info(snd);

   /* Note that this returns frames while audio_write
      returns bytes */

   if (snd->write_flag == SND_READ)
      return (dp->len / snd_bytes_per_frame(snd));
   else
      return ((dp->buffersize - dp->len) /
              snd_bytes_per_frame(snd));
}


long audio_read(snd_type snd, void *buffer, long length_in_bytes)
{
   oss_info dp = get_oss_info(snd);

   if (length_in_bytes <= 0)
      return 0;

   pthread_mutex_lock(&dp->mutex);

   if (length_in_bytes > dp->len)
      length_in_bytes = dp->len;

   copy_from_wrap_buffer(dp, buffer, length_in_bytes);

   pthread_mutex_unlock(&dp->mutex);

   return (length_in_bytes / snd_bytes_per_frame(snd));
}


long audio_write(snd_type snd, void *buffer, long length_in_bytes)
{
   oss_info dp = get_oss_info(snd);

   pthread_mutex_lock(&dp->mutex);

   if (dp->len + length_in_bytes > dp->buffersize)
      length_in_bytes = dp->buffersize - dp->len;

   copy_to_wrap_buffer(dp, buffer, length_in_bytes);

   pthread_mutex_unlock(&dp->mutex);

   return (length_in_bytes / snd_bytes_per_frame(snd));
}


/* audio_flush -- finish audio output */
int audio_flush(snd_type snd)
{
   oss_info dp = get_oss_info(snd);

   if (snd->write_flag = SND_WRITE) {
      /* wait until what's already queued up has
         finished playing */

      while(dp->len > 0) {
         sched_yield();
      }
   }
   
   dp->stop_flag = 1;
   pthread_join(dp->thread, 0);
   dp->thread = 0;
   
   return SND_SUCCESS;
}


int audio_reset(snd_type snd)
{
   oss_info dp = get_oss_info(snd);

   dp->stop_flag = 1;
   /* Try to force it to stop immediately by closing the
      audio file descriptor */
   close(dp->audio_fd);
   /* Wait for our thread to finish */
   pthread_join(dp->thread, 0);
   dp->audio_fd = 0;
   dp->thread = 0;
   
   return SND_SUCCESS;
}


int audio_close(snd_type snd)
{
   oss_info dp = get_oss_info(snd);

   if (dp->thread)
      audio_reset(snd);
   
   if (dp->audio_fd) {
      close(dp->audio_fd);
      dp->audio_fd = 0;
   }
   
   free(dp->buffer);
   dp->buffer = 0;

   free((void *)snd->u.audio.descriptor);

   return SND_SUCCESS;
}


snd_fns_node osssystem_dictionary = { audio_poll, audio_read, audio_write, 
                                      audio_open, audio_close, audio_reset,
                                      audio_flush };

void snd_init()
{
   snd_add_device("oss", "default", &osssystem_dictionary);
}

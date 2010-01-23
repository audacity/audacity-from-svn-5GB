/**********************************************************************

  Audacity: A Digital Audio Editor

  PlayLinux.cpp

  Dominic Mazzoni

**********************************************************************/

#ifdef __WXGTK__

#include "PlayLinux.h"

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/soundcard.h>

#ifndef AUDIODEV
#define AUDIODEV   "/dev/dsp"    // Default path for audio device
#endif

int SoundPlayer::numSoundsPlaying = 0;

SoundPlayer::SoundPlayer()
{ 
}

SoundPlayer::~SoundPlayer()
{
}

bool SoundPlayer::Begin(WaveTrack *track, double t0, double t1)
{
  int s0 = (int)((t0 - track->tOffset) * track->rate);
  if (s0 < 0)
	s0 = 0;
  int s1 = (int)((t1 - track->tOffset) * track->rate);
  if (s1 > track->numSamples)
	s1 = track->numSamples;

  if (s0 >= s1) {
    printf("No samples selected.\n");
    return true;
  }

  int s = s0;

  if (numSoundsPlaying != 0) {
	printf("Sound already playing.\n");
	return false;
  }

  numSoundsPlaying++;

  int dev=-1;

  if ((dev = open(AUDIODEV,O_WRONLY,0)) <0) {
    printf("Could not initialize audio driver: %d.\n",dev);
    numSoundsPlaying--;
    return false;
  }

  sampleType *buffer;
  bool play;
  int DSPblkSize;
  int iDataBits = 16; // TODO: generic
  int iStereo = 0; /* 0 = MONO, 1 = STEREO */
  int iChannels = 1;
  int iFormat = AFMT_S16_LE; // Little-endian signed 16
  unsigned int ulSamplingRate = (unsigned int)track->rate;

  if ( ioctl(dev, SNDCTL_DSP_GETBLKSIZE, &DSPblkSize) < 0 ) 
	goto failinit;
  if (DSPblkSize < 4096 || DSPblkSize > 65536)
	goto failinit;
  if ( ioctl(dev, SNDCTL_DSP_SAMPLESIZE, &iDataBits) < 0 )
	goto failinit;
  if ( ioctl(dev, SNDCTL_DSP_STEREO, &iStereo) < 0 )
	goto failinit;
  if ( ioctl(dev, SNDCTL_DSP_CHANNELS, &iChannels) < 0 )
	goto failinit;
  if ( ioctl(dev, SNDCTL_DSP_SPEED, &ulSamplingRate) < 0 ) 
	goto failinit;
  if ( ioctl(dev, SNDCTL_DSP_SETFMT, &iFormat) < 0 ) 
	goto failinit;

  ioctl(dev,SNDCTL_DSP_SYNC,0);

  buffer = new sampleType[DSPblkSize];
  play=TRUE;
  int count;
  do {
    count = (int)(s+DSPblkSize < s1 ? DSPblkSize : s1 - s);
    track->Get(buffer, s, count);
    if (write(dev, buffer, count*2) != 2*count )
      play=FALSE;
    s += count;
  } while (play == TRUE && s < s1);

  delete[] buffer;

  close(dev);
  numSoundsPlaying--;
  return true;

failinit:

  printf("SoundPlayer failed.\n");

  close(dev);
  numSoundsPlaying--;
  return false;
}

#endif



/**********************************************************************

  Audacity: A Digital Audio Editor

  PlayMac.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include "wx/wx.h"
#endif

#ifdef __WXMAC__

//#include <fp.h>
#include "PlayMac.h"

void ldtox80(const long double *x, extended80 *x80);

#include <Sound.h>

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
    return true;
  }

  if (numSoundsPlaying != 0)
	return false;

  numSoundsPlaying++;

  OSErr	err;
	SndChannelPtr chan = NULL;
	SndCommand cmd;
	ExtSoundHeader header;
	
	err = SndNewChannel(&chan, sampledSynth, 0, NULL);
	
	if (err) {
		printf("Error %d in SndNewChannel.\n",err);
		return false;
	}
	
	sampleCount len = s1-s0;

	header.samplePtr = (Ptr)(new sampleType[len]);
	header.numChannels = 1;
	header.sampleRate = 0xAC440000; // Fixed for 44100
	header.loopStart = 0;
	header.loopEnd = 0;
	header.encode = extSH;
	header.baseFrequency = 45; // A-440
  header.numFrames = len;
  header.markerChunk = 0;
  header.instrumentChunks = 0;
  header.AESRecording = 0;
  header.sampleSize = 16;
  header.futureUse1 = 0;
  header.futureUse2 = 0;
  header.futureUse3 = 0;
  header.futureUse4 = 0;
	header.sampleArea[0] = 0;
	((char *)&header.AIFFSampleRate)[0] = 0;
	((char *)&header.AIFFSampleRate)[1] = 0;
	((char *)&header.AIFFSampleRate)[2] = 0;
	((char *)&header.AIFFSampleRate)[3] = 0;
	((char *)&header.AIFFSampleRate)[4] = 0;
	((char *)&header.AIFFSampleRate)[5] = 0;
	((char *)&header.AIFFSampleRate)[6] = 0;
	((char *)&header.AIFFSampleRate)[7] = 0;
	((char *)&header.AIFFSampleRate)[8] = 0;
	((char *)&header.AIFFSampleRate)[9] = 0;
  //ldtox80(&rate, &header.AIFFSampleRate);
	
	track->Get((sampleType *)header.samplePtr, s0, len);

	cmd.cmd = bufferCmd;
	cmd.param1 = 0;
	cmd.param2 = (long)&header;

	err = SndDoCommand(chan, &cmd, 0);
	
	if (err) {
		printf("Error %d in SndDoCommand.\n",err);
		return false;
	}
	
	delete[] (sampleType *)header.samplePtr;
	
	SndDisposeChannel(chan, 0);

  numSoundsPlaying--;
  
  return true;
}

#endif



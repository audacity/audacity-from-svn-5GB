/**********************************************************************

  Audacity: A Digital Audio Editor

  PlaySnd.cpp

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

#include "PlaySnd.h"

#include "snd/snd.h"

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
  if ((sampleCount)s1 > track->numSamples)
	  s1 = track->numSamples;

  if (s0 >= s1) {
    return true;
  }

  sampleCount len = s1 - s0;

  if (numSoundsPlaying != 0)
	return false;

  wxBusyCursor busy;

  numSoundsPlaying++;

  snd_node out;
  out.device = SND_DEVICE_AUDIO;
  out.write_flag = SND_WRITE;
  out.format.channels = 1;
  out.format.mode = SND_MODE_PCM;
  out.format.bits = 16;
  out.format.srate = track->rate;
  strcpy(out.u.audio.devicename,"");
  strcpy(out.u.audio.interfacename,""); //RBD 15jun00
  out.u.audio.descriptor = 0;
  out.u.audio.protocol = SND_COMPUTEAHEAD;
  out.u.audio.latency = 1.0;  // use defaults
  out.u.audio.granularity = 0.0;  // use defaults

  long flags = 0;
  int err = snd_open(&out, &flags);

  if (err) {
	wxString errStr;
	errStr.Printf("Error playing sound: %d",err);
	wxMessageBox(errStr);
    numSoundsPlaying--;
	return false;
  }

  int pos=0;

  sampleType *buffer = new sampleType[len];
  if (buffer) {
	track->Get(buffer, s0, len);

	int rslt;
	int p=0;
	while (p < len) {
		/* this loop is a busy-wait loop! */
		rslt = snd_poll(&out); /* wait for buffer space */
		rslt = min(rslt, len - p);
		if (rslt) {
			snd_write(&out, &buffer[p], rslt);
			p += rslt;
		}
    }
	
	while(snd_flush(&out) != SND_SUCCESS)
	{
		// wait until it finishes...
	}

    delete[] buffer;
  }

  snd_close(&out);

  numSoundsPlaying--;
  return true;
}




/**********************************************************************

  Audacity: A Digital Audio Editor

  PlayWin.cpp

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

#ifdef __WXMSW__

#include "PlayWin.h"

int SoundPlayer::numSoundsPlaying = 0;

SoundPlayer::SoundPlayer()
{ 
}

SoundPlayer::~SoundPlayer()
{
}

bool SoundPlayer::Begin(WaveTrack *track, double t0, double t1)
{
  if (numSoundsPlaying != 0)
	return false;

  numSoundsPlaying++;

  // Play the sound

  numSoundsPlaying--;
  return true;
}

#endif



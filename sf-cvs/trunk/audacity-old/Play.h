/**********************************************************************

  Audacity: A Digital Audio Editor

  PlaySnd.h

  Dominic Mazzoni

  Use the SND library to play sound

**********************************************************************/

#ifndef __AUDACITY_PLAY__
#define __AUDACITY_PLAY__

#include "snd/snd.h"

#include <wx/string.h>
#include <wx/timer.h>

#include "WaveTrack.h"

class SoundPlayer;
class AudacityProject;

extern SoundPlayer *gSoundPlayer;

void InitSoundPlayer();

class SoundTimer: public wxTimer
{
public:
  virtual void Notify();
};

class SoundPlayer {

public:
  SoundPlayer();
  ~SoundPlayer();

  bool Begin(AudacityProject *project,
			 TrackList *tracks,
			 double t0, double t1);

  void Stop();

  bool IsBusy();

  void OnTimer();

private:

  void Finish();
  
  AudacityProject *mProject;
  TrackList       *mTracks;
  double          mT;
  double          mT0;
  double          mT1;
  int             mTicks;
  bool            mStop;
  snd_node        mAudioOut;
  SoundTimer      mTimer;
};

#endif





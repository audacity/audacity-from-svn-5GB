/**********************************************************************

  Audacity: A Digital Audio Editor

  Play.h

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

  void OnTimer();

  void Stop();
  bool IsBusy();

  AudacityProject *GetProject();
  double GetIndicator();

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
  wxStopWatch     mStopWatch;
  
  #ifdef __WXMAC__
  int             mStartTicks;
  #endif
};

#endif





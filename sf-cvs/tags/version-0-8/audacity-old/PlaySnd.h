/**********************************************************************

  Audacity: A Digital Audio Editor

  PlaySnd.h

  Dominic Mazzoni

  Use the SND library to play sound

**********************************************************************/

#ifndef __PLAY_SND__
#define __PLAY_SND__

#include <wx/string.h>

#include "WaveTrack.h"

class SoundPlayer {

public:
  SoundPlayer();
  ~SoundPlayer();

  bool Begin(WaveTrack *track, double t0, double t1);

private:
  static int numSoundsPlaying;
};

#endif





/**********************************************************************

  Audacity: A Digital Audio Editor

  PlayWin.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __PLAY_WIN__
#define __PLAY_WIN__

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




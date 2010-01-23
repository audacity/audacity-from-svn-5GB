/**********************************************************************

  Audacity: A Digital Audio Editor

  PlayMac.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __PLAY__
#define __PLAY__

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




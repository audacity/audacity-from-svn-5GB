/**********************************************************************

  Audacity: A Digital Audio Editor

  Play.h

  Dominic Mazzoni

  Note: This one header file, Play.h, is associated with many
  different sound playing modules, such as PlayLinux.cpp,
  PlayMac.cpp, PlayWin.cpp, and later much more.

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




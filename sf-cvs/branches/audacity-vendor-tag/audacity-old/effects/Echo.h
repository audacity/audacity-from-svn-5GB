/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectEcho: public Effect {

public:
  virtual wxString GetEffectName() { return wxString("Echo..."); }

  virtual bool DoIt(WaveTrack *t,
		    sampleCount start,
		    sampleCount len);
};

#endif

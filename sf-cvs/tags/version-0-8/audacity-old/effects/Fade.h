/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FADE__
#define __AUDACITY_EFFECT_FADE__

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectFadeIn: public Effect {

public:
  virtual wxString GetEffectName() { return wxString("Fade In"); }

  virtual bool DoIt(WaveTrack *t,
		    sampleCount start,
		    sampleCount len);
};

class EffectFadeOut: public Effect {

public:
  virtual wxString GetEffectName() { return wxString("Fade Out"); }

  virtual bool DoIt(WaveTrack *t,
		    sampleCount start,
		    sampleCount len);
};

#endif

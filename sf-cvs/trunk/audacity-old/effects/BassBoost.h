/**********************************************************************

  Audacity: A Digital Audio Editor

  Bass Boost

  Nasca Octavian Paul   <paulnasca@email.ro> or <paulnasca@yahoo.com>

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_BOOST__
#define __AUDACITY_EFFECT_BASS_BOOST__

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectBassBoost: public Effect {

public:
  EffectBassBoost();

  virtual wxString GetEffectName() { return wxString("BassBoost"); }

  virtual bool Begin(wxWindow *parent);
  virtual bool DoIt(WaveTrack *t,
		    sampleCount start,
		    sampleCount len);

private:
  float frequency, dB_boost;
};

#endif

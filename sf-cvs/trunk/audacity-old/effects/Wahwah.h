/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah

  Nasca Octavian Paul   <paulnasca@email.ro> or <paulnasca@yahoo.com>

**********************************************************************/

#ifndef __AUDACITY_EFFECT_WAHWAH__
#define __AUDACITY_EFFECT_WAHWAH__

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectWahwah: public Effect {

public:
  EffectWahwah();

  virtual wxString GetEffectName() { return wxString("Wahwah..."); }

  virtual bool Begin(wxWindow *parent);
  virtual bool DoIt(WaveTrack *t,
					sampleCount start,
					sampleCount len);

/* Parameters:
   freq - LFO frequency 
   startphase - LFO startphase in RADIANS - usefull for stereo WahWah
   depth - Wah depth
   freqofs - Wah frequency offset
   res - Resonance

   !!!!!!!!!!!!! IMPORTANT!!!!!!!!! :
   depth and freqofs should be from 0(min) to 1(max) !
   res should be greater than 0 !  */

private:
	float freq, startphase;
	float depth, freqofs, res;
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleMono.h

  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  monaural effect.  Inherit from it if your effect doesn't just
  modifies a track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetEffectName, GetEffectAction, and ProcessSimpleMono.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SIMPLE_MONO__
#define __AUDACITY_EFFECT_SIMPLE_MONO__

#include "Effect.h"

class WaveTrack;

class EffectSimpleMono:public Effect {

 public:
   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   double start, sampleCount len);

 protected:
   // Override this method
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len) = 0;

};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundTouchEffect.h

  Dominic Mazzoni

  This abstract class contains all of the common code for an
  effect that uses SoundTouch to do its processing (ChangeTempo
  and ChangePitch).

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_SOUNDTOUCH__
#define __AUDACITY_EFFECT_SOUNDTOUCH__

#include "Effect.h"

class WaveTrack;
class SoundTouch;

class EffectSoundTouch:public Effect {

 public:
   virtual bool Process();

 protected:
   SoundTouch *mSoundTouch;

 private:
   bool ProcessOne(WaveTrack * t,
                   longSampleCount start, longSampleCount end);

   int    mCurTrackNum;
   double mCurRate;
   double mCurT0;
   double mCurT1;
   int    mCurChannel;

};

#endif

#endif

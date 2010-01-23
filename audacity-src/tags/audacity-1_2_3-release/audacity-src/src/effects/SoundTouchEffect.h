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
#include "SoundTouch.h"

using namespace soundtouch;


class WaveTrack;

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

	double m_maxNewLength;
};

#endif

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 6f08259e-6cac-4c47-9aa5-1de7b68eb495


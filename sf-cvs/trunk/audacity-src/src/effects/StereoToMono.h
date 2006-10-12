/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_STEREO_TO_MONO__
#define __AUDACITY_EFFECT_STEREO_TO_MONO__

#include "Effect.h"

class EffectStereoToMono: public Effect {

public:

   EffectStereoToMono();

   virtual wxString GetEffectName() {
      return wxString(_("Stereo To Mono"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Stereo To Mono"));
   }
   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
 protected:
    virtual bool Process();

private:
   void ProcessOne();

   longSampleCount mLeftTrackLen;
   longSampleCount mRightTrackLen;
   WaveTrack *mLeftTrack;
   WaveTrack *mRightTrack;

};

#endif


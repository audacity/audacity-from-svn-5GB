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
      return wxString(_NoAcc("&Stereo to Mono"));
   }

   // Used internally, users will not see this.  Do not translate.
   virtual wxString GetEffectIdentifier() {
      return wxT("StereoToMono");
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Stereo to Mono"));
   }
   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
 protected:
    virtual bool Process();

private:
   bool ProcessOne(int);

   longSampleCount mLeftTrackLen;
   longSampleCount mRightTrackLen;
   WaveTrack *mLeftTrack;
   WaveTrack *mRightTrack;

};

#endif


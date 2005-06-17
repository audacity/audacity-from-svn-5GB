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
 protected:
    virtual bool Process();

private:
   sampleCount mnBlockSize;	// 0 if no processing done, thus no buffers allocated

   float *mLeftBuffer;
   float *mRightBuffer;

   int    mnTracks;			// either 1 or 2, set in Init
   double mLeftTrackEnd;
   double mRightTrackEnd;
   double mLeftRate;
   longSampleCount mLeftTrackLen;
   longSampleCount mRightTrackLen;
   WaveTrack *mLeftTrack;
   WaveTrack *mRightTrack;

};

#endif


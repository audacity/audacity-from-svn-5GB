/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

class wxString;

#include <wx/intl.h>
#include "SimpleMono.h"

class WaveTrack;

class EffectCompressor: public EffectSimpleMono {
   
public:
   
   EffectCompressor();
   
   virtual wxString GetEffectName() {
      return wxString(_("Compressor"));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Compressing"));
   }
   
   virtual bool PromptUser();
   
 protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

 private:

   bool NewTrackSimpleMono();

   float DoCompression(float x);
   
   bool      mRMS;
   double    mAttackTime;
   double    mDecayTime;
   double    mThresholdDB;
   double    mRatio;
   double    mGainDB;
   
   double    mMult;
   double    mThreshold;
   double    mDecayMult;
   double    mGain;
   double    mInvRatio;
   double    mRMSSum;
   int       mCircleSize;
   int       mCirclePos;
   double   *mCircle;
};

#endif


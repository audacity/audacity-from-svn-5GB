/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectCompressor: public Effect {
   
public:
   
   EffectCompressor();
   
   virtual wxString GetEffectName() {
      return wxString("Compressor/Expander...");
   }
   
   virtual wxString GetEffectAction() {
      return wxString("Compressing/Expanding");
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();
   
private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
                   
   sampleType DoCompression(sampleType x);
   
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


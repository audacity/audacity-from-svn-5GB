/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

class wxString;

#include <wx/intl.h>
#include "Effect.h"

class WaveTrack;

class EffectCompressor: public Effect {
   
public:
   
   EffectCompressor();
   
   virtual wxString GetEffectName() {
      return wxString(_("Compressor..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Compressing"));
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();
   
private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
                   
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


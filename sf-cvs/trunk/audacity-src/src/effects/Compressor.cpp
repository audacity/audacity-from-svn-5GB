/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.cpp

  Dominic Mazzoni

  Rewritten to inherit from EffectSimpleMono by Steve Jolly, who is 
  very unhappy with the way it sounds and wonders if he's broken
  something.  Mind you, it pretty much zeroed everything back in 
  Audacity 1.1, so I guess it's an improvement over that...

  \todo consider implementing a softer start to the effect
  \todo add a gui!
**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>

#include "Compressor.h"
#include "../WaveTrack.h"

EffectCompressor::EffectCompressor()
{
   mRMS = true;
   mAttackTime = 0.01;    // seconds
   mDecayTime = 0.5;       // seconds
   mThresholdDB = -15.0;   // dB
   mRatio = 2.0;           // positive number
   mGainDB = 4.0;          // not sure about this value
   mCircle = NULL;         // prevent access violation in NewTrackSimpleMono
}

bool EffectCompressor::PromptUser()
{
   //Only we don't.  But we'd do it here if we did.
   mThreshold = pow(10.0, mThresholdDB/10);
   mGain = pow(10.0, mGainDB/10);
   mInvRatio = 1.0 - 1.0 / mRatio;

   return true;
}

bool EffectCompressor::NewTrackSimpleMono(int count, double samplerate)
{
   if (mCircle) delete mCircle;
   
   mDecayMult = exp(log(0.1)/(mDecayTime*samplerate));
   mCircleSize = int(mAttackTime * samplerate + 0.5);
   mCircle = new double[mCircleSize];
   for(int j=0; j<mCircleSize; j++)
      mCircle[j] = 0.0;
   mCirclePos = 0;
   mRMSSum = 0.0;
   mMult = 1.0;

   return true;
}

bool EffectCompressor::ProcessSimpleMono(float *buffer, sampleCount len, double samplerate)
{
   for (int i = 0; i < len; i++) {
      buffer[i] = DoCompression(buffer[i]);
   }

   return true;
}

float EffectCompressor::DoCompression(float x)
{
   float value = x; // /32767.0;
   float level;
   float mult;
   
   if (mRMS) {
      // Calculate current level from root-mean-squared of
      // circular buffer ("RMS")
      mRMSSum -= mCircle[mCirclePos];
      mCircle[mCirclePos] = value*value;
      mRMSSum += mCircle[mCirclePos];
      mCirclePos = (mCirclePos+1)%mCircleSize;
      level = sqrt(mRMSSum/mCircleSize);
   }
   else {
      // Calculate current level from value at other end of
      // circular buffer ("Peak")
      level = mCircle[mCirclePos];
      mCircle[mCirclePos] = value>0? value: -value;
      mCirclePos = (mCirclePos+1)%mCircleSize;
   }
   
   if (level > mThreshold)
      mult = mGain * pow(mThreshold/level, mInvRatio);
   else
      mult = 1.0;

   mMult = mult*mDecayMult + mMult*(1.0-mDecayMult);

   return value*mMult;
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>

#include "Compressor.h"
#include "../WaveTrack.h"

EffectCompressor::EffectCompressor()
{
   mRMS = true;
   mAttackTime = 0.1;
   mDecayTime = 0.1;
   mThresholdDB = -32;
   mRatio = 8.0;        // positive number
   mGainDB = 4.0;
}

bool EffectCompressor::PromptUser()
{
   return true;
}

bool EffectCompressor::Process()
{
   mThreshold = pow(10.0, mThresholdDB/10);
   mGain = pow(10.0, mGainDB/10);
   mInvRatio = 1.0 - 1.0 / mRatio;

   TrackListIterator iter(mWaveTracks);
   VTrack *t = iter.First();
   int count = 0;
   while(t) {
      sampleCount start, len;
      GetSamples((WaveTrack *)t, &start, &len);
      
      bool success = ProcessOne(count, (WaveTrack *)t, start, len);
      
      if (!success)
         return false;
   
      t = iter.Next();
      count++;
   }

   return true;
}

bool EffectCompressor::ProcessOne(int count, WaveTrack * t,
                                    sampleCount start, sampleCount len)
{
   mDecayMult = exp(log(0.1)/(mDecayTime*t->rate));
   mCircleSize = int(mAttackTime * t->rate + 0.5);
   mCircle = new double[mCircleSize];
   for(int j=0; j<mCircleSize; j++)
      mCircle[j] = 0.0;
   mCirclePos = 0;
   mRMSSum = 0.0;
   mMult = 1.0;
   
   sampleCount s = start;
   sampleCount originalLen = len;
   sampleCount blockSize = t->GetMaxBlockSize();

   sampleType *buffer = new sampleType[blockSize];
   
   while (len) {
      unsigned int block = t->GetBestBlockSize(s);
      if (block > len)
         block = len;

      t->Get(buffer, s, block);
      for (unsigned int i = 0; i < block; i++) {
         buffer[i] = DoCompression(buffer[i]);
      }
      t->Set(buffer, s, block);

      len -= block;
      s += block;
      
      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[] buffer;
   delete[] mCircle;

   return true;
}

sampleType EffectCompressor::DoCompression(sampleType x)
{
   double value = x/32767.0;
   double level;
   double mult;
   
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

   return (sampleType)(value*mMult*32767.0);
}

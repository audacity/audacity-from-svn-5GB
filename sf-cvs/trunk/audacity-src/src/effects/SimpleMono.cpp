/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleMono.cpp

  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  monaural effect.  Inherit from it if your effect doesn't just
  modifies a track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetEffectName, GetEffectAction, and ProcessSimpleMono.

**********************************************************************/

#include <math.h>

#include "SimpleMono.h"
#include "../WaveTrack.h"

bool EffectSimpleMono::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *t = (WaveTrack *)iter.First();
   int count = 0;
   while(t) {
      double start = mT0;
      sampleCount len = (sampleCount)floor((mT1 - mT0)*t->GetRate() + 0.5);
      bool success = ProcessOne(count, t, start, len);
      
      if (!success)
         return false;
   
      t = (WaveTrack *)iter.Next();
      count++;
   }
   
   return true;
}

bool EffectSimpleMono::ProcessOne(int count, WaveTrack *track,
                              double start, sampleCount len)
{
   double t = start;
   sampleCount s = 0;
   
   float *buffer = new float[track->GetMaxBlockSize()];
   
   while (s < len) {
      sampleCount block = track->GetBestBlockSize(t);
      if (s+block > len)
         block = len-s;

      track->Get((samplePtr)buffer, floatSample, t, block);
      if (!ProcessSimpleMono(buffer, block)) {
         delete[] buffer;
         return false;
      }
      track->Set((samplePtr)buffer, floatSample, t, block);

      s += block;
      t += (block/track->GetRate());

      TrackProgress(count, s/(double)len);
   }

   delete[] buffer;

   return true;
}


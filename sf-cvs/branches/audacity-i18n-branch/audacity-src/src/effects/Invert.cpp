/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.cpp

  Mark Phillips

  This class inverts the selected audio.

**********************************************************************/

#include "Invert.h"
#include "../WaveTrack.h"

//
// EffectInvert
//

EffectInvert::EffectInvert()
{
}

bool EffectInvert::Process()
{
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

bool EffectInvert::ProcessOne(int count, WaveTrack *t,
                              sampleCount start, sampleCount len)
{
   // keep track of two blocks whose data we will swap
   sampleCount s = start;
   sampleCount originalLen = len;
   sampleCount blockSize = t->GetMaxBlockSize();
   
   sampleType *buffer = new sampleType[blockSize];
   
   while (len > 0) {
      unsigned int block = t->GetBestBlockSize(s);
      if (block > len)
         block = len;

      t->Get(buffer, s, block);
      for (unsigned int i = 0; i < block; i++) {
         buffer[i] = (sampleType) (-buffer[i]);
      }
      t->Set(buffer, s, block);

      len -= block;
      s += block;

      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[] buffer;

   return true;
}


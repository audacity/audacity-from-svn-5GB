/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.cpp

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#include "Reverse.h"
#include "../WaveTrack.h"

//
// EffectReverse
//

EffectReverse::EffectReverse()
{
}

bool EffectReverse::Process()
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

bool EffectReverse::ProcessOne(int count, WaveTrack *t,
                               sampleCount start, sampleCount len)
{
   // keep track of two blocks whose data we will swap
   sampleCount first = start, second;
   sampleCount originalLen = len;
   sampleCount blockSize = t->GetMaxBlockSize();
   sampleType tmp;
   sampleType *buffer1 = new sampleType[blockSize],
              *buffer2 = new sampleType[blockSize];
   
   while (len > 1) {
      unsigned int block = t->GetBestBlockSize(first);
      if (block > len / 2)
         block = len / 2;
      second = first + len - block;

      t->Get(buffer1, first, block);
      t->Get(buffer2, second, block);
      for (unsigned int i = 0; i < block; i++) {
         tmp = buffer1[i];
         buffer1[i] = buffer2[block-i-1];
         buffer2[block-i-1] = tmp;
      }
      t->Set(buffer1, first, block);
      t->Set(buffer2, second, block);

      len -= 2 * block;
      first += block;
      
      TrackProgress(count, 2*(first-start)/(double)originalLen);
   }

   delete[] buffer1;
   delete[] buffer2;

   return true;
}


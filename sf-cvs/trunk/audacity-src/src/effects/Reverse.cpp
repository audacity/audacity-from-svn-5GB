/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.cpp

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#include <math.h>

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
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double starttime = mT0;
      double endtime = mT1;

      if (starttime < track->GetEndTime()) {    //make sure part of track is within selection
         if (endtime > track->GetEndTime())
            endtime = track->GetEndTime();      //make sure all of track is within selection
         sampleCount len =
             (sampleCount) floor((endtime - starttime) * track->GetRate() + 0.5);

         if (!ProcessOne(count, track, starttime, len))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}

bool EffectReverse::ProcessOne(int count, WaveTrack *track,
                               double start, sampleCount len)
{
   // keep track of two blocks whose data we will swap
   double first = start, second;

   sampleCount blockSize = track->GetMaxBlockSize();
   float tmp;
   float *buffer1 = new float[blockSize];
   float *buffer2 = new float[blockSize];
   
   sampleCount originalLen = len;

   while (len > 1) {
      sampleCount block = track->GetBestBlockSize(first);
      if (block > len / 2)
         block = len / 2;
      second = first + (len - block)/track->GetRate();

      track->Get((samplePtr)buffer1, floatSample, first, block);
      track->Get((samplePtr)buffer2, floatSample, second, block);
      for (int i = 0; i < block; i++) {
         tmp = buffer1[i];
         buffer1[i] = buffer2[block-i-1];
         buffer2[block-i-1] = tmp;
      }
      track->Set((samplePtr)buffer1, floatSample, first, block);
      track->Set((samplePtr)buffer2, floatSample, second, block);

      len -= 2 * block;
      first += block/track->GetRate();
      
      TrackProgress(count, 2*(first-start) / (double) originalLen);
   }

   delete[] buffer1;
   delete[] buffer2;

   return true;
}


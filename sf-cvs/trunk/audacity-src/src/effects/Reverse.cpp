/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.cpp

  Mark Phillips

*******************************************************************//**

\class EffectReverse
\brief An Effect that reverses the selected audio.

*//********************************************************************/


#include "../Audacity.h"

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
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         longSampleCount start = track->TimeToLongSamples(t0);
         longSampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if (!ProcessOne(count, track, start, len))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}

bool EffectReverse::ProcessOne(int count, WaveTrack *track,
                               longSampleCount start, sampleCount len)
{
   // keep track of two blocks whose data we will swap
   longSampleCount first = start;
   longSampleCount second;

   sampleCount blockSize = track->GetMaxBlockSize();
   float tmp;
   float *buffer1 = new float[blockSize];
   float *buffer2 = new float[blockSize];
   
   sampleCount originalLen = len;

   while (len > 1) {
      sampleCount block = track->GetBestBlockSize(first);
      if (block > len / 2)
         block = len / 2;
      second = first + (len - block);

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
      first += block;
      
      TrackProgress(count, 2*(first-start) / (double) originalLen);
   }

   delete[] buffer1;
   delete[] buffer2;

   return true;
}


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 47d91d2a-befd-4921-a19d-6cb8376428eb


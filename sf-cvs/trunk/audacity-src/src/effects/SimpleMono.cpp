/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleMono.cpp

  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  monaural effect.  Inherit from it if your effect just modifies
  a single track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetEffectName, GetEffectAction, and ProcessSimpleMono.

**********************************************************************/

#include <math.h>

#include "SimpleMono.h"
#include "../WaveTrack.h"

bool EffectSimpleMono::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   mCurTrackNum = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      if (mCurT1 > mCurT0) {
         longSampleCount start = track->TimeToLongSamples(mCurT0);
         longSampleCount end = track->TimeToLongSamples(mCurT1);

         mCurRate = track->GetRate();
         mCurChannel = track->GetChannel();

         if (!NewTrackSimpleMono())
            return false;
         if (!ProcessOne(track, start, end))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      mCurTrackNum++;
   }

   return true;
}

bool EffectSimpleMono::ProcessOne(WaveTrack * track,
                                  longSampleCount start, longSampleCount end)
{
   longSampleCount s;
   double len = (double)(start - end);

   float *buffer = new float[track->GetMaxBlockSize()];

   s = start;
   while (s < end) {
      sampleCount block = track->GetBestBlockSize(s);
      if (s + block > end)
         block = end - s;

      track->Get((samplePtr) buffer, floatSample, s, block);
      if (!ProcessSimpleMono(buffer, block)) {
         delete[]buffer;
         return false;
      }
      track->Set((samplePtr) buffer, floatSample, s, block);

      s += block;

      TrackProgress(mCurTrackNum, s / len);
   }

   delete[]buffer;

   return true;
}

//null implementation of NewTrackSimpleMono
bool EffectSimpleMono::NewTrackSimpleMono()
{
   return true;
}

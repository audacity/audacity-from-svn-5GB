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
   int count = 0;
   while (track) {
      double starttime = mT0;
      double endtime = mT1;

      if (starttime < track->GetEndTime()) {    //make sure part of track is within selection
         if (endtime > track->GetEndTime())
            endtime = track->GetEndTime();      //make sure all of track is within selection
         sampleCount len =
             (sampleCount) floor((endtime - starttime) * track->GetRate() + 0.5);

         if (!NewTrackSimpleMono(count, track->GetRate()))
            return false;
         if (!ProcessOne(count, track, starttime, len))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}

bool EffectSimpleMono::ProcessOne(int count, WaveTrack * track,
                                  double start, sampleCount len)
{
   double t = start;
   sampleCount s = 0;

   float *buffer = new float[track->GetMaxBlockSize()];

   while (s < len) {
      sampleCount block = track->GetBestBlockSize(t);
      if (s + block > len)
         block = len - s;

      track->Get((samplePtr) buffer, floatSample, t, block);
      if (!ProcessSimpleMono(buffer, block, track->GetRate())) {
         delete[]buffer;
         return false;
      }
      track->Set((samplePtr) buffer, floatSample, t, block);

      s += block;
      t += (block / track->GetRate());

      TrackProgress(count, s / (double) len);
   }

   delete[]buffer;

   return true;
}

//null implementation of NewTrackSimpleMono
bool EffectSimpleMono::NewTrackSimpleMono(int count, double samplerate)
{
   return true;
}

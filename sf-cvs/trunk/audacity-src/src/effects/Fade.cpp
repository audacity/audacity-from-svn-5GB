/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.cpp

  Robert Leidle

**********************************************************************/

#include <wx/generic/textdlgg.h>
#include <math.h>

#include "Fade.h"
#include "../WaveTrack.h"

bool EffectFadeIn::Process()
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

bool EffectFadeIn::ProcessOne(int count, WaveTrack * track,
                              double start, sampleCount len)
{
   double t=start;
   sampleCount s = 0;
   sampleCount blockSize = track->GetMaxBlockSize();

   float *buffer = new float[blockSize];
   
   while (s < len) {
      sampleCount block = track->GetBestBlockSize(t);
      if (s + block > len)
         block = len - s;

      track->Get((samplePtr) buffer, floatSample, t, block);
      for (sampleCount i = 0; i < block; i++)
         buffer[i] = (float) (buffer[i]
                                   * (float) (s + i)
                                   / (float) (len));
      track->Set((samplePtr) buffer, floatSample, t, block);

      s += block;
      t += (block / track->GetRate());
      
      TrackProgress(count, s / (double) len);
   }

   delete[]buffer;

   return true;
}

bool EffectFadeOut::Process()
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

bool EffectFadeOut::ProcessOne(int count, WaveTrack * track,
                              double start, sampleCount len)
{
   double t=start;
   sampleCount s = 0;
   sampleCount blockSize = track->GetMaxBlockSize();

   float *buffer = new float[blockSize];
   
   while (s < len) {
      sampleCount block = track->GetBestBlockSize(t);
      if (s + block > len)
         block = len - s;

      track->Get((samplePtr) buffer, floatSample, t, block);
      for (sampleCount i = 0; i < block; i++)
         buffer[i] = (float) (buffer[i]
                                   * (float) (len - 1 - (s + i - start))
                                   / (float) (len));
      track->Set((samplePtr) buffer, floatSample, t, block);

      s += block;
      t += (block / track->GetRate());
      
      TrackProgress(count, s / (double) len);
   }

   delete[]buffer;

   return true;
}

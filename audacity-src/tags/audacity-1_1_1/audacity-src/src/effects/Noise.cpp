/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.cpp

  Dominic Mazzoni
  
  An effect for the "Generator" menu to add white noise.

**********************************************************************/

#include <stdlib.h>

#include <wx/defs.h> 

#include "Noise.h"
#include "../WaveTrack.h"

void MakeNoise(float *buffer, sampleCount len)
{
   sampleCount i;
   float div = ((float)RAND_MAX) / 2.0f;

   for(i=0; i<len; i++)
      buffer[i] = (rand() / div) - 1.0;
}

bool EffectNoise::Process()
{
   double length = mT1 - mT0;

   if (length <= 0.0)
      length = sDefaultGenerateLen;

   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat());
      tmp->SetRate(track->GetRate());
      longSampleCount numSamples =
         (longSampleCount)(length * track->GetRate() + 0.5);
      longSampleCount i = 0;
      float *data = new float[tmp->GetMaxBlockSize()];
      sampleCount block;

      while(i < numSamples) {
         block = tmp->GetBestBlockSize(i);
         if (block > (numSamples - i))
             block = numSamples - i;
         MakeNoise(data, block);
         tmp->Append((samplePtr)data, floatSample, block);
         i += block;
      }
      delete[] data;

      track->Paste(mT0, tmp);
      delete tmp;
      
      //Iterate to the next track
      track = (WaveTrack *)iter.Next();
   }

   return true;
}

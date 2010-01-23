/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.cpp

  Robert Leidle

**********************************************************************/

#include <wx/generic/textdlgg.h>

#include "Fade.h"
#include "../WaveTrack.h"

bool EffectFadeIn::Process()
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

bool EffectFadeIn::ProcessOne(int count, WaveTrack * t,
                              sampleCount start, sampleCount len)
{
   sampleCount s = start;
   sampleCount blockSize = t->GetMaxBlockSize();

   float *buffer = new float[blockSize];
   
   sampleCount originalLen = len;

   while ((s - start) < len) {
      sampleCount block = t->GetBestBlockSize(s);
      if (s - start + block > len)
         block = start + len - s;

      t->Get(buffer, s, block);
      for (sampleCount i = 0; i < block; i++)
         buffer[i] = (float) (buffer[i]
                                   * (float) (s + i - start)
                                   / (float) (len));
      t->Set(buffer, s, block);

      s += block;
      
      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[]buffer;

   return true;
}

bool EffectFadeOut::Process()
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

bool EffectFadeOut::ProcessOne(int count, WaveTrack * t,
                               sampleCount start, sampleCount len)
{
   sampleCount s = start;
   sampleCount blockSize = t->GetMaxBlockSize();

   float *buffer = new float[blockSize];
   
   sampleCount originalLen = len;

   while ((s - start) < len) {
      sampleCount block = t->GetBestBlockSize(s);
      if (s - start + block > len)
         block = start + len - s;

      t->Get(buffer, s, block);
      for (sampleCount i = 0; i < block; i++)
         buffer[i] = (float) (buffer[i]
                                   * (float) (len - 1 - (s + i - start))
                                   / (float) (len));
      t->Set(buffer, s, block);

      s += block;
      
      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[]buffer;

   return true;
}

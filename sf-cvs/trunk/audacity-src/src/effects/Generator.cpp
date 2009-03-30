/**********************************************************************

  Audacity: A Digital Audio Editor

  Generator.h

  Two Abstract classes, Generator, and BlockGenerator, that effects which
  generate audio should derive from.  
  
  Block Generator breaks the synthesis task up into smaller parts.

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/
#include "Generator.h"


bool Generator::Process()
{
   if (mDuration < 0.0)
      return false;

   BeforeGenerate();

   // Set up mOutputWaveTracks
   this->CopyInputWaveTracks();

   // Iterate over the tracks
   bool bGoodResult = true;
   int ntrack = 0;
   TrackListIterator iter(mOutputWaveTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      if (mDuration > 0.0)
      {
         // Create a temporary track
         WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat(),
                                                 track->GetRate());
         BeforeTrack(*track);

         // Fill it with data
         if (!GenerateTrack(tmp, *track, ntrack))
            bGoodResult = false;

         // Transfer the data from the temporary track to the actual one
         tmp->Flush();
         track->ClearAndPaste(mT0, mT1, tmp);
         delete tmp;

         if (!bGoodResult) {
            Failure();
            return false;
         }
      }
      else
      {
         // If the duration is zero, there's no need to actually
         // generate anything
         track->Clear(mT0, mT1);
      }

      // Move on to the next track
      ntrack++;
      track = (WaveTrack *)iter.Next();
   }

   Success();

   this->ReplaceProcessedWaveTracks(bGoodResult);
   HandleLinkedTracksOnGenerate(mDuration, mT0);

   mT1 = mT0 + mDuration; // Update selection.

   return true;
}

bool BlockGenerator::GenerateTrack(WaveTrack *tmp,
                                   const WaveTrack &track,
                                   int ntrack)
{
   bool bGoodResult = true;
   numSamples = track.TimeToLongSamples(mDuration);
   sampleCount i = 0;
   float *data = new float[tmp->GetMaxBlockSize()];
   sampleCount block = 0;

   while ((i < numSamples) && bGoodResult) {
      block = tmp->GetBestBlockSize(i);
      if (block > (numSamples - i))
         block = numSamples - i;

      GenerateBlock(data, track, block);

      // Add the generated data to the temporary track
      tmp->Append((samplePtr)data, floatSample, block);
      i += block;

      // Update the progress meter
      if (TrackProgress(ntrack, (double)i / numSamples))
         bGoodResult = false;
   }
   delete[] data;
   return bGoodResult;
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundTouchEffect.cpp

  Dominic Mazzoni

  This abstract class contains all of the common code for an
  effect that uses SoundTouch to do its processing (ChangeTempo
  and ChangePitch).

**********************************************************************/

#include "../Audacity.h"

#if USE_SOUNDTOUCH

#include <math.h>

#include "SoundTouchEffect.h"
#include "../WaveTrack.h"


bool EffectSoundTouch::Process()
{
   //Assumes that mSoundTouch has already been initialized
   //by the subclass

   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   mCurTrackNum = 0;
	m_maxNewLength = 0.0;
   while (track) {
      //Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {

         //Transform the marker timepoints to samples
         longSampleCount start = track->TimeToLongSamples(mCurT0);
         longSampleCount end = track->TimeToLongSamples(mCurT1);
         
         //Get the track rate and samples
         mCurRate = track->GetRate();
         mCurChannel = track->GetChannel();

         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(track, start, end))
            return false;
      }
      
      //Iterate to the next track
      track = (WaveTrack *) iter.Next();
      mCurTrackNum++;
   }

   delete mSoundTouch;
   mSoundTouch = NULL;

	mT1 = mT0 + m_maxNewLength; // Update selection.
   return true;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessSoundTouch on these blocks
bool EffectSoundTouch::ProcessOne(WaveTrack *track,
                                  longSampleCount start, longSampleCount end)
{
   WaveTrack *outputTrack;
   longSampleCount s;

   mSoundTouch->setSampleRate((unsigned int)(track->GetRate()+0.5));
   
   outputTrack = mFactory->NewWaveTrack(track->GetSampleFormat());

   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later 
   double len = (double)(end - start);

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer = new float[track->GetMaxBlockSize()];

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      sampleCount block = track->GetBestBlockSize(s);

      //Adjust the block size if it is the final block in the track
      if (s + block > end)
         block = end - s;

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr) buffer, floatSample, s, block);

      //Add samples to SoundTouch
      mSoundTouch->putSamples(buffer, block);

      //Get back samples from SoundTouch
      unsigned int outputCount = mSoundTouch->numSamples();
      if (outputCount > 0) {
         float *buffer2 = new float[outputCount];
         mSoundTouch->receiveSamples(buffer2, outputCount);
         outputTrack->Append((samplePtr)buffer2, floatSample, outputCount);
         delete[] buffer2;
      }

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (TrackProgress(mCurTrackNum, (s - start) / len))
         return false;
   }

   // Tell SoundTouch to finish processing any remaining samples
   mSoundTouch->flush();

   unsigned int outputCount = mSoundTouch->numSamples();
   if (outputCount > 0) {
      float *buffer2 = new float[outputCount];
      mSoundTouch->receiveSamples(buffer2, outputCount);
      outputTrack->Append((samplePtr)buffer2, floatSample, outputCount);
      delete[] buffer2;
   }

   // Flush the output WaveTrack (since it's buffered, too)
   outputTrack->Flush();

   // Clean up the buffer
   delete[]buffer;

   // Take the output track and insert it in place of the original
   // sample data

   track->Clear(mT0, mT1);
   track->Paste(mT0, outputTrack);

	double newLength = outputTrack->GetEndTime(); 
	if (newLength > m_maxNewLength) 
		m_maxNewLength = newLength; 

   // Delete the outputTrack now that its data is inserted in place
   delete outputTrack;

   //Return true because the effect processing succeeded.
   return true;
}

#endif // USE_SOUNDTOUCH

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 43212604-7b03-4b49-85bd-adde0f909460


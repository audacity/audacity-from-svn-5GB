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
   //Track::All is needed because Reverse should move the labels too
   this->CopyInputTracks(Track::All); // Set up mOutputTracks.
   bool bGoodResult = true;

   TrackListIterator iter(mOutputTracks);
   Track *t = iter.First();
   int count = 0;
   double lt0 = -2.0; // -2.0 = not initialized yet; -1.0 = is not possible to reverse labels
   double lt1 = -2.0;
   while (t) {
      if (t->GetKind() == Track::Wave) {
         WaveTrack *track = (WaveTrack*)t;
         double trackStart = track->GetStartTime();
         double trackEnd = track->GetEndTime();
         double t0 = mT0 < trackStart? trackStart: mT0;
         double t1 = mT1 > trackEnd? trackEnd: mT1;

         if (track->GetSelected()) {
            if (t1 > t0) {
               sampleCount start = track->TimeToLongSamples(t0);
               sampleCount end = track->TimeToLongSamples(t1);
               sampleCount len = (sampleCount)(end - start);

               if (!ProcessOne(count, track, start, len))
               {
                  bGoodResult = false;
                  break;
               }

               //if this is the first selected track in the group we can define the label
               //region to be reversed without any more verifications
               if (lt0 == -2.0) {
                  lt0 = t0;
                  lt1 = t1;
               }
               //track has not the same selection result (after checking start and end of the track as the first track)
               //it's not possible to know how to reverse the label in this situation.
               else if ((lt0 != t0) || (lt1 != t1)) {
                  lt0 = -1.0;
                  lt1 = -1.0;
               }
            } 
         }
         //if the track is not selected but has no content inside the selected region, there is no problem.
         //otherwise we should not change the labels.
         else if (t1 > t0) {
            lt0 = -1.0;
            lt1 = -1.0;
         }
      }
      else if (t->GetKind() == Track::Label) {
         //if we can reverse the label
         if (lt0 != -1.0) {
            LabelTrack *track = (LabelTrack*)t;
            track->ChangeLabelsOnReverse(lt0, lt1);
         }
         //otherwise we set the region to the not initialized state -2.0 to begin the process in the next group
         else {
            lt0 = -2.0;
            lt1 = -2.0;
         }
      }
      t = iter.Next();
      count++;
   }

   this->ReplaceProcessedTracks(bGoodResult); 
   return bGoodResult;
}

bool EffectReverse::ProcessOne(int count, WaveTrack *track,
                               sampleCount start, sampleCount len)
{
   bool rc = true;
   // keep track of two blocks whose data we will swap
   sampleCount first = start;
   sampleCount second;

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
      
      if( TrackProgress(count, 2*(first-start) / (double) originalLen) ) {
         rc = false;
         break;
      }
   }

   delete[] buffer1;
   delete[] buffer2;

   return rc;
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


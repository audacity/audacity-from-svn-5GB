/**********************************************************************

  Audacity: A Digital Audio Editor

  SimplePairedTwoTrack.h

  Vincent A. Busam
  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  two track effect where you want the values of both tracks
  together.  Inherit from it if your effect doesn't just
  modifies a track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetEffectName, GetEffectAction, and ProcessSimplePairedTwoTrack.

**********************************************************************/

#include <math.h>

#include "SimplePairedTwoTrack.h"
#include "../WaveTrack.h"
#include <wx/msgdlg.h>

bool EffectSimplePairedTwoTrackInt16::Init()
{
   mnTracks = 1;
   mnBlockSize = 0;

   TrackListIterator iter(mWaveTracks);
   Track *left = iter.First();
   if ( left == 0 )
	  return false;		// we need an existing track

   while(left) {
       longSampleCount lstart, rstart;
       sampleCount llen, rlen;
       GetSamples((WaveTrack *)left, &lstart, &llen);
     
       if (left->GetLinked()) {
	 	 mnTracks = 2;
         Track *right = iter.Next();
         GetSamples((WaveTrack *)right, &rstart, &rlen);
        
         if (llen != rlen || ((WaveTrack *)left)->GetRate() != ((WaveTrack *)right)->GetRate()) {
            wxMessageBox("Sorry, This effect cannot be performed on stereo tracks where "
                        "the individual channels of the track do not match.");
            return false;
		 }
      }
     
      left = iter.Next();
   }

   return true;
}

bool EffectSimplePairedTwoTrackInt16::Process()
{
   TrackListIterator iter(mWaveTracks);
   int count = 0;
   Track *left = iter.First();
   Track *right;
   while(left) {
      longSampleCount lstart, rstart;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);

      right = NULL;
      if (left->GetLinked()) {
         right = iter.Next();         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      bool success = ProcessTwo(count,
                                   (WaveTrack *)left, (WaveTrack *)right,
                                   lstart, rstart, len);
      if (!success)
         return false;
   
      left = iter.Next();
      count++;
   }
   
   return true;
}

bool EffectSimplePairedTwoTrackInt16::ProcessTwo(int count, WaveTrack *left, WaveTrack *right,
										    longSampleCount lstart,
										    longSampleCount rstart, sampleCount len)
{
   if (mnBlockSize == 0) {
      mnBlockSize = left->GetMaxBlockSize();

      mLeftBuffer  = new short int[mnBlockSize];
	  if ( mnTracks > 1 )
		  mRightBuffer = new short int[mnBlockSize];
	  else
		  mRightBuffer = 0;
   }

   // Get both buffers here

   sampleCount originalLen = len;
   longSampleCount ls = lstart;
   longSampleCount rs = rstart;
   while (len) {
      int block = mnBlockSize;
      if (block > len)
         block = len;

      left->Get((samplePtr)mLeftBuffer, int16Sample, ls, block);
      if (right) {
         right->Get((samplePtr)mRightBuffer, int16Sample, rs, block);
      }

	  // The derived class process the tracks here
      ProcessSimplePairedTwoTrack(mLeftBuffer, mRightBuffer, block);

      left->Set((samplePtr)mLeftBuffer, int16Sample, ls, block);
      
      if (right) {
         right->Set((samplePtr)mRightBuffer, int16Sample, rs, block);
      }      

      len -= block;
      ls += block;
      rs += block;
      
      if (mnTracks > 1) {      
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen))
            break;
      }
      else {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen))
            break;
      }
   }

   return true;
}

void EffectSimplePairedTwoTrackInt16::GetSamples(WaveTrack *track,
										         longSampleCount *start,
										         sampleCount *len)
{
   double trackStart = track->GetStartTime();
   double trackEnd = track->GetEndTime();
   double t0 = mT0 < trackStart? trackStart: mT0;
   double t1 = mT1 > trackEnd? trackEnd: mT1;
   
   if (t1 > t0) {
      *start = track->TimeToLongSamples(t0);
      longSampleCount end = track->TimeToLongSamples(t1);
      *len = (sampleCount)(end - *start);
   }
   else {
      *start = 0;
      *len  = 0;
   }
}

void EffectSimplePairedTwoTrackInt16::End()
{
   if (mnBlockSize) {
      delete[]mLeftBuffer;
      delete[]mRightBuffer;

   }
   mLeftBuffer = NULL;
   mRightBuffer = NULL;
}

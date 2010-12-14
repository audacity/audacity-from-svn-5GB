/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.cpp

  Lynn Allan

*******************************************************************//**

\class EffectStereoToMono
\brief An Effect.

*//*******************************************************************/


#include "../Audacity.h"

// For compilers that support precompilation, includes "wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif

#include <math.h>
#include "StereoToMono.h"
#include "../Project.h"

EffectStereoToMono::EffectStereoToMono()
{
   Init();
}

bool EffectStereoToMono::Init()
{
   return true;
}

void EffectStereoToMono::End()
{
}

//TODO: There are a lot of places where a track is being checked
//      to see if it is stereo. Consolidate these
bool EffectStereoToMono::CheckWhetherSkipEffect()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *t = (WaveTrack*)iter.First();
   while (t) {
      if (t->GetLinked()) {
         return false;
      }
      t = (WaveTrack *)iter.Next();
   }

   return true;
}

bool EffectStereoToMono::ProcessOne(int count)
{
   float  curLeftFrame;
   float  curRightFrame;
   float  curMonoFrame;

   sampleCount idealBlockLen = mLeftTrack->GetMaxBlockSize() * 2;
   sampleCount index = 0;
   sampleCount outTrackOffset = 0;
   float *leftBuffer = new float[idealBlockLen];
   float *rightBuffer = new float[idealBlockLen];
   bool rc;

   while (index < mLeftTrackLen) {
      rc = mLeftTrack->Get((samplePtr)leftBuffer, floatSample, index, idealBlockLen);
      rc = mRightTrack->Get((samplePtr)rightBuffer, floatSample, index, idealBlockLen);
      sampleCount limit = idealBlockLen;
      if ((index + idealBlockLen) > mLeftTrackLen) {
         limit = mLeftTrackLen - index;
      }
      for (sampleCount i = 0; i < limit; ++i) {
         index++;
         curLeftFrame = leftBuffer[i];
         curRightFrame = rightBuffer[i];
         curMonoFrame = (curLeftFrame + curRightFrame) / 2.0;
         leftBuffer[i] = curMonoFrame;
      }
      rc = mLeftTrack->Set((samplePtr)leftBuffer, floatSample, outTrackOffset, limit);
      outTrackOffset += limit;
      if (TrackProgress(count, ((double)index / (double)mLeftTrackLen)))
         return false;
   }

   mLeftTrack->SetLinked(false);
   mRightTrack->SetLinked(false);
   mLeftTrack->SetChannel(Track::MonoChannel);
   m_pOutputWaveTracks->Remove(mRightTrack);
   delete mRightTrack;

   delete [] leftBuffer;
   delete [] rightBuffer;

   return true;
}

bool EffectStereoToMono::Process()
{
   // Do not use mWaveTracks here.  We will possibly delete tracks,
   // so we must use the "real" tracklist.
   this->CopyInputWaveTracks(); // Set up m_pOutputWaveTracks.
   bool bGoodResult = true;

   TrackListIterator iter(m_pOutputWaveTracks);
   mLeftTrack = (WaveTrack *)iter.First();
   bool refreshIter = false;

   int count = 0;
   while (mLeftTrack) {
      if (mLeftTrack->GetKind() == Track::Wave &&
          mLeftTrack->GetSelected() &&
          mLeftTrack->GetLinked()) {

         mRightTrack = (WaveTrack *)iter.Next();

         mLeftTrackLen = mLeftTrack->TimeToLongSamples(mLeftTrack->GetEndTime()); 
         mRightTrackLen = mRightTrack->TimeToLongSamples(mRightTrack->GetEndTime()); 
         long diff = abs((long)mRightTrackLen - (long)mLeftTrackLen);
         
         if ((diff <= 2) && (mLeftTrack->GetRate() == mRightTrack->GetRate())) {
            bGoodResult = ProcessOne(count);
            if (!bGoodResult)
               break;

            // The right channel has been deleted, so we must restart from the beginning
            refreshIter = true;
         }
      }
            
      if (refreshIter) {
         mLeftTrack = (WaveTrack *)iter.First();
         refreshIter = false;
      }
      else {
         mLeftTrack = (WaveTrack *)iter.Next();
      }
      count++;
   }
         
   this->ReplaceProcessedWaveTracks(bGoodResult); 
   return bGoodResult;
}

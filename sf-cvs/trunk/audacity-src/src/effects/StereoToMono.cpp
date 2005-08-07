/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.cpp

  Lynn Allan

**********************************************************************/
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
   TrackListIterator iter(mTracks);
   mLeftTrack = (WaveTrack*)(iter.First());
   if ( mLeftTrack == 0 ) {
      return true;  // we need an existing track .. this is actually an error that will be caught later
   }
   int channelLeftNum = mLeftTrack->GetChannel();

   if (mLeftTrack->GetLinked()) {
      return false;
   }
   return true;  // already mono
}

bool EffectStereoToMono::Process()
{
   mnTracks = 1;
   mnBlockSize = 0;
   bool validStereo = false;

   TrackListIterator iter(mTracks);
   mLeftTrack = (WaveTrack*)(iter.First());
   if ( mLeftTrack == 0 ) {
      return false;		// we need an existing track
   }
   //int channelLeftNum = mLeftTrack->GetChannel();

   mLeftTrackEnd = mLeftTrack->GetEndTime();
   mLeftTrackLen = mLeftTrack->TimeToLongSamples(mLeftTrackEnd); 
   mLeftRate = mLeftTrack->GetRate();
   
   if (mLeftTrack->GetLinked()) {
      mnTracks = 2;
      mRightTrack = (WaveTrack*)(iter.Next());
      //int channelRightNum = mRightTrack->GetChannel();
      mRightTrackEnd = mRightTrack->GetEndTime();
      mRightTrackLen = mRightTrack->TimeToLongSamples(mRightTrackEnd); 
      double rightRate = mRightTrack->GetRate();
      long diff = abs((long)mRightTrackLen - (long)mLeftTrackLen);
      WaveTrack *thirdTrack = (WaveTrack*)(iter.Next());
      
      if ((diff <= 2) && (mLeftRate == rightRate) && (thirdTrack == 0)) {
         validStereo = true;
      }
   }
   if (validStereo == false) {
      wxMessageBox(_("Sorry, This effect can only be performed on stereo tracks where the individual channels of the track match."));
      return false;
   }
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
      TrackProgress(0, ((double)index / (double)mLeftTrackLen));
   }

   mLeftTrack->SetLinked( false );
   mRightTrack->SetLinked( false );
   mLeftTrack->SetChannel( Track::MonoChannel );
   mTracks->Remove( mRightTrack );
   delete mRightTrack;

   delete [] leftBuffer;
   delete [] rightBuffer;
   return true;
}

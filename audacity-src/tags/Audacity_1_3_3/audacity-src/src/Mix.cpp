/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.cpp

  Dominic Mazzoni
  Markus Meyer

*******************************************************************//**

\class Mixer
\brief Functions for doing the mixdown of the tracks.

*//****************************************************************//**

\class MixerSpec
\brief Class used with Mixer.

*//*******************************************************************/


#include "Audacity.h"

#include "Mix.h"

#include <math.h>

#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>

#include "WaveTrack.h"
#include "DirManager.h"
#include "Envelope.h"
#include "Internat.h"
#include "Prefs.h"
#include "Project.h"
#include "Resample.h"

bool MixAndRender(TrackList *tracks, TrackFactory *trackFactory,
                  double rate, sampleFormat format,
                  double startTime, double endTime,
                  WaveTrack **newLeft, WaveTrack **newRight)
{
   // This function was formerly known as "Quick Mix".  It takes one or
   // more tracks as input; of all tracks that are selected, it mixes
   // them together, applying any envelopes, amplitude gain, panning,
   // and real-time effects in the process.  The resulting pair of
   // tracks (stereo) are "rendered" and have no effects, gain, panning,
   // or envelopes.

   WaveTrack **waveArray;
   Track *t;
   int numWaves = 0;
   int numMono = 0;
   bool mono = false;
   int w;

   TrackListIterator iter(tracks);

   t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         numWaves++;
         float pan = ((WaveTrack*)t)->GetPan();
         if (t->GetChannel() == Track::MonoChannel && pan == 0)
            numMono++;
      }
      t = iter.Next();
   }

   if (numMono == numWaves)
      mono = true;

   double totalTime = 0.0;

   waveArray = new WaveTrack *[numWaves];
   w = 0;
   t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         waveArray[w++] = (WaveTrack *) t;
         if (t->GetEndTime() > totalTime)
            totalTime = t->GetEndTime();
      }
      t = iter.Next();
   }

   WaveTrack *mixLeft = trackFactory->NewWaveTrack(format, rate);
   mixLeft->SetName(_("Mix"));
   WaveTrack *mixRight = 0;
   if (mono) {
      mixLeft->SetChannel(Track::MonoChannel);
   }
   else {
      mixRight = trackFactory->NewWaveTrack(format, rate);
      mixRight->SetName(_("Mix"));
      mixLeft->SetChannel(Track::LeftChannel);
      mixRight->SetChannel(Track::RightChannel);
      mixLeft->SetLinked(true);
      mixRight->SetTeamed(true);
   }

   int maxBlockLen = mixLeft->GetIdealBlockSize();

   if (startTime == endTime) {
      startTime = 0.0;
      endTime = totalTime;
   }

   Mixer *mixer = new Mixer(numWaves, waveArray, tracks->GetTimeTrack(),
                            startTime, endTime, mono ? 1 : 2, maxBlockLen, false,
                            rate, format);

   wxYield();
   GetActiveProject()->ProgressShow(_NoAcc("&Mix and Render"),
                                    _("Mixing and rendering tracks"));
   wxBusyCursor busy;
   
   bool cancelling = false;
   while(!cancelling) {
      sampleCount blockLen = mixer->Process(maxBlockLen);

      if (blockLen == 0)
         break;

      if (mono) {
         samplePtr buffer = mixer->GetBuffer();
         mixLeft->Append(buffer, format, blockLen);
      }
      else {
         samplePtr buffer;
         buffer = mixer->GetBuffer(0);
         mixLeft->Append(buffer, format, blockLen);
         buffer = mixer->GetBuffer(1);
         mixRight->Append(buffer, format, blockLen);
      }

      int progressvalue = int (1000 * (mixer->MixGetCurrentTime() / totalTime));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   mixLeft->Flush();
   *newLeft = mixLeft;
   if (!mono) {
      mixRight->Flush();
      *newRight = mixRight;
   }

#if 0
   int elapsedMS = wxGetElapsedTime();
   double elapsedTime = elapsedMS * 0.001;
   double maxTracks = totalTime / (elapsedTime / numWaves);

   // Note: these shouldn't be translated - they're for debugging
   // and profiling only.
   printf("      Tracks: %d\n", numWaves);
   printf("  Mix length: %f sec\n", totalTime);
   printf("Elapsed time: %f sec\n", elapsedTime);
   printf("Max number of tracks to mix in real time: %f\n", maxTracks);
#endif

   delete[] waveArray;
   delete mixer;

   return true;
}

Mixer::Mixer(int numInputTracks, WaveTrack **inputTracks,
             TimeTrack *timeTrack,
             double startTime, double stopTime,
             int numOutChannels, int outBufferSize, bool outInterleaved,
             double outRate, sampleFormat outFormat,
             bool highQuality, MixerSpec *mixerSpec)
{
   int i;

   mNumInputTracks = numInputTracks;
   mInputTrack = new WaveTrack*[mNumInputTracks];
   mSamplePos = new longSampleCount[mNumInputTracks];
   for(i=0; i<mNumInputTracks; i++) {
      mInputTrack[i] = inputTracks[i];
      mSamplePos[i] = inputTracks[i]->TimeToLongSamples(startTime);
   }
   mTimeTrack = timeTrack;
   mT0 = startTime;
   mT1 = stopTime;
   mT = startTime;
   mNumChannels = numOutChannels;
   mBufferSize = outBufferSize;
   mInterleaved = outInterleaved;
   mRate = outRate;
   mFormat = outFormat;
   mApplyTrackGains = true;
   mGains = new float[mNumChannels];
   if( mixerSpec && mixerSpec->GetNumChannels() == mNumChannels &&
         mixerSpec->GetNumTracks() == mNumInputTracks )
      mMixerSpec = mixerSpec;
   else
      mMixerSpec = NULL;

   if (mInterleaved) {
      mNumBuffers = 1;
      mInterleavedBufferSize = mBufferSize * mNumChannels;
   }
   else {
      mNumBuffers = mNumChannels;
      mInterleavedBufferSize = mBufferSize;
   }
   
   mBuffer = new samplePtr[mNumBuffers];
   mTemp = new samplePtr[mNumBuffers];
   for (int c = 0; c < mNumBuffers; c++) {
      mBuffer[c] = NewSamples(mInterleavedBufferSize, mFormat);
      mTemp[c] = NewSamples(mInterleavedBufferSize, floatSample);
   }
   mFloatBuffer = new float[mInterleavedBufferSize];

   mQueueMaxLen = 65536;
   mProcessLen = 1024;

   mQueueStart = new int[mNumInputTracks];
   mQueueLen = new int[mNumInputTracks];
   mSampleQueue = new float *[mNumInputTracks];
   mSRC = new Resample*[mNumInputTracks];
   for(i=0; i<mNumInputTracks; i++) {
      double factor = (mRate / mInputTrack[i]->GetRate());
      double lowFactor = factor, highFactor = factor;
      if (timeTrack) {
         highFactor /= timeTrack->GetRangeLower() / 100.0;
         lowFactor /= timeTrack->GetRangeUpper() / 100.0;
      }
      mSRC[i] = new Resample(highQuality, lowFactor, highFactor);
      mSampleQueue[i] = new float[mQueueMaxLen];
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }

   int envLen = mInterleavedBufferSize;
   if (mQueueMaxLen > envLen)
      envLen = mQueueMaxLen;
   mEnvValues = new double[envLen];
}

Mixer::~Mixer()
{
   int i;

   for (i = 0; i < mNumBuffers; i++) {
      DeleteSamples(mBuffer[i]);
      DeleteSamples(mTemp[i]);
   }
   delete[] mBuffer;
   delete[] mInputTrack;
   delete[] mEnvValues;
   delete[] mFloatBuffer;
   delete[] mGains;
	delete[] mSamplePos;

   for(i=0; i<mNumInputTracks; i++) {
      delete mSRC[i];
      delete[] mSampleQueue[i];
   }
	delete[] mSRC;
   delete[] mSampleQueue;
   delete[] mQueueStart;
   delete[] mQueueLen;
}

void Mixer::ApplyTrackGains(bool apply)
{
   mApplyTrackGains = apply;
}

void Mixer::Clear()
{
   for (int c = 0; c < mNumBuffers; c++) {
      memset(mTemp[c], 0, mInterleavedBufferSize * SAMPLE_SIZE(floatSample));
   }
}

void MixBuffers(int numChannels, int *channelFlags, float *gains,
                samplePtr src, samplePtr *dests,
                int len, bool interleaved)
{
   for (int c = 0; c < numChannels; c++) {
      if (!channelFlags[c])
         continue;
      
      samplePtr destPtr;
      int skip;
      
      if (interleaved) {
         destPtr = dests[0] + c*SAMPLE_SIZE(floatSample);
         skip = numChannels;
      } else {
         destPtr = dests[c];
         skip = 1;
      }

      float gain = gains[c];
      float *dest = (float *)destPtr;
      float *temp = (float *)src;
      for (int j = 0; j < len; j++) {
         *dest += temp[j] * gain;   // the actual mixing process
         dest += skip;
      }
   }
}
   
sampleCount Mixer::MixVariableRates(int *channelFlags, WaveTrack *track,
                                    longSampleCount *pos, float *queue,
                                    int *queueStart, int *queueLen,
                                    Resample *SRC)
{
   double initialWarp = mRate / track->GetRate();
   double t = *pos / track->GetRate();
   int sampleSize = SAMPLE_SIZE(floatSample);
   int i, c;

   sampleCount out = 0;

   while(out < mMaxOut) {
      if (*queueLen < mProcessLen) {
         memmove(queue, &queue[*queueStart],
                 (*queueLen)*sampleSize);
         *queueStart = 0;

         int getLen = mQueueMaxLen - *queueLen;

         #if 0

         // TODO: fix this code so that extra silence isn't added
         // to the end of a track

         double trackTime = (*pos + getLen) / track->GetRate();
         if (trackTime > track->GetEndTime()) {
            getLen = (int)(0.5 + track->GetRate() *
                           (track->GetEndTime() -
                            ((*pos) / track->GetRate())));
         }
         #endif

         track->Get((samplePtr)&queue[*queueLen], floatSample,
                    *pos, getLen);

         track->GetEnvelopeValues(mEnvValues, getLen, (*pos) / track->GetRate(),
                                  1.0 / track->GetRate());

         for(i=0; i<getLen; i++)
            queue[(*queueLen)+i] *= mEnvValues[i];
         *queueLen += getLen;
         *pos += getLen;
      }

      sampleCount thisProcessLen = mProcessLen;
      if (*queueLen < mProcessLen)
         thisProcessLen = *queueLen;

      double factor = initialWarp;
      if (mTimeTrack) {
         double warpFactor = mTimeTrack->GetEnvelope()->GetValue(t);
         warpFactor = (mTimeTrack->GetRangeLower() * (1 - warpFactor) +
                       warpFactor * mTimeTrack->GetRangeUpper())/100.0;

         factor /= warpFactor;
      }

      int input_used;
      bool last = (*queueLen < mProcessLen);
      int outgen = SRC->Process(factor,
                                &queue[*queueStart],
                                thisProcessLen,
                                last,
                                &input_used,
                                &mFloatBuffer[out],
                                mMaxOut - out);

      if (outgen < 0)
         return 0;

      *queueStart += input_used;
      *queueLen -= input_used;
      out += outgen;
      t += (input_used / track->GetRate());

      if (last)
         break;
   }

   for(c=0; c<mNumChannels; c++)
      if (mApplyTrackGains)
         mGains[c] = track->GetChannelGain(c);
      else
         mGains[c] = 1.0;

   MixBuffers(mNumChannels, channelFlags, mGains,
              (samplePtr)mFloatBuffer, mTemp, mMaxOut, mInterleaved);

   return mMaxOut;
}

sampleCount Mixer::MixSameRate(int *channelFlags, WaveTrack *track,
                               longSampleCount *pos)
{
   int slen = mMaxOut;
   int c;
   double t = *pos / track->GetRate();

   if (t + slen/track->GetRate() > mT1)
      slen = (int)((mT1 - t) * track->GetRate() + 0.5);
   
   if (slen <= 0)
      return 0;

   if (slen > mMaxOut)
      slen = mMaxOut;

   track->Get((samplePtr)mFloatBuffer, floatSample, *pos, slen);
   track->GetEnvelopeValues(mEnvValues, slen, t, 1.0 / mRate);
   for(int i=0; i<slen; i++)
      mFloatBuffer[i] *= mEnvValues[i]; // Track gain control will go here?
   
   for(c=0; c<mNumChannels; c++)
      if (mApplyTrackGains)
         mGains[c] = track->GetChannelGain(c);
      else
         mGains[c] = 1.0;

   MixBuffers(mNumChannels, channelFlags, mGains,
              (samplePtr)mFloatBuffer, mTemp, slen, mInterleaved);

   *pos += slen;

   return slen;
}

sampleCount Mixer::Process(int maxToProcess)
{
   if (mT >= mT1)
      return 0;
   
   int i, j;
   sampleCount out;
   sampleCount maxOut = 0;
   int *channelFlags = new int[mNumChannels];

   mMaxOut = maxToProcess;

   Clear();
   for(i=0; i<mNumInputTracks; i++) {
      WaveTrack *track = mInputTrack[i];
      for(j=0; j<mNumChannels; j++)
         channelFlags[j] = 0;

      if( mMixerSpec ) {
         //ignore left and right when downmixing is not required
         for( j = 0; j < mNumChannels; j++ )
            channelFlags[ j ] = mMixerSpec->mMap[ i ][ j ] ? 1 : 0;
      }
      else {
         switch(track->GetChannel()) {
         case Track::MonoChannel:
         default:
            for(j=0; j<mNumChannels; j++)
               channelFlags[j] = 1;
            break;
         case Track::LeftChannel:
            channelFlags[0] = 1;
            break;
         case Track::RightChannel:
            if (mNumChannels >= 2)
               channelFlags[1] = 1;
            else
               channelFlags[0] = 1;
            break;
         }
      }

      if (mTimeTrack ||
          track->GetRate() != mRate)
         out = MixVariableRates(channelFlags, track,
                                &mSamplePos[i], mSampleQueue[i],
                                &mQueueStart[i], &mQueueLen[i], mSRC[i]);
      else
         out = MixSameRate(channelFlags, track, &mSamplePos[i]);

      if (out > maxOut)
         maxOut = out;
   }
   out = mInterleaved ? maxOut * mNumChannels : maxOut;
   for(int c=0; c<mNumBuffers; c++)
      CopySamples(mTemp[c], floatSample, mBuffer[c], mFormat, out);

   mT += (maxOut / mRate);

   delete [] channelFlags; 

   return maxOut;
}

samplePtr Mixer::GetBuffer()
{
   return mBuffer[0];
}

samplePtr Mixer::GetBuffer(int channel)
{
   return mBuffer[channel];
}

double Mixer::MixGetCurrentTime()
{
   return mT;
}

void Mixer::Restart()
{
   int i;

   mT = mT0;

   for(i=0; i<mNumInputTracks; i++)
      mSamplePos[i] = mInputTrack[i]->TimeToLongSamples(mT0);

   for(i=0; i<mNumInputTracks; i++) {
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }
}

void Mixer::Reposition(double t)
{
   int i;

   mT = t;
   if( mT < mT0 )
      mT = mT0;
   if( mT > mT1 )
      mT = mT1;
   
   for(i=0; i<mNumInputTracks; i++) {
      mSamplePos[i] = mInputTrack[i]->TimeToLongSamples(mT);
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }
}

MixerSpec::MixerSpec( int numTracks, int maxNumChannels )
{
   mNumTracks = mNumChannels = numTracks;
   mMaxNumChannels = maxNumChannels;
   
   if( mNumChannels > mMaxNumChannels )
         mNumChannels = mMaxNumChannels;
   
   Alloc();

   for( int i = 0; i < mNumTracks; i++ )
      for( int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = ( i == j );
}

MixerSpec::MixerSpec( const MixerSpec &mixerSpec )
{
   mNumTracks = mixerSpec.mNumTracks;
   mMaxNumChannels = mixerSpec.mMaxNumChannels;
   mNumChannels = mixerSpec.mNumChannels;

   Alloc();

   for( int i = 0; i < mNumTracks; i++ )
      for( int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];
}

void MixerSpec::Alloc()
{
   mMap = new bool*[ mNumTracks ];
   for( int i = 0; i < mNumTracks; i++ )
      mMap[ i ] = new bool[ mMaxNumChannels ];
}

MixerSpec::~MixerSpec()
{
   Free();
}

void MixerSpec::Free()
{
   for( int i = 0; i < mNumTracks; i++ )
      delete[] mMap[ i ];

   delete[] mMap;
}

bool MixerSpec::SetNumChannels( int newNumChannels )
{
   if( mNumChannels == newNumChannels )
      return true;

   if( newNumChannels > mMaxNumChannels )
      return false;
   
   for( int i = 0; i < mNumTracks; i++ )
   {
      for( int j = newNumChannels; j < mNumChannels; j++ )
         mMap[ i ][ j ] = false;

      for( int j = mNumChannels; j < newNumChannels; j++ )
         mMap[ i ][ j ] = false;
   }

   mNumChannels = newNumChannels;
   return true;
}

MixerSpec& MixerSpec::operator=( const MixerSpec &mixerSpec )
{
   Free();
   
   mNumTracks = mixerSpec.mNumTracks;
   mNumChannels = mixerSpec.mNumChannels;
   mMaxNumChannels = mixerSpec.mMaxNumChannels;

   Alloc();

   for( int i = 0; i < mNumTracks; i++ )
      for( int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];

   return *this;
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
// arch-tag: d4e10e74-cdf9-46ac-b309-91b115d2a78f


/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include <wx/string.h>

#include "SampleFormat.h"
#include "WaveTrack.h"
#include "TimeTrack.h"
#include "ControlToolBar.h"

#if USE_LIBSAMPLERATE
#include <samplerate.h>
#endif

class ControlToolBar;
class DirManager;

bool QuickMix(TrackList * tracks, TrackFactory *factory,
              double rate, sampleFormat format);

void MixBuffers(int numChannels, int *channelFlags, sampleFormat format,
                samplePtr src, samplePtr *dests, int len, bool interleaved);

class Mixer {
 public:
   // 
   // Constructor / Destructor
   //

   Mixer(int numInputTracks, WaveTrack **inputTracks,
         TimeTrack *timeTrack,
         double startTime, double stopTime,
         int numOutChannels, int outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         bool highQuality = true);

   virtual ~ Mixer();

   //
   // Processing
   //

   /// Returns number of output samples
   sampleCount Process(sampleCount maxSamples);

   /// Current time in seconds
   double MixGetCurrentTime();

   /// Retrieve the main buffer or the interleaved buffer
   samplePtr GetBuffer();

   // Retrieve one of the non-interleaved buffers
   samplePtr GetBuffer(int channel);

 private:

   void Clear();
   sampleCount MixSameRate(int *channelFlags, WaveTrack *src,
                           longSampleCount *pos);

#if USE_LIBSAMPLERATE
   sampleCount MixVariableRates(int *channelFlags, WaveTrack *track,
                                longSampleCount *pos, float *queue,
                                int *queueStart, int *queueLen,
                                SRC_STATE *SRC);
#endif

 private:
   // Input
   int              mNumInputTracks;
   WaveTrack      **mInputTrack;
   TimeTrack       *mTimeTrack;
   longSampleCount *mSamplePos;
   double          *mEnvValues;
   double           mT;  // Current time
   double           mT0; // Start time
   double           mT1; // Stop time (none if mT0==mT1)   
#if USE_LIBSAMPLERATE
   SRC_STATE      **mSRC;
   float          **mSampleQueue;
   int             *mQueueStart;
   int             *mQueueLen;
   int              mQueueMaxLen;
   int              mProcessLen;
#endif

   // Output
   int              mMaxOut;
   int              mNumChannels;
   int              mNumBuffers;
   int              mBufferSize;
   int              mInterleavedBufferSize;
   sampleFormat     mFormat;
   bool             mInterleaved;
   samplePtr       *mBuffer;
   samplePtr        mTemp;
   float           *mFloatBuffer;
   double           mRate;
};

#endif

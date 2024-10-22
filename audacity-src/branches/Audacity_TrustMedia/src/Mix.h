/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni
  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include <wx/string.h>

#include "SampleFormat.h"
#include "WaveTrack.h"
#include "TimeTrack.h"
#include "Resample.h"

bool MixAndRender(TrackList *tracks, TrackFactory *trackFactory,
                  double rate, sampleFormat format,
                  double startTime, double endTime,
                  WaveTrack **newLeft, WaveTrack **newRight);

void MixBuffers(int numChannels, int *channelFlags, float *gains,
                samplePtr src,
                samplePtr *dests, int len, bool interleaved);

class AUDACITY_DLL_API MixerSpec
{
   int mNumTracks, mNumChannels, mMaxNumChannels;
  
   void Alloc();
   void Free();

   public:
   bool **mMap;
   
   MixerSpec( int numTracks, int maxNumChannels );
   MixerSpec( const MixerSpec &mixerSpec );
   virtual ~MixerSpec();

   bool SetNumChannels( int numChannels );
   int GetNumChannels() { return mNumChannels; }

   int GetMaxNumChannels() { return mMaxNumChannels; }
   int GetNumTracks() { return mNumTracks; }

   MixerSpec& operator=( const MixerSpec &mixerSpec );
};

class AUDACITY_DLL_API Mixer {
 public:
   // 
   // Constructor / Destructor
   //

   Mixer(int numInputTracks, WaveTrack **inputTracks,
         TimeTrack *timeTrack,
         double startTime, double stopTime,
         int numOutChannels, int outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         bool highQuality = true, MixerSpec *mixerSpec = NULL);

   virtual ~ Mixer();

   //
   // Setup
   //

   void ApplyTrackGains(bool apply = true); // True by default

   //
   // Processing
   //

   /// Process a maximum of 'maxSamples' samples and put them into
   /// a buffer which can be retrieved by calling GetBuffer().
   /// Returns number of output samples, or 0, if there are no
   /// more samples that must be processed.
   sampleCount Process(sampleCount maxSamples);

   /// Restart processing at beginning of buffer next time
   /// Process() is called.
   void Restart();

   /// Reposition processing to absolute time next time
   /// Process() is called.
   void Reposition(double t);

   /// Current time in seconds
   double MixGetCurrentTime();

   /// Retrieve the main buffer or the interleaved buffer
   samplePtr GetBuffer();

   /// Retrieve one of the non-interleaved buffers
   samplePtr GetBuffer(int channel);

 private:

   void Clear();
   sampleCount MixSameRate(int *channelFlags, WaveTrack *src,
                           sampleCount *pos);

   sampleCount MixVariableRates(int *channelFlags, WaveTrack *track,
                                sampleCount *pos, float *queue,
                                int *queueStart, int *queueLen,
                                Resample *SRC);

 private:
   // Input
   int              mNumInputTracks;
   WaveTrack      **mInputTrack;
   TimeTrack       *mTimeTrack;
   sampleCount     *mSamplePos;
   bool             mApplyTrackGains;
   float           *mGains;
   double          *mEnvValues;
   double           mT;  // Current time
   double           mT0; // Start time
   double           mT1; // Stop time (none if mT0==mT1)   
   Resample       **mSRC;
   float          **mSampleQueue;
   int             *mQueueStart;
   int             *mQueueLen;
   int              mQueueMaxLen;
   int              mProcessLen;
   MixerSpec        *mMixerSpec;

   // Output
   int              mMaxOut;
   int              mNumChannels;
   int              mNumBuffers;
   int              mBufferSize;
   int              mInterleavedBufferSize;
   sampleFormat     mFormat;
   bool             mInterleaved;
   samplePtr       *mBuffer;
   samplePtr       *mTemp;
   float           *mFloatBuffer;
   double           mRate;
};

#ifdef IS_TRUSTMEDIA_VERSION
   //vvvvvv

   // TrustMediaMix is derived from MixAndRender.
   bool TrustMediaMix(TrackList *tracks, TrackFactory *trackFactory,
                        double rate, sampleFormat format,
                        double startTime, double endTime,
                        WaveTrack **newLeft, WaveTrack **newRight);

   class TrustMedia_AuditionMixer : public Mixer
   {
      TrustMedia_AuditionMixer(int numInputTracks, WaveTrack **inputTracks,
                                 TimeTrack *timeTrack,
                                 double startTime, double stopTime,
                                 int numOutChannels, int outBufferSize, bool outInterleaved,
                                 double outRate, sampleFormat outFormat,
                                 bool highQuality = true, MixerSpec *mixerSpec = NULL);
      virtual ~TrustMedia_AuditionMixer() {}; 
   };
#endif // IS_TRUSTMEDIA_VERSION

#endif // __AUDACITY_MIX__


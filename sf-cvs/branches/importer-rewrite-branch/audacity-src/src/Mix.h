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
#include "ControlToolBar.h"

class ControlToolBar;
class DirManager;

bool QuickMix(TrackList * tracks, DirManager * dirManager,
              double rate, sampleFormat format);

class Mixer {
 public:
   Mixer(int numChannels, int bufferSize, bool interleaved,
         double rate, sampleFormat format);
   virtual ~ Mixer();

   void UseVolumeSlider(ControlToolBar * c);
   void Clear();
   void MixLeft(WaveTrack * src, double t0, double t1);
   void MixRight(WaveTrack * src, double t0, double t1);
   void MixMono(WaveTrack * src, double t0, double t1);
   void Mix(int *channelFlags, WaveTrack * src, double t0, double t1);

   // Interleaved
   samplePtr GetBuffer();

   // Non-interleaved
   samplePtr GetBuffer(int channel);

 private:
   void MixDiffRates(int *channelFlags, WaveTrack * src, double t0, double t1);
   void MixSameRate(int *channelFlags, WaveTrack * src, double t0, double t1);

 private:
   int             mNumChannels;
   int             mNumBuffers;
   int             mBufferSize;
   int             mInterleavedBufferSize;
   sampleFormat    mFormat;
   bool            mInterleaved;
   bool            mUseVolumeSlider;
   ControlToolBar *mControlToolBar;
   samplePtr      *mBuffer;
   double         *mEnvValues;
   double          mRate;

   samplePtr      mTemp;
   int            mTempBufferSize;
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include <wx/string.h>

#include "WaveTrack.h"

class APalette;
class DirManager;

bool QuickMix(TrackList * tracks, DirManager * dirManager, double rate);

class Mixer {
 public:
   Mixer(int numChannels, int bufferSize, bool interleaved, double rate);
   virtual ~ Mixer();

   void UseVolumeSlider(APalette * palette);

   void Clear();
   void MixLeft(WaveTrack * src, double t0, double t1);
   void MixRight(WaveTrack * src, double t0, double t1);
   void MixMono(WaveTrack * src, double t0, double t1);
   void Mix(int *channelFlags, WaveTrack * src, double t0, double t1);

   // Interleaved
   sampleType *GetBuffer();

   // Non-interleaved
   sampleType *GetBuffer(int channel);

 private:
   void GetSamples(WaveTrack *src, int s0, int slen);
   void MixDiffRates(int *channelFlags, WaveTrack * src, double t0, double t1);
   void MixSameRate(int *channelFlags, WaveTrack * src, double t0, double t1);

   int mNumChannels;
   int mNumBuffers;
   int mBufferSize;
   int mInterleavedBufferSize;
   bool mInterleaved;
   bool mUseVolumeSlider;
   APalette *mAPalette;
   sampleType **mBuffer;
   double *mEnvValues;
   double mRate;

   sampleType *mTemp;
   int mTempBufferSize;
};

#endif

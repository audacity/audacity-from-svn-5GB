/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>

#include "Mix.h"

#include "WaveTrack.h"
#include "DirManager.h"
#include "Envelope.h"
#include "APalette.h"

bool QuickMix(TrackList * tracks, DirManager * dirManager, double rate)
{
   WaveTrack **waveArray;
   VTrack *t;
   int numWaves = 0;
   int numLeft = 0;
   int numRight = 0;
   int numMono = 0;
   bool mono = false;
   int w;

   TrackListIterator iter(tracks);

   t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == VTrack::Wave) {
         numWaves++;
         switch (t->GetChannel()) {
         case VTrack::MonoChannel:
            numLeft++;
            numRight++;
            numMono++;
            break;
         case VTrack::LeftChannel:
            numLeft++;
            break;
         case VTrack::RightChannel:
            numRight++;
            break;
         }
      }
      t = iter.Next();
   }

   if (numWaves < 2) {
      wxMessageBox("First select two or more tracks to mix together");
      return false;
   }

   if (numLeft == 1 && numRight == 1 && numWaves == 2) {
      wxMessageBox
          ("Mix would have no effect.  If you want to mix a left and "
           "a right channel together, turn them both into mono "
           "channels first.");
      return false;
   }

   if (numMono == numWaves || numLeft == numWaves || numRight == numWaves)
      mono = true;

   double totalTime = 0.0;

   waveArray = new WaveTrack *[numWaves];
   w = 0;
   t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == VTrack::Wave) {
         waveArray[w++] = (WaveTrack *) t;
         if (t->GetMaxLen() > totalTime)
            totalTime = t->GetMaxLen();
      }
      t = iter.Next();
   }

   WaveTrack *mixLeft = new WaveTrack(dirManager);
   mixLeft->SetRate(rate);
   mixLeft->SetChannel(VTrack::MonoChannel);
   mixLeft->SetName("Mix");
   WaveTrack *mixRight = 0;
   if (!mono) {
      mixRight = new WaveTrack(dirManager);
      mixRight->SetRate(rate);
      mixRight->SetName("Mix");
      mixLeft->SetChannel(VTrack::LeftChannel);
      mixRight->SetChannel(VTrack::RightChannel);
      mixLeft->SetLinked(true);
   }

   int maxBlockLen = mixLeft->GetIdealBlockSize();
   double maxBlockTime = maxBlockLen / mixLeft->GetRate();

   Mixer *mixer = new Mixer(mono ? 1 : 2, maxBlockLen, false, rate);

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;

   double tt = 0.0;
   while (tt < totalTime) {

      double blockTime = maxBlockTime;
      if (tt + blockTime > totalTime)
         blockTime = totalTime - tt;
      int blockLen = int (blockTime * mixLeft->GetRate());

      mixer->Clear();

      for (int i = 0; i < numWaves; i++) {
         if (mono)
            mixer->MixMono(waveArray[i], tt, tt + blockTime);
         else {
            switch (waveArray[i]->GetChannel()) {
            case VTrack::LeftChannel:
               mixer->MixLeft(waveArray[i], tt, tt + blockTime);
               break;
            case VTrack::RightChannel:
               mixer->MixRight(waveArray[i], tt, tt + blockTime);
               break;
            case VTrack::MonoChannel:
               mixer->MixMono(waveArray[i], tt, tt + blockTime);
               break;
            }
         }
      }

      if (mono) {
         sampleType *buffer = mixer->GetBuffer();
         mixLeft->Append(buffer, blockLen);
      } else {
         sampleType *buffer;
         buffer = mixer->GetBuffer(0);
         mixLeft->Append(buffer, blockLen);
         buffer = mixer->GetBuffer(1);
         mixRight->Append(buffer, blockLen);
      }

      tt += blockTime;

      if (!progress && wxGetElapsedTime(false) > 500) {
         progress =
             new wxProgressDialog("Quick Mix", "Mixing tracks", 1000);
      }
      if (progress) {
         int progressvalue = int (1000 * (tt / totalTime));
         progress->Update(progressvalue);
      }
   }

   tracks->Add(mixLeft);
   if (!mono)
      tracks->Add(mixRight);

   delete progress;

   int elapsedMS = wxGetElapsedTime();
   double elapsedTime = elapsedMS * 0.001;
   double maxTracks = totalTime / (elapsedTime / numWaves);

#ifdef __WXGTK__
   printf("      Tracks: %d\n", numWaves);
   printf("  Mix length: %f sec\n", totalTime);
   printf("Elapsed time: %f sec\n", elapsedTime);
   printf("Max number of tracks to mix in real time: %f\n", maxTracks);
#endif

   delete waveArray;
   delete mixer;

   return true;
}

Mixer::Mixer(int numChannels, int bufferSize, bool interleaved, double rate)
{
   mNumChannels = numChannels;
   mBufferSize = bufferSize;
   mInterleaved = interleaved;
   mRate = rate;
   mUseVolumeSlider = false;
   mAPalette = NULL;

   if (mInterleaved) {
      mNumBuffers = 1;
      mInterleavedBufferSize = mBufferSize * mNumChannels;
   } else {
      mNumBuffers = mNumChannels;
      mInterleavedBufferSize = mBufferSize;
   }
   
   mBuffer = new sampleType *[mNumBuffers];
   for (int c = 0; c < mNumBuffers; c++)
      mBuffer[c] = new sampleType[mInterleavedBufferSize];
   mTempBufferSize = mBufferSize*2;
   mTemp = new sampleType[mTempBufferSize];
   mEnvValues = new double[mBufferSize];
}

Mixer::~Mixer()
{
   for (int c = 0; c < mNumBuffers; c++)
      delete[]mBuffer[c];
   delete[]mBuffer;
   delete[]mTemp;
   delete[]mEnvValues;
}

void Mixer::UseVolumeSlider(APalette * palette)
{
   mUseVolumeSlider = true;
   mAPalette = palette;
}

void Mixer::Clear()
{
   for (int c = 0; c < mNumBuffers; c++)
      memset(mBuffer[c], 0, mInterleavedBufferSize * sizeof(sampleType));
}

void Mixer::MixLeft(WaveTrack * src, double t0, double t1)
{
   int *flags = new int[mNumChannels];
   for (int c = 0; c < mNumChannels; c++)
      flags[c] = (c == 0);
   Mix(flags, src, t0, t1);
   delete flags;
}

void Mixer::MixRight(WaveTrack * src, double t0, double t1)
{
   int *flags = new int[mNumChannels];
   for (int c = 0; c < mNumChannels; c++)
      flags[c] = (c == 1);
   Mix(flags, src, t0, t1);
   delete flags;
}

void Mixer::MixMono(WaveTrack * src, double t0, double t1)
{
   int *flags = new int[mNumChannels];
   for (int c = 0; c < mNumChannels; c++)
      flags[c] = 1;
   Mix(flags, src, t0, t1);
   delete flags;
}

void Mixer::GetSamples(WaveTrack *src, int s0, int slen)
{
   // Retrieves samples from a track, even outside of the range which
   // contains samples.  (Fills in extra space with zeros.)
   // Puts samples in mTemp
   
   if (slen > mTempBufferSize) {
      mTempBufferSize = slen;
      delete[] mTemp;
      mTemp = new sampleType[mTempBufferSize];
   }
   
   int soffset = 0;
   int getlen = slen;
   if (s0 < 0) {
      soffset = -s0;
      getlen -= soffset;
      s0 = 0;
   }
   if (s0+getlen > src->GetNumSamples()) {
      getlen = src->GetNumSamples() - s0;
   }
   
   src->Get(&mTemp[soffset], (sampleCount)s0, (sampleCount)getlen);
   
   int i;
   for(i=0; i<soffset; i++)
      mTemp[i] = 0;
   for(i=soffset+getlen; i<slen; i++)
      mTemp[i] = 0;
}

void Mixer::MixDiffRates(int *channelFlags, WaveTrack * src, double t0, double t1)
{
   if ((t0 - src->GetOffset()) >= src->GetNumSamples() / src->GetRate() ||
       (t1 - src->GetOffset()) <= 0)
      return;
      
   int s0 = int ((t0 - src->GetOffset()) * src->GetRate());
   int slen = int ((t1 - t0) * src->GetRate()) + 2;  // get a couple more samples than we need
   int destlen = int ((t1 - t0) * mRate + 0.5);
   int frac = int(32768.0 * (t0 - s0/src->GetRate()));
   int fracstep = int(32768.0 * src->GetRate()/mRate + 0.5);

   GetSamples(src, s0, slen);

   double volume;
   if (mUseVolumeSlider)
      volume = mAPalette->GetSoundVol();
   else
      volume = 1.0;

   Envelope *e = src->GetEnvelope();

   e->GetValues(mEnvValues, mBufferSize, t0, 1.0 / mRate);

   // Mix it down to the appropriate tracks

   for (int c = 0; c < mNumChannels; c++) {
      if (!channelFlags[c])
         continue;

      sampleType *dest;
      int skip;

      if (mInterleaved) {
         dest = &mBuffer[0][c];
         skip = mNumChannels;
      } else {
         dest = &mBuffer[c][0];
         skip = 1;
      }

      // This is the mixing inner loop, which we want
      // as optimized as possible

      int i = 0;
      for (int j = 0; j < destlen; j++) {
         sampleType value = (mTemp[i]*(32768-frac) + mTemp[i+1]*frac) >> 15;
         frac += fracstep;
         i += (frac >> 15);      // frac/32768
         frac = (frac & 0x7FFF); // frac%32768
      
         *dest += sampleType(value * volume * mEnvValues[j] + 0.5);
         dest += skip;
      }
   }
}

void Mixer::MixSameRate(int *channelFlags, WaveTrack * src, double t0, double t1)
{
   if ((t0 - src->GetOffset()) >= src->GetNumSamples() / src->GetRate() ||
       (t1 - src->GetOffset()) <= 0)
      return;
      
   int s0 = int ((t0 - src->GetOffset()) * src->GetRate() + 0.5);
   int s1 = int ((t1 - src->GetOffset()) * src->GetRate() + 0.5);

   int slen = s1 - s0;

   if (slen <= 0)
      return;
   if (slen > mBufferSize)
      slen = mBufferSize;

   GetSamples(src, s0, slen);

   double volume;
   if (mUseVolumeSlider)
      volume = mAPalette->GetSoundVol();
   else
      volume = 1.0;

   Envelope *e = src->GetEnvelope();

   e->GetValues(mEnvValues, slen, t0, 1.0 / mRate);

   // Mix it down to the appropriate tracks

   for (int c = 0; c < mNumChannels; c++) {
      if (!channelFlags[c])
         continue;

      sampleType *dest;
      int skip;

      if (mInterleaved) {
         dest = &mBuffer[0][c];
         skip = mNumChannels;
      } else {
         dest = &mBuffer[c][0];
         skip = 1;
      }

      // This is the mixing inner loop, which we want
      // as optimized as possible

      for (int j = 0; j < slen; j++) {
         *dest += sampleType(mTemp[j] * volume * mEnvValues[j] + 0.5);
         dest += skip;
      }
   }
}

void Mixer::Mix(int *channelFlags, WaveTrack * src, double t0, double t1)
{
   if (src->GetRate() - mRate >= 0.5 || src->GetRate() - mRate <= -0.5)
      MixDiffRates(channelFlags, src, t0, t1);
   else
      MixSameRate(channelFlags, src, t0, t1);
}

sampleType *Mixer::GetBuffer()
{
   return mBuffer[0];
}

sampleType *Mixer::GetBuffer(int channel)
{
   return mBuffer[channel];
}

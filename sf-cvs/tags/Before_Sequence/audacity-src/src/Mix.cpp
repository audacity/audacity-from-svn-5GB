/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.cpp

  Dominic Mazzoni

**********************************************************************/

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
#include "ControlToolBar.h"

#define rint(x)   (floor((x)+0.5f)) 


bool QuickMix(TrackList *tracks, DirManager *dirManager,
              double rate, sampleFormat format)
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
   mixLeft->SetSampleFormat(format);
   mixLeft->SetRate(rate);
   mixLeft->SetChannel(VTrack::MonoChannel);
   mixLeft->SetName(_("Mix"));
   WaveTrack *mixRight = 0;
   if (!mono) {
      mixRight = new WaveTrack(dirManager);
      mixRight->SetSampleFormat(format);
      mixRight->SetRate(rate);
      mixRight->SetName(_("Mix"));
      mixLeft->SetChannel(VTrack::LeftChannel);
      mixRight->SetChannel(VTrack::RightChannel);
      mixLeft->SetLinked(true);
   }

   int maxBlockLen = mixLeft->GetIdealBlockSize();
   double maxBlockTime = maxBlockLen / mixLeft->GetRate();

   Mixer *mixer = new Mixer(mono ? 1 : 2, maxBlockLen, false,
                            rate, format);

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
         samplePtr buffer = mixer->GetBuffer();
         mixLeft->Append(buffer, format, blockLen);
      } else {
         samplePtr buffer;
         buffer = mixer->GetBuffer(0);
         mixLeft->Append(buffer, format, blockLen);
         buffer = mixer->GetBuffer(1);
         mixRight->Append(buffer, format, blockLen);
      }

      tt += blockTime;

      if (!progress && wxGetElapsedTime(false) > 500) {
         progress =
             new wxProgressDialog(_("Quick Mix"), _("Mixing tracks"), 1000);
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
   printf(_("      Tracks: %d\n"), numWaves);
   printf(_("  Mix length: %f sec\n"), totalTime);
   printf(_("Elapsed time: %f sec\n"), elapsedTime);
   printf(_("Max number of tracks to mix in real time: %f\n"), maxTracks);
#endif

   delete waveArray;
   delete mixer;

   return true;
}

Mixer::Mixer(int numChannels, int bufferSize, bool interleaved,
             double rate, sampleFormat format)
{
   mNumChannels = numChannels;
   mBufferSize = bufferSize;
   mInterleaved = interleaved;
   mRate = rate;
   mFormat = format;
   mUseVolumeSlider = false;
   mControlToolBar= NULL;

   if (mInterleaved) {
      mNumBuffers = 1;
      mInterleavedBufferSize = mBufferSize * mNumChannels;
   } else {
      mNumBuffers = mNumChannels;
      mInterleavedBufferSize = mBufferSize;
   }
   
   mBuffer = new samplePtr[mNumBuffers];
   for (int c = 0; c < mNumBuffers; c++)
      mBuffer[c] = NewSamples(mInterleavedBufferSize, mFormat);
   mTempBufferSize = mBufferSize*2;
   mTemp = NewSamples(mTempBufferSize, mFormat);
   mEnvValues = new double[mBufferSize];
}

Mixer::~Mixer()
{
   for (int c = 0; c < mNumBuffers; c++)
      DeleteSamples(mBuffer[c]);
   delete[]mBuffer;
   DeleteSamples(mTemp);
   delete[]mEnvValues;
}

void Mixer::UseVolumeSlider(ControlToolBar * ctb)
{
   mUseVolumeSlider = true;
   mControlToolBar = ctb;
}

void Mixer::Clear()
{
   for (int c = 0; c < mNumBuffers; c++)
      memset(mBuffer[c], 0, mInterleavedBufferSize * SAMPLE_SIZE(mFormat));
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
      DeleteSamples(mTemp);
      mTemp = NewSamples(mTempBufferSize, mFormat);
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
   
   src->Get(mTemp + soffset*SAMPLE_SIZE(mFormat), mFormat,
            (sampleCount)s0, (sampleCount)getlen);

   ClearSamples(mTemp, mFormat, 0, soffset);
   ClearSamples(mTemp, mFormat, soffset+getlen, slen-(soffset+getlen));
}

void Mixer::MixDiffRates(int *channelFlags, WaveTrack * src,
                         double t0, double t1)
{
   if ((t0 - src->GetOffset()) >= src->GetNumSamples() / src->GetRate() ||
       (t1 - src->GetOffset()) <= 0)
      return;

   int s0 = int ((t0 - src->GetOffset()) * src->GetRate());

   // get a couple more samples than we need
   int slen = int ((t1 - t0) * src->GetRate()) + 2;

   int destlen = int ((t1 - t0) * mRate + 0.5);

   GetSamples(src, s0, slen);

   double volume;
   if (mUseVolumeSlider)
      volume = mControlToolBar->GetSoundVol();
   else
      volume = 1.0;

   Envelope *e = src->GetEnvelope();

   e->GetValues(mEnvValues, mBufferSize, t0, 1.0 / mRate);

   // Mix it down to the appropriate tracks

   for (int c = 0; c < mNumChannels; c++) {
      if (!channelFlags[c])
         continue;

      samplePtr destPtr;
      int skip;

      if (mInterleaved) {
         destPtr = mBuffer[0] + c*SAMPLE_SIZE(mFormat);
         skip = mNumChannels;
      } else {
         destPtr = mBuffer[c];
         skip = 1;
      }

      // This is the mixing inner loop, which we want
      // as optimized as possible

      int i = 0;
      switch(mFormat) {
      case int16Sample: {
         short *temp = (short *)mTemp;
         short *dest = (short *)destPtr;
         int frac = int(32768.0 * (t0 - s0/src->GetRate()));
         int fracstep = int(32768.0 * src->GetRate()/mRate + 0.5);

         for (int j = 0; j < destlen; j++) {
            short value = (temp[i]*(32768-frac) + temp[i+1]*frac) >> 15;
            frac += fracstep;
            i += (frac >> 15);      // frac/32768
            frac = (frac & 0x7FFF); // frac%32768
            
            *dest += short(value * volume * mEnvValues[j] + 0.5);
            dest += skip;
         }
      } break;
      case int24Sample: {
         int *temp = (int *)mTemp;
         int *dest = (int *)destPtr;
         float frac = t0 - s0/src->GetRate();
         float fracstep = src->GetRate()/mRate;

         for (int j = 0; j < destlen; j++) {
            float value = (temp[i]*(1.0-frac) + temp[i+1]*frac);
            frac += fracstep;
            int integerPart = (int)frac;
            i += integerPart;
            frac -= (float)integerPart;
            
            *dest += int(value * volume * mEnvValues[j] + 0.5);
            dest += skip;
         }
      } break;
      case floatSample: {
         float *temp = (float *)mTemp;
         float *dest = (float *)destPtr;
         float frac = t0 - s0/src->GetRate();
         float fracstep = src->GetRate()/mRate;

         for (int j = 0; j < destlen; j++) {
            float value = temp[i]*(1.0-frac) + temp[i+1]*frac;
            frac += fracstep;
            int integerPart = (int)frac;
            i += integerPart;
            frac -= (float)integerPart;
            
            *dest += value * volume * mEnvValues[j];
            dest += skip;
         }
      } break;
      } // switch

   }
}

void Mixer::MixSameRate(int *channelFlags, WaveTrack * src,
                        double t0, double t1)
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
      volume = mControlToolBar->GetSoundVol();
   else
      volume = 1.0;

   Envelope *e = src->GetEnvelope();

   e->GetValues(mEnvValues, slen, t0, 1.0 / mRate);

   // Mix it down to the appropriate tracks

   for (int c = 0; c < mNumChannels; c++) {
      if (!channelFlags[c])
         continue;

      samplePtr destPtr;
      int skip;

      if (mInterleaved) {
         destPtr = mBuffer[0] + c*SAMPLE_SIZE(mFormat);
         skip = mNumChannels;
      } else {
         destPtr = mBuffer[c];
         skip = 1;
      }

      // This is the mixing inner loop, which we want
      // as optimized as possible

      switch(mFormat) {
      case int16Sample: {
         short *dest = (short *)destPtr;
         short *temp = (short *)mTemp;
         for (int j = 0; j < slen; j++) {
            *dest += (short)rint(temp[j] * volume * mEnvValues[j]);
            dest += skip;
         }
      } break;
      case int24Sample: {
         int *dest = (int *)destPtr;
         int *temp = (int *)mTemp;
         for (int j = 0; j < slen; j++) {
            *dest += (int)rint(temp[j] * volume * mEnvValues[j]);
            dest += skip;
         }
      } break;
      case floatSample: {
         float *dest = (float *)destPtr;
         float *temp = (float *)mTemp;
         for (int j = 0; j < slen; j++) {
            *dest += temp[j] * volume * mEnvValues[j];
            dest += skip;
         }
      } break;
      } // switch
   }
}

void Mixer::Mix(int *channelFlags, WaveTrack * src, double t0, double t1)
{
   if (src->GetRate() - mRate >= 0.5 || src->GetRate() - mRate <= -0.5)
      MixDiffRates(channelFlags, src, t0, t1);
   else
      MixSameRate(channelFlags, src, t0, t1);
}

samplePtr Mixer::GetBuffer()
{
   return mBuffer[0];
}

samplePtr Mixer::GetBuffer(int channel)
{
   return mBuffer[channel];
}

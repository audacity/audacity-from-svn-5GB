/**********************************************************************

  Audacity: A Digital Audio Editor

  Filter.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/timer.h>

#include "Mix.h"

#include "WaveTrack.h"
#include "DirManager.h"
#include "Envelope.h"
#include "APalette.h"

bool QuickMix(TrackList *tracks, DirManager *dirManager)
{
  WaveTrack **waveArray;
  VTrack *t;
  int numWaves = 0;
  int numLeft = 0;
  int numRight = 0;
  int numMono = 0;
  bool mono = false;
  int w;

  t = tracks->First();
  while(t) {
    if (t->selected && t->GetKind() == VTrack::Wave) {
      numWaves++;
	  switch(t->channel) {
	  case VTrack::MonoChannel: numLeft++; numRight++; numMono++; break;
	  case VTrack::LeftChannel: numLeft++; break;
	  case VTrack::RightChannel: numRight++; break;
	  }
	}
    t = tracks->Next();
  }
  
  if (numWaves < 2) {
	wxMessageBox("First select two or more tracks to mix together");
	return false;
  }
  
  if (numLeft == 1 && numRight == 1 && numWaves==2) {
	wxMessageBox("Mix would have no effect.  If you want to mix a left and "
                 "a right channel together, turn them both into mono "
				 "channels first.");
	return false;
  }

  if (numMono == numWaves || numLeft == numWaves || numRight == numWaves)
	mono = true;

  double totalTime = 0.0;

  waveArray = new WaveTrack*[numWaves];
  w = 0;
  t = tracks->First();
  while(t) {
    if (t->selected && t->GetKind() == VTrack::Wave) {
      waveArray[w++] = (WaveTrack *)t;
	  if (t->GetMaxLen() > totalTime)
		totalTime = t->GetMaxLen();
	}
    t = tracks->Next();
  }

  WaveTrack *mixLeft = new WaveTrack(dirManager);
  mixLeft->rate = waveArray[0]->rate;
  WaveTrack *mixRight = 0;
  if (!mono) {
	mixRight = new WaveTrack(dirManager);
	mixRight->rate = waveArray[0]->rate;
	mixLeft->channel = VTrack::LeftChannel;
	mixRight->channel = VTrack::RightChannel;
  }

  int maxBlockLen = mixLeft->GetIdealBlockSize();
  double maxBlockTime = maxBlockLen / mixLeft->rate;

  Mixer *mixer = new Mixer(mono? 1: 2, maxBlockLen, false);

  wxProgressDialog *progress = NULL;  
  wxYield();
  wxStartTimer();
  wxBusyCursor busy;

  double tt = 0.0;
  while(tt < totalTime) {

	double blockTime = maxBlockTime;
	if (tt+blockTime > totalTime)
	  blockTime = totalTime - tt;
	int blockLen = int(blockTime * mixLeft->rate);

	mixer->Clear();

    for(int i=0; i<numWaves; i++) {
	  if (mono)
		mixer->MixMono(waveArray[i], tt, tt+blockTime);
	  else {
		switch(waveArray[i]->channel) {
		case VTrack::LeftChannel:
		  mixer->MixLeft(waveArray[i], tt, tt+blockTime);
		  break;
		case VTrack::RightChannel:
		  mixer->MixRight(waveArray[i], tt, tt+blockTime);
		  break;
		case VTrack::MonoChannel:
		  mixer->MixMono(waveArray[i], tt, tt+blockTime);
		  break;
	    }
	  }
    }

	if (mono) {
	  sampleType *buffer = mixer->GetBuffer();
	  mixLeft->Append(buffer, blockLen);
	}
	else {
	  sampleType *buffer;
	  buffer = mixer->GetBuffer(0);
	  mixLeft->Append(buffer, blockLen);
	  buffer = mixer->GetBuffer(1);
	  mixRight->Append(buffer, blockLen);
	}

	tt += blockTime;

	if (!progress && wxGetElapsedTime(false) > 500) {
	  progress =
		new wxProgressDialog("Quick Mix","Mixing tracks",
							 1000);
	}	
	if (progress) {
	  int progressvalue = int(1000*(tt/totalTime));
	  progress->Update(progressvalue);
	}
  }

  tracks->Add(mixLeft);
  if (!mono)
	tracks->Add(mixRight);

  delete progress;

  int elapsedMS = wxGetElapsedTime();
  double elapsedTime = elapsedMS * 0.001;
  double maxTracks = totalTime / (elapsedTime/numWaves);

  #ifdef __WXGTK__
  printf("      Tracks: %d\n",numWaves);
  printf("  Mix length: %lf sec\n",totalTime);
  printf("Elapsed time: %lf sec\n",elapsedTime);
  printf("Max number of tracks to mix in real time: %lf\n",maxTracks);
  #endif

  delete waveArray;
  delete mixer;

  return true;
}

Mixer::Mixer(int numChannels, int bufferSize, bool interleaved)
{
  mNumChannels = numChannels;
  mBufferSize = bufferSize;
  mInterleaved = interleaved;
  mUseVolumeSlider = false;

  if (mInterleaved) {
	mNumBuffers = 1;
	mInterleavedBufferSize = mBufferSize * mNumChannels;
  }
  else {
	mNumBuffers = mNumChannels;
	mInterleavedBufferSize = mBufferSize;
  }

  mBuffer = new sampleType*[mNumBuffers];
  for(int c=0; c<mNumBuffers; c++)
	mBuffer[c] = new sampleType[mInterleavedBufferSize];
  mTemp = new sampleType[mBufferSize];
}

Mixer::~Mixer()
{
  for(int c=0; c<mNumBuffers; c++)
	delete[] mBuffer[c];
  delete[] mBuffer;
  delete[] mTemp;
}

void Mixer::UseVolumeSlider(bool yes)
{
  mUseVolumeSlider = yes;
}

void Mixer::Clear()
{
  for(int c=0; c<mNumBuffers; c++)
	memset(mBuffer[c], 0, mInterleavedBufferSize * sizeof(sampleType));
}

void Mixer::MixLeft(WaveTrack *src, double t0, double t1)
{
  int *flags = new int[mNumChannels];
  for(int c=0; c<mNumChannels; c++)
	flags[c] = (c==0);
  Mix(flags, src, t0, t1);
  delete flags;
}

void Mixer::MixRight(WaveTrack *src, double t0, double t1)
{
  int *flags = new int[mNumChannels];
  for(int c=0; c<mNumChannels; c++)
	flags[c] = (c==1);
  Mix(flags, src, t0, t1);
  delete flags;
}

void Mixer::MixMono(WaveTrack *src, double t0, double t1)
{
  int *flags = new int[mNumChannels];
  for(int c=0; c<mNumChannels; c++)
	flags[c] = 1;
  Mix(flags, src, t0, t1);
  delete flags;
}

void Mixer::Mix(int *channelFlags, WaveTrack *src, double t0, double t1)
{
  // First get the samples

  if ((t0-src->tOffset) >= src->numSamples/src->rate ||
	  (t1-src->tOffset) <= 0)
	return;
  
  int s0 = int((t0-src->tOffset)*src->rate);
  int s1 = int((t1-src->tOffset)*src->rate);

  int slen = s1 - s0;
  int soffset = 0;

  if (s0 < 0) {
	soffset = -s0;
	slen -= soffset;
	s0 = 0;
  }
  if (s1 > (int)src->numSamples) {
	slen -= (s1 - src->numSamples);
	s1 = src->numSamples;
  }

  // Sometimes the length of the data we want to grab
  // ends up one sample too big, so we truncate it
  if (soffset+slen >= mBufferSize)
	slen = (mBufferSize - soffset);

  if (slen <= 0)
	return;
  
  src->Get(&mTemp[soffset], (sampleCount)s0, (sampleCount)slen);

  // Apply the envelope and volume

  t0 = s0/src->rate + src->tOffset;
  t1 = s1/src->rate + src->tOffset;
	
  double volume;
  if (mUseVolumeSlider)
	volume = gAPalette->GetSoundVol();
  else
	volume = 1.0;

  Envelope *e = src->GetEnvelope();
  if (e->IsLinearInRegion(t0, t1)) {
	double leftVal = e->GetValue(t0) * volume;
	double rightVal = e->GetValue(t1) * volume;

	double val = leftVal;
	double step = (rightVal-leftVal)/slen;
	sampleType *buffer = &mTemp[soffset];
	for(int i=0; i<slen; i++) {
	  buffer[i] = sampleType(buffer[i]*val);
	  val += step;
	}
  }
  else {
	// The envelope changes within this region, so we
	// do things the slow way, calculating the value of
	// the envelope at each pixel

	double t = t0;
	double tstep = 1.0 / src->rate;
	for(int i=0; i<slen; i++) {
	  mTemp[soffset+i] =
		sampleType(mTemp[soffset+i]*volume*e->GetValue(t));
	  t += tstep;
	}
  }
	
  // Then mix it down to the appropriate tracks

  for(int c=0; c<mNumChannels; c++) {
	if (!channelFlags[c])
	  continue;

	sampleType *src = &mTemp[soffset];
	sampleType *dest;
	int skip;

	if (mInterleaved) {
	  dest = &mBuffer[0][(mNumChannels*soffset) + c];
	  skip = mNumChannels;
	}
	else {
	  dest = &mBuffer[c][soffset];
	  skip = 1;
	}

	// This is the mixing inner loop, which we want
	// as optimized as possible

	for(int j=0; j<slen; j++) {
	  *dest += mTemp[j];
	  dest += skip;
	}
  }
}
  
  
sampleType *Mixer::GetBuffer()
{
  return mBuffer[0];
}

sampleType *Mixer::GetBuffer(int channel)
{
  return mBuffer[channel];
}



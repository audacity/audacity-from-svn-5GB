/**********************************************************************

  Audacity: A Digital Audio Editor

  Pitch.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>
#include "Pitch.h"

#include "WaveTrack.h"
#include "NoteTrack.h"
#include "AutoCorrelate.h"
#include "Landmark.h"
#include "Peak.h"

double Pitch2Freq(int pitch)
{
  return (440.0 * pow(2.0, ((pitch - 69) / 12.0)));
}

int Freq2Pitch(double freq)
{
  return (int)(69.5 + 12.0 * (log(freq/440.0)/log(2.0)));
}

NoteTrack *PitchExtract(WaveTrack *wave, DirManager *dirManager)
{
  NoteTrack *note = new NoteTrack(dirManager);

  int windowSize = 2048;
  int windowStep = 1024;

  int lowPitch = 53;
  int highPitch = 79;
  
  int flats = 1; // flat key vs. sharp key

  double lowFreq = Pitch2Freq(lowPitch-1);
  double highFreq = Pitch2Freq(highPitch+1);
  
  int lowSamples = (int)(wave->rate / highFreq);
  int highSamples = (int)(wave->rate / lowFreq);
  
  if (lowSamples < 1)
    lowSamples = 1;
  
  if (highSamples >= windowSize)
    highSamples = windowSize - 1;

  int start = 0;
  int lastPitch = 0;
  int changed = 1;

  double *correlated = new double[windowSize];
  wxASSERT(correlated);

  double *window = new double[windowSize];
  wxASSERT(window);

  sampleType *windowRaw = new sampleType[windowSize];
  wxASSERT(windowRaw);

  while(start+windowSize < wave->numSamples) {
    wave->Get(windowRaw, start, windowSize);
    for(int s=0; s<windowSize; s++)
      window[s] = windowRaw[s]/32768.0;

    ::AutoCorrelate(windowSize, window, correlated);
	
    int peak = FirstPeak(correlated, lowSamples, highSamples);

    if (peak>0) {
      double peakFreq = wave->rate / peak;
      int peakPitch = Freq2Pitch(peakFreq);
	  
      if (peakPitch != lastPitch)
	changed = 1;

      if (peakPitch == lastPitch && changed) {
	changed = 0;
      }

      if (peakPitch >= lowPitch && peakPitch == lastPitch) {
	note->Add(start, windowStep, peakPitch);
      }

      lastPitch = peakPitch;

      /*
	int lmp = GetLandmarkPeriod(windowSize, window);
	if (lmp) {
	  double lmpFreq = wave->rate / lmp;
	  int lmpPitch = Freq2Pitch(lmpFreq);
	  
	  if (lmpPitch >= lowPitch && lmpPitch==lastLmpPitch)
	    wave3->sample[start/windowStep] = lmpPitch;
	  
	  lastLmpPitch = lmpPitch;
	}
	else
	  lastLmpPitch = 0;
      */
	
    }

    start += windowStep;
  }

  delete[] correlated;
  delete[] window;
  delete[] windowRaw;

  return note;
}

char gWaveformPitchName[10];

char *PitchName(int pitch, int flats)
{
  char *p = gWaveformPitchName;

  switch(pitch%12) {
  case 0:
	*p++ = 'C'; break;
  case 1:
	if (flats) {*p++='D'; *p++='b';} else {*p++='C'; *p++='#';} break;
  case 2:
	*p++ = 'D'; break;
  case 3:
	if (flats) {*p++='E'; *p++='b';} else {*p++='D'; *p++='#';} break;
  case 4:
	*p++ = 'E'; break;
  case 5:
	*p++ = 'F'; break;
  case 6:
	if (flats) {*p++='G'; *p++='b';} else {*p++='F'; *p++='#';} break;
  case 7:
	*p++ = 'G'; break;
  case 8:
	if (flats) {*p++='A'; *p++='b';} else {*p++='G'; *p++='#';} break;
  case 9:
	*p++ = 'A'; break;
  case 10:
	if (flats) {*p++='B'; *p++='b';} else {*p++='A'; *p++='#';} break;
  case 11:
	*p++ = 'B'; break;
  }
  
  sprintf(p,"%d",(pitch/12)-1);

  return gWaveformPitchName;
}



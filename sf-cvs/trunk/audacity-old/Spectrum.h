/**********************************************************************

  Audacity: A Digital Audio Editor

  Spectrum.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM__
#define __AUDACITY_SPECTRUM__

#include "WaveTrack.h"

/*
  This function computes the power (mean square amplitude) as
  a function of frequency, for some block of audio data.

  width: the number of samples
  height: the desired number of frequencies

  The function returns false if it is not given enough data to
  compute an accurate spectrogram - this allows you to call it with
  a bigger window if possible.
*/

bool ComputeSpectrum(sampleType *data, int width, int height,
		     double rate, float *out);

#endif




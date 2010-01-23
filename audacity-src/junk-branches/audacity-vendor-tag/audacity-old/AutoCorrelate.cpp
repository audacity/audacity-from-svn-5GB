/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoCorrelate.cpp

  Dominic Mazzoni

  Perform AutoCorrelation of a set of data, placing results in a
  second array.

**********************************************************************/

#include <stdio.h>

#include "AutoCorrelate.h"
#include "Fourier.h"

void AutoCorrelate(int numSamples, double *sample, double *result)
{
  // FFT-based AutoCorrelation
  
  int i;
  double max;
  
  double *t1r = new double[numSamples];
  double *t1i = new double[numSamples];
  double *t2r = new double[numSamples];
  double *t2i = new double[numSamples];
  
  fft_double(numSamples, 0, sample, 0, t1r, t1i);
  
  for(i=0; i<numSamples; i++)
	t1r[i] = (t1r[i])*(t1r[i]) + (t1i[i])*(t1i[i]);
  
  fft_double(numSamples, 0, t1r, 0, t2r, t2i);
  
  max = 0.0;
  for(i=0; i<numSamples; i++)
	if (t2r[i] > max)
	  max = t2r[i];
  
  for(i=0; i<numSamples; i++)
	result[i] = t2r[i]/max;
  
  delete[] t1r;
  delete[] t1i;
  delete[] t2r;
  delete[] t2i;
}

/*

This is the slow way to do an AutoCorrelation, but it is possibly
easier to understand.  It produces exactly the same result as the
FFT-based algorithm, above.  Note that the AutoCorrelation is
defined here to wrap around after (numSamples-1), which is
probably not ideal, but it mimicks the FFT-based algorithm.

void AutoCorrelate(int numSamples, double *sample, double *result)
{
    // Slow naive AutoCorrelate

	int delta, x;
	double max = 0.0;
	for(delta = 0; delta < numSamples; delta++)
	{
		double c = 0.0;
		
		for(x = 0; x < numSamples; x++)
			c += sample[x] * sample[(x+delta)%numSamples];
		
		if (c > max)
			max = c;
		
		result[delta] = c;
	}
	
	// Normalize
	if (max != 0.0)
		for(delta = 0; delta < numSamples; delta++)
			result[delta] /= max;
	
}

*/


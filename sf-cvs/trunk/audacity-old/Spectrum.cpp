/**********************************************************************

  Audacity: A Digital Audio Editor

  Spectrum.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include "Spectrum.h"

#include "FFT.h"

#include "Prefs.h"

int GetSpectrumWindowSize()
{
  return gPrefs->Read("/Spectrum/FFTSize", 256);
}

bool ComputeSpectrum(sampleType *data, int width, int height,
					 double rate, float *grayscaleOut,
					 bool autocorrelation)
{
  int windowSize = GetSpectrumWindowSize();
  int windowFunc = 3;

  if (width < windowSize)
    return false;
  
  if (!data || !grayscaleOut)
    return true;
    
  float *processed = new float[windowSize];

  int i;
  for(i=0; i<windowSize; i++)
	processed[i] = 0.0;
  int half = windowSize/2;

  float *in = new float[windowSize];
  float *out = new float[windowSize];
  float *out2 = new float[windowSize];

  int start = 0;
  int windows = 0;
  while(start + windowSize <= width) {
	for(i=0; i<windowSize; i++)
	  in[i] = data[start+i]/32767.;
	
	WindowFunc(windowFunc, windowSize, in);

	if (autocorrelation) {
	  // Take FFT
	  FFT(windowSize, false, in, NULL, out, out2);

	  // Compute power
	  for(i=0; i<windowSize; i++)
		in[i] = (out[i]*out[i]) + (out2[i]*out2[i]);

	  // Tolonen and Karjalainen recommend taking the cube root
	  // of the power, instead of the square root

	  for(i=0; i<windowSize; i++)
		in[i] = pow(in[i], 1.0/3.0);

	  // Take FFT
	  FFT(windowSize, false, in, NULL, out, out2);

	  // Take real part of result
	  for(i=0; i<half; i++)
		processed[i] += out[i];
	}
	else {
	  PowerSpectrum(windowSize, in, out);

	  for(i=0; i<half; i++)
		processed[i] += out[i];
	}

	start += half;
	windows++;
  }

  int maxFreq = gPrefs->Read("/Spectrum/MaxFreq", 8000);
  int maxSamples = int(maxFreq * windowSize / rate + 0.5);
  if (maxSamples > half)
	maxSamples = half;

  if (autocorrelation) {
	maxSamples = half;

	// Peak Pruning as described by Tolonen and Karjalainen, 2000

	// Clip at zero, copy to temp array
	for(i=0; i<maxSamples; i++) {
	  if (processed[i] < 0.0)
		processed[i] = 0.0;
	  out[i] = processed[i];
	}

	// Subtract a time-doubled signal (linearly interp.) from the original
	// (clipped) signal
	for(i=0; i<maxSamples; i++)
	  if ((i%2)==0)
		processed[i] -= out[i/2];
	  else
		processed[i] -= ((out[i/2] + out[i/2+1]) / 2);

	// Clip at zero again
	for(i=0; i<maxSamples; i++)
	  if (processed[i] < 0.0)
		processed[i] = 0.0;

	// Find new max
	float max = 0;
	for(i=1; i<maxSamples; i++)
	  if (processed[i] > max)
		max = processed[i];

	// Reverse and scale
	for(i=0; i<maxSamples; i++)
	  in[i] = processed[i] / (windowSize/4);
	for(i=0; i<maxSamples; i++)
	  processed[maxSamples-1-i] = in[i];
  }
  else {
	// Convert to decibels
	for(i=0; i<maxSamples; i++)
	  processed[i] = 10*log10(processed[i]/windowSize/windows);
  }

  // Finally, put it into bins in grayscaleOut[], normalized to a 0.0-1.0 scale
  
  for(i=0; i<height; i++) {
    float bin0 = float(i)*maxSamples/height;
    float bin1 = float(i+1)*maxSamples/height;
    
    float binwidth = bin1-bin0;

    float value = 0.0;

	if (int(bin1) == int(bin0))
	  value = processed[int(bin0)];
	else {
	  value += processed[int(bin0)]*(int(bin0)+1-bin0);
	  bin0 = 1+int(bin0);
	  while(bin0 < int(bin1)) {
		value += processed[int(bin0)];
		bin0 += 1.0;
	  }
	  value += processed[int(bin1)]*(bin1-int(bin1));

	  value /= binwidth;
    }

	if (!autocorrelation) {
	  // Last step converts dB to a 0.0-1.0 range	  
	  value = (value+80.0)/80.0;
	}
	
	if (value > 1.0) value = 1.0;
	if (value < 0.0) value = 0.0;
    
    grayscaleOut[i] = value;
  }

  delete[] in;
  delete[] out;
  delete[] out2;
  delete[] processed;
  
  return true;
}

/*

// Bartlett Window (looks like a triangle)
#define WINDOW(j,a,b) (1.0-fabs((((j)-1)-(a))*(b)))

// Square
#ifndef SQR
#define SQR(A) ((A)*(A))
#endif

*/

/*
  This function computes the power (mean square amplitude) as
  a function of frequency, for some block of audio data.

  width: the number of samples
  height: the desired number of frequencies

  The function returns false if it is not given enough data to
  compute an accurate spectrogram - this allows you to call it with
  a bigger window if possible.

  Algorithm based on "Numerical Recipes in C"
*/

/*

bool ComputeSpectrum(sampleType *data, int width, int height,
		     double rate, float *out)
{
  if (height<16 || height>8192) return false;

  // m is the smallest power of two greater than or equal to height
  // We will take the Fourier transform of 2*m
  //
  // The number (2*m) comes up so often we store it in mm  

  int m=16;
  while(m<height)
    m = (m << 1);

  m = (m << 3);

  int mm = m+m;

  // Fail if we don't have enough data to do two complete
  // Fourier transforms

  if (width < mm*2)
    return false;

  // Otherwise we're okay - if this was just a test, return true
  
  if (data == 0)
    return true;

  // TODO: Fix bugs so these don't have to be extrasized

  float *p = new float[2*m];
  float *w = new float[2*m*4];
  float *real = new float[2*m*4];
  float *imag = new float[2*m*4];

  // k is the number of windows we are going to take (and then average
  // together)

  int k = width / (4*m);

  // Accumulate the squared sum of the weights

  int j;
  float facm = m;
  float facp = 1.0/m;
  float sumw=0.0;
  for(j=0; j<mm; j++)
    sumw += SQR(WINDOW(j,facm,facp));

  // Initialize the spectrum to zero

  for(j=0; j<m; j++)
    p[j] = 0.0;
  float den = 0;

  for(int kk=0; kk<k; kk++) {

    // Grab a segment
    
    for(j=0; j<mm; j++) {
      w[2*j] = (data[mm*kk+j])/32767.;
    }

    // Apply window

    for(j=0; j<mm; j++) {
      float wc = WINDOW(j,facm,facp);
      w[j] *= wc;
    }

    // Apply fourier transform

    FFT(mm,0,w,0,real,imag);

    for(j=0; j<mm; j++)
      w[j] = sqrt(SQR(real[j])+SQR(imag[j]));

    // Sum results into previous segments

    p[0] += SQR(w[0]);
    for(j=1; j<m; j++)
      p[j] += (SQR(w[j])+SQR(w[mm-j]));

    den += sumw;
  }
  den *= mm;

  // Normalize the output

  for(j=0; j<m; j++)
    p[j] /= den;

  // Copy into "out" buffer
  // TODO: Should grab all frequencies, averaging

  float max = 0.0;
  for(j=0; j<height; j++) {
    out[j] = p[j];
    if (fabs(out[j]) > max)
      max = fabs(out[j]);
  }

  for(j=0; j<height; j++)
    out[j] /= max;

  // Log scale:

  float min_freq = rate/mm;
  float max_freq = 4000.0;
  float ratio = max_freq / min_freq;
  float a = (log(ratio) / log(2.0)) / (height-1);

  max = 0.0;
  
  for(j=0; j<height-1; j++) {
    float freq0 = min_freq*pow(2.0,a*j);
    float freq1 = min_freq*pow(2.0,a*(j+1));
    float index0 = freq0 / min_freq;
    float index1 = freq1 / min_freq;
    float value;
    if (((int)index0) == ((int)index1)) {
      value = p[(int)index0]*(index1 - index0);
    } else {
      int i = (int)index0;
      value = p[i]*(i+1-index0);
      i++;
      while(i<(int)index1) {
	value += p[i];
	i++;
      }
      value += p[i] * (index1-i);
    }
    out[j] = value;
    if (value > max)
      max = value;
  }
  for(j=0; j<height-1; j++)
    out[j] /= max;

  delete[] p;
  delete[] w;
  delete[] real;
  delete[] imag;

  return true;
}

*/


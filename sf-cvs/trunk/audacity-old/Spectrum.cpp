/**********************************************************************

  Audacity: A Digital Audio Editor

  Spectrum.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include "Spectrum.h"

#include "FFT.h"

// Bartlett Window (looks like a triangle)
#define WINDOW(j,a,b) (1.0-fabs((((j)-1)-(a))*(b)))

// Square
#ifndef SQR
#define SQR(A) ((A)*(A))
#endif

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




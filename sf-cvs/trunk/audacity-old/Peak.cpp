/**********************************************************************

  Audacity: A Digital Audio Editor

  Peak.cpp

  - Routine for finding the first peak in a waveform

  Dominic Mazzoni

**********************************************************************/

#include "Peak.h"

const double minPeakHeight = 0.8;

// peakHeightThreshold:
//  Any point which is this fraction of the highest point in the
//  range is a candidate for a peak.
const double peakHeightThreshold = 0.9;

// peakPosThreshold:
// Any point which is no more than this fraction along the way
// to the peak is a candidate for being a better peak.
const double peakPosThreshold = 0.60;

// With these numbers, the first peak is defined as the leftmost point
// such that it is within 95% of the next-highest peak, but no point
// sufficiently to the left (60% of its x-position or leftwards) is
// 95% of this point's height.

int FirstPeak(double *sample, int start, int stop)
{
  int x;
  double peakHeight=0.0;
  int peak = -1;

  for(x=start; x<=stop; x++)
	if (sample[x] > peakHeight) {
	  peak = x;
	  peakHeight = sample[peak];
	}

  if (peakHeight < minPeakHeight)
	return -1;

  int again;
  double newPeakHeight;

  do {
	stop = (int)(peak*peakPosThreshold);

	newPeakHeight = 0.0;
	again = 0;

	for(x=start; x<=stop; x++)
	  if (sample[x] > peakHeight*peakHeightThreshold &&
		  sample[x] > newPeakHeight) {
		peak = x;
		newPeakHeight = sample[x];
		again = 1;
	  }
	
	if (again) {
	  peakHeight = newPeakHeight;
	}

  } while (again);
  
  return peak;
}


/*****************************************************************************
 *
 * General FIR digital filter routines with MMX optimization. 
 *
 * Note : MMX optimized functions reside in a separate, platform-specific file, 
 * e.g. 'mmx_win.cpp' or 'mmx_gcc.cpp'
 * 
 * Author        : Copyright (c) Olli Parviainen 2002
 * Author e-mail : oparviai@iki.fi
 * File created  : 13-Jan-2002
 * Last modified : 13-Jan-2002
 *
 * License :
 *
 *  This file is part of SoundTouch sound processing library.
 *
 *  SoundTouch is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  SoundTouch is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with SoundTouch; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *****************************************************************************/

#include <memory.h>
#include <math.h>
#include <stdlib.h>

#include "FIRFilter.h"
#include "mmx.h"


/*****************************************************************************
 *
 * Implementation of the class 'FIRFilter'
 *
 *****************************************************************************/

FIRFilter::FIRFilter()
{
    bMMX = mmxDetect();
    resultDivFactor = -1; // set these in inherited classes
    resultDivisor = 0;    // ditto
    length = 0;           // 
    lengthDiv8 = 0;       //
}

// Usual C-version of the filter routine for stereo sound
uint FIRFilter::evaluateFilterStereo(Sample *dest, const Sample *src, const uint numSamples) const
{
    uint i, j, end, pos;

    end = 2 * (numSamples - length);
    for (j = 0; j < end; j += 2) {
#if SAMPLES_ARE_SHORT
	    BiggerSample suml, sumr;
#endif
	    pos = j;
	    
#if SAMPLES_ARE_SHORT
	    suml = sumr = 0;
	    
	    for (i = 0; i < length; i ++) {
		    suml += src[pos + i] * filterCoeffs[i];
		    sumr += src[pos + i + 1] * filterCoeffs[i];
	    }
	    
	    suml >>= resultDivFactor;
	    sumr >>= resultDivFactor;
	    // saturate to 16 bit integer limits
	    suml = (suml < -32768) ? -32768 : (suml > 32767) ? 32767 : suml;
	    // saturate to 16 bit integer limits
	    sumr = (sumr < -32768) ? -32768 : (sumr > 32767) ? 32767 : sumr;
	    dest[j] = (Sample)suml;
	    dest[j + 1] = (Sample)sumr;
#else
	    for (i = 0; i < length; i ++) {
		    dest[j] += (src[pos + i] * filterCoeffs[i]) / resultDivisor;
		    dest[j+1] += (src[pos + i + 1] * filterCoeffs[i]) / resultDivisor;
	    }
#endif

    }

    return numSamples - length;
}

// Usual C-version of the filter routine for mono sound
uint FIRFilter::evaluateFilterMono(Sample *dest, const Sample *src, const uint numSamples) const
{
    uint i, j, end;

    end = numSamples - length;
    for (j = 0; j < end; j ++) {
#if SAMPLES_ARE_SHORT
	    BiggerSample sum;
	    sum = 0;
	    for (i = 0; i < length; i ++) {
		    sum += src[j + i] * filterCoeffs[i];
	    }
	    sum >>= resultDivFactor;
	    // saturate to 16 bit integer limits
	    sum = (sum < -32768) ? -32768 : (sum > 32767) ? 32767 : sum;
	    dest[j] = (Sample)sum;
#else 
	    for (i = 0; i < length; i ++) {
		    dest[j] += src[j + i] * filterCoeffs[i] / resultDivisor;
	    }
#endif		
    }
    return end;
}



// Sets number of FIR filter taps
void FIRFilter::setLength(const uint newLength)
{
    if (length < 8) length = 8;     // min length

    if (newLength > FIR_MAX_FILTER_LENGTH) {
        lengthDiv8 = FIR_MAX_FILTER_LENGTH / 8;
    } else {
        lengthDiv8 = newLength / 8;
    }
    length = lengthDiv8 * 8;
}



uint FIRFilter::getLength() const
{
    return length;
}



// Applies the filter to the given sequence of samples. 
//
// Note : The amount of outputted samples is by value of 'filter_length' 
// smaller than the amount of input samples.
uint FIRFilter::evaluate(Sample *dest, const Sample *src, const uint numSamples, const uint numChannels) const
{
    st_assert(numChannels == 1 || numChannels == 2);

    st_assert(length > 0);
    st_assert(lengthDiv8 * 8 == length);
    if (numSamples < length) return 0;
    st_assert(resultDivFactor >= 0);
    if (numChannels == 2) {
        if (bMMX) {
            return mmxEvaluateFilterStereo(dest, src, numSamples);
        } else {
            return evaluateFilterStereo(dest, src, numSamples);
        }
    } else {
        return evaluateFilterMono(dest, src, numSamples);
    }
}



void FIRFilter::calculateCoeffs()
{
    if (bMMX) {
        mmxCalculateCoeffs();
    }
}

/*****************************************************************************
 *
 * FIR low-pass (anti-alias) filter with filter coefficient design routine and
 * MMX optimization. 
 * 
 * Anti-alias filter is used to prevent folding of high frequencies when 
 * transposing the sample rate with interpolation.
 *
 * Author        : Copyright (c) Olli Parviainen 2002
 * Author e-mail : oparviai@iki.fi
 * File created  : 13-Jan-2002
 * Last modified : 29-Jan-2002
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

#include "AAFilter.h"

#define PI        3.141592655357989
#define TWOPI    (2 * PI)

/*****************************************************************************
 *
 * Implementation of the class 'AAFilter'
 *
 *****************************************************************************/

AAFilter::AAFilter(const uint length) :
    FIRFilter()
{
    cutoffFreq = 0.5;
    resultDivFactor = 14;    // divide result by 16384
    resultDivisor = 16384;   // for float work
    setLength(length);
}




// Sets new anti-alias filter cut-off edge frequency, scaled to
// sampling frequency (nyquist frequency = 0.5).
// The filter will cut frequencies higher than the given frequency.
void AAFilter::setCutoffFreq(const double newCutoffFreq)
{
    cutoffFreq = newCutoffFreq;
    calculateCoeffs();
}



// Sets number of FIR filter taps
void AAFilter::setLength(const uint newLength)
{
    FIRFilter::setLength(newLength);
    calculateCoeffs();
}



// Calculates coefficients for a low-pass FIR filter using Hamming window
void AAFilter::calculateCoeffs()
{
    uint i;
    double cntTemp, temp, tempCoeff,h, w;
    double fc2, wc;
    double scaleCoeff, sum;
    double *work;

    st_assert(length > 0);
    st_assert(length % 4 == 0);
    st_assert(cutoffFreq >= 0);
    st_assert(cutoffFreq <= 0.5);

    work = new double[length];

    fc2 = 2.0 * cutoffFreq; 
    wc = PI * fc2;
    tempCoeff = TWOPI / (double)length;

    sum = 0;
    for (i = 0; i < length; i ++) {
        cntTemp = (double)i - (double)(length / 2);

        temp = cntTemp * wc;
        if (temp != 0) {
            h = fc2 * sin(temp) / temp;                     // sinc function
        } else {
            h = 1.0;
        }
        w = 0.54 + 0.46 * cos(tempCoeff * cntTemp);       // hamming window

        temp = w * h;
        work[i] = temp;

        // calc net sum of coefficients 
        sum += temp;
    }

    // ensure the sum of coefficients is larger than zero
    st_assert(sum > 0);

    // ensure we've really designed a lowpass filter...
    st_assert(work[length/2] > 0);
    st_assert(work[length/2 + 1] > -1e-6);
    st_assert(work[length/2 - 1] > -1e-6);

    // Calculate a scaling coefficient in such a way that the result can be
    // divided by 16384
    scaleCoeff = 16384.0f / sum;

    for (i = 0; i < length; i ++) {
        // scale & round to nearest integer
        temp = work[i] * scaleCoeff;
        temp += (temp >= 0) ? 0.5 : -0.5;
        // ensure no overfloods
        st_assert(temp >= -32768 && temp <= 32767);
        filterCoeffs[i] = (short)temp;
    }

    delete[] work;
    // call inherited function to calc mmx coeffs if necessary
    FIRFilter::calculateCoeffs();
}

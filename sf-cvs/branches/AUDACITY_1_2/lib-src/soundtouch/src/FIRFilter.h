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


#ifndef FIRFilter_H
#define FIRFilter_H

#include "STTypes.h"
// Maximal length (# of coeffs) of the anti-alias filter
#define FIR_MAX_FILTER_LENGTH    128


class FIRFilter {
protected:
    // Number of FIR filter taps
    uint length;    
    // Number of FIR filter taps divided by 8
    uint lengthDiv8;

    // Result divider factor in 2^k format
    int resultDivFactor;
    // Result divisor for float work
    float resultDivisor;

    // are the mmx-routines being used
    bool bMMX;

    // Memory for filter coefficients
    Sample filterCoeffs[2 * FIR_MAX_FILTER_LENGTH];

    // Filter coefficients for MMX routines. Additional array is required as the
    // MMX coefficients are arranged in a different way to the ones used by the 
    // normal routine
    short filterCoeffsMMX[2 * FIR_MAX_FILTER_LENGTH];

    virtual void calculateCoeffs() = 0;
    void mmxCalculateCoeffs();
    void setLength(const uint newLength);
    uint evaluateFilterStereo(Sample *dest, const Sample *src, const uint numSamples) const;
    uint evaluateFilterMono(Sample *dest, const Sample *src, const uint numSamples) const;
    uint mmxEvaluateFilterStereo(Sample *dest, const Sample *src, const uint numSamples) const;

public:
    FIRFilter();

    // Applies the filter to the given sequence of samples. 
    // Note : The amount of outputted samples is by value of 'filter_length' 
    // smaller than the amount of input samples.
    uint evaluate(Sample *dest, const Sample *src, const uint numSamples, const uint numChannels) const;

    uint getLength() const;
};

#endif

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

#ifndef AAFilter_H
#define AAFilter_H

#include "FIRFilter.h"


class AAFilter : public FIRFilter {
protected:
    // Low-pass filter cut-off frequency, negative = invalid
    double cutoffFreq;

    // Calculate the FIR coefficients realizing the given cutoff-frequency
    virtual void calculateCoeffs();
public:
    AAFilter(const uint length);

    // Sets new anti-alias filter cut-off edge frequency, scaled to sampling 
    // frequency (nyquist frequency = 0.5). The filter will cut off the 
    // frequencies than that.
    void setCutoffFreq(const double newCutoffFreq);

    // Sets number of FIR filter taps, i.e. ~filter complexity
    void setLength(const uint newLength);
};

#endif

/*****************************************************************************
 * 
 * Sample rate transposer. Changes sample rate by using linear interpolation 
 * together with anti-alias filtering (first order interpolation with anti-
 * alias filtering should be quite adequate for this application)
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

#ifndef RateTransposer_H
#define RateTransposer_H

#include "AAFilter.h"
#include "FIFOSamplePipe.h"
#include "FIFOSampleBuffer.h"

#include "STTypes.h"

class RateTransposer : public FIFOProcessor
{
private:
    // Anti-alias filter object
    AAFilter *pAAFilter;

    int iSlopeCount;
    uint uRate;
    uint uChannels;
    Sample sPrevSampleL, sPrevSampleR;

    // Buffer for collecting samples to feed the anti-alias filter between
    // two batches
    FIFOSampleBuffer storeBuffer;

    // Buffer for keeping samples between transposing & anti-alias filter
    FIFOSampleBuffer tempBuffer;

    // Output sample buffer
    FIFOSampleBuffer outputBuffer;

    bool bUseAAFilter;

    void resetRegisters();

    uint transposeStereo(Sample *dest, const Sample *src, const uint numSamples);
    uint transposeMono(Sample *dest, const Sample *src, const uint numSamples);
    uint transpose(Sample *dest, const Sample *src, const uint numSamples);

//    void resizeStore(const uint newSizeSamples);
//    void resize_temp(const uint newSizeSamples);
//    void add_toStoreBuffer(const Sample *samples, const uint numSamples);
//    void take_fromStoreBuffer(Sample *output, const uint numSamples);
    void flushStoreBuffer();

//    void check_outbuff_size(const uint size_requirement);

    void downsample(const Sample *src, const uint numSamples);
    void upsample(const Sample *src, const uint numSamples);

    // Transposes sample rate by applying anti-alias filter to prevent folding. 
    // Returns amount of samples returned in the "dest" buffer.
    // The maximum amount of samples that can be returned at a time is set by
    // the 'set_returnBuffer_size' function.
    void processSamples(const Sample *src, const uint numSamples);

public:
    RateTransposer();
    virtual ~RateTransposer();

    // Returns the output buffer object
    FIFOSamplePipe *getOutput() { return &outputBuffer; };

    // Returns the store buffer object
    FIFOSamplePipe *getStore() { return &storeBuffer; };

    // Return anti-alias filter object
    AAFilter *getAAFilter() const;

    // Enables/disables the anti-alias filter. Zero to disable, nonzero to enable
    void enableAAFilter(const bool newMode);

    // Returns nonzero if anti-alias filter is enabled.
    bool isAAFilterEnabled() const;

    // Sets new target rate. Normal rate = 1.0, smaller values represent slower 
    // rate, larger faster rates.
    void setRate(const float newRate);

    // Sets the number of channels, 1 = mono, 2 = stereo
    void setChannels(const uint channels);

    // Adds 'numSamples' pcs of samples from the 'samples' memory position into
    // the input of the object.
    virtual void putSamples(const Sample *samples, const uint numSamples);

    // Clears all the samples in the object
    virtual void clear();

    // Returns nonzero if there aren't any samples available for outputting.
    virtual uint isEmpty();
};

#endif

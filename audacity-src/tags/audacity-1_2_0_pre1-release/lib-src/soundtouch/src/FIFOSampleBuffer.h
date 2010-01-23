/*****************************************************************************
 *
 * A buffer class for temporarily storaging sound samples, operates as a 
 * first-in-first-out pipe.
 *
 * Samples are added to the end of the sample buffer with the 'putSamples' 
 * function, and are received from the beginning of the buffer by calling
 * the 'receiveSamples' function. The class automatically removes the 
 * outputted samples from the buffer, as well as grows the buffer size 
 * whenever necessary.
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

#ifndef FIFOSampleBuffer_H
#define FIFOSampleBuffer_H

#include "STTypes.h"
#include "FIFOSamplePipe.h"

class FIFOSampleBuffer : public FIFOSamplePipe
{
private:
    Sample *buffer;
    uint sizeInBytes;
    uint samplesInBuffer;
    uint channels;
    uint bufferPos;

    void rewind();
    void ensureCapacity(const uint capacityRequirement);
    uint getCapacity() const;
 
public:

    // Constructor
    FIFOSampleBuffer(const uint numChannels = 2);

    // destructor
    virtual ~FIFOSampleBuffer();

    // Returns a pointer to the beginning of the currently non-outputted samples. 
    // This function is provided for accessing the output samples directly. 
    // Please be careful!
    //
    // When using this function to output samples, also remember to 'remove' the
    // outputted samples from the buffer by calling the 
    // 'receiveSamples(numSamples)' function
    virtual Sample *ptrBegin() const;

    // Returns a pointer to the end of the used part of the sample buffer (i.e. 
    // where the new samples are to be inserted). This function may be used for 
    // inserting new samples into the sample buffer directly. Please be careful! 
    //
    // Parameter 'slackCapacity' tells the function how much free capacity (in
    // terms of samples) there _at least_ should be, in order to the caller to
    // succesfully insert all the required samples to the buffer. When necessary, 
    // the function grows the buffer size to comply with this requirement.
    //
    // When using this function as means for inserting new samples, also remember 
    // to increase the sample count afterwards, by calling  the 
    // 'putSamples(numSamples)' function.
    Sample *ptrEnd(const uint slackCapacity);

    // Adds 'numSamples' pcs of samples from the 'samples' memory position to
    // the sample buffer.
    virtual void putSamples(const Sample *samples, const uint numSamples);

    // Increases the number of samples in the buffer without copying any actual
    // samples.
    //
    // This function is used to update the number of samples in the sample buffer
    // when accessing the buffer directly with 'ptrEnd' function. Please be 
    // careful though!
    virtual void putSamples(const uint numSamples);

    // Output samples from beginning of the sample buffer. Copies demanded number
    // of samples to output and removes them from the sample buffer. If there
    // are less than 'numsample' samples in the buffer, returns all available.
    //
    // Returns number of samples copied.
    virtual uint receiveSamples(Sample *output, const uint maxSamples);

    // Removes samples from the beginning of the sample buffer without copying them
    // anywhere. Used to reduce the number of samples in the buffer, when accessing
    // the sample buffer with the 'ptrBegin' function.
    virtual uint receiveSamples(const uint maxSamples);

    // Returns the number of samples currently available in the object for 
    // outputting.
    virtual uint numSamples() const;

    // Sets number of channels, 1 = mono, 2 = stereo
    void setChannels(const uint numChannels);

    // Returns nonzero if there aren't any samples available for outputting.
    virtual int isEmpty() const;

    // Clears all the samples in the object
    virtual void clear();
};

#endif

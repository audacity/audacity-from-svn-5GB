/*****************************************************************************
 *
 * 'FIFOSamplePipe' : An abstract base class for classes that manipulate sound
 * samples by operating like a first-in-first-out pipe: New samples are fed
 * into one end of the pipe with the 'putSamples' function, and the processed
 * samples are received from the other end with the 'receiveSamples' function.
 *
 * 'FIFOProcessor' : A base class for classes the do signal processing with 
 * the samples while operating like a first-in-first-out pipe. When samples
 * are inputted with the 'putSamples' function, the class processes them
 * and moves the processed samples to the given 'output' pipe object, which
 * may be either another processing stage, or a fifo sample buffer object.
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

#ifndef FIFOSamplePipe_H
#define FIFOSamplePipe_H

#include <stdlib.h>
#include "STTypes.h"

class FIFOSamplePipe
{
public:
    // Returns a pointer to the beginning of the currently non-outputted samples. 
    // This function is provided for accessing the output samples directly. 
    // Please be careful!
    //
    // When using this function to output samples, also remember to 'remove' the
    // outputted samples from the buffer by calling the 
    // 'receiveSamples(numsamples)' function
    virtual Sample *ptrBegin() const = 0;

    // Adds 'numSamples' pcs of samples from the 'samples' memory position into
    // the input of the object.
    virtual void putSamples(const Sample *samples, const uint numSamples) = 0;

    // Moves samples from the 'other' pipe object to this object by copying and
    // then removing all the samples from the 'other' object.
    void moveSamples(FIFOSamplePipe &other)
    {
        int oNumSamples = other.numSamples();

        putSamples(other.ptrBegin(), oNumSamples);
        other.receiveSamples(oNumSamples);
    };

    // Output samples from the object. Copies demanded number samples to output and 
    // removes them from the object's output buffer. If there are less than 'numsample' 
    // samples available, returns all the available samples.
    //
    // Returns number of samples copied.
    virtual uint receiveSamples(Sample *output, const uint maxSamples) = 0;

    // Removes samples from the output of the object copying them anywhere. 
    // Used to reduce the number of samples in the object, when accessing
    // the object's output buffer directly with the 'ptrBegin' function.
    virtual uint receiveSamples(const uint maxSamples) = 0;

    // Returns the number of samples currently available in the object for 
    // outputting.
    virtual uint numSamples() const = 0;

    // Returns nonzero if there aren't any samples available for outputting.
    virtual int isEmpty() const = 0;

    // Clears all the samples in the object
    virtual void clear() = 0;
};



class FIFOProcessor :public FIFOSamplePipe
{
protected:
    FIFOSamplePipe *output;

    void setOutPipe(FIFOSamplePipe *pOutput)
    {
        st_assert(output == NULL);
        st_assert(pOutput != NULL);
        output = pOutput;
    }


    FIFOProcessor()
    {
        output = NULL;
    }


    FIFOProcessor(FIFOSamplePipe *pOutput)
    {
        output = pOutput;
    }


    virtual ~FIFOProcessor()
    {
    }


    virtual Sample *ptrBegin() const
    {
        return output->ptrBegin();
    }

public:

    // Output samples from the object. Copies demanded number samples to output and 
    // removes them from the object's output buffer. If there are less than 'numsample' 
    // samples available, returns all the available samples.
    //
    // Returns number of samples copied.
    virtual uint receiveSamples(Sample *outBuffer, const uint maxSamples)
    {
        return output->receiveSamples(outBuffer, maxSamples);
    }


    // Removes samples from the output of the object copying them anywhere. 
    // Used to reduce the number of samples in the object, when accessing
    // the object's output buffer directly with the 'ptrBegin' function.
    virtual uint receiveSamples(const uint maxSamples)
    {
        return output->receiveSamples(maxSamples);
    }


    // Returns the number of samples currently available in the object for 
    // outputting.
    virtual uint numSamples() const
    {
        return output->numSamples();
    }


    // Returns nonzero if there aren't any samples available for outputting.
    virtual int isEmpty() const
    {
        return output->isEmpty();
    }
};

#endif

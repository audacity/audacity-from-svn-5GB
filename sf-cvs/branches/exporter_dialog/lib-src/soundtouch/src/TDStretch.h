/*****************************************************************************
 * 
 * Sampled sound tempo changer. Changes the sound tempo while maintaining the
 * original pitch by using time domain harmonic scaling (TDHS) method with 
 * several performance-increasing tweaks.
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

#ifndef TDStretch_H
#define TDStretch_H

#include "STTypes.h"
#include "RateTransposer.h"
#include "FIFOSamplePipe.h"


class TDStretch : public FIFOProcessor
{
private:
    uint channels;
    uint sampleReq;
    float tempo;

    Sample *pMidBuffer;
    Sample *pRefMidBuffer;
    uint overlapLength;
    uint overlapDividerBits;
    uint slopingDividerBits;
    float overlapDivisor;
    float slopingDivisor;
    uint seekLength;
    uint seekWindowLength;
    uint maxOffset;
    FIFOSampleBuffer outputBuffer;
    FIFOSampleBuffer inputBuffer;
    bool bQuickseek;
    bool bMMX;
    bool bMidBufferDirty;

    uint seekBestOverlapPositionStereo(const Sample *refPos);
    uint seekBestOverlapPositionStereoQuick(const Sample *refPos);
    uint seekBestOverlapPositionMono(const Sample *refPos);
    uint seekBestOverlapPositionMonoQuick(const Sample *refPos);
    uint mmxSeekBestOverlapPositionStereo(const Sample *refPos);
    uint mmxSeekBestOverlapPositionStereoQuick(const Sample *refPos);
    uint seekBestOverlapPosition(const Sample *refPos);

    void overlapStereo(Sample *output, const Sample *input) const;
    void overlapMono(Sample *output, const Sample *input) const;

    int mmxCalculateCrossCorrelation(const Sample *mixingPos, const Sample *compare) const;
    void mmxOverlapStereo(Sample *output, const Sample *input) const;

    void clearMidBuffer();
    void overlap(Sample *output, const Sample *input, const uint ovlPos) const;

    void slopeReferenceSamplesStereo();

    void processNominalTempo();

    // Changes the tempo of the given sound samples.
    // Returns amount of samples returned in the "output" buffer.
    // The maximum amount of samples that can be returned at a time is set by
    // the 'set_returnBuffer_size' function.
    void processSamples();
    
public:
    TDStretch();
    virtual ~TDStretch();

    // Returns the output buffer object
    FIFOSamplePipe *getOutput() { return &outputBuffer; };

    // Returns the input buffer object
    FIFOSamplePipe *getInput() { return &inputBuffer; };

    // Sets new target tempo. Normal tempo = 'SCALE', smaller values represent slower 
    // tempo, larger faster tempo.
    void setTempo(const float newTempo);

    // Returns nonzero if there aren't any samples available for outputting.
    virtual void clear();

    // Clears the input buffer
    void clearInput();

    // Sets the number of channels, 1 = mono, 2 = stereo
    void setChannels(const uint numChannels);

    // Enables/disables the quick position seeking algorithm. Zero to disable, 
    // nonzero to enable
    void enableQuickSeek(const bool enable);

    // Returns nonzero if the quick seeking algorithm is enabled.
    bool isQuickSeekEnabled() const;

    // Sets routine control parameters. These control are certain time constants
    // defining how the sound is stretched to the desired duration.
    //
    // 'sampleRate' = sample rate of the sound
    // 'sequenceMS' = one processing sequence length in milliseconds (default = 82 ms)
    // 'seekwindowMS' = seeking window length for scanning the best overlapping 
    //      position (default = 28 ms)
    // 'overlapMS' = overlapping length (default = 12 ms)
    void setParameters(const uint sampleRate, const uint sequenceMS = 82, 
        const uint seekwindowMS = 28, const uint overlapMS = 12);

    // Adds 'numsamples' pcs of samples from the 'samples' memory position into
    // the input of the object.
    virtual void putSamples(const Sample *samples, const uint numSamples);
};

#endif

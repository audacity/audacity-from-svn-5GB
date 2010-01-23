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


#include <stdlib.h>
#include <memory.h>
#include <limits.h>
#include <math.h>
#include <iostream>

#include "mmx.h"
#include "TDStretch.h"

typedef unsigned int uint;

#ifndef min
#define min(a,b) ((a > b) ? b : a)
#define max(a,b) ((a < b) ? b : a)
#endif


/*****************************************************************************
 *
 * Constant definitions
 *
 *****************************************************************************/


#define MAX_SCAN_DELTA      124

// Table for the hierarchical mixing position seeking algorithm

int scanOffsets[4][24]={
    { 124,  186,  248,  310,  372,  434,  496,  558,  620,  682,  744, 806, 
//      868,  930,  992, 1054, 1116, 1178, 1240,    0,    0,    0,    0,   0}, 
      868,  930,  992, 1054, 1116, 1178, 1240, 1302, 1364, 1426, 1488,   0}, 
    {-100,  -75,  -50,  -25,   25,   50,   75,  100,    0,    0,    0,   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   0},
    { -20,  -15,  -10,   -5,    5,   10,   15,   20,    0,    0,    0,   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   0},
    {  -4,   -3,   -2,   -1,    1,    2,    3,    4,    0,    0,    0,   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   0}};

/*****************************************************************************
 *
 * Implementation of the class 'TDStretch'
 *
 *****************************************************************************/


TDStretch::TDStretch() : FIFOProcessor(&outputBuffer)
{
    bMMX = mmxDetect();

    bQuickseek = false;
    channels = 2;
    bMidBufferDirty = false;

    setParameters(44100, 82, 28, 12);

    pMidBuffer = new Sample[2 * overlapLength];
    memset(pMidBuffer, 0, 2 * overlapLength);

    pRefMidBuffer = new Sample[2*overlapLength];

    setTempo(1.0f);
}




TDStretch::~TDStretch()
{
    delete[] pMidBuffer;
    delete[] pRefMidBuffer;
}


    
// Calculates the x having the closest 2^x value for the given value
static int _getClosest2Power(double value)
{
    return (int)(log(value) / log(2) + 0.5);
}



// Sets routine control parameters. These control are certain time constants
// defining how the sound is stretched to the desired duration.
//
// 'sampleRate' = sample rate of the sound
// 'sequenceMS' = one processing sequence length in milliseconds (default = 82 ms)
// 'seekwindowMS' = seeking window length for scanning the best overlapping 
//      position (default = 28 ms)
// 'overlapMS' = overlapping length (default = 12 ms)

void TDStretch::setParameters(const uint sampleRate, const uint sequenceMS, 
                             const uint seekwindowMS, const uint overlapMS)
{
    seekLength = (sampleRate * seekwindowMS) / 1000;
    seekWindowLength = (sampleRate * sequenceMS) / 1000;
    overlapDivisor = (sampleRate * overlapMS) / 1000.0;
    overlapDividerBits = _getClosest2Power(overlapDivisor);

    if (overlapDividerBits > 9) overlapDividerBits = 9;
    if (overlapDividerBits < 4) overlapDividerBits = 4;

    overlapLength = (int)pow(2, overlapDividerBits);
    
    slopingDividerBits = 2 * (overlapDividerBits - 1) - 1;
    slopingDivisor = (float) pow(2,slopingDividerBits);

    maxOffset = seekLength;

}


// Overlaps samples in 'midBuffer' with the samples in 'input'
 void TDStretch::overlapMono(Sample *output, const Sample *input) const
{
    int i;
    BiggerSample itemp;

    for (i = 0; i < (int)overlapLength ; i ++) {
        itemp = overlapLength - i;
#if SAMPLES_ARE_SHORT
        output[i] = (input[i] * i + pMidBuffer[i] * itemp ) >> overlapDividerBits;
#else 
        output[i] = (input[i] * i + pMidBuffer[i] * itemp ) / overlapDivisor;
#endif
    }
}



// Overlaps samples in 'midBuffer' with the samples in 'input'. The 'Stereo' 
// version of the routine.
void TDStretch::overlapStereo(Sample *output, const Sample *input) const
{
    int i;
    BiggerSample itemp;
    uint cnt2;

    for (i = 0; i < (int)overlapLength ; i ++) {
        itemp = overlapLength - i;
        cnt2 = 2 * i;
#if SAMPLES_ARE_SHORT
        output[cnt2] = (input[cnt2] * i + pMidBuffer[cnt2] * itemp ) >> overlapDividerBits;
	output[cnt2 + 1] = (input[cnt2 + 1] * i + pMidBuffer[cnt2 + 1] * itemp ) >> overlapDividerBits;
#else 
        output[cnt2] = (input[cnt2] * i + pMidBuffer[cnt2] * itemp ) / overlapDivisor;
	output[cnt2 + 1] = (input[cnt2 + 1] * i + pMidBuffer[cnt2 + 1] * itemp ) / overlapDivisor;
#endif
    }
}



// Slopes the amplitude of the 'midBuffer' samples
void TDStretch::slopeReferenceSamplesStereo()
{
    int i, cnt2;
    BiggerSample itemp, itemp2;

    for (i=0 ; i < (int)overlapLength ;i ++) {
        itemp = i * (overlapLength - i);
        cnt2 = i * 2;
#if SAMPLES_ARE_SHORT
        itemp2 = (pMidBuffer[cnt2] * itemp) >> slopingDividerBits;
        pRefMidBuffer[cnt2] = itemp2;
        itemp2 = (pMidBuffer[cnt2 + 1] * itemp) >> slopingDividerBits;
        pRefMidBuffer[cnt2 + 1] = itemp2;
#else 
        itemp2 = (pMidBuffer[cnt2] * itemp) / slopingDivisor;
        pRefMidBuffer[cnt2] = itemp2;
        itemp2 = (pMidBuffer[cnt2 + 1] * itemp) / slopingDivisor;
        pRefMidBuffer[cnt2 + 1] = itemp2;
#endif
    }
}



void TDStretch::clearMidBuffer()
{
    if (bMidBufferDirty) {
        memset(pMidBuffer, 0, 2 * sizeof(Sample) * overlapLength);
        bMidBufferDirty = false;
    }
}


void TDStretch::clearInput()
{
    inputBuffer.clear();
    clearMidBuffer();
}


// Clears the sample buffers
void TDStretch::clear()
{
    outputBuffer.clear();
    inputBuffer.clear();
    clearMidBuffer();
}



// Enables/disables the quick position seeking algorithm. Zero to disable, nonzero
// to enable
void TDStretch::enableQuickSeek(const bool enable)
{
    bQuickseek = enable;
}


// Returns nonzero if the quick seeking algorithm is enabled.
bool TDStretch::isQuickSeekEnabled() const
{
    return bQuickseek;
}


// Seeks for the optimal overlap-mixing position.
uint TDStretch::seekBestOverlapPosition(const Sample *refPos)
{
    if (channels == 2) {
        // stereo sound
        if (bMMX) {
            // use mmx routines
            if (bQuickseek) {
                return mmxSeekBestOverlapPositionStereoQuick(refPos);
            } else {
                return mmxSeekBestOverlapPositionStereo(refPos);
            }
        } else {
            // use normal routines
            if (bQuickseek) {
                return seekBestOverlapPositionStereoQuick(refPos);
            } else {
                return seekBestOverlapPositionStereo(refPos);
            }
        }
    } else {
        // mono sound. no mmx routines implemented for mono sound
        if (bQuickseek) {
            return seekBestOverlapPositionMonoQuick(refPos);
        } else {
            return seekBestOverlapPositionMono(refPos);
        }
    }
}




// Overlaps samples in 'midBuffer' with the samples in 'inputBuffer' at position
// of 'ovlPos'.
inline void TDStretch::overlap(Sample *output, const Sample *input, const uint ovlPos) const
{
    if (channels == 2) {
        // stereo sound
        if (bMMX) {
            mmxOverlapStereo(output, input + 2 * ovlPos);
        } else {
            overlapStereo(output, input + 2 * ovlPos);
        }
    } else {
        // mono sound. no mmx version implemented.
        overlapMono(output, input + ovlPos);
    }
}



// Seeks for the optimal overlap-mixing position. The 'stereo' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
uint TDStretch::seekBestOverlapPositionStereo(const Sample *refPos) 
{
    uint i;
    uint bestOffs;
    uint tempOffset;
    BiggerSample bestCorr,corr;
    BiggerSample itemp, itemp2;
    Sample *compare;

    // Slopes the amplitudes of the 'midBuffer' samples
    slopeReferenceSamplesStereo();

    bestCorr = INT_MIN;
    bestOffs = 0;


    // Scans for the best correlation value by testing each possible position
    // over the permitted range.
    for (tempOffset = 0; tempOffset < seekLength; tempOffset ++) {
        compare = (Sample*)refPos + 2 * tempOffset;

        // Calculates the cross-correlation value for the mixing position 
        // corresponding to 'tempOffset'
        corr = 0;
        for (i = 2; i < 2 * overlapLength; i += 2) {
            itemp = pRefMidBuffer[i] * compare[i];
            itemp2 = pRefMidBuffer[i + 1] * compare[i + 1];
#if SAMPLES_ARE_SHORT
            corr += (itemp + itemp2) >> overlapDividerBits;
#else
            corr += (itemp + itemp2) / overlapDivisor;
#endif
        }

        // Checks for the highest correlation value
        if (corr > bestCorr) {
            bestCorr = corr;
            bestOffs = tempOffset;
        }
    }
    return bestOffs;
}




// Seeks for the optimal overlap-mixing position. The 'mono' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
uint TDStretch::seekBestOverlapPositionMono(const Sample *refPos) 
{
    uint i;
    uint bestOffs;
    uint tempOffset;
    BiggerSample itemp, itemp2;
    BiggerSample bestCorr,corr;
    Sample *compare;

    // Slopes the amplitude of the 'midBuffer' samples
    for (i=0 ; i < overlapLength ;i ++) {
        // Note : 'i' is unsigned, this step's required to converts the result to signed!
        itemp = i * (overlapLength - i);
#if SAMPLES_ARE_SHORT
        itemp2 = (pMidBuffer[i] * itemp) >> slopingDividerBits;
#else 
        itemp2 = (pMidBuffer[i] * itemp) / slopingDivisor;
#endif
        pRefMidBuffer[i] = itemp2;
    }

    bestCorr = INT_MIN;
    bestOffs = 0;


    // Scans for the best correlation value by testing each possible position
    // over the permitted range.
    for (tempOffset = 0; tempOffset < seekLength; tempOffset ++) {
        compare = (Sample*)refPos + tempOffset;

        // Calculates correlation value for the mixing position corresponding
        // to 'tempOffset'
        corr = 0;
        for (i = 1; i < overlapLength; i ++) {
            itemp = pRefMidBuffer[i] * compare[i];
#if SAMPLES_ARE_SHORT
            corr += itemp >> overlapDividerBits;
#else 
            corr += itemp / overlapDivisor;
#endif
        }

        // Checks for the highest correlation value
        if (corr > bestCorr) {
            bestCorr = corr;
            bestOffs = tempOffset;
        }
    }
    return bestOffs;
}




// Seeks for the optimal overlap-mixing position. The 'stereo' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
uint TDStretch::seekBestOverlapPositionStereoQuick(const Sample *refPos) 
{
    uint j,i;
    uint bestOffs;
    uint scanCount, corrOffset, tempOffset;
    BiggerSample itemp, itemp2;
    BiggerSample bestCorr,corr;
    Sample *compare;

    // Slopes the amplitude of the 'midBuffer' samples
    slopeReferenceSamplesStereo();

    bestCorr = INT_MIN;
    bestOffs = 0;
    corrOffset = 0;
    tempOffset = 0;


    // Scans for the best correlation value using four-pass hierarchical search.
    //
    // The look-up table 'scans' has hierarchical position adjusting steps.
    // In first pass the routine searhes for the highest correlation with 
    // relatively coarse steps, then rescans the neighbourhood of the highest
    // correlation with better resolution and so on.
    for (scanCount = 0;scanCount < 4; scanCount ++) {
        j = 0;
        while (scanOffsets[scanCount][j]) {
            tempOffset = corrOffset + scanOffsets[scanCount][j];
            if (tempOffset >= seekLength) break;
            compare = (Sample*)refPos + 2 * tempOffset;

            /* Calculates correlation value for the mixing position corresponding
             * to 'tempOffset' */
            corr = 0;
            for (i = 2; i < 2 * overlapLength; i += 2) {
                itemp = pRefMidBuffer[i] * compare[i];
                itemp2 = pRefMidBuffer[i + 1] * compare[i + 1];
#if SAMPLES_ARE_SHORT
                corr += (itemp + itemp2) >> overlapDividerBits;
#else 
                corr += (itemp + itemp2) / overlapDivisor;
#endif
            }

            /* Checks for the highest correlation value */
            if (corr > bestCorr) {
                bestCorr = corr;
                bestOffs = tempOffset;
            }
            j ++;
        }
        corrOffset = bestOffs;
    }
    return bestOffs;
}


// Seeks for the optimal overlap-mixing position. The 'mono' version of the
// routine
//
// The best position is determined as the position where the two overlapped
// sample sequences are 'most alike', in terms of the highest cross-correlation
// value over the overlapping period
uint TDStretch::seekBestOverlapPositionMonoQuick(const Sample *refPos) 
{
    uint j,i;
    uint bestOffs;
    uint scanCount, corrOffset, tempOffset;
    BiggerSample itemp, itemp2;
    BiggerSample bestCorr,corr;
    Sample *compare;

    // Slopes the amplitude of the 'midBuffer' samples
    for (i=0 ; i < overlapLength ;i ++) {
        // Note : 'i' is unsigned, this step's required to converts the result to signed!
        itemp = i * (overlapLength - i);
#if SAMPLES_ARE_SHORT
        itemp2 = (pMidBuffer[i] * itemp) >> slopingDividerBits;
#else 
        itemp2 = (pMidBuffer[i] * itemp) / slopingDivisor;
#endif
        pRefMidBuffer[i] = itemp2;
    }

    bestCorr = INT_MIN;
    bestOffs = 0;
    corrOffset = 0;
    tempOffset = 0;


    // Scans for the best correlation value using four-pass hierarchical search.
    //
    // The look-up table 'scans' has hierarchical position adjusting steps.
    // In first pass the routine searhes for the highest correlation with 
    // relatively coarse steps, then rescans the neighbourhood of the highest
    // correlation with better resolution and so on.
    for (scanCount = 0;scanCount < 4; scanCount ++) {
        j = 0;
        while (scanOffsets[scanCount][j]) {
            tempOffset = corrOffset + scanOffsets[scanCount][j];
            if (tempOffset >= seekLength) break;

            compare = (Sample*)refPos + tempOffset;

            // Calculates correlation value for the mixing position corresponding
            // to 'tempOffset'
            corr = 0;
            for (i = 1; i < overlapLength; i ++) {
                itemp = pRefMidBuffer[i] * compare[i];
#if SAMPLES_ARE_SHORT
                corr += itemp >> overlapDividerBits;
#else 
                corr += itemp / overlapDivisor;
#endif
            }

            // Checks for the highest correlation value
            if (corr > bestCorr) {
                bestCorr = corr;
                bestOffs = tempOffset;
            }
            j ++;
        }
        corrOffset = bestOffs;
    }
    return bestOffs;
}


// Sets new target tempo. Normal tempo = 'SCALE', smaller values represent slower 
// tempo, larger faster tempo.
void TDStretch::setTempo(const float newTempo)
{
    uint optimSkip;

    tempo = newTempo;

    // Calculate ideal skip length (according to tempo value) and how many 
    // samples are needed in the 'inputBuffer' to process a batch of samples
    optimSkip = (int)(tempo * (seekWindowLength - overlapLength) + 0.5f);
    sampleReq = max(optimSkip + overlapLength, seekWindowLength) + maxOffset;
}



// Sets the number of channels, 1 = mono, 2 = stereo
void TDStretch::setChannels(const uint numChannels)
{
    if (channels == numChannels) return;
    st_assert(numChannels == 1 || numChannels == 2);

    channels = numChannels;
    inputBuffer.setChannels(channels);
    outputBuffer.setChannels(channels);
}


// nominal tempo, no need for processing, just pass the samples through
// to outputBuffer
void TDStretch::processNominalTempo()
{
    st_assert(tempo == 1.0f);

    if (bMidBufferDirty) {
        // If there are samples in pMidBuffer waiting for overlapping,
        // do a single sliding overlapping with them in order to prevent a 
        // clicking distortion in the output sound
        if (inputBuffer.numSamples() < overlapLength) {
            // wait until we've got overlapLength input samples
            return;
        }
        // Mix the samples in the beginning of 'inputBuffer' with the 
        // samples in 'midBuffer' using sliding overlapping 
        overlap(outputBuffer.ptrEnd(overlapLength), inputBuffer.ptrBegin(), 0);
        outputBuffer.putSamples(overlapLength);
        inputBuffer.receiveSamples(overlapLength);
        clearMidBuffer();
        // now we've caught the nominal sample flow and may switch to
        // bypass mode
    }

    // Simply bypass samples from input to output
    outputBuffer.moveSamples(inputBuffer);
}


// Processes as many processing frames of the samples 'inputBuffer', store
// the result into 'outputBuffer'
void TDStretch::processSamples()
{
    uint ovlSkip, temp, offset;

    if (tempo == 1.0f) {
        // tempo not changed from the original, so bypass the processing
        processNominalTempo();
        return;
    }

    if (bMidBufferDirty == false) {
        // if midBuffer is empty, move the first samples of the input stream 
        // into it
        if (inputBuffer.numSamples() < overlapLength) {
            // wait until we've got overlapLength samples
            return;
        }
        memcpy(pMidBuffer, inputBuffer.ptrBegin(), channels * overlapLength * sizeof(Sample));
        inputBuffer.receiveSamples(overlapLength);
        bMidBufferDirty = true;
    }

    // Process samples as long as there are enough samples in 'inputBuffer'
    // to form a processing frame.
    while (inputBuffer.numSamples() >= sampleReq) {
        // If tempo differs from the normal ('SCALE'), scan for the best overlapping
        // position
        offset = seekBestOverlapPosition(inputBuffer.ptrBegin());

        // Mix the samples in the 'inputBuffer' at position of 'offset' with the 
        // samples in 'midBuffer' using sliding overlapping
        // ... first partially overlap with the end of the previous sequence
        // (that's in 'midBuffer')
        overlap(outputBuffer.ptrEnd(overlapLength), inputBuffer.ptrBegin(), offset);
        outputBuffer.putSamples(overlapLength);

        // ... then copy sequence samples from 'inputBuffer' to output
        temp = (seekWindowLength - 2 * overlapLength);// & 0xfffffffe;
        outputBuffer.putSamples(inputBuffer.ptrBegin() + channels * (offset + overlapLength), temp);

        // Copies the end of the current sequence from 'inputBuffer' to 
        // 'midBuffer' for being mixed with the beginning of the next 
        // processing sequence and so on
        st_assert(offset + seekWindowLength <= inputBuffer.numSamples());
        memcpy(pMidBuffer, inputBuffer.ptrBegin() + channels * (offset + seekWindowLength - overlapLength), channels * sizeof(Sample) * overlapLength);
        bMidBufferDirty = true;

        // Remove the processed samples from the input buffer
        ovlSkip = (int)(tempo * (seekWindowLength - overlapLength) + 0.5f);
        inputBuffer.receiveSamples(ovlSkip);
    }
}


// Adds 'numsamples' pcs of samples from the 'samples' memory position into
// the input of the object.
void TDStretch::putSamples(const Sample *samples, const uint numSamples)
{
    // Add the samples into the input buffer
    inputBuffer.putSamples(samples, numSamples);
    // Process the samples in input buffer
    processSamples();
}


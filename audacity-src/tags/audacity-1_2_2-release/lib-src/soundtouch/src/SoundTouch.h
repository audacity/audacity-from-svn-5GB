/*****************************************************************************
 *
 * A main class for tempo/pitch/rate adjusting routines. 
 *
 * Initialize the object by setting up the sound stream parameters with the
 * 'setSampleRate' and 'setChannels' functions, and set the desired
 * tempo/pitch/rate settings with the corresponding functions.
 *
 * Notes:
 * * This class behaves like an first-in-first-out pipe: The samples to be 
 * processed are fed into one of the pipe with the 'putSamples' function,
 * and the ready, processed samples are read from the other end with the 
 * 'receiveSamples' function. 
 *
 * * The tempo/pitch/rate control parameters may freely be altered during 
 * processing.
 *
 * * The processing routines introduce a certain 'latency' between the
 * input and output, so that the inputted samples aren't immediately 
 * transferred to the output, and neither the number of output samples 
 * immediately available after inputting some samples isn't in direct 
 * relationship to the number of previously inputted samples.
 *
 * * This class utilizes classes 'tempochanger' to change tempo of the
 * sound (without changing pitch) and 'transposer' to change rate
 * (that is, both tempo and pitch) of the sound. The third available control 
 * 'pitch' (change pitch but maintain tempo) is produced by suitably 
 * combining the two preceding controls.
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

#ifndef SoundTouch_H
#define SoundTouch_H

#include "FIFOSamplePipe.h"
#include "STTypes.h"

//
// Available setting IDs for the 'setSetting' & 'get_setting' functions:

// Enable/disable anti-alias filter in pitch transposer (0 = disable)
#define SETTING_USE_AA_FILTER       0

// Pitch transposer anti-alias filter length (8 .. 128 taps, default = 32)
#define SETTING_AA_FILTER_LENGTH    1

// Enable/disable quick seeking algorithm in tempo changer routine
// (enabling quick seeking lowers CPU utilization but causes a minor sound
//  quality compromising)
#define SETTING_USE_QUICKSEEK       2


class SoundTouch : public FIFOProcessor
{
private:
    class RateTransposer *pRateTransposer;
    class TDStretch *pTDStretch;
    float virtualRate;
    float virtualTempo;
    float virtualPitch;
    bool  bSrateSet;

    void calcEffectiveRateAndTempo();

protected :
    uint  channels;
    float rate;
    float tempo;

public:
    SoundTouch();
    virtual ~SoundTouch();

    // Sets new rate control value. Normal rate = 1.0, smaller values
    // represent slower rate, larger faster rates.
    void setRate(const float newRate);

    // Sets new tempo control value. Normal tempo = 1.0, smaller values
    // represent slower tempo, larger faster tempo.
    void setTempo(const float newTempo);

    // Sets new rate control value as a difference in percents compared
    // to the original rate (-50 .. +100 %)
    void setRateChange(const float newRate);

    // Sets new tempo control value as a difference in percents compared
    // to the original tempo (-50 .. +100 %)
    void setTempoChange(const float newTempo);

    // Sets new pitch control value. Original pitch = 1.0, smaller values
    // represent lower pitches, larger values higher pitch.
    void setPitch(const float newPitch);

    // Sets pitch change in octaves compared to the original pitch  
    // (-1.00 .. +1.00)
    void setPitchOctaves(const float newPitch);

    // Sets pitch change in semi-tones compared to the original pitch
    // (-12 .. +12)
    void setPitchSemiTones(const int newPitch);
    void setPitchSemiTones(const float newPitch);

    // Sets the number of channels, 1 = mono, 2 = stereo
    void setChannels(const uint numChannels);

    // Sets sample rate.
    void setSampleRate(const uint srate);

    // Flushes the last samples from the processing pipeline to the output.
    // Clears also the internal processing buffers.
    //
    // Note: This function is meant for extracting the last samples of a sound
    // stream. This function may introduce additional blank samples in the end
    // of the sound stream, and thus it's not recommended to call this function
    // in the middle of a sound stream.
    void flush();

    // Adds 'numSamples' pcs of samples from the 'samples' memory position into
    // the input of the object.
    virtual void putSamples(const Sample *samples, const uint numSamples);

    // Clears all the samples in the object's output and internal processing
    // buffers.
    virtual void clear();

    // Changes a setting controlling the processing system behaviour. See the
    // 'SETTING_...' defines for available setting ID's.
    // 
    // 'value' = the new setting value
    // Returns 'true' if the setting was succesfully changed
    bool setSetting(const uint settingId, const uint value);

    // Reads a setting controlling the processing system behaviour. See the
    // 'SETTING_...' defines for available setting ID's.
    //
    // Returns the setting value.
    uint getSetting(const uint settingId) const;

};

#endif

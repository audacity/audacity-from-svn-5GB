/**********************************************************************

  Audacity: A Digital Audio Editor

  PitchName.h

  Dominic Mazzoni, Vaughan Johnson

  Utilities for converting from frequency to pitch 
  and from pitch to absolute (e.g., C4 for middle C) 
  or nominal (A through G#) pitch name.

**********************************************************************/

#ifndef __AUDACITY_PITCHNAME__
#define __AUDACITY_PITCHNAME__

// Freq2Pitch takes a frequency in Hz (exponential scale relative to 
// alphabetic pitch names) and returns a pitch ID number (linear 
// scale), such that A440 (A4) is 57, middle C is 48, etc.
// The offset to 57 is used to determine the register. 
// Each register starts with C (e.g., for middle C and A440, 
// it's register 4).
float Freq2Pitch(float freq);

// PitchName takes an integer version of a pitch ID (result from 
// Freq2Pitch) and returns a standard pitch/note name [C, C#, etc.). 
// Sharps are the default, unless, b_Wantb_WantFlats is true.
char * PitchName(int pitchID, bool b_WantFlats = false);

// PitchName_Absolute does the same thing as PitchName, but appends 
// the register number, e.g., instead of "C" it will return "C4" 
// if the pitchID corresonds to middle C.
char * PitchName_Absolute(int pitchID, bool b_WantFlats = false);

#endif	// __AUDACITY_PITCHNAME__

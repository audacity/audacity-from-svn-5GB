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

#include <wx/defs.h>

// Freq2Pitch takes a frequency in Hz (exponential scale relative to 
// alphabetic pitch names) and returns a pitch ID number (linear 
// scale), such that A440 (A4) is 57, middle C (C4) is 48, etc.
// The offset to 57 is used to determine the register. 
// Each register starts with C (e.g., for middle C and A440, 
// it's register 4).
float Freq2Pitch(float freq);

// PitchIndex returns the [0,11] index for a float pitchNum, 
// as per result from Freq2Pitch, corresponding to modulo 12 
// of the integer part of (pitchNum + 0.5), so 0=C, 1=C#, etc.
unsigned int PitchIndex(float pitchNum);

// PitchName takes pitchNum (as per result from 
// Freq2Pitch) and returns a standard pitch/note name [C, C#, etc.). 
// Sharps are the default, unless, bWantFlats is true.
wxChar * PitchName(float pitchNum, bool bWantFlats = false);

// PitchName_Absolute does the same thing as PitchName, but appends 
// the register number, e.g., instead of "C" it will return "C4" 
// if the pitchNum corresonds to middle C.
wxChar * PitchName_Absolute(float pitchNum, bool bWantFlats = false);

#endif	// __AUDACITY_PITCHNAME__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 1cd2b819-63b6-4051-9991-37165f236037


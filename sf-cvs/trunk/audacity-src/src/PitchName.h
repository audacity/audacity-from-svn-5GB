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

float Freq2Pitch(float freq);
char * PitchName_Absolute(int pitch, bool flats);
char * PitchName(int pitch, bool flats);

#endif	// __AUDACITY_PITCHNAME__

/**********************************************************************

  Audacity: A Digital Audio Editor

  PitchName.cpp

  Dominic Mazzoni, Vaughan Johnson

  Utilities for converting from frequency to pitch  
  and from pitch to absolute (e.g., C4 for middle C) 
  or nominal (A through G#) pitch name.

**********************************************************************/

#include <math.h>
#include <stdio.h>

#include "PitchName.h"


// Freq2Pitch takes a frequency in Hz (exponential scale relative to 
// alphabetic pitch names) and returns a pitch ID number (linear 
// scale), such that A440 (A4) is 57, middle C is 48, etc.
// The offset to 57 is used to determine the register. 
// Each register starts with C (e.g., for middle C and A440, 
// it's register 4).
float Freq2Pitch(float freq)
{
   return float (57.0 + (12.0 * (log(freq / 440.0) / log(2.0))));
}


char gPitchName[10];
char * p_PitchName;

// PitchName takes an integer version of a pitch ID (result from 
// Freq2Pitch) and returns a standard pitch/note name [C, C#, etc.). 
// Sharps are the default, unless, b_Wantb_WantFlats is true.
char * PitchName(int pitchID, bool b_WantFlats)
{
   p_PitchName = gPitchName;

   switch (pitchID % 12) {
   case 0:
      *p_PitchName++ = 'C';
      break;
   case 1:
      if (b_WantFlats) {
         *p_PitchName++ = 'D';
         *p_PitchName++ = 'b';
      } else {
         *p_PitchName++ = 'C';
         *p_PitchName++ = '#';
      }
      break;
   case 2:
      *p_PitchName++ = 'D';
      break;
   case 3:
      if (b_WantFlats) {
         *p_PitchName++ = 'E';
         *p_PitchName++ = 'b';
      } else {
         *p_PitchName++ = 'D';
         *p_PitchName++ = '#';
      }
      break;
   case 4:
      *p_PitchName++ = 'E';
      break;
   case 5:
      *p_PitchName++ = 'F';
      break;
   case 6:
      if (b_WantFlats) {
         *p_PitchName++ = 'G';
         *p_PitchName++ = 'b';
      } else {
         *p_PitchName++ = 'F';
         *p_PitchName++ = '#';
      }
      break;
   case 7:
      *p_PitchName++ = 'G';
      break;
   case 8:
      if (b_WantFlats) {
         *p_PitchName++ = 'A';
         *p_PitchName++ = 'b';
      } else {
         *p_PitchName++ = 'G';
         *p_PitchName++ = '#';
      }
      break;
   case 9:
      *p_PitchName++ = 'A';
      break;
   case 10:
      if (b_WantFlats) {
         *p_PitchName++ = 'B';
         *p_PitchName++ = 'b';
      } else {
         *p_PitchName++ = 'A';
         *p_PitchName++ = '#';
      }
      break;
   case 11:
      *p_PitchName++ = 'B';
      break;
   }

	*p_PitchName = '\0';

   return gPitchName;
}

// PitchName_Absolute does the same thing as PitchName, but appends 
// the register number, e.g., instead of "C" it will return "C4" 
// if the pitchID corresonds to middle C.
char * PitchName_Absolute(int pitchID, bool b_WantFlats)
{
   PitchName(pitchID, b_WantFlats); 

	// PitchName sets p_PitchName to the next char in gPitchName, 
	// so it's ready to append the register number.
   sprintf(p_PitchName, "%d", (pitchID / 12));

   return gPitchName;
}


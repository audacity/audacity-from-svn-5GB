/**********************************************************************

  Audacity: A Digital Audio Editor

  PitchName.cpp

  Dominic Mazzoni
  Vaughan Johnson

*******************************************************************//*!

\file PitchName.cpp
\brief   Utilities for converting from frequency to pitch  
  and from pitch to absolute (e.g., C4 for middle C) 
  or nominal (A through G#) pitch name.

*//*******************************************************************/


#include <math.h>
#include <stdio.h>

#include "PitchName.h"


// Freq2Pitch takes a frequency in Hz (exponential scale relative to 
// alphabetic pitch names) and returns a pitch ID number (linear 
// scale), such that A440 (A4) is 57, middle C (C4) is 48, etc.
// The offset to 57 is used to determine the register. 
// Each register starts with C (e.g., for middle C and A440, 
// it's register 4).
float Freq2Pitch(float freq)
{
   return float (57.0 + (12.0 * (log(freq / 440.0) / log(2.0))));
}

// PitchIndex returns the [0,11] index for a float pitchNum, 
// as per result from Freq2Pitch, corresponding to modulo 12 
// of the integer part of (pitchNum + 0.5), so 0=C, 1=C#, etc.
unsigned int PitchIndex(float pitchNum)
{
	return ((int)(pitchNum + 0.5) % 12);
}


wxChar gPitchName[10];
wxChar * p_PitchName;

// PitchName takes pitchNum (as per result from 
// Freq2Pitch) and returns a standard pitch/note name [C, C#, etc.). 
// Sharps are the default, unless, bWantFlats is true.
wxChar * PitchName(float pitchNum, bool bWantFlats /* = false */)
{
   p_PitchName = gPitchName;

   switch (PitchIndex(pitchNum)) {
   case 0:
      *p_PitchName++ = wxT('C');
      break;
   case 1:
      if (bWantFlats) {
         *p_PitchName++ = wxT('D');
         *p_PitchName++ = wxT('b');
      } else {
         *p_PitchName++ = wxT('C');
         *p_PitchName++ = wxT('#');
      }
      break;
   case 2:
      *p_PitchName++ = wxT('D');
      break;
   case 3:
      if (bWantFlats) {
         *p_PitchName++ = wxT('E');
         *p_PitchName++ = wxT('b');
      } else {
         *p_PitchName++ = wxT('D');
         *p_PitchName++ = wxT('#');
      }
      break;
   case 4:
      *p_PitchName++ = wxT('E');
      break;
   case 5:
      *p_PitchName++ = wxT('F');
      break;
   case 6:
      if (bWantFlats) {
         *p_PitchName++ = wxT('G');
         *p_PitchName++ = wxT('b');
      } else {
         *p_PitchName++ = wxT('F');
         *p_PitchName++ = wxT('#');
      }
      break;
   case 7:
      *p_PitchName++ = wxT('G');
      break;
   case 8:
      if (bWantFlats) {
         *p_PitchName++ = wxT('A');
         *p_PitchName++ = wxT('b');
      } else {
         *p_PitchName++ = wxT('G');
         *p_PitchName++ = wxT('#');
      }
      break;
   case 9:
      *p_PitchName++ = wxT('A');
      break;
   case 10:
      if (bWantFlats) {
         *p_PitchName++ = wxT('B');
         *p_PitchName++ = wxT('b');
      } else {
         *p_PitchName++ = wxT('A');
         *p_PitchName++ = wxT('#');
      }
      break;
   case 11:
      *p_PitchName++ = wxT('B');
      break;
   }

	*p_PitchName = wxT('\0');

   return gPitchName;
}

// PitchName_Absolute does the same thing as PitchName, but appends 
// the register number, e.g., instead of "C" it will return "C4" 
// if the pitchNum corresonds to middle C.
wxChar * PitchName_Absolute(float pitchNum, bool bWantFlats /* = false */)
{
   PitchName(pitchNum, bWantFlats); 

	// PitchName sets p_PitchName to the next available char in gPitchName, 
	// so it's ready to append the register number.
   wxSnprintf(p_PitchName, 8, wxT("%d"), ((int)(pitchNum + 0.5) / 12));

   return gPitchName;
}


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 71a57231-e6fd-4e65-8839-08451f7b4dff


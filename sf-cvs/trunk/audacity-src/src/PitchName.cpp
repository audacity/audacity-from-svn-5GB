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


float Freq2Pitch(float freq)
{
   return float (69.0 + (12.0 * (log(freq / 440.0) / log(2.0))));
}


char gPitchName[10];

char * PitchName_Absolute(int pitch, bool flats)
{
   char *p = gPitchName;

   switch (pitch % 12) {
   case 0:
      *p++ = 'C';
      break;
   case 1:
      if (flats) {
         *p++ = 'D';
         *p++ = 'b';
      } else {
         *p++ = 'C';
         *p++ = '#';
      }
      break;
   case 2:
      *p++ = 'D';
      break;
   case 3:
      if (flats) {
         *p++ = 'E';
         *p++ = 'b';
      } else {
         *p++ = 'D';
         *p++ = '#';
      }
      break;
   case 4:
      *p++ = 'E';
      break;
   case 5:
      *p++ = 'F';
      break;
   case 6:
      if (flats) {
         *p++ = 'G';
         *p++ = 'b';
      } else {
         *p++ = 'F';
         *p++ = '#';
      }
      break;
   case 7:
      *p++ = 'G';
      break;
   case 8:
      if (flats) {
         *p++ = 'A';
         *p++ = 'b';
      } else {
         *p++ = 'G';
         *p++ = '#';
      }
      break;
   case 9:
      *p++ = 'A';
      break;
   case 10:
      if (flats) {
         *p++ = 'B';
         *p++ = 'b';
      } else {
         *p++ = 'A';
         *p++ = '#';
      }
      break;
   case 11:
      *p++ = 'B';
      break;
   }

   sprintf(p, "%d", ((pitch + 3) / 12) - 2);

   return gPitchName;
}

char * PitchName(int pitch, bool flats)
{
	return PitchName_Absolute(pitch, flats); //v
}


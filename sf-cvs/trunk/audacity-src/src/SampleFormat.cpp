/**********************************************************************

  Audacity: A Digital Audio Editor

  SampleFormat.h

  Dominic Mazzoni

  This file handles converting between all of the different
  sample formats that Audacity supports, such as 16-bit,
  24-bit (packed into a 32-bit int), and 32-bit float.

  Floating-point samples use the range -1.0...1.0, inclusive.
  Integer formats use the full signed range of their data type,
  for example 16-bit samples use the range -32768...32767.
  This presents a problem, because either the conversion to
  or from a float would have to be asymmetric, or zero would
  have to be mapped to something other than zero.  (A third
  option is to use a symmetric, zero-preserving mapping that
  might clip, and check for clipping...but this option is
  both uncompelling and slow.)

  Audacity chooses to use a symmetric mapping that doesn't
  preserve 0:

  16-bit    float        16-bit
  -32768 -> -1.000000 -> -32768

       0 ->  0.000015 ->      0
             0.000000 ->      0

   32767 ->  1.000000 ->  32767

   Note that 0.0 (float) still maps to 0 (int), so it is nearly
   an ideal mapping.  An analogous mapping is used between
   24-bit ints and floats.

**********************************************************************/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SampleFormat.h"

int gDither = 0;

int GetNumDithers()
{
   return 1;
}

int GetCurrentDither()
{
   return gDither;
}

const char *GetSampleFormatStr(sampleFormat format)
{
   switch(format) {
   case int16Sample:
      return "16-bit";
   case int24Sample:
      return "24-bit";
   case floatSample:
      return "32-bit float";
   }
}

samplePtr NewSamples(int count, sampleFormat format)
{
   return (samplePtr)malloc(count * SAMPLE_SIZE(format));
}

void DeleteSamples(samplePtr p)
{
   free(p);
}

void ClearSamples(samplePtr src, sampleFormat format,
                  int start, int len)
{
   int size = SAMPLE_SIZE(format);
   memset(src + start*size, 0, len*size);
}

void CopySamples(samplePtr src, sampleFormat srcFormat,
                 samplePtr dst, sampleFormat dstFormat,
                 unsigned int len,
                 bool highQuality /* = true */)
{
   if (len == 0)
      return;

   int srcBytes = SAMPLE_SIZE(srcFormat);
   int dstBytes = SAMPLE_SIZE(dstFormat);

   if (srcFormat == dstFormat) {
      memcpy(dst, src, len*srcBytes);
      return;
   }

   if (src == dst) {
      printf("TODO: handle conversion in place\n");
      return;
   }

   int i;
   float fHalf = 0.5;
   float fDiv16 = 32767.5;
   float fDiv24 = 8388607.5;

   switch(dstFormat) {
   case floatSample:
      if (srcFormat == int16Sample)
         for(i=0; i<len; i++)
            ((float *)dst)[i] = (((short *)src)[i] + fHalf) / fDiv16;
      else if (srcFormat == int24Sample)
         for(i=0; i<len; i++)
            ((float *)dst)[i] = (((short *)src)[i] + fHalf) / fDiv24;
      break;

   case int24Sample:
      if (srcFormat == int16Sample)
         for(i=0; i<len; i++)
            ((int *)dst)[i] = ((int)((short *)src)[i]) << 8;
      else if (srcFormat == floatSample)
         switch(gDither) {
         case 0:
            for(i=0; i<len; i++)
               ((int *)dst)[i] = (int)floor(((float *)src)[i] * fDiv24);
            break;
         }
      break;

   case int16Sample:
      if (srcFormat == floatSample)
         switch(gDither) {
         case 0:
            for(i=0; i<len; i++)
               ((short *)dst)[i] = (short)floor(((float *)src)[i] * fDiv16);
            break;
         }
      else if (srcFormat == int24Sample)
         switch(gDither) {
         case 0:
            for(i=0; i<len; i++)
               ((short *)dst)[i] = (short)((int *)src)[i] >> 8;
            break;
         }
      break;
   }
}


/**********************************************************************

  Audacity: A Digital Audio Editor

  FilterFade.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include "wx/wx.h"
#endif

#include "Filter.h"
#include "FilterCompressor.h"

bool FilterCompressor::Prepare(WaveTrack * t, double t0, double t1,
                               sampleCount total)
{
   wxString temp;
   wxWindow *parent = NULL;

   temp =
       wxGetTextFromUser
       ("Please enter an increment (in milliseconds) to apply the filter to",
        "Increment: ", "1", parent, -1, -1, TRUE);

   if (temp == "")
      return false;

   while (sscanf((const char *) temp, "%d", &increment) < 0) {
      temp = wxGetTextFromUser("Please enter a value greater than zero:",
                               "Increment: ", "1", parent, -1, -1, TRUE);
      if (temp == "")
         return false;
   }

   how_far = 0;

   increment *= t->rate / 100;

   return true;
}

void FilterCompressor::DoIt(sampleType * src, sampleType * dst,
                            sampleCount total, sampleCount x,
                            sampleCount len)
{
   for (int i = 0; i < len; i++) {
      if (how_far == 2 * increment)
         how_far = 0;
      if (how_far == 0) {
         for (int i2 = i; i2 < len && i2 <= increment; i2++) {
            if (i2 == i)
               max_region = src[i];
            else if (src[i2] > max_region)
               max_region = src[i2];
         }
      } else if (how_far <= increment) {
         dst[i] = (sampleType) (src[i] * 32767 / max_region);
      } else
         dst[i] = (sampleType) (src[i]);
      ++how_far;
   }
}
}

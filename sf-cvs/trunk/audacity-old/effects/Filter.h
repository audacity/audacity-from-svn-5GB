/**********************************************************************

  Audacity: A Digital Audio Editor

  Filter.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FILTER__
#define __AUDACITY_EFFECT_FILTER__

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectFilter:public Effect {

 public:

   EffectFilter();

   virtual wxString GetEffectName() {
      return wxString("Filter...");
   } virtual bool Begin(wxWindow * parent);
   virtual bool DoIt(WaveTrack * t, sampleCount start, sampleCount len);

 private:

   void Filter(sampleCount len, sampleType * buffer);
};

#endif

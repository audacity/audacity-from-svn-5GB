/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FADE__
#define __AUDACITY_EFFECT_FADE__

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectFadeIn:public Effect {

 public:
   virtual wxString GetEffectName() {
      return wxString("Fade In");
   }
   
   virtual wxString GetEffectAction() {
      return wxString("Fading In");
   }
   
   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
};

class EffectFadeOut:public Effect {

 public:
   virtual wxString GetEffectName() {
      return wxString("Fade Out");
   }
   
   virtual wxString GetEffectAction() {
      return wxString("Fading Out");
   }
   
   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.h

  Robert Leidle

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AMPLIFY__
#define __AUDACITY_EFFECT_AMPLIFY__

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectAmplify:public Effect {

 public:
   EffectAmplify();

   virtual wxString GetEffectName() {
      return wxString("Amplify...");
   }
   
   virtual wxString GetEffectAction() {
      return wxString("Amplifying");
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

 private:
   float ratio;
};

class EffectMaxAmplify:public Effect {

 public:
   EffectMaxAmplify();

   virtual wxString GetEffectName() {
      return wxString("Maximize Amplitude");
   }
   
   virtual wxString GetEffectAction() {
      return wxString("Maximizing Amplitude");
   }
   
   virtual bool Process();

private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);
};

#endif

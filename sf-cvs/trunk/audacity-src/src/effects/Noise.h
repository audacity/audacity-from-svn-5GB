/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.h

  Dominic Mazzoni
  
  An effect for the "Generator" menu to add white noise.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE__
#define __AUDACITY_EFFECT_NOISE__

#include <wx/defs.h>
#include <wx/intl.h>

#include "Effect.h"

class EffectNoise:public Effect {

 public:
   EffectNoise() {}

   virtual wxString GetEffectName() {
      return wxString(_("White Noise"));
   }

   virtual wxString GetEffectDescription() { 
      return wxString(_("Applied effect: Generate White Noise")); 
   } 

   virtual wxString GetEffectAction() {
      return wxString(_("Generating White Noise"));
   }

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | INSERT_EFFECT;
   }

   virtual bool Process();

};

#endif

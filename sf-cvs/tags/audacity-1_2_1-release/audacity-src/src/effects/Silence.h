/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.h

  Dominic Mazzoni
  
  An effect for the "Generator" menu to add silence.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SILENCE__
#define __AUDACITY_EFFECT_SILENCE__

#include <wx/defs.h>
#include <wx/intl.h>

#include "Effect.h"

class EffectSilence:public Effect {

 public:
   EffectSilence() {}

   virtual wxString GetEffectName() {
      return wxString(_("Silence"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating Silence"));
   }

   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription() { 
      return wxString(_("Applied effect: Generate Silence")); 
   } 

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | INSERT_EFFECT;
   }

   virtual bool Process();

};

#endif

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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 3d52765e-51bb-4f53-8ed8-4239f7b42d16


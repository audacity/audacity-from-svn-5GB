/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

class wxString;

#include <wx/intl.h>
#include "Effect.h"

class WaveTrack;

class EffectEcho:public Effect {

 public:

   EffectEcho();

   virtual wxString GetEffectName() {
      return wxString(_("Echo..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Performing Echo"));
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   double start, sampleCount len);
 
   float delay;
   float decay;
};

#endif

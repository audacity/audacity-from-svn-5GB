/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.h

  Mark Phillips
  
  This class inverts the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_INVERT__
#define __AUDACITY_EFFECT_INVERT__

#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>

#include "Effect.h"

#define __UNINITIALIZED__ (-1)

class WaveTrack;

class EffectInvert:public Effect {

 public:
   EffectInvert();

   virtual wxString GetEffectName() {
      return wxString("Invert");
   }
   
   virtual wxString GetEffectAction() {
      return wxString("Inverting");
   }
   
   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

 private:
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT__
#define __AUDACITY_EFFECT__

#include <wx/dynarray.h>
#include <wx/string.h>

#include "../WaveTrack.h"

class Effect;

WX_DEFINE_ARRAY(Effect *, EffectArray);

class Effect {

public:

  // override these
  
  virtual wxString GetEffectName() = 0;
  
  virtual bool DoIt(WaveTrack *t,
		    sampleCount start,
		    sampleCount len) = 0;
  
  // call these
  
  static int RegisterEffect(Effect *f);
  static int GetNumEffects();
  static Effect *GetEffect(int i);
  
  bool DoInPlaceEffect(WaveTrack *t, double t0, double t1);

 private:
  static EffectArray *Effects;
};

#endif




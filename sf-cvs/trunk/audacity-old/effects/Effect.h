/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT__
#define __AUDACITY_EFFECT__

#include <wx/dynarray.h>
#include <wx/string.h>

class wxFrame;

#ifdef __WXMAC__
#include "WaveTrack.h"
#else
#include "../WaveTrack.h"
#endif

class Effect;

WX_DEFINE_ARRAY(Effect *, EffectArray);

class Effect {

public:

  // override these
  
  // this will go in the menu bar
  // append "..." if your effect pops up a dialog
  virtual wxString GetEffectName() = 0;

  virtual bool Begin(wxWindow *parent) // init and pop up your dialog here
    {return true;} 
  virtual bool DoIt(WaveTrack *t,
		    sampleCount start,
		    sampleCount len) = 0; // process some samples
  virtual void End() {}   // clean up any temporary memory
  
  // call these
  
  static int RegisterEffect(Effect *f);
  static int GetNumEffects();
  static Effect *GetEffect(int i);
  
  bool DoInPlaceEffect(WaveTrack *t, double t0, double t1,
                       int trackIndex = 0, int numTracks = 0);

 private:
  static EffectArray *Effects;
};

#endif




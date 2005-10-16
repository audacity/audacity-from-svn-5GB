/**********************************************************************

  Audacity: A Digital Audio Editor

  TwoPassSimpleMono.cpp

  Dominic Mazzoni

  This bit by Martyn Shaw.
  This class implements a two pass process by using EffectSimpleMono.
  Inherit from it if your effect needs to pass twice over the data.
  It does the first pass on all selected tracks before going back and
  doing the second pass over all selected tracks.

**********************************************************************/
#include "TwoPassSimpleMono.h"

bool EffectTwoPassSimpleMono::Process()
{
    InitFirstPass();
    if (!EffectSimpleMono::Process())
        return false;
	if(InitSecondPass()){
		TrackProgressRestart();
		return EffectSimpleMono::Process();
	}
	else
		return true;
}

//Initialisations before the first pass
bool EffectTwoPassSimpleMono::InitFirstPass()
{
	return true;
}

//Initialisations before the second pass.
//Return true if you actually want the second pass to go ahead
bool EffectTwoPassSimpleMono::InitSecondPass()
{
	return true;
}


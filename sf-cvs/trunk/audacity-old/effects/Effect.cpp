/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/string.h"

#include "Effect.h"
#include "../WaveTrack.h"

// static

EffectArray *Effect::Effects = new EffectArray();

int Effect::RegisterEffect(Effect *f)
{
  Effects->Add(f);
  return Effects->Count();
}

int Effect::GetNumEffects()
{
  return Effects->Count();
}

Effect *Effect::GetEffect(int i)
{
  return (*Effects)[i];
}

// methods

bool Effect::DoInPlaceEffect(WaveTrack *t, double t0, double t1,
                             int trackIndex,
                             int numTracks)
{
  wxBusyCursor busy;
  wxYield();

  wxASSERT(t0 <= t1);
  
  int s0 = (int)((t0 - t->tOffset) * t->rate);
  int s1 = (int)((t1 - t->tOffset) * t->rate);
  
  if (s0 < 0)
    s0 = 0;
  if (s1 >= t->numSamples)
    s1 = t->numSamples;
  
  if (s0 >= s1 || s0>=t->numSamples || s1<0)
    return false;

  return DoIt(t, s0, s1-s0);
}


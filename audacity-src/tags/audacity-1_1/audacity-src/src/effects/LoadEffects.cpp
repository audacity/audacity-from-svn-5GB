/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include "Effect.h"

#include "Amplify.h"
#include "BassBoost.h"
#include "Compressor.h"
#include "Echo.h"
#include "Fade.h"
#include "Filter.h"
#include "Invert.h"
#include "NoiseRemoval.h"
#include "Phaser.h"
#include "Reverse.h"
#include "Wahwah.h"

#ifdef USE_WAVELET
#include "WaveletDenoise.h"
#endif

#ifdef __WXMAC__
#include "VST/LoadVSTMac.h"
#endif

#ifdef __WXMSW__
#include "VST/LoadVSTWin.h"
#endif

#ifdef USE_LADSPA
#include "ladspa/LoadLadspa.h"
#endif

void LoadEffects(wxString searchDir)
{
   Effect::RegisterEffect(new EffectAmplify(), false);
   Effect::RegisterEffect(new EffectBassBoost(), false);
   Effect::RegisterEffect(new EffectCompressor(), false);
   Effect::RegisterEffect(new EffectEcho(), false);
   Effect::RegisterEffect(new EffectFadeIn(), false);
   Effect::RegisterEffect(new EffectFadeOut(), false);
   Effect::RegisterEffect(new EffectFilter(), false);
   Effect::RegisterEffect(new EffectInvert(), false);
   Effect::RegisterEffect(new EffectNoiseRemoval(), false);
   Effect::RegisterEffect(new EffectPhaser(), false);
   Effect::RegisterEffect(new EffectReverse(), false);
   Effect::RegisterEffect(new EffectWahwah(), false);

#ifdef USE_WAVELET
   Effect::RegisterEffect(new EffectWaveletDenoise(), false);
#endif

#if defined(__WXMAC__) || defined(__WXMSW__)
   LoadVSTPlugins(searchDir);
#endif

#ifdef USE_LADSPA
   LoadLadspaPlugins();
#endif

   // TODO: sort
}

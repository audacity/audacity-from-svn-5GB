/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Effect.h"

#include "Amplify.h"
#include "BassBoost.h"
#include "Compressor.h"
#include "Echo.h"
#include "Fade.h"
#include "Filter.h"
#include "NoiseRemoval.h"
#include "Phaser.h"
#include "Reverse.h"
#include "Wahwah.h"
#include "WaveletDenoise.h"

#ifdef __WXMAC__
#include "VST/LoadVSTMac.h"
#endif

#ifdef __WXMSW__
#include "VST/LoadVSTWin.h"
#endif

#ifdef __WXGTK__
#include "ladspa/LoadLadspa.h"
#endif

void LoadEffects()
{
   Effect::RegisterEffect(new EffectAmplify(), false);
   Effect::RegisterEffect(new EffectBassBoost(), false);
   Effect::RegisterEffect(new EffectCompressor(), false);
   Effect::RegisterEffect(new EffectEcho(), false);
   Effect::RegisterEffect(new EffectFadeIn(), false);
   Effect::RegisterEffect(new EffectFadeOut(), false);
   Effect::RegisterEffect(new EffectFilter(), false);
   Effect::RegisterEffect(new EffectNoiseRemoval(), false);
   Effect::RegisterEffect(new EffectPhaser(), false);
   Effect::RegisterEffect(new EffectReverse(), false);
   Effect::RegisterEffect(new EffectWahwah(), false);
   Effect::RegisterEffect(new EffectWaveletDenoise(), false);

#if defined(__WXMAC__) || defined(__WXMSW__)
   LoadVSTPlugins();
#endif

#if defined(__WXGTK__)
   LoadLadspaPlugins();
#endif

   // TODO: sort
}

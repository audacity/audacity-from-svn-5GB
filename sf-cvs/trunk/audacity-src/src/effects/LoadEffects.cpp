/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include "Effect.h"

#include "Amplify.h"
#include "AvcCompressor.h"
#include "BassBoost.h"
#include "Compressor.h"
#include "Echo.h"
#include "Fade.h"
#include "Filter.h"
#include "Invert.h"
#include "NoiseRemoval.h"
#include "Phaser.h"
#include "Reverse.h"
#include "ToneGen.h"
#include "Wahwah.h"
#include "LoadEffects.h"

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

void LoadEffects()
{
   Effect::RegisterEffect(new EffectAmplify());
   Effect::RegisterEffect(new EffectAvcCompressor());
   Effect::RegisterEffect(new EffectBassBoost());
   Effect::RegisterEffect(new EffectCompressor());
   Effect::RegisterEffect(new EffectEcho());
   Effect::RegisterEffect(new EffectFadeIn());
   Effect::RegisterEffect(new EffectFadeOut());
   Effect::RegisterEffect(new EffectFilter());
   Effect::RegisterEffect(new EffectInvert());
   Effect::RegisterEffect(new EffectNoiseRemoval());
   Effect::RegisterEffect(new EffectPhaser());
   Effect::RegisterEffect(new EffectReverse());
   Effect::RegisterEffect(new EffectToneGen());
   Effect::RegisterEffect(new EffectWahwah());


#ifdef USE_WAVELET
   Effect::RegisterEffect(new EffectWaveletDenoise());
#endif

#if defined(__WXMAC__) || defined(__WXMSW__)
   LoadVSTPlugins();
#endif

#ifdef USE_LADSPA
   LoadLadspaPlugins();
#endif

   // TODO: sort
}

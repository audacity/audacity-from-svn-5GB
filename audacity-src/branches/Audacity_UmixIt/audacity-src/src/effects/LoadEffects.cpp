/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include "Effect.h"

#include "Amplify.h"
// #include "AvcCompressor.h"
#include "BassBoost.h"
#include "ChangeSpeed.h"
#include "ClickRemoval.h"
#include "Compressor.h"
#include "Echo.h"
#include "Equalization.h"
#include "Fade.h"
#include "Filter.h"
#include "Invert.h"
#include "Noise.h"
#include "NoiseRemoval.h"
#include "Normalize.h"
#include "Phaser.h"
#include "Repeat.h"
#include "Reverse.h"
#include "Silence.h"
#include "ToneGen.h"
#include "Wahwah.h"
#include "LoadEffects.h"

#ifdef USE_SOUNDTOUCH
#include "ChangePitch.h"
#include "ChangeTempo.h"
#endif

#ifdef USE_WAVELET
#include "WaveletDenoise.h"
#endif

#ifdef USE_NYQUIST
#include "nyquist/LoadNyquist.h"
#endif

#ifdef __WXMAC__
#include "VST/LoadVSTMac.h"
#endif

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include "VST/LoadVSTWin.h"
#endif

#ifdef USE_LADSPA
#include "ladspa/LoadLadspa.h"
#endif

void LoadEffects()
{
   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      // Generate menu
      Effect::RegisterEffect(new EffectNoise());
      Effect::RegisterEffect(new EffectSilence());
      Effect::RegisterEffect(new EffectToneGen());
   #endif

   // Effect menu
   
   Effect::RegisterEffect(new EffectAmplify());

   //Commented out now that the Compressor effect works better
   //Effect::RegisterEffect(new EffectAvcCompressor());

   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      Effect::RegisterEffect(new EffectBassBoost());
      Effect::RegisterEffect(new EffectChangeSpeed());
   #endif
	#ifdef USE_SOUNDTOUCH
      #if (AUDACITY_BRANDING != BRAND_THINKLABS)
		   Effect::RegisterEffect(new EffectChangePitch());
      #endif
		Effect::RegisterEffect(new EffectChangeTempo());
	#endif
   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      Effect::RegisterEffect(new EffectClickRemoval());
      Effect::RegisterEffect(new EffectCompressor());
      Effect::RegisterEffect(new EffectEcho());
   #endif
   Effect::RegisterEffect(new EffectEqualization());
   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      Effect::RegisterEffect(new EffectFadeIn());
      Effect::RegisterEffect(new EffectFadeOut());
   #endif
   Effect::RegisterEffect(new EffectFilter());
   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      Effect::RegisterEffect(new EffectInvert());
      Effect::RegisterEffect(new EffectNoiseRemoval());
   #endif
   Effect::RegisterEffect(new EffectNormalize());
   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      Effect::RegisterEffect(new EffectPhaser());
      Effect::RegisterEffect(new EffectRepeat());
      Effect::RegisterEffect(new EffectReverse());
      Effect::RegisterEffect(new EffectWahwah());
   #endif

   // Analyze menu
   // [nothing built-in, but plug-ins might go here]


#if (AUDACITY_BRANDING != BRAND_THINKLABS)
   #ifdef USE_WAVELET
      Effect::RegisterEffect(new EffectWaveletDenoise());
   #endif
#endif

// Thinklabs wants High-pass and Low-pass only, so LoadNyquistPlugins, 
// but install only those two in the folder.
#ifdef USE_NYQUIST
   LoadNyquistPlugins();
#endif

#if (AUDACITY_BRANDING != BRAND_THINKLABS)
   #if defined(__WXMAC__) || defined(__WXMSW__)  && !defined(__CYGWIN__)
      LoadVSTPlugins();
   #endif

   #ifdef USE_LADSPA
      LoadLadspaPlugins();
   #endif
#endif
}

void UnloadEffects()
{
   Effect::UnregisterEffects();
}

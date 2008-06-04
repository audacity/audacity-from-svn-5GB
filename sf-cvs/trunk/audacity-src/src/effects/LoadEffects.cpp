/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include "LoadEffects.h"

#include "EffectManager.h"

#include "Amplify.h"
// #include "AvcCompressor.h"
#include "AutoDuck.h"
#include "BassBoost.h"
#include "ChangeSpeed.h"
#include "ClickRemoval.h"
#include "Compressor.h"
#include "DtmfGen.h"
#include "Echo.h"
#include "Equalization.h"
#include "Fade.h"
#include "Invert.h"
#include "Leveller.h"
#include "Noise.h"
#include "NoiseRemoval.h"
#include "Normalize.h"
#include "Phaser.h"
#include "Repair.h"
#include "Repeat.h"
#include "Reverse.h"
#include "Silence.h"
#include "StereoToMono.h"
#include "ToneGen.h"
#include "TruncSilence.h"
#include "Wahwah.h"

#include "FindClipping.h"

#ifdef USE_SOUNDTOUCH
#include "ChangePitch.h"
#include "ChangeTempo.h"
#endif

#ifdef USE_NYQUIST
#include "nyquist/LoadNyquist.h"
#endif

#ifdef USE_AUDIO_UNITS
#include "audiounits/LoadAudioUnits.h"
#endif

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include "VST/LoadVSTWin.h"
#endif

#ifdef USE_LADSPA
#include "ladspa/LoadLadspa.h"
#endif

#ifdef USE_VAMP
#include "vamp/LoadVamp.h"
#endif


void LoadEffects()
{

   EffectManager& em = EffectManager::Get();

#ifdef EFFECT_CATEGORIES
   
   // Create effect category graph. These categories and relationships
   // are taken from revision 2 of lv2.ttl, loaders for other plugin systems 
   // (such as LADSPA/LRDF) should map their categories to these ones when 
   // applicable. Individual LADSPA/LRDF and LV2 plugins can add new 
   // categories and make them subcategories of the existing ones, but not 
   // add subcategory relationships between these categories.
   //
   // We need some persistent, global identifiers for categories - LRDF
   // and LV2 uses URI strings so we do that too. The URIs here are the
   // same ones as in lv2.ttl. Category identifiers in other plugin systems
   // must be mapped to URIs by their loaders.

#define LV2PREFIX "http://lv2plug.in/ns/lv2core#"
   
   typedef EffectCategory* CatPtr;
   
   CatPtr gen = em.AddCategory(wxT(LV2PREFIX "GeneratorPlugin"),
                               _("Generator"));
   CatPtr inst = em.AddCategory(wxT(LV2PREFIX "IntrumentPlugin"),
                                _("Instrument"));
   CatPtr osc = em.AddCategory(wxT(LV2PREFIX "OscillatorPlugin"),
                               _("Oscillator"));
   CatPtr util = em.AddCategory(wxT(LV2PREFIX "UtilityPlugin"),
                                _("Utility"));
   CatPtr conv = em.AddCategory(wxT(LV2PREFIX "ConverterPlugin"),
                                _("Converter"));
   CatPtr anal = em.AddCategory(wxT(LV2PREFIX "AnalyserPlugin"),
                                _("Analyser"));
   CatPtr mix = em.AddCategory(wxT(LV2PREFIX "MixerPlugin"),
                               _("Mixer"));
   CatPtr sim = em.AddCategory(wxT(LV2PREFIX "SimulatorPlugin"),
                               _("Simulator"));
   CatPtr del = em.AddCategory(wxT(LV2PREFIX "DelayPlugin"),
                               _("Delay"));
   CatPtr mod = em.AddCategory(wxT(LV2PREFIX "ModulatorPlugin"),
                               _("Modulator"));
   CatPtr rev = em.AddCategory(wxT(LV2PREFIX "ReverbPlugin"),
                               _("Reverb"));
   CatPtr phas = em.AddCategory(wxT(LV2PREFIX "PhaserPlugin"),
                                _("Phaser"));
   CatPtr flng = em.AddCategory(wxT(LV2PREFIX "FlangerPlugin"),
                                _("Flanger"));
   CatPtr chor = em.AddCategory(wxT(LV2PREFIX "ChorusPlugin"),
                                _("Chorus"));
   CatPtr flt = em.AddCategory(wxT(LV2PREFIX "FilterPlugin"),
                               _("Filter"));
   CatPtr lp = em.AddCategory(wxT(LV2PREFIX "LowpassPlugin"),
                              _("Lowpass"));
   CatPtr bp = em.AddCategory(wxT(LV2PREFIX "BandpassPlugin"),
                              _("Bandpass"));
   CatPtr hp = em.AddCategory(wxT(LV2PREFIX "HighpassPlugin"),
                              _("Highpass"));
   CatPtr comb = em.AddCategory(wxT(LV2PREFIX "CombPlugin"),
                                _("Comb"));
   CatPtr alp = em.AddCategory(wxT(LV2PREFIX "AllpassPlugin"),
                               _("Allpass"));
   CatPtr eq = em.AddCategory(wxT(LV2PREFIX "EQPlugin"),
                              _("Equaliser"));
   CatPtr peq = em.AddCategory(wxT(LV2PREFIX "ParaEQPlugin"),
                               _("Parametric"));
   CatPtr meq = em.AddCategory(wxT(LV2PREFIX "MultiEQPlugin"),
                               _("Multiband"));
   CatPtr spec = em.AddCategory(wxT(LV2PREFIX "SpectralPlugin"),
                                _("Spectral Processor"));
   CatPtr ptch = em.AddCategory(wxT(LV2PREFIX "PitchPlugin"),
                                _("Pitch Shifter"));
   CatPtr amp = em.AddCategory(wxT(LV2PREFIX "AmplifierPlugin"),
                               _("Amplifier"));
   CatPtr dist = em.AddCategory(wxT(LV2PREFIX "DistortionPlugin"),
                                _("Distortion"));
   CatPtr shp = em.AddCategory(wxT(LV2PREFIX "WaveshaperPlugin"),
                               _("Waveshaper"));
   CatPtr dyn = em.AddCategory(wxT(LV2PREFIX "DynamicsPlugin"),
                               _("Dynamics Processor"));
   CatPtr cmp = em.AddCategory(wxT(LV2PREFIX "CompressorPlugin"),
                               _("Compressor"));
   CatPtr exp = em.AddCategory(wxT(LV2PREFIX "ExpanderPlugin"),
                               _("Expander"));
   CatPtr lim = em.AddCategory(wxT(LV2PREFIX "LimiterPlugin"),
                               _("Limiter"));
   CatPtr gate = em.AddCategory(wxT(LV2PREFIX "GatePlugin"),
                                _("Gate"));
   
   em.AddCategoryParent(inst, gen);
   em.AddCategoryParent(osc, gen);
   em.AddCategoryParent(conv, util);
   em.AddCategoryParent(anal, util);
   em.AddCategoryParent(mix, util);
   em.AddCategoryParent(rev, sim);
   em.AddCategoryParent(rev, del);
   em.AddCategoryParent(phas, mod);
   em.AddCategoryParent(flng, mod);
   em.AddCategoryParent(chor, mod);
   em.AddCategoryParent(lp, flt);
   em.AddCategoryParent(bp, flt);
   em.AddCategoryParent(hp, flt);
   em.AddCategoryParent(comb, flt);
   em.AddCategoryParent(alp, flt);
   em.AddCategoryParent(eq, flt);
   em.AddCategoryParent(peq, eq);
   em.AddCategoryParent(meq, eq);
   em.AddCategoryParent(ptch, spec);
   em.AddCategoryParent(shp, dist);
   em.AddCategoryParent(cmp, dyn);
   em.AddCategoryParent(exp, dyn);
   em.AddCategoryParent(lim, dyn);
   em.AddCategoryParent(gate, dyn);

#endif
   
   // Generate menu
   em.RegisterEffect(new EffectNoise());
   em.RegisterEffect(new EffectSilence());
   em.RegisterEffect(new EffectToneGen());
   em.RegisterEffect(new EffectDtmf());
   // A little magic to convert 'Tone' to chirps.
   em.RegisterEffect(&((new EffectToneGen())->EnableForChirps()));

   // Effect menu
   
   em.RegisterEffect(new EffectAmplify());

   //Commented out now that the Compressor effect works better
   //em.RegisterEffect(new EffectAvcCompressor());

   const int SIMPLE_EFFECT = BUILTIN_EFFECT | PROCESS_EFFECT;
   // In this list, designating an effect as 'SIMPLE_EFFECT' just means
   // that it should be included in even the most basic of menus.
   // This was introduced for CleanSpeech mode.
   
   em.RegisterEffect(new EffectAutoDuck());
   em.RegisterEffect(new EffectBassBoost());
   em.RegisterEffect(new EffectChangeSpeed());
	#ifdef USE_SOUNDTOUCH
		em.RegisterEffect(new EffectChangePitch());
		em.RegisterEffect(new EffectChangeTempo());
	#endif
   em.RegisterEffect(new EffectClickRemoval());
   em.RegisterEffect(new EffectCompressor());
   em.RegisterEffect(new EffectEcho());
   em.RegisterEffect(new EffectEqualization());
   em.RegisterEffect(new EffectFadeIn(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectFadeOut(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectInvert());
   em.RegisterEffect(new EffectLeveller(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectNoiseRemoval(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectNormalize(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectPhaser());
   em.RegisterEffect(new EffectRepair());
   em.RegisterEffect(new EffectRepeat());
   em.RegisterEffect(new EffectReverse());
   em.RegisterEffect(new EffectStereoToMono(), HIDDEN_EFFECT);// NOT in normal effects list.
   em.RegisterEffect(new EffectTruncSilence(), SIMPLE_EFFECT);
   em.RegisterEffect(new EffectWahwah());

   // Analyze menu
   em.RegisterEffect(new EffectFindClipping(), ANALYZE_EFFECT);

#ifdef USE_NYQUIST
   LoadNyquistPlugins();
#endif

#ifdef USE_LADSPA
   LoadLadspaPlugins();
#endif

#ifdef USE_AUDIO_UNITS
   LoadAudioUnits();
#endif

#ifdef USE_VAMP
   LoadVampPlugins();
#endif
   
}

void UnloadEffects()
{
   EffectManager::Get().UnregisterEffects();

#ifdef USE_LADSPA
   UnloadLadspaPlugins();
#endif

#ifdef USE_VAMP
   UnloadVampPlugins();
#endif
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 3a6c5930-9015-4fd0-96f2-2eb31da1c785


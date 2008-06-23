/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVamp.cpp

  Chris Cannam

**********************************************************************/

#include "../EffectManager.h"
#include "VampEffect.h"

#include <iostream>
#include <map>

using namespace Vamp;
using namespace Vamp::HostExt;

#ifdef EFFECT_CATEGORIES

static std::map<wxString, wxString> gCategoryMap;

#define VAMP(S) wxT("http://audacityteam.org/namespace#VampCategories") wxT(S)
#define ATEAM(S) wxT("http://audacityteam.org/namespace#") wxT(S)

/** Initialise the data structure that maps locally generated URI strings to
    internal ones. */
static void InitCategoryMap()
{
   gCategoryMap[VAMP("/Time")] = ATEAM("TimeAnalyser");
   gCategoryMap[VAMP("/Time/Onsets")] = ATEAM("OnsetDetector");
}

/** Map the generated VAMP category URI to the internal ones. */
static wxString MapCategoryUri(const wxString& uri)
{
   std::map<wxString, wxString>::const_iterator iter;
   iter = gCategoryMap.find(uri);
   if (iter != gCategoryMap.end())
      return iter->second;
   return uri;
}

/** Generate category URIs for all levels in a VAMP category hierarchy,
    add them to the EffectManager and return the most detailed one. */
static wxString VampHierarchyToUri(const PluginLoader::PluginCategoryHierarchy& h)
{
   // Else, generate URIs and add them to the EffectManager
   EffectManager& em = EffectManager::Get();
   wxString vampCategory = 
      wxString::FromAscii("http://audacityteam.org/namespace#VampCategories");
   EffectCategory* parent = 
      em.LookupCategory(wxT("http://lv2plug.in/ns/lv2core#AnalyserPlugin"));
   if (parent) {
      for (size_t c = 0; c < h.size(); ++c) {
         vampCategory += wxT("/");
         wxString catName = wxString::FromAscii(h[c].c_str());
         vampCategory += catName;
         EffectCategory* ec = em.AddCategory(MapCategoryUri(vampCategory),
                                             catName);
         em.AddCategoryParent(ec, parent);
         parent = ec;
      }
   }
   return MapCategoryUri(vampCategory);
}

#endif

void LoadVampPlugins()
{

#ifdef EFFECT_CATEGORIES
   InitCategoryMap();
#endif

   PluginLoader *loader = PluginLoader::getInstance();
   
   EffectManager& em = EffectManager::Get();
   
   PluginLoader::PluginKeyList keys = loader->listPlugins();

   for (PluginLoader::PluginKeyList::iterator i = keys.begin();
        i != keys.end(); ++i) {

      Plugin *vp = loader->loadPlugin(*i, 48000); // rate doesn't matter here
      if (!vp) continue;
      
#ifdef EFFECT_CATEGORIES

      PluginLoader::PluginCategoryHierarchy category = 
         loader->getPluginCategory(*i);
      wxString vampCategory = VampHierarchyToUri(category);

#endif
      
      // For now we only support "time instants" conformable outputs

      Plugin::OutputList outputs = vp->getOutputDescriptors();

      int n = 0;

      bool hasParameters = !vp->getParameterDescriptors().empty();
      
      for (Plugin::OutputList::iterator j = outputs.begin();
           j != outputs.end(); ++j) {
         
         if (j->hasFixedBinCount &&
             j->binCount == 0 &&
             j->sampleType == Plugin::OutputDescriptor::VariableSampleRate) {

            wxString name = LAT1CTOWX(vp->getName().c_str());

            if (outputs.size() > 1) {
               // This is not the plugin's only output.
               // Use "plugin name: output name" as the effect name,
               // unless the output name is the same as the plugin name
               wxString outputName = LAT1CTOWX(j->name.c_str());
               if (outputName != name) {
                  name = wxString::Format(wxT("%s: %s"),
                                          name.c_str(), outputName.c_str());
               }
            }

#ifdef EFFECT_CATEGORIES
            VampEffect *effect = new VampEffect(*i, n, hasParameters, name,
                                                vampCategory);
#else
            VampEffect *effect = new VampEffect(*i, n, hasParameters, name);
#endif
            em.RegisterEffect(effect);
         }

         ++n;
      }

      delete vp;
   }
}

void UnloadVampPlugins()
{
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
// arch-tag: 21a6d3fe-1003-4cec-a44c-4a16335b5cda


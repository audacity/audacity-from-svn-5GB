/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*******************************************************************//**

\file
Functions that find and load all LV2 plugins on the system.

*//*******************************************************************/


#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <wx/dynlib.h>
#include <wx/hashmap.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "../../Experimental.h"
#include "../../Internat.h"
#include "../EffectManager.h"
#include "LV2Effect.h"

#include "LoadLV2.h"


SLV2World gWorld = 0;

SLV2Value gAudioPortClass;
SLV2Value gControlPortClass;
SLV2Value gInputPortClass;
SLV2Value gOutputPortClass;


/** This function determines whether a plugin should be displayed or not.
    It checks if the required features and non-optional port types are
    supported. */
static bool PluginFilter(SLV2Plugin plug) {
   
   // We don't support any features at all, so if the plugin requires
   // any we skip it.
   SLV2Values req = slv2_plugin_get_required_features(plug);
   if (slv2_values_size(req) > 0)
      return false;
   
   // We only understand audio and control ports, so if there are any others
   // we skip the plugin.
   uint32_t nPorts = slv2_plugin_get_num_ports(plug);
   for (uint32_t i = 0; i < nPorts; ++i) {
      SLV2Port port = slv2_plugin_get_port_at_index(plug, i);
      if (!slv2_port_is_a(mData, port, gAudioPortClass) &&
          !slv2_port_is_a(mData, port, gControlPortClass))
         return false;
   }
   
   return true;
}

void LoadLV2Plugins() {
   
   EffectManager& em = EffectManager::Get();
   
   // If gWorld isn't 0 we have already initialised SLV2 - unload all plugins
   // and initialise again.
   if (gWorld)
      UnloadLV2Plugins();
   
   // Try to initialise SLV2, or return.
   gWorld = slv2_world_new();
   if (!gWorld) {
      std::cerr<<"Could not initialise SLV2!"<<std::endl;
      return;
   }
   
   gAudioPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_AUDIO);
   gControlPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_CONTROL);
   gInputPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_INPUT);
   gOutputPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_OUTPUT);
   
#ifdef EFFECT_CATEGORIES
   
   // Add all LV2 categories and their relationships
   SLV2PluginClasses classes = slv2_world_get_plugin_classes(gWorld);
   for (unsigned index = 0; index < slv2_plugin_classes_size(classes);++index){
      SLV2PluginClass c = slv2_plugin_classes_get_at(classes, index);
      em.AddCategory(wxString::FromUTF8(slv2_value_as_uri(slv2_plugin_class_get_uri(c))), 
                     wxString::FromUTF8(slv2_value_as_string(slv2_plugin_class_get_label(c))));
   }
   for (unsigned index = 0; index < slv2_plugin_classes_size(classes);++index){
      SLV2PluginClass c = slv2_plugin_classes_get_at(classes, index);
      SLV2PluginClasses ch = slv2_plugin_class_get_children(c);
      EffectCategory* pCat = em.LookupCategory(wxString::FromUTF8(slv2_value_as_uri(slv2_plugin_class_get_uri(c))));
      for (unsigned j = 0; j < slv2_plugin_classes_size(ch); ++j) {
         EffectCategory* chCat = em.LookupCategory(wxString::FromUTF8(slv2_value_as_uri(slv2_plugin_class_get_uri(slv2_plugin_classes_get_at(ch, j)))));
         if (chCat && pCat) {
            em.AddCategoryParent(chCat, pCat);
         }
      }
   }

#endif
   
   // Retrieve data about all plugins
   slv2_world_load_all(gWorld);
   SLV2Plugins plugs = slv2_world_get_plugins_by_filter(gWorld, &PluginFilter);
   
   // Iterate over all plugins and register them with the EffectManager
   for (unsigned index = 0; index < slv2_plugins_size(plugs); ++index) {
      SLV2Plugin plug = slv2_plugins_get_at(plugs, index);
      std::set<wxString> cats;
      cats.insert(wxString::FromUTF8(slv2_value_as_uri(slv2_plugin_class_get_uri(slv2_plugin_get_class(plug)))));
      LV2Effect *effect = new LV2Effect(plug, cats);
      em.RegisterEffect(effect);
      std::cerr<<"Loaded LV2 \""<<slv2_value_as_string(slv2_plugin_get_name(plug))<<"\""<<std::endl;
   }
   
   // Deallocate the plugin list (but not the plugins)
   slv2_plugins_free(gWorld, plugs);
}

void UnloadLV2Plugins()
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
// arch-tag: 2f903c43-d9fe-4875-bb10-b9ae843f36a8


/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVamp.cpp

  Chris Cannam

**********************************************************************/

#include "VampEffect.h"

#include <iostream>

using namespace Vamp;
using namespace Vamp::HostExt;

void LoadVampPlugins()
{
   PluginLoader *loader = PluginLoader::getInstance();

   PluginLoader::PluginKeyList keys = loader->listPlugins();

   for (PluginLoader::PluginKeyList::iterator i = keys.begin();
        i != keys.end(); ++i) {

      Plugin *vp = loader->loadPlugin(*i, 48000); // rate doesn't matter here
      if (!vp) continue;

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

            VampEffect *effect = new VampEffect(*i, n, hasParameters, name);
            Effect::RegisterEffect(effect);
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


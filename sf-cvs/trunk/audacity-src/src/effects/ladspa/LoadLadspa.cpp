/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadLadspa.cpp

  Dominic Mazzoni

  From the ladspa docs:
  "To allow multiple hosts to
   share plugin types, hosts may wish to check for environment
   variable LADSPA_PATH. If present, this should contain a
   colon-separated path indicating directories that should be searched
   (in order) when loading plugin types."

**********************************************************************/

#include <wx/dynlib.h>
#include <wx/log.h>
#include <wx/string.h>

wxString searchDir =
"/home/dmazzoni/install/ladspa_sdk/plugins";

#include "ladspa.h"

#include "LadspaEffect.h"

void LoadLadspaPlugins()
{
   wxString fname =
      wxFindFirstFile((const char *)(searchDir + wxFILE_SEP_PATH + "*.so"));

   while (fname != "") {
      wxLogNull logNo;
      wxDllType libHandle = NULL;
      LADSPA_Descriptor_Function mainFn;
      
      libHandle = wxDllLoader::LoadLibrary(fname);
      
      mainFn = (LADSPA_Descriptor_Function)
         wxDllLoader::GetSymbol(libHandle,
                                "ladspa_descriptor");

      if (mainFn) {
         int index = 0;
         const LADSPA_Descriptor *data;

         data = mainFn(index);
         while(data) {
            LadspaEffect *effect = new LadspaEffect(data);
            Effect::RegisterEffect(effect);

            // Get next plugin
            index++;
            data = mainFn(index);            
         }
      }

      fname = wxFindNextFile();
   }
}

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
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "ladspa.h"

#include "LadspaEffect.h"

void SearchLadspaInDir(wxString dir)
{
   wxLogNull nolog;
   
   wxString fname =
      wxFindFirstFile((const char *)(dir + wxFILE_SEP_PATH + "*.so"));

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
            Effect::RegisterEffect(effect, true);

            // Get next plugin
            index++;
            data = mainFn(index);            
         }
      }

      fname = wxFindNextFile();
   }
}

void LoadLadspaPlugins()
{
   wxStringList paths;

   wxString pathVar = wxGetenv("LADSPA_PATH");
   if (pathVar != "") {
      wxString onePath = pathVar.BeforeFirst(wxPATH_SEP[0]);
      pathVar = pathVar.AfterFirst(wxPATH_SEP[0]);
      if (!paths.Member(onePath))
         paths.Add(onePath);
   }

   #ifdef __WXGTK__
   wxString stdPath = "/usr/local/lib/ladspa";
   if (!paths.Member(stdPath))
      paths.Add(stdPath);
   stdPath = "/usr/lib/ladspa";
   if (!paths.Member(stdPath))
      paths.Add(stdPath);
   #endif

   for ( wxStringList::Node *node = paths.GetFirst();
         node;
         node = node->GetNext() ) {
      SearchLadspaInDir(node->GetData());
   }

}

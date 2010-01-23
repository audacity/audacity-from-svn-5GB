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

// For temporary dlopen() fix.
#include <dlfcn.h>

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
      LADSPA_Descriptor_Function mainFn = NULL;

      // XXX: The following code uses the wxWindows DLL class, which does
      //      not allow us to control the flags passed to dlopen().  This
      //      leads to potential segfault bugs when plugins have conflicting
      //      symbols, so until wxWindows adds this capability we are calling
      //      dlopen() by hand.
      //
      // wxDllType libHandle = NULL;
      //  
      // libHandle = wxDllLoader::LoadLibrary(fname);
      //
      // mainFn = (LADSPA_Descriptor_Function)
      //    wxDllLoader::GetSymbol(libHandle,
      //                           "ladspa_descriptor");
      
#ifdef __WXGTK__
      void *libHandle = NULL;

      libHandle = dlopen(fname, RTLD_LAZY);

      mainFn = (LADSPA_Descriptor_Function)
                  dlsym(libHandle, "ladspa_descriptor");
#endif

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

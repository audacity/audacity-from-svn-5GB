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
#ifdef __WXMAC__
#else
#include <dlfcn.h>
#endif  //  __WXMAC__

#include <wx/dynlib.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "ladspa.h"

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "LadspaEffect.h"

void LoadLadspaEffect(wxString fname)
{
   wxLogNull logNo;
   LADSPA_Descriptor_Function mainFn = NULL;
   
#ifdef __WXGTK__
   void *libHandle = NULL;
   
   libHandle = dlopen(fname, RTLD_LAZY);
   
   mainFn = (LADSPA_Descriptor_Function)
      dlsym(libHandle, "ladspa_descriptor");
#else
   // The following code uses the wxWindows DLL class, which does
   // not allow us to control the flags passed to dlopen().  This
   // leads to potential segfault bugs when plugins have conflicting
   // symbols, so until wxWindows adds this capability we are calling
   // dlopen() by hand under WXGTK, above...

   wxDllType libHandle = NULL;
     
   libHandle = wxDllLoader::LoadLibrary(fname);
   mainFn = (LADSPA_Descriptor_Function)
      wxDllLoader::GetSymbol(libHandle,
                             "ladspa_descriptor");
#endif

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
}

void LoadLadspaPlugins()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;
   wxString pathVar;
   unsigned int i;

   pathVar = wxGetenv("LADSPA_PATH");
   if (pathVar != "")
      wxGetApp().AddMultiPathsToPathList(pathVar, pathList);

   #ifdef __WXGTK__
   wxGetApp().AddUniquePathToPathList(INSTALL_PREFIX "/ladspa", pathList);
   wxGetApp().AddUniquePathToPathList("/usr/local/lib/ladspa", pathList);
   wxGetApp().AddUniquePathToPathList("/usr/lib/ladspa", pathList);
   #endif

   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + "ladspa",
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + "plugins",
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + "plug-ins",
                                         pathList);
   }

   wxGetApp().FindFilesInPathList("*.so", pathList, wxFILE, files);

   for(i=0; i<files.GetCount(); i++)
      LoadLadspaEffect(files[i]);
}

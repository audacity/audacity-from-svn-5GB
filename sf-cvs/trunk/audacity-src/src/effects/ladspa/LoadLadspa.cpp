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

#define descriptorFnName "ladspa_descriptor"

#include <wx/dynlib.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "ladspa.h"

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "../../Internat.h"
#include "LadspaEffect.h"

void LoadLadspaEffect(wxString fname)
{
   wxLogNull logNo;
   LADSPA_Descriptor_Function mainFn = NULL;

   // As a courtesy to some plug-ins that might be bridges to
   // open other plug-ins, we set the current working
   // directory to be the plug-in's directory.

   wxString saveOldCWD = ::wxGetCwd();
   wxString prefix = ::wxPathOnly(fname);
   ::wxSetWorkingDirectory(prefix);

   wxDynamicLibrary* pDLL = new wxDynamicLibrary();
   if (pDLL && pDLL->Load(fname, wxDL_LAZY)) {
      mainFn = (LADSPA_Descriptor_Function)(pDLL->GetSymbol(wxT(descriptorFnName)));
   }

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

   ::wxSetWorkingDirectory(saveOldCWD);
}

void LoadLadspaPlugins()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;
   wxString pathVar;
   unsigned int i;

   pathVar = wxGetenv(wxT("LADSPA_PATH"));
   if (pathVar != wxT(""))
      wxGetApp().AddMultiPathsToPathList(pathVar, pathList);

   #ifdef __WXGTK__
   wxGetApp().AddUniquePathToPathList(wxT(INSTALL_PREFIX) wxT("/ladspa"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT("/usr/local/lib/ladspa"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT("/usr/lib/ladspa"), pathList);
   #endif

   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("ladspa"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plugins"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plug-ins"),
                                         pathList);
   }

   #ifdef __WXMSW__
   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, wxFILE, files);   
   #else
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, wxFILE, files);
   #endif

   for(i=0; i<files.GetCount(); i++)
      LoadLadspaEffect(files[i]);
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


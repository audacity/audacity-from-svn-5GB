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
#include <wx/hashmap.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "ladspa.h"

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "../../Internat.h"
#include "LadspaEffect.h"

WX_DEFINE_ARRAY(wxDynamicLibrary*, DL_Array);

DL_Array ladspa_dls;

void LoadLadspaEffect(wxSortedArrayString &uniq, wxString fname,
DL_Array &dls)
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
   dls.push_back(pDLL);
   if (pDLL && pDLL->Load(fname, wxDL_LAZY)) {
      mainFn = (LADSPA_Descriptor_Function)(pDLL->GetSymbol(wxT(descriptorFnName)));
   }

   if (mainFn) {
      int index = 0;
      const LADSPA_Descriptor *data;

      data = mainFn(index);
      while(data) {

         wxString uniqid = wxString::Format(wxT("%08x-%s"), data->UniqueID, LAT1CTOWX(data->Label).c_str());
         if (uniq.Index(uniqid) == wxNOT_FOUND) {
            uniq.Add(uniqid);
            LadspaEffect *effect = new LadspaEffect(data);
            Effect::RegisterEffect(effect);
         }
            
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
   wxSortedArrayString uniq;
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

   #ifdef __WXMAC__
   wxGetApp().AddUniquePathToPathList(wxT("~/Library/Audio/Plug-Ins/LADSPA"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT("/Library/Audio/Plug-Ins/LADSPA"), pathList);
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
      LoadLadspaEffect(uniq, files[i], ladspa_dls);
}

void UnloadLadspaPlugins()
{
   int count=ladspa_dls.GetCount();
   for (int i=0; i<count; i++) 
   {
      delete ladspa_dls[i];
   }
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


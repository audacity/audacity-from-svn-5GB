/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/filefn.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "Nyquist.h"

void SearchNyquistInDir(wxString dir)
{
   wxLogNull logNo;
   wxString fname =
      wxFindFirstFile((const char *)(dir + wxFILE_SEP_PATH + "*.ny"));

   while (fname != "") {
      EffectNyquist *effect = new EffectNyquist(fname);
      if (effect->LoadedNyFile())
         Effect::RegisterEffect(effect);
      else
         delete effect;
 
      fname = wxFindNextFile();
   }
}

void LoadNyquistPlugins()
{
   wxStringList paths;

   // Create one "interactive Nyquist"
   EffectNyquist *effect = new EffectNyquist("");
   Effect::RegisterEffect(effect);

   // Load .ny plug-ins
   wxString pathVar = wxGetenv("NYQUIST_PATH");
   if (pathVar != "") {
      wxString onePath = pathVar.BeforeFirst(wxPATH_SEP[0]);
      pathVar = pathVar.AfterFirst(wxPATH_SEP[0]);
      if (!paths.Member(onePath))
         paths.Add(onePath);
   }

   #ifdef __WXGTK__
   wxString stdPath = "/usr/share/audacity/nyquist";
   if (!paths.Member(stdPath))
      paths.Add(stdPath);
   stdPath = "/usr/share/nyquist";
   if (!paths.Member(stdPath))
      paths.Add(stdPath);
   stdPath = ".";
   if (!paths.Member(stdPath))
      paths.Add(stdPath);
   #endif

   for ( wxStringList::Node *node = paths.GetFirst();
         node;
         node = node->GetNext() ) {
      SearchNyquistInDir(node->GetData());
   }
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVSTWin.cpp

  Dominic Mazzoni
  Mark Tomlinson

**********************************************************************/

#include "../../Audacity.h"

#if USE_VST

#include "AudioEffect.hpp"      // VST API

#include "VSTEffect.h"
#include "../../AudacityApp.h"
#include "LoadVSTWin.h"

#include <windows.h>

int audacityVSTID = 1;

extern "C" {

   long audioMaster(AEffect * effect, long opcode, long index,
                    long value, void *ptr, float opt) {
      switch (opcode) {
      case audioMasterVersion:
         return 2;
      case audioMasterCurrentId:return audacityVSTID;
      default:return 0;
      }
   }
   
   typedef AEffect *(*vstPluginMain) (audioMasterCallback audioMaster);
   
   void LoadVSTPlugin(wxString fname) {
      HANDLE hLib = LoadLibrary(fname);
      
      if (hLib != NULL) {
         
         // get the address of the main() function
         
         vstPluginMain pDllMain =
            (vstPluginMain) GetProcAddress((HINSTANCE) hLib, "main");
         
         if (pDllMain != NULL) {
            
            AEffect *theEffect;
            
            theEffect = (pDllMain) (audioMaster);
            
            if (theEffect->magic == kEffectMagic) {
               wxString title = wxFileNameFromPath(fname);
               int len = title.Len();
               if (len > 4 && (title.Mid(len - 4, 1) == "."))
                  title = title.Mid(0, len - 4);
               
               VSTEffect *vst = new VSTEffect(title, theEffect);
               Effect::RegisterEffect(vst);
            }
         }
         
         audacityVSTID++;
      } else {
         FreeLibrary((HINSTANCE) hLib);
      }
   }

   void LoadVSTPlugins() {
      wxArrayString audacityPathList = wxGetApp().audacityPathList;
      wxArrayString pathList;
      wxArrayString files;
      unsigned int i;
      
      for(i=0; i<audacityPathList.GetCount(); i++) {
         wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
         wxGetApp().AddUniquePathToPathList(prefix + "VST",
                                            pathList);
         wxGetApp().AddUniquePathToPathList(prefix + "plugins",
                                            pathList);
         wxGetApp().AddUniquePathToPathList(prefix + "plug-ins",
                                            pathList);
      }

      wxGetApp().FindFilesInPathList("*.dll", pathList, wxFILE, files);
      
      for(i=0; i<files.GetCount(); i++)
         LoadVSTPlugin(files[i]);
   }


};                              // extern "C"

#else

extern "C" {
   void LoadVSTPlugins() {}
}

#endif // USE_VST


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 9f1d7f8b-23a6-4e16-80cd-f5f976f5b213


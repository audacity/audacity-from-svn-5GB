/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVSTWin.cpp

  Mark Tomlinson

**********************************************************************/

#include <windows.h>

#include "AudioEffect.hpp"      // VST API

#include "VSTEffect.h"          // This class's header
#include "../DirManager.h"      // Audacity class which handles data structures

int audacityVSTID = 1;

extern "C" {

   long audioMaster(AEffect * effect, long opcode, long index,
                    long value, void *ptr, float opt) {
      switch (opcode) {
      case audioMasterVersion:
         return 2;
         case audioMasterCurrentId:return audacityVSTID;
         default:return 0;
   }} typedef AEffect *(*vstPluginMain) (audioMasterCallback audioMaster);

   void LoadVSTPlugins() {
      wxString home = DirManager::GetHomeDir();
      wxString pathChar = DirManager::GetPathChar();
      wxString vstDirPath = home + pathChar + "vst";
      wxString fname;

      fname =
          wxFindFirstFile((const char *) (vstDirPath + pathChar +
                                          "*.DLL"));

      while (fname != "") {
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
                  if (len > 4 && (title.Mid(len - 4) == ".DLL"
                                  || title.Mid(len - 4) == ".dll"))
                     title = title.Mid(0, len - 4);

                  VSTEffect *vst = new VSTEffect(title, theEffect);
                  Effect::RegisterEffect(vst, true);
               }
            }

            audacityVSTID++;
         } else {
            FreeLibrary((HINSTANCE) hLib);
         }

         fname = wxFindNextFile();
      }
   }


};                              // extern "C"

/*

How to enumerate symbols:

        long numSymbols;
    
        err = CountSymbols(connID, &numSymbols);
        if (!err)
            for(int y=0; y<numSymbols; y++) {
            
                Str255 symbolName;
            
                err = GetIndSymbol(connID, y, symbolName, &symbolAddress, &symbolClass);
                
                
            }

*/

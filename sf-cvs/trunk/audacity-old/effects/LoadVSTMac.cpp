/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVSTMac.cpp

  Dominic Mazzoni

**********************************************************************/

#include <CodeFragments.h>
#include <Resources.h>

#include "AudioEffect.hpp"      // VST API

#include "VSTEffect.h"          // This class's header
#include "DirManager.h"         // Audacity class which handles data structures

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
          wxFindFirstFile((const char *) (vstDirPath + pathChar + "*"));

      while (fname != "") {
         short resID;
         FSSpec spec;

         wxUnixFilename2FSSpec(fname, &spec);
         resID = FSpOpenResFile(&spec, fsRdPerm);
         Handle codeH;

         int count = Count1Resources('aEff');
         for (int i = 0; i < count; i++) {
            CFragConnectionID connID;
            Ptr mainAddr;
            Str255 errName;
            Str255 fragName;
            char fragNameCStr[256];
            short resID;
            OSType resType;
            OSErr err;

            codeH = Get1IndResource('aEff', count);
            GetResInfo(codeH, &resID, &resType, fragName);
            DetachResource(codeH);
            HLock(codeH);

            err = GetMemFragment(*codeH,
                                 GetHandleSize(codeH),
                                 fragName,
                                 kPrivateCFragCopy,
                                 &connID, (Ptr *) & mainAddr, errName);

            if (err >= 0) {

               Ptr symbolAddress;
               CFragSymbolClass symbolClass;

               err =
                   FindSymbol(connID, "\pmain", &symbolAddress,
                              &symbolClass);
               if (!err) {
                  vstPluginMain pluginMain = (vstPluginMain) symbolAddress;

                  AEffect *theEffect;

                  theEffect = pluginMain(audioMaster);

                  if (theEffect->magic == kEffectMagic) {

                     memcpy(fragNameCStr, &fragName[1], fragName[0]);
                     fragNameCStr[fragName[0]] = 0;

                     VSTEffect *vst =
                         new VSTEffect(wxString(fragNameCStr), theEffect);
                     Effect::RegisterEffect(vst);
                  }
               }
            } else {
               HUnlock(codeH);
            }

            audacityVSTID++;

            // Don't HUnlock unless you don't want to keep it in memory
         }

         CloseResFile(resID);
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

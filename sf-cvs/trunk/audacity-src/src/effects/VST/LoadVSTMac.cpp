/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVSTMac.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../../Audacity.h"

#ifdef __MACOS9__
#include <Resources.h>
#include <Files.h>
#include <Memory.h>
#include <CodeFragments.h>
#endif

#ifdef __MACOSX__
#include <CoreServices/CoreServices.h>
#endif

void wxMacFilename2FSSpec( const char *path , FSSpec *spec ) ;

#include "AEffect.h"
#include "AudioEffect.hpp"        // VST API

#include "VSTEffect.h"            // This class's header
#include "../../DirManager.h"     // Audacity class which handles data structures
#include "LoadVSTMac.h"   

int audacityVSTID = 1;

extern "C" {
   
   long audioMaster(AEffect * effect, long opcode, long index,
                    long value, void *ptr, float opt);
   long audioMaster(AEffect * effect, long opcode, long index,
                    long value, void *ptr, float opt)
   {
      switch (opcode) {
      case audioMasterVersion:
         return 2;
      case audioMasterCurrentId:
         return audacityVSTID;
      default:
         return 0;
      }
   }

   typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

   void LoadVSTPlugins(wxString searchDir) 
   {
      wxString pathChar = DirManager::GetPathChar();
      wxString home = DirManager::GetHomeDir();
      #ifdef __MACOSX__
        wxString vstDirPath = searchDir + pathChar + "vst";
      #else
        wxString vstDirPath = home + pathChar + "vst";
      #endif

      wxString fname;
      
#ifdef __MACOSX__
      audioMasterCallback audioMasterFPtr =
         (audioMasterCallback)NewCFMFromMachO(audioMaster);
#else
    // What is the corrct way of creating an audioMasterCallback in OS 9/Carbon 
	// audioMasterCallback audioMasterFPtr = NULL; 
      audioMasterCallback audioMasterFPtr = audioMaster;
#endif      

      fname = wxFindFirstFile((const char *) (vstDirPath + pathChar + "*"));

      while (fname != "") {
         short   resFileID;
         FSSpec  spec;
         int     count;

         wxMacFilename2FSSpec(fname, &spec);
         resFileID = FSpOpenResFile(&spec, fsRdPerm);
         count = Count1Resources('aEff');

         for (int i = 0; i < count; i++) {
            Handle             codeH;
            CFragConnectionID  connID;
            Ptr                mainAddr;
            Str255             errName;
            Str255             fragName;
            char               fragNameCStr[256];
            short              resID;
            OSType             resType;
            OSErr              err;

            codeH = Get1IndResource('aEff', i+1);
            if (!codeH)
               continue;

            GetResInfo(codeH, &resID, &resType, fragName);
            DetachResource(codeH);
            HLock(codeH);

            err = GetMemFragment(*codeH,
                                 GetHandleSize(codeH),
                                 fragName,
                                 kPrivateCFragCopy,
                                 &connID, (Ptr *) & mainAddr, errName);

            if (!err) {
               vstPluginMain   pluginMain;
               AEffect        *theEffect;

               #ifdef __MACOSX__
               pluginMain = (vstPluginMain)NewMachOFromCFM(mainAddr);
               #else
               pluginMain = (vstPluginMain)mainAddr;
               #endif

               theEffect = pluginMain(audioMasterFPtr);

               if (theEffect->magic == kEffectMagic) {
                  
                  memcpy(fragNameCStr, &fragName[1], fragName[0]);
                  fragNameCStr[fragName[0]] = 0;
                  
                  VSTEffect *vst =
                     new VSTEffect(wxString(fragNameCStr), theEffect);
                  Effect::RegisterEffect(vst, true);
               }

               #ifdef __MACOSX__
               DisposeMachOFromCFM(pluginMain);
               #endif
               
               audacityVSTID++;
            }
         }

         CloseResFile(resFileID);
         fname = wxFindNextFile();
      }
#ifdef __MACOSX__
      DisposeCFMFromMachO(audioMasterFPtr);
#endif  //   __MACOSX__
   }

};                              // extern "C"

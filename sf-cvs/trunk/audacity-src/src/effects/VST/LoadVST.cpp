/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVST.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../../Audacity.h"

#if USE_VST

#if defined(__WXMAC__)
   #include <wx/mac/private.h>
#else
   #include <wx/dynlib.h>
#endif

#if defined(__WXMSW__)
   #include <windows.h>
   #include <shlwapi.h>
   #pragma comment(lib, "shlwapi")
#endif

#include "../../AudacityApp.h"
#include "../../Internat.h"
#include "../EffectManager.h"

#include "VSTEffect.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

static long int audioMaster(AEffect * effect,
                            long int opcode,
                            long int index,
                            long int value,
                            void * ptr,
                            float opt)
{
   // Forward to VSTEffect if it's active
   if (effect && effect->user) {
      VSTEffect *vst = (VSTEffect *) effect->user;
      return vst->audioMaster(effect, opcode, index, value, ptr, opt);
   }

   // Handles operations during initialization...before VSTEffect has had a
   // chance to set its instance pointer.
   switch (opcode)
   {
      case audioMasterVersion:
         return 2;

      case audioMasterCurrentId:
         return audacityVSTID;

      // Ignore these
      case audioMasterIdle:
      case audioMasterWantMidi:
         return 0;

      case audioMasterCanDo:
         wxPrintf(wxT("effect: %p cando: %s\n"), effect, LAT1CTOWX((char *)ptr).c_str());
         return 0;

      default:
#if 1
#if defined(__WXDEBUG__)
#if !defined(__WXMSW__)
         wxPrintf(wxT("effect: %p opcode: %d index: %d value: %d ptr: %p opt: %f user: %p\n"),
                  effect, opcode, index, value, ptr, opt, effect->user);
#else
         wxLogDebug(wxT("effect: %p opcode: %d index: %d value: %d ptr: %p opt: %f user: %p"),
                    effect, opcode, index, value, ptr, opt, effect->user);
#endif
#endif
#endif
         return 0;
   }
}

static void LoadVSTPlugin(const wxString & fname)
{
   vstPluginMain pluginMain;
   void *module = NULL;

#if defined(__WXDEBUG__)
//   wxPrintf(wxT("%s\n"), fname.c_str());
//   wxLogDebug(wxT("%s"), fname.c_str());
#endif

#if defined(__WXMAC__)

   // Remove the 'Contents/Info.plist' portion of the name and create a
   // CFString
   wxString name(wxPathOnly(wxPathOnly(fname)));
   wxMacCFStringHolder path(name);

   // Convert the path to a URL
   CFURLRef urlRef =
      CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                                    path,
                                    kCFURLPOSIXPathStyle,
                                    true);
   if (urlRef == NULL) {
      return;
   }

   // Create the bundle using the URL
   CFBundleRef bundleRef = CFBundleCreate(kCFAllocatorDefault, urlRef);

   // Done with the URL
   CFRelease(urlRef);

   // Bail if the bundle wasn't created
   if (bundleRef == NULL) {
      return;
   }

   // Try to locate the new plugin entry point
   pluginMain = (vstPluginMain)
      CFBundleGetFunctionPointerForName(bundleRef,
                                        CFSTR("VSTPluginMain"));

   // If not found, try finding the old entry point
   if (pluginMain == NULL) {
      pluginMain = (vstPluginMain)
         CFBundleGetFunctionPointerForName(bundleRef,
                                           CFSTR("main_macho"));
   }

   // Must not be a VST plugin
   if (pluginMain == NULL) {
      CFRelease(bundleRef);
      return;
   }

   // Save the bundle reference
   module = bundleRef;

#else

   // Try to load the library
   wxDynamicLibrary *lib = new wxDynamicLibrary(fname);
   if (!lib) {
      return;
   }

   // Bail if it wasn't successful
   if (!lib->IsLoaded()) {
      delete lib;
      return;
   }

   // Try to find the entry point, while suppressing error messages
   {
      wxLogNull logNo;
      pluginMain = (vstPluginMain) lib->GetSymbol(wxT("main"));
      if (pluginMain == NULL) {
         delete lib;
         return;
      }
   }

   // Save the library reference
   module = lib;

#endif

   // Initialize the plugin
   AEffect *aeffect = pluginMain(audioMaster);

   // Was it successful?
   if (aeffect) {

      // Ensure that it looks like a plugin and can deal with ProcessReplacing
      // calls.  Also exclude synths for now.
      if (aeffect->magic == kEffectMagic &&
         !(aeffect->flags & effFlagsIsSynth) &&
         aeffect->flags & effFlagsCanReplacing) {

         // Looks good...try to create the VSTEffect
         VSTEffect *vst = new VSTEffect(fname, module, aeffect);
         if (vst != NULL) {

            // Success...register it and get out
            EffectManager::Get().RegisterEffect(vst);
            return;
         }
      }
#if defined(__WXDEBUG__)
      else {
         wxPrintf(wxT("bypassing %s - magic %08x flags %08x\n"),
                  fname.c_str(), aeffect->magic, aeffect->flags);
         wxLogDebug(wxT("bypassing %s - magic %08x flags %08x"),
                    fname.c_str(), aeffect->magic, aeffect->flags);
      }
#endif
   }

   // Only way we can get here is if something went wrong...clean up
   
#if defined(__WXMAC__)
   CFRelease(bundleRef);
#else
   delete lib;
#endif
}

void LoadVSTPlugins()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;

   // Check for the VST_PATH environment variable
   wxString vstpath = wxGetenv(wxT("VST_PATH"));
   if (!vstpath.IsEmpty()) {
      wxGetApp().AddUniquePathToPathList(vstpath, pathList);
   }

   // Add Audacity specific paths
   for (size_t i = 0; i < audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("VST"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plugins"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plug-ins"),
                                         pathList);
   }

#if defined(__WXMAC__)
#define VSTPATH wxT("/Library/Audio/Plug-Ins/VST")

   // Look in /Library/Audio/Plug-Ins/VST and $HOME/Library/Audio/Plug-Ins/VST
   wxGetApp().AddUniquePathToPathList(VSTPATH, pathList);
   wxGetApp().AddUniquePathToPathList(wxString(wxGetenv(wxT("HOME"))) + VSTPATH,
                                      pathList);

   // Recursively search all paths for Info.plist files.  This will identify all
   // bundles.
   wxGetApp().FindFilesInPathList(wxT("Info.plist"), pathList, files, wxDIR_DEFAULT);
#elif defined(__WXMSW__)
   TCHAR dpath[MAX_PATH];
   TCHAR tpath[MAX_PATH];
   DWORD len = WXSIZEOF(tpath);

   // Setup the default VST path.
   dpath[0] = '\0';
   ExpandEnvironmentStrings(wxT("%ProgramFiles%\\Steinberg\\VSTPlugins"),
                            dpath,
                            WXSIZEOF(dpath));

   // Check registry for the real path
   if (SHRegGetUSValue(wxT("Software\\VST"),
                          wxT("VSTPluginsPath"),
                          NULL,
                          tpath,
                          &len,
                          FALSE,
                          dpath,
                          (DWORD) _tcslen(dpath)) == ERROR_SUCCESS) {
      tpath[len] = 0;
      ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
      wxGetApp().AddUniquePathToPathList(LAT1CTOWX(dpath), pathList);
   }

   // Recursively scan for all DLLs
   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, files, wxDIR_DEFAULT);

#else

   // Recursively scan for all shared objects
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, files);

#endif

   // Try loading everything found
   for (size_t i = 0; i < files.GetCount(); i++) {
      LoadVSTPlugin(files[i]);
   }
}

#ifdef __cplusplus
}
#endif

#else

#ifdef __cplusplus
extern "C" {
#endif

void LoadVSTPlugins()
{
   return;
}

#ifdef __cplusplus
}
#endif

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
// arch-tag: d75c9c59-4f9b-4398-b4e8-11ffddc283cf


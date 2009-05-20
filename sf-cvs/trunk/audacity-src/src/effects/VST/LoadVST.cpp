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

static long audioMaster(AEffect * effect, long opcode, long index,
                        long value, void * ptr, float opt)
{
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

      default:
#if defined(__WXDEBUG__)
         wxPrintf(wxT("effect: %p opcode: %d index: %d value: %d ptr: %p opt: %f\n"),
                  effect, opcode, index, value, ptr, opt);
         wxLogDebug(wxT("effect: %p opcode: %d index: %d value: %d ptr: %p opt: %f"),
                    effect, opcode, index, value, ptr, opt);
#endif
         return 0;
   }
}

typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

static void LoadVSTPlugin(const wxString & fname)
{
   vstPluginMain pluginMain;
   void *module = NULL;

#if defined(__WXDEBUG__)
   wxPrintf(wxT("%s\n"), fname.c_str());
   wxLogDebug(wxT("%s"), fname.c_str());
#endif

#if defined(__WXMAC__)
   wxString name(wxPathOnly(wxPathOnly(fname)));
   wxMacCFStringHolder path(name);

   CFURLRef urlRef =
      CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                                    path,
                                    kCFURLPOSIXPathStyle,
                                    true);
   if (urlRef == NULL) {
      return;
   }

   CFBundleRef bundleRef = CFBundleCreate(kCFAllocatorDefault, urlRef);

   CFRelease(urlRef);

   if (bundleRef == NULL) {
      return;
   }

   pluginMain = (vstPluginMain)
      CFBundleGetFunctionPointerForName(bundleRef,
                                        CFSTR("VSTPluginMain"));
   if (pluginMain == NULL) {
      pluginMain = (vstPluginMain)
         CFBundleGetFunctionPointerForName(bundleRef,
                                           CFSTR("main_macho"));
   }

   if (pluginMain == NULL) {
      CFRelease(bundleRef);
      return;
   }

   module = bundleRef;
#else
   wxDynamicLibrary *lib = new wxDynamicLibrary(fname);
   if (!lib) {
      return;
   }

   if (!lib->IsLoaded()) {
      delete lib;
      return;
   }

   pluginMain = (vstPluginMain) lib->GetSymbol(wxT("main"));
   if (pluginMain == NULL) {
      delete lib;
      return;
   }

   module = lib;
#endif

   AEffect *aeffect = pluginMain(audioMaster);

   if (aeffect) {
      if (aeffect->magic == kEffectMagic &&
         !(aeffect->flags & effFlagsIsSynth) &&
         aeffect->flags & effFlagsCanReplacing) {

         VSTEffect *vst = new VSTEffect(fname, module, aeffect);
         if (vst != NULL) {

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
   
   wxString vstpath = wxGetenv(wxT("VST_PATH"));

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
   wxGetApp().AddUniquePathToPathList(VSTPATH, pathList);
   wxGetApp().AddUniquePathToPathList(wxString(wxGetenv(wxT("HOME"))) + VSTPATH,
                                      pathList);

   wxGetApp().FindFilesInPathList(wxT("Info.plist"), pathList, files, wxDIR_DEFAULT);
#elif defined(__WXMSW__)
   TCHAR dpath[MAX_PATH];
   TCHAR tpath[MAX_PATH];
   DWORD len = sizeof(tpath);

   dpath[0] = '\0';
   ExpandEnvironmentStrings(_T("%ProgramFiles%\\Steinberg\\VSTPlugins"),
                            dpath,
                            sizeof(dpath));

   LSTATUS s = SHRegGetUSValue(_T("Software\\VST"),
                               _T("VSTPluginsPath"),
                               NULL,
                               tpath,
                               &len,
                               FALSE,
                               dpath,
                               (DWORD) _tcslen(dpath));
   if (s == ERROR_SUCCESS) {
      tpath[len] = 0;
      ExpandEnvironmentStrings(tpath, dpath, sizeof(dpath));
      wxGetApp().AddUniquePathToPathList(LAT1CTOWX(dpath), pathList);
   }

   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, files, wxDIR_DEFAULT);
#else
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, files);
#endif

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


/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadModules.cpp

  Dominic Mazzoni
  James Crook


*******************************************************************//*!

\file LoadModules.cpp
\brief Based on LoadLadspa, this code loads pluggable Audacity 
extension modules.  It also has the code to (a) invoke a script
server and (b) invoke a function returning a replacement window,
i.e. an alternative to the usual interface, for Audacity.

*//*******************************************************************/

#include <wx/dynlib.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "Audacity.h"
#include "AudacityApp.h"
#include "Internat.h"
#include "BatchCommands.h"
#include "../lib-src/lib-widget-extra/NonGuiThread.h"

#define initFnName      "ExtensionModuleInit"
#define scriptFnName    "RegScriptServerFunc"
#define mainPanelFnName "MainPanelFunc"

typedef _declspec( dllimport) int (*tModuleInit)(int);
typedef _declspec( dllimport) wxWindow * (*tPanelFn)(int);

// This variable will hold the address of a subroutine in 
// a DLL that can hijack the normal panel.
tPanelFn pPanelHijack=NULL;

// Next two commented out lines are handy when investigating
// strange DLL behaviour.  Instead of dynamic linking,
// link the library which has the replacement panel statically.
// Give the address of the routine here.
// This is a great help in identifying missing 
// symbols which otherwise cause a dll to unload after loading
// without an explanation as to why!
//extern wxWindow * MainPanelFunc( int i );
//tPanelFn pPanelHijack=&MainPanelFunc;

/// IF pPanelHijack has been found in a module DLL
/// THEN when this function is called we'll go and
/// create that window instead of the normal one.
wxWindow * MakeHijackPanel()
{
   if( pPanelHijack == NULL )
      return NULL;
   return pPanelHijack(0);
}

//------- Start of stuff related to invoking a batch command ----
// Our DLL may call commands back in Audacity.
// It will do that through the ExecCommand function.
extern "C" {

typedef __declspec( dllexport) int (*tpExecScriptServerFunc)( wxString * pOut, wxString * pIn);
typedef __declspec( dllimport) int (*tpRegScriptServerFunc)(tpExecScriptServerFunc pFn);

// This is the function which actually obeys one command.
__declspec( dllexport) int ExecCommand( wxString * pOut, wxString * pIn )
{
   // Create a Batch that will have just one command in it...
   BatchCommands Batch;
   bool rc;

   // Find the command name terminator...ingore line if not found
   int splitAt = pIn->Find(wxT(':'));
   if (splitAt < 0) {
      *pOut= wxT("BAD - Missing ':'?");
      return false;
   }

   // Parse and clean
   wxString cmd = pIn->Left(splitAt).Strip(wxString::both);
   wxString parm = pIn->Mid(splitAt + 1).Strip(wxString::trailing);

   rc = Batch.ApplyCommand( cmd, parm );
   if( rc )
   {
      *pOut = wxT("OK");
      return rc;
   }
   *pOut = wxT("FAILED to Execute");
   return rc;
}
}

// This variable will hold the address of a subroutine in
// a DLL that starts a thread and reads script commands.
tpRegScriptServerFunc scriptFn = NULL;

// We pass the ExecFunction to any scripting DLL that needs it
// right here.
void RegisterAndRun(  )
{
   wxASSERT( scriptFn != NULL );
   while( true )
      scriptFn(&ExecCommand);
}

//------- End of stuff related to invoking a batch command ----

void LoadModule(wxString fname)
{
   wxLogDebug(wxT("About to load %s"), fname );
   wxLogNull logNo;
   tModuleInit mainFn = NULL;

   // As a courtesy to some modules that might be bridges to
   // open other modules, we set the current working
   // directory to be the module's directory.

   wxString saveOldCWD = ::wxGetCwd();
   wxString prefix = ::wxPathOnly(fname);
   ::wxSetWorkingDirectory(prefix);

   wxDynamicLibrary* pDLL = new wxDynamicLibrary();
   if (pDLL && pDLL->Load(fname, wxDL_LAZY)) 
   {
      int result =1;
      mainFn   = (tModuleInit)(pDLL->GetSymbol(wxT(initFnName)));

      if (mainFn) 
         result = mainFn( 0 );

      if(( scriptFn == NULL ) &&(result>=0 ))
         scriptFn = (tpRegScriptServerFunc)(pDLL->GetSymbol(wxT(scriptFnName)));

      if((pPanelHijack==NULL ) && (result>=0))
         pPanelHijack = (tPanelFn)(pDLL->GetSymbol(wxT(mainPanelFnName)));
   }

   ::wxSetWorkingDirectory(saveOldCWD);
}

void LoadModules()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;
   wxString pathVar;
   unsigned int i;

#if 0
   // Code from LoadLadspa that might be useful in load modules.
   pathVar = wxGetenv(wxT("AUDACITY_MODULES_PATH"));
   if (pathVar != wxT(""))
      wxGetApp().AddMultiPathsToPathList(pathVar, pathList);

   #ifdef __WXGTK__
   wxGetApp().AddUniquePathToPathList(INSTALL_PREFIX wxT("/modules"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT("/usr/local/lib/modules"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT("/usr/lib/modules"), pathList);
   #endif
#endif

   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("modules"),
                                         pathList);
   }

   #ifdef __WXMSW__
   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, wxFILE, files);   
   #else
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, wxFILE, files);
   #endif

   for(i=0; i<files.GetCount(); i++)
      LoadModule(files[i]);
   // After loading all the modules, we may have a registered scripting function.
   if( scriptFn )
   {
      NonGuiThread::StartChild( &RegisterAndRun );
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



/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Audacity.h" // This should always be included first

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/window.h>
#include <wx/intl.h>
#include <wx/menu.h>

#include <wx/fs_zip.h>
#include <wx/image.h>

#if (wxMAJOR_VERSION >= 2 && wxMINOR_VERSION >= 3)
#include <wx/filename.h>
#endif

#ifdef __WXGTK__
#include <unistd.h>
#endif

#ifdef __MACOS9__
#include <AEDataModel.h>
#include <AppleEvents.h>
#include <Files.h>
#endif

#ifdef __MACOSX__
#include <ApplicationServices/ApplicationServices.h>
#endif

#include "AudacityApp.h"

#include "AboutDialog.h"
#include "AudioIO.h"
#include "Benchmark.h"
#include "effects/LoadEffects.h"
#include "FreqWindow.h"
#include "Help.h"
#include "LangChoice.h"
#include "Prefs.h"
#include "Project.h"
#include "Sequence.h"
#include "WaveTrack.h"
#include "prefs/PrefsDialog.h"

class ToolBar;
class ControlToolBar;

#ifdef __WXGTK__
void wxOnAssert(const char *fileName, int lineNumber, const char *msg)
{
   if (msg)
      printf("ASSERTION FAILED: %s\n%s: %d\n", msg, fileName, lineNumber);
   else
      printf("ASSERTION FAILED!\n%s: %d\n", fileName, lineNumber);

   // Force core dump
   int *i = 0;
   if (*i)
      exit(1);

   exit(0);
}
#endif

wxFrame *gParentFrame = NULL;
wxWindow *gParentWindow = NULL;
ToolBarStub *gControlToolBarStub = NULL;
ToolBarStub *gEditToolBarStub = NULL;

void QuitAudacity()
{
   // Try to close each open window.  If the user hits Cancel
   // in a Save Changes dialog, don't continue.
   size_t len = gAudacityProjects.Count();
   for (size_t i = 0; i < len; i++) {
      if (!gAudacityProjects[i]->Close())
         return;
   }


   if (gFreqWindow)
      gFreqWindow->Destroy();


   if (gParentFrame)
      gParentFrame->Destroy();

   gFreqWindow = NULL;
   gParentFrame = NULL;



   if (gControlToolBarStub) {
      delete gControlToolBarStub;
      gControlToolBarStub = NULL;
   }

   if (gEditToolBarStub) {
      delete gEditToolBarStub;
      gEditToolBarStub = NULL;
   }


   QuitHelp();

   FinishPreferences();
}

IMPLEMENT_APP(AudacityApp)

#ifdef __WXMAC__

#define __MOVIES__      /* Apple's Movies.h not compatible with Audacity */
/*#define __MACHELP__*/

#include <wx/mac/private.h>

/* Declare Static functions */
static pascal OSErr AEQuit(const AppleEvent * theAppleEvent,
                    AppleEvent * theReply, long Refcon);
pascal OSErr AEOpenFiles(const AppleEvent * theAppleEvent, AppleEvent * theReply,
                         long Refcon);
                         
pascal OSErr AEQuit(const AppleEvent * theAppleEvent,
                    AppleEvent * theReply, long Refcon)
{
   QuitAudacity();
   return noErr;
}

/* prototype of MoreFiles fn, included in wxMac already */

pascal OSErr AEOpenFiles(const AppleEvent * theAppleEvent, AppleEvent * theReply,
                         long Refcon)
{
   AEDescList docList;
   AEKeyword keywd;
   DescType returnedType;
   Size actualSize;
   long itemsInList;
   FSSpec theSpec;
   OSErr err;
   short i;

   err =
       AEGetParamDesc(theAppleEvent, keyDirectObject, typeAEList,
                      &docList);
   if (err != noErr)
      return err;

   err = AECountItems(&docList, &itemsInList);
   if (err != noErr)
      return err;

   for (i = 1; i <= itemsInList; i++) {
      AEGetNthPtr(&docList, i, typeFSS, &keywd, &returnedType,
                  (Ptr) & theSpec, sizeof(theSpec), &actualSize);
      wxString fName = wxMacFSSpec2MacFilename(&theSpec);

      AudacityProject *project = GetActiveProject();
      if (project == NULL || !project->GetTracks()->IsEmpty()) {
         project = CreateNewAudacityProject(gParentWindow);
      }
      project->OpenFile(fName);
   }

   return noErr;
}
#endif

BEGIN_EVENT_TABLE(AudacityApp, wxApp)
  EVT_CHAR(AudacityApp::OnKey)

   //These appear to cause trouble on non-mac platforms:
#ifdef __WXMAC__
	EVT_MENU(AboutID, AudacityApp::OnMenuAbout)
	EVT_MENU(NewID, AudacityApp::OnMenuNew)
	EVT_MENU(OpenID, AudacityApp::OnMenuOpen)
	EVT_MENU(PreferencesID, AudacityApp::OnMenuPreferences)
	EVT_MENU(ExitID, AudacityApp::OnMenuExit)
#endif

END_EVENT_TABLE()

// The `main program' equivalent, creating the windows and returning the
// main frame
bool AudacityApp::OnInit()
{
   // mChecker = new wxSingleInstanceChecker(GetAppName());
   ::wxInitAllImageHandlers();

   wxFileSystem::AddHandler(new wxZipFSHandler);

   #ifdef __WXMSW__

   //BG: On Windows, associate the aup file type with Audacity
   {
      wxRegKey associateFileTypes;

      associateFileTypes.SetName("HKCR\\.AUP");
      associateFileTypes.Create(true);
      associateFileTypes = "Audacity.Project";

      associateFileTypes.SetName("HKCR\\Audacity.Project");
      associateFileTypes.Create(true);
      associateFileTypes = "Audacity Project File";

      associateFileTypes.SetName("HKCR\\Audacity.Project\\shell");
      associateFileTypes.Create(true);
      associateFileTypes = "";

      associateFileTypes.SetName("HKCR\\Audacity.Project\\shell\\open");
      associateFileTypes.Create(true);

      associateFileTypes.SetName("HKCR\\Audacity.Project\\shell\\open\\command");
      associateFileTypes.Create(true);
      associateFileTypes = (wxString)argv[0] + (wxString)" %1";
   }

   #endif

   InitPreferences();

   //
   // Paths: set search path and temp dir path
   //

   wxString home = wxGetHomeDir();

   // On Unix systems, the default temp dir is in /tmp.
   // Search path (in this order):
   // * The AUDACITY_PATH environment variable
   // * The current directory
   // * The user's .audacity-files directory in their home directory
   // * The "share" and "share/doc" directories in their install path
   #ifdef __WXGTK__
   defaultTempDir.Printf("/tmp/audacity1.1-%s", wxGetenv("USER"));
   wxString pathVar = wxGetenv("AUDACITY_PATH");
   if (pathVar != "")
      AddMultiPathsToPathList(pathVar, audacityPathList);
   AddUniquePathToPathList(::wxGetCwd(), audacityPathList);
   AddUniquePathToPathList(wxString::Format("%s/.audacity-files",
                                            (const char *)home),
                           audacityPathList);
   AddUniquePathToPathList(wxString::Format("%s/share/audacity",
                                            INSTALL_PREFIX),
                           audacityPathList);
   AddUniquePathToPathList(wxString::Format("%s/share/doc/audacity",
                                            INSTALL_PREFIX),
                           audacityPathList);
   #endif
   
   // On Mac and Windows systems, use the directory which contains Audacity.
   #ifdef __WXMSW__
   // On Windows, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+"\\Languages", audacityPathList);
   defaultTempDir.Printf("%s\\audacity_1_1_temp", (const char *)progPath);
   #endif
   #ifdef __MACOSX__
   // On Mac OS X, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+"/Languages", audacityPathList);
   defaultTempDir.Printf("%s/audacity_1_1_temp", (const char *)progPath);
   #endif
   #ifdef __MACOS9__
   // On Mac OS 9, the initial working directory is the one that
   // contains the program.
   wxString progPath = wxGetCwd();
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+":Languages", audacityPathList);
   defaultTempDir.Printf("%s/audacity_1_1_temp", (const char *)progPath);
   #endif

   // Locale
   // wxWindows 2.3 has a much nicer wxLocale API.  We can make this code much
   // better once we move to wx 2.3/2.4.

   wxString lang = gPrefs->Read("/Locale/Language", "");

   // Pop up a dialog the first time the program is run
   if (lang == "")
      lang = ChooseLanguage(NULL);

   gPrefs->Write("/Locale/Language", lang);

   if (lang != "en") {
      wxLogNull nolog;
      mLocale = new wxLocale("", lang, "", true, true);

      for(unsigned int i=0; i<audacityPathList.GetCount(); i++)
         mLocale->AddCatalogLookupPathPrefix(audacityPathList[i]);

      mLocale->AddCatalog("audacity");
   } else
      mLocale = NULL;

   InitAudioIO();

   LoadEffects();

#ifdef __WXMAC__

   // Install AppleEvent handlers (allows us to open documents
   // that are dragged to our application's icon)

   AEInstallEventHandler(kCoreEventClass,
                         kAEOpenDocuments,
                         NewAEEventHandlerUPP(AEOpenFiles), 0, 0);
   AEInstallEventHandler(kCoreEventClass,
                         kAEQuitApplication,
                         NewAEEventHandlerUPP(AEQuit), 0, 0);


   // On the Mac, users don't expect a program to quit when you close the last window.
   // Create an offscreen frame with a menu bar.  The frame should never
   // be visible, but when all other windows are closed, this menu bar should
   // become visible.

   gParentFrame = new wxFrame(NULL, -1, "invisible", wxPoint(5000, 5000), wxSize(100, 100));
   wxMenu *fileMenu = new wxMenu();
   fileMenu->Append(NewID, "&New\tCtrl+N");
   fileMenu->Append(OpenID, "&Open...\tCtrl+O");
   fileMenu->AppendSeparator();
   fileMenu->Append(PreferencesID, "&Preferences...\tCtrl+P");
   fileMenu->AppendSeparator();
   fileMenu->Append(ExitID, "Quit\tCtrl+Q");
   wxMenu *helpMenu = new wxMenu();
   helpMenu->Append(AboutID, "About Audacity...");
   wxApp::s_macAboutMenuItemId = AboutID;
   
   wxMenuBar *menuBar = new wxMenuBar();
   menuBar->Append(fileMenu, "&File");
   menuBar->Append(helpMenu, "&Help");
   
   gParentFrame->SetMenuBar(menuBar);
   gParentFrame->Show();

   SetTopWindow(gParentFrame);

#endif

   SetExitOnFrameDelete(true);

   //Initiate pointers to toolbars here, and create 
   //the toolbars that should be loaded at startup.


   //Initiate globally-held toolbar stubs here.
   gControlToolBarStub = new ToolBarStub(gParentWindow, ControlToolBarID);
   
   // Changing the following to NULL will make the application
   // load without the toolbar in memory at all.

   bool editToolBar;
   gPrefs->Read("/GUI/EnableEditToolBar", &editToolBar, true);
   if(editToolBar)
      gEditToolBarStub =  new ToolBarStub(gParentWindow, EditToolBarID);
   else
      gEditToolBarStub = NULL;


   InitFreqWindow(gParentWindow);
   AudacityProject *project = CreateNewAudacityProject(gParentWindow);
   SetTopWindow(project);

   // Can't handle command-line args on Mac OS X yet...
   #ifndef __MACOSX__

   // Parse command-line arguments

   if (argc > 1) {
      for (int option = 1; option < argc; option++) {
         if (!argv[option])
            continue;
         bool handled = false;

         if (!wxString("-help").CmpNoCase(argv[option])) {
            printf(_("Command-line options supported:\n"
                     "  -help (this message)\n"
                     "  -test (run self diagnostics)\n"
                     "  -blocksize ### (set max disk block size in bytes)\n"
                     "\n"
                     "In addition, specify the name of an audio file or "
                     "Audacity project\n" "to open it.\n" "\n"));
            exit(0);
         }

         if (option < argc - 1 &&
             argv[option + 1] &&
             !wxString("-blocksize").CmpNoCase(argv[option])) {
            long theBlockSize;
            if (wxString(argv[option + 1]).ToLong(&theBlockSize)) {
               if (theBlockSize >= 256 && theBlockSize < 100000000) {
                  fprintf(stderr, _("Using block size of %ld\n"),
                          theBlockSize);
                  Sequence::SetMaxDiskBlockSize(theBlockSize);
               }
            }
            option++;
            handled = true;
         }

         if (!handled && !wxString("-test").CmpNoCase(argv[option])) {
            RunBenchmark(NULL);
            exit(0);
         }

         if (argv[option][0] == '-' && !handled) {
            printf(_("Unknown command line option: %s\n"), argv[option]);
            exit(0);
         }

         if (!handled)
            project->OpenFile(argv[option]);

      }                         // for option...
   }                            // if (argc>1)

   #endif // not Mac OS X

   return TRUE;
}

// static
void AudacityApp::AddUniquePathToPathList(wxString path,
                                          wxArrayString &pathList)
{
   #if (wxMAJOR_VERSION >= 2 && wxMINOR_VERSION >= 3)
   wxFileName pathNorm = path;
   pathNorm.Normalize();
   path = pathNorm.GetFullPath();
   #endif

   for(unsigned int i=0; i<pathList.GetCount(); i++) {
      #if (wxMAJOR_VERSION >= 2 && wxMINOR_VERSION >= 3)
      if (wxFileName(path) == wxFileName(pathList[i]))
         return;
      #else
      if (path == pathList[i])
         return;
      #endif
   }

   pathList.Add(path);
}

// static
void AudacityApp::AddMultiPathsToPathList(wxString multiPathString,
                                          wxArrayString &pathList)
{
   while (multiPathString != "") {
      wxString onePath = multiPathString.BeforeFirst(wxPATH_SEP[0]);
      multiPathString = multiPathString.AfterFirst(wxPATH_SEP[0]);
      AddUniquePathToPathList(onePath, pathList);
   }
}

// static
void AudacityApp::FindFilesInPathList(wxString pattern,
                                      wxArrayString pathList,
                                      int flags,
                                      wxArrayString &results)
{
   wxLogNull nolog;

   if (pattern == "")
      return;

   for(unsigned i=0; i<pathList.GetCount(); i++) {
      wxString path = pathList[i];

      wxString fname = 
         wxFindFirstFile((const char *)(path + wxFILE_SEP_PATH + pattern));
      while(fname != "") {
         results.Add(fname);
         fname = wxFindNextFile();
      }
   }
}

void AudacityApp::OnKey(wxKeyEvent& event)
{
   AudacityProject *audacityPrj = GetActiveProject();
   wxString newStr = "";

   long key = event.GetKeyCode();

   if(event.ControlDown())
      newStr += "Ctrl+";

   if(event.AltDown())
      newStr += "Alt+";

   if(event.ShiftDown())
      newStr += "Shift+";

   if (event.ControlDown() && key >= 1 && key <= 26)
      newStr += (char)(64 + key);
   else if (key >= 33 && key <= 126)
      newStr += (char)key;
   else if (key == WXK_BACK)
      newStr = "Backspace";
   else if (key == WXK_DELETE)
      newStr = "Delete";
   else if (key == WXK_SPACE)
      newStr = "Spacebar";
   else
   {
      event.Skip();
      return; // Don't change it if we don't recognize the key
   }

   if(audacityPrj->IsActive())
   {
      int commandIndex = audacityPrj->FindCommandByCombos(newStr);

      if(audacityPrj->GetCommandState(commandIndex) == enabledMenu)
      {
         audEventFunction audFunc = audacityPrj->GetCommandFunc(commandIndex);

         if(audFunc)
            (audacityPrj->*((wxEventFunction) audFunc))(event);

         return;
      }
   }

   event.Skip();
}

int AudacityApp::OnExit()
{
   while(Pending())
   {
      Dispatch();
   }

//   delete mChecker;
   return 0;
}

// The following five methods are currently only used on Mac OS,
// where it's possible to have a menu bar but no windows open.
// It doesn't hurt any other platforms, though.

// ...That is, as long as you check to see if no windows are open 
// before executing the stuff.
// To fix this, check to see how many project windows are open,
// and skip the event unless none are open (which should only happen
// on the Mac, at least currently.)

void AudacityApp::OnMenuAbout(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that 
   // this happens, and enable the same code to be present on
   // all platforms.
   if(gAudacityProjects.GetCount() == 0) {
   AboutDialog dlog(NULL);
   dlog.ShowModal();
   }
   else
      event.Skip();
}

void AudacityApp::OnMenuNew(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that 
   // this happens, and enable the same code to be present on
   // all platforms.
 
   if(gAudacityProjects.GetCount() == 0)
      CreateNewAudacityProject(gParentWindow);
   else
      event.Skip();
}


void AudacityApp::OnMenuOpen(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that 
   // this happens, and enable the same code to be present on
   // all platforms.


   if(gAudacityProjects.GetCount() == 0)
      AudacityProject::ShowOpenDialog(NULL);
   else
      event.Skip();


}

void AudacityApp::OnMenuPreferences(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that 
   // this happens, and enable the same code to be present on
   // all platforms.

   if(gAudacityProjects.GetCount() == 0) {
      PrefsDialog dialog(NULL /* parent */ );
      dialog.ShowModal();
   }
   else
      event.Skip();
   
}

void AudacityApp::OnMenuExit(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that 
   // this happens, and enable the same code to be present on
   // all platforms.

   if(gAudacityProjects.GetCount() == 0)
      QuitAudacity();
   else
      event.Skip();
   
}


/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Audacity.h" // This should always be included first

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/log.h>
#include <wx/window.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/snglinst.h>

#include <wx/fs_zip.h>
#include <wx/image.h>

#include <wx/filename.h>

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
#include "DirManager.h"
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
ToolBarStub *gMixerToolBarStub = NULL;
ToolBarStub *gEditToolBarStub = NULL;

bool gIsQuitting = false;

void SaveWindowSize()
{
   if(!gAudacityProjects.IsEmpty())
   {
      // BG: Save size of Window 0.
      wxSize wndSize = gAudacityProjects[0]->GetSize();
      bool wndMaximized = gAudacityProjects[0]->IsMaximized();
      gPrefs->Write("/Window/Width", wndSize.GetWidth());
      gPrefs->Write("/Window/Height", wndSize.GetHeight());
      gPrefs->Write("/Window/Maximized", wndMaximized);   
   }
}

void QuitAudacity(bool bForce)
{
   if (gIsQuitting)
      return;

   gIsQuitting = true;

   if(bForce)
   {
      wxMessageBox(_("WARNING: You may be prompted to save your work. Clicking cancel will have the same effect as clicking no."));
   }

   // Try to close each open window.  If the user hits Cancel
   // in a Save Changes dialog, don't continue.
   // BG: unless force is true

   SaveWindowSize();

   // BG: Are there any projects open?
   if(!gAudacityProjects.IsEmpty())
   {
      size_t len = gAudacityProjects.Count();
      for (size_t i = 0; i < len; i++) {
         if(bForce)
         {
            gAudacityProjects[i]->Close(true);
         }
         else
         {
            if (!gAudacityProjects[i]->Close())
               return;
         }
      }
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

   if (gMixerToolBarStub) {
      delete gMixerToolBarStub;
      gMixerToolBarStub = NULL;
   }

   if (gEditToolBarStub) {
      delete gEditToolBarStub;
      gEditToolBarStub = NULL;
   }

   //Delete the clipboard
   AudacityProject::DeleteClipboard();

   QuitHelp();

   if(bForce)
   {
      wxExit();
   }
}

void QuitAudacity()
{
   QuitAudacity(false);
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

pascal OSErr AEOpenPrefs(const AppleEvent * theAppleEvent,
                         AppleEvent * theReply,
                         long Refcon)
{
   PrefsDialog dialog(GetActiveProject());
   dialog.ShowModal();
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

typedef int (AudacityApp::*SPECIALKEYEVENT)(wxKeyEvent&);

BEGIN_EVENT_TABLE(AudacityApp, wxApp)

   //These appear to cause trouble on non-mac platforms:
#if 0 //def __WXMAC__
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
   // Unused strings that we want to be translated, even though
   // we're not using them yet...
   wxString future1 = _("Master Gain Control");
   wxString future2 = _("Input Meter");
   wxString future3 = _("Output Meter");

   {
      wxLogNull dontLog;

      const wxString name = wxString::Format("Audacity-%s", wxGetUserId().c_str());
      mChecker = new wxSingleInstanceChecker(name);
      if ( mChecker->IsAnotherRunning() ) {
         wxString prompt =
            _("The system has detected that another copy of Audacity may be running.\n"
            "Running two copies of Audacity simultaneously may lead to data loss or\n"
            "cause your system to crash.\n\n"
            "Are you sure you want to launch Audacity now?");
         int action = wxMessageBox(prompt,
                                   _("Audacity is already running"),
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   NULL);
         if (action == wxNO) {
            delete mChecker;
            return false;
         }
      }
   }

   ::wxInitAllImageHandlers();

   wxFileSystem::AddHandler(new wxZipFSHandler);

   #if defined(__WXMSW__) && !defined(__WXUNIVERSAL__)

   //BG: On Windows, associate the aup file type with Audacity
   {
      wxRegKey associateFileTypes;

      associateFileTypes.SetName("HKCR\\.AUP");
      if(!associateFileTypes.Exists())
      {
         associateFileTypes.Create(true);
         associateFileTypes = "Audacity.Project";
      }

      associateFileTypes.SetName("HKCR\\Audacity.Project");
      if(!associateFileTypes.Exists())
      {
         associateFileTypes.Create(true);
         associateFileTypes = "Audacity Project File";
      }

      associateFileTypes.SetName("HKCR\\Audacity.Project\\shell");
      if(!associateFileTypes.Exists())
      {
         associateFileTypes.Create(true);
         associateFileTypes = "";
      }

      associateFileTypes.SetName("HKCR\\Audacity.Project\\shell\\open");
      if(!associateFileTypes.Exists())
      {
         associateFileTypes.Create(true);
      }

      associateFileTypes.SetName("HKCR\\Audacity.Project\\shell\\open\\command");

      wxString tmpRegAudPath;
      if(associateFileTypes.Exists())
      {
         tmpRegAudPath = wxString(associateFileTypes).Lower();
      }
      if(!associateFileTypes.Exists() || (tmpRegAudPath.Find("\\audacity.exe") >= 0))
      {
         associateFileTypes.Create(true);
         associateFileTypes = (wxString)argv[0] + (wxString)" \"%1\"";
      }
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
   defaultTempDir.Printf("/tmp/audacity1.2-%s", wxGetUserId().c_str());
   wxString pathVar = wxGetenv("AUDACITY_PATH");
   if (pathVar != "")
      AddMultiPathsToPathList(pathVar, audacityPathList);
   AddUniquePathToPathList(::wxGetCwd(), audacityPathList);
   AddUniquePathToPathList(wxString::Format("%s/.audacity-files",
                                            (const char *)home),
                           audacityPathList);
   #ifdef AUDACITY_NAME
      AddUniquePathToPathList(wxString::Format("%s/share/%s",
                                               INSTALL_PREFIX, AUDACITY_NAME),
                              audacityPathList);
      AddUniquePathToPathList(wxString::Format("%s/share/doc/%s",
                                               INSTALL_PREFIX, AUDACITY_NAME),
                              audacityPathList);
   #else
      AddUniquePathToPathList(wxString::Format("%s/share/audacity",
                                               INSTALL_PREFIX),
                              audacityPathList);
      AddUniquePathToPathList(wxString::Format("%s/share/doc/audacity",
                                               INSTALL_PREFIX),
                              audacityPathList);
   #endif

   AddUniquePathToPathList(wxString::Format("%s/share/locale",
                                            INSTALL_PREFIX),
                           audacityPathList);

   #endif

   wxFileName tmpFile;
   tmpFile.AssignTempFileName("nn");
   wxString tmpDirLoc = tmpFile.GetPath(wxPATH_GET_VOLUME);
   ::wxRemoveFile(tmpFile.GetFullPath());

   // On Mac and Windows systems, use the directory which contains Audacity.
   #ifdef __WXMSW__
   // On Windows, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+"\\Languages", audacityPathList);
   defaultTempDir.Printf("%s\\audacity_1_2_temp", (const char *)tmpDirLoc);
   #endif
   #ifdef __MACOSX__
   // On Mac OS X, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+"/Languages", audacityPathList);
   defaultTempDir.Printf("%s/audacity1.2-%s",
                         (const char *)tmpDirLoc,
                         (const char *)wxGetUserId());
   #endif
   #ifdef __MACOS9__
   // On Mac OS 9, the initial working directory is the one that
   // contains the program.
   wxString progPath = wxGetCwd();
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+":Languages", audacityPathList);
   defaultTempDir.Printf("%s/audacity_1_2_temp", (const char *)tmpDirLoc);
   #endif

   // BG: Create a temporary window to set as the top window
   wxFrame *temporarywindow = new wxFrame(NULL, -1, "temporarytopwindow");
   SetTopWindow(temporarywindow);

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

#ifdef AUDACITY_NAME
      mLocale->AddCatalog(AUDACITY_NAME);
#else
      mLocale->AddCatalog("audacity");
#endif
   } else
      mLocale = NULL;

   // Init DirManager, which initializes the temp directory
   // If this fails, we must exit the program.

   if (!DirManager::InitDirManager()) {
      FinishPreferences();
      return false;
   }

   // More initialization
   InitDitherers();
   InitAudioIO();

   LoadEffects();


#ifdef __WXMAC__

#ifdef __MACOSX__
   EnableMenuCommand(NULL, kHICommandPreferences);
#endif

   AEInstallEventHandler(kCoreEventClass,
                         kAEShowPreferences,
                         NewAEEventHandlerUPP(AEOpenPrefs), 0, 0);   

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

#if 0

   wxMenu *fileMenu = new wxMenu();
   fileMenu->Append(NewID, "&New\tCtrl+N");
   fileMenu->Append(OpenID, "&Open...\tCtrl+O");

#ifdef __MACOS9__
   fileMenu->AppendSeparator();
   fileMenu->Append(PreferencesID, "&Preferences...\tCtrl+P");
#endif

   fileMenu->AppendSeparator();
   fileMenu->Append(ExitID, "Quit\tCtrl+Q");
   wxMenu *helpMenu = new wxMenu();
   helpMenu->Append(AboutID, "About Audacity...");
   wxApp::s_macAboutMenuItemId = AboutID;
   
   wxMenuBar *menuBar = new wxMenuBar();
   menuBar->Append(fileMenu, "&File");
   menuBar->Append(helpMenu, "&Help");

   gParentFrame->SetMenuBar(menuBar);

#endif // #if 0

   gParentFrame->Show();

   SetTopWindow(gParentFrame);

#endif

   SetExitOnFrameDelete(true);

   //Initiate pointers to toolbars here, and create 
   //the toolbars that should be loaded at startup.


   //Initiate globally-held toolbar stubs here.
   gControlToolBarStub = new ToolBarStub(gParentWindow, ControlToolBarID);



   //Only load the mixer toolbar if it says so in the preferences
   bool mixerToolBar;
   gPrefs->Read("/GUI/EnableMixerToolBar", &mixerToolBar, true);
   if(mixerToolBar)
      gMixerToolBarStub =  new ToolBarStub(gParentWindow, MixerToolBarID);
   else
      gMixerToolBarStub = NULL;


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

   delete temporarywindow;

   // Can't handle command-line args on Mac OS X yet...
   #ifndef __MACOSX__

   // Parse command-line arguments

   if (argc > 1) {
      for (int option = 1; option < argc; option++) {
         if (!argv[option])
            continue;
         bool handled = false;

         if (!wxString("-help").CmpNoCase(argv[option])) {
            printf(/* i18n-hint: '-help', '-test' and
                      '-blocksize' need to stay in English. */
                   _("Command-line options supported:\n"
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
   wxFileName pathNorm = path;
   pathNorm.Normalize();
   path = pathNorm.GetFullPath();

   for(unsigned int i=0; i<pathList.GetCount(); i++) {
      if (wxFileName(path) == wxFileName(pathList[i]))
         return;
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

//BG: Filters all events before they are processed
int AudacityApp::FilterEvent(wxEvent& event)
{
   //Send key events to the commands code
   if(event.GetEventType() == wxEVT_KEY_DOWN ||
      event.GetEventType() == wxEVT_KEY_UP)
      return (this->*((SPECIALKEYEVENT) (&AudacityApp::OnAllKeys)))
         ((wxKeyEvent&)event);

   return -1;
}

//BG: THIS IS NOT A NORMAL KEY CALLBACK
//BG: it gets called before everything else
//BG: return -1 to allow other things to process keydown events
//BG: return TRUE to signify that the event has been processed, but do not allow anything else to process this event
//BG: return FALSE to signigy that the event has not been processed and should not be processed
//BG: do not call event.skip, I do not know what would happen
int AudacityApp::OnAllKeys(wxKeyEvent& event)
{
   AudacityProject *audacityPrj = GetActiveProject();

   if (!audacityPrj) {
      return -1;
   }

   if(audacityPrj->IsActive())
   {
      if (event.GetEventType() == wxEVT_KEY_DOWN) {
         if (audacityPrj->HandleKeyDown(event))
            return true;
      }
      if (event.GetEventType() == wxEVT_KEY_UP) {
         if (audacityPrj->HandleKeyUp(event))
            return true;
      }
   }

   return -1;
}




int AudacityApp::OnExit()
{
   gIsQuitting = true;
   while(Pending())
   {
      Dispatch();
   }

   if(gPrefs)
   {
      bool bFalse = false;
      //Should we change the commands.cfg location next startup?
      if(gPrefs->Read("/QDeleteCmdCfgLocation", &bFalse))
      {
         gPrefs->DeleteEntry("/QDeleteCmdCfgLocation");
         gPrefs->Write("/DeleteCmdCfgLocation", true);
      }
   }

   FinishPreferences();

   UnloadEffects();

   DeinitAudioIO();

   delete mChecker;

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


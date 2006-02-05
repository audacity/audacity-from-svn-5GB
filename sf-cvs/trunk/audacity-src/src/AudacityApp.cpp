/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Audacity.h" // This should always be included first

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/docview.h>
#include <wx/log.h>
#include <wx/window.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/snglinst.h>

#include <wx/fs_zip.h>
#include <wx/image.h>

#include <wx/file.h>
#include <wx/filename.h>

#ifdef __WXGTK__
#include <unistd.h>
#endif

// chmod
#ifdef __UNIX__
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "AudacityApp.h"

#include "AboutDialog.h"
#include "AColor.h"
#include "AudioIO.h"
#include "Benchmark.h"
#include "DirManager.h"
#include "effects/LoadEffects.h"
#include "FreqWindow.h"
#include "Help.h"
#include "Internat.h"
#include "LangChoice.h"
#include "Prefs.h"
#include "Project.h"
#include "Sequence.h"
#include "WaveTrack.h"
#include "Internat.h"
#include "prefs/PrefsDialog.h"

#if wxUSE_ACCESSIBILITY
const wxChar *overrideTextCtrlNameStr = wxT("");
const wxChar *overrideChoiceNameStr = wxT("");
const wxChar *overrideComboBoxNameStr = wxT("");
#endif

#ifdef __WXGTK__
void wxOnAssert(const wxChar *fileName, int lineNumber, const wxChar *msg)
{
   if (msg)
      printf("ASSERTION FAILED: %s\n%s: %d\n", (const char *)wxString(msg).mb_str(), (const char *)wxString(fileName).mb_str(), lineNumber);
   else
      printf("ASSERTION FAILED!\n%s: %d\n", (const char *)wxString(fileName).mb_str(), lineNumber);

   // Force core dump
   int *i = 0;
   if (*i)
      exit(1);

   exit(0);
}
#endif

wxFrame *gParentFrame = NULL;
wxWindow *gParentWindow = NULL;

bool gInited = false;
bool gIsQuitting = false;

void SaveWindowSize()
{
   if(!gAudacityProjects.IsEmpty())
   {
      // BG: Save size of Window 0.
      if (!gAudacityProjects[0]->IsIconized()) {
         wxSize wndSize = gAudacityProjects[0]->GetSize();
         bool wndMaximized = gAudacityProjects[0]->IsMaximized();
         gPrefs->Write(wxT("/Window/Width"), wndSize.GetWidth());
         gPrefs->Write(wxT("/Window/Height"), wndSize.GetHeight());
         gPrefs->Write(wxT("/Window/Maximized"), wndMaximized);   
      }
   }
}

void QuitAudacity(bool bForce)
{
   if (gIsQuitting)
      return;

   gIsQuitting = true;

   // Try to close each open window.  If the user hits Cancel
   // in a Save Changes dialog, don't continue.
   // BG: unless force is true

   SaveWindowSize();

   // BG: Are there any projects open?
   if (!gAudacityProjects.IsEmpty())
   {
      size_t len = gAudacityProjects.Count();
      for (size_t i = 0; i < len; i++)
      {
         if (bForce)
         {
            gAudacityProjects[i]->Close(true);
         }
         else
         {
            if (!gAudacityProjects[i]->Close())
            {
               gIsQuitting = false;
               return;
            }
         }
      }
   }
   
   if (gFreqWindow)
      gFreqWindow->Destroy();

   if (gParentFrame)
      gParentFrame->Destroy();

   gFreqWindow = NULL;
   gParentFrame = NULL;

   //Delete the clipboard
   AudacityProject::DeleteClipboard();

   QuitHelp();

   if (bForce)
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

// in response of an open-document apple event
void AudacityApp::MacOpenFile(const wxString &fileName)
{
   if (!gInited)
      return;

   AudacityProject *project = GetActiveProject();
   if (project == NULL || !project->GetTracks()->IsEmpty()) {
      project = CreateNewAudacityProject(gParentWindow);
   }
   project->OpenFile(fileName);
}

// in response of a print-document apple event
void AudacityApp::MacPrintFile(const wxString &fileName)
{
   if (!gInited)
      return;

   AudacityProject *project = GetActiveProject();
   if (project == NULL || !project->GetTracks()->IsEmpty()) {
      project = CreateNewAudacityProject(gParentWindow);
   }
   project->OpenFile(fileName);
}

// in response of a open-application apple event
void AudacityApp::MacNewFile()
{
   if (!gInited)
      return;

   // This method should only be used on the Mac platform
   // when no project windows are open.
 
   if(gAudacityProjects.GetCount() == 0)
      CreateNewAudacityProject(gParentWindow);
}

// in response of a reopen-application apple event
void AudacityApp::MacReopenApp()
{
   // Not sure what to do here...bring it to the foreground???
}
#endif

typedef int (AudacityApp::*SPECIALKEYEVENT)(wxKeyEvent&);

BEGIN_EVENT_TABLE(AudacityApp, wxApp)
   EVT_KEY_DOWN(AudacityApp::OnKeyDown)
   EVT_CHAR(AudacityApp::OnChar)
   EVT_KEY_UP(AudacityApp::OnKeyUp)
#ifdef __WXMAC__
   EVT_MENU(wxID_NEW, AudacityApp::OnMenuNew)
   EVT_MENU(wxID_OPEN, AudacityApp::OnMenuOpen)
   EVT_MENU(wxID_ABOUT, AudacityApp::OnMenuAbout)
   EVT_MENU(wxID_PREFERENCES, AudacityApp::OnMenuPreferences)
   EVT_MENU(wxID_EXIT, AudacityApp::OnMenuExit)
#endif
   // Recent file event handlers.  
   EVT_MENU_RANGE(wxID_FILE1, wxID_FILE9, AudacityApp::OnMRUFile)
// EVT_MENU_RANGE(6050, 6060, AudacityApp::OnMRUProject)
END_EVENT_TABLE()

// Backend for OnMRUFile and OnMRUProject
bool AudacityApp::MRUOpen(wxString fileName) {
   // Most of the checks below are copied from AudacityProject::ShowFileDialog
   // - some rationalisation might be possible.
   
   AudacityProject *proj = GetActiveProject();
   
   if(!fileName.IsEmpty()) {
      
      // verify that the file exists 
      if(wxFile::Exists(fileName)) {
         wxFileName newFileName(fileName);
         
         gPrefs->Write(wxT("/DefaultOpenPath"), wxPathOnly(fileName));
         
         // Make sure it isn't already open
         size_t numProjects = gAudacityProjects.Count();
         for (size_t i = 0; i < numProjects; i++) {
            if (newFileName.SameAs(gAudacityProjects[i]->GetFileName())) {
               wxMessageBox(wxString::Format(_("%s is already open in another window."),
                  newFileName.GetName().c_str()),
                  _("Error opening project"),
                  wxOK | wxCENTRE);
               continue;
            }
         }
         
         // DMM: If the project is dirty, that means it's been touched at
         // all, and it's not safe to open a new project directly in its
         // place.  Only if the project is brand-new clean and the user
         // hasn't done any action at all is it safe for Open to take place
         // inside the current project.
         //
         // If you try to Open a new project inside the current window when
         // there are no tracks, but there's an Undo history, etc, then
         // bad things can happen, including data files moving to the new
         // project directory, etc.
         if (!proj || proj->GetDirty() || !proj->GetIsEmpty()) {
            proj = CreateNewAudacityProject(gParentWindow);
         }
         // This project is clean; it's never been touched.  Therefore
         // all relevant member variables are in their initial state,
         // and it's okay to open a new project inside this window.
         proj->OpenFile(fileName);

         // Add file to "recent files" list.
         proj->GetRecentFiles()->AddFileToHistory(fileName);
         gPrefs->SetPath(wxT("/RecentFiles"));
         proj->GetRecentFiles()->Save(*gPrefs);
         gPrefs->SetPath(wxT(".."));
      }
      else {
         // File doesn't exist - remove file from history
         wxMessageBox(wxString::Format(_("%s does not exist and could not be opened.\n\nIt has been removed from the history list."), 
                      fileName.c_str()));
         return(false);
      }
   }
   return(true);
}

void AudacityApp::OnMRUFile(wxCommandEvent& event) {
   AudacityProject *proj = GetActiveProject();

   int n = event.GetId() - wxID_FILE1;
   wxString fileName = proj->GetRecentFiles()->GetHistoryFile(n);

   bool opened = MRUOpen(fileName);
   if(!opened) {
      proj->GetRecentFiles()->RemoveFileFromHistory(n);
      gPrefs->SetPath(wxT("/RecentFiles"));
      proj->GetRecentFiles()->Save(*gPrefs);
      gPrefs->SetPath(wxT(".."));
   }
}

#if 0
//FIX-ME: Was this OnMRUProject lost in an edit??  Should we have it back?
void AudacityApp::OnMRUProject(wxCommandEvent& event) {
   AudacityProject *proj = GetActiveProject();

   int n = event.GetId() - 6050;//FIX-ME: Use correct ID name.
   wxString fileName = proj->GetRecentProjects()->GetHistoryFile(n);

   bool opened = MRUOpen(fileName);
   if(!opened) {
      proj->GetRecentProjects()->RemoveFileFromHistory(n);
      gPrefs->SetPath("/RecentProjects");
      proj->GetRecentProjects()->Save(*gPrefs);
      gPrefs->SetPath("..");
   }
}
#endif


// The `main program' equivalent, creating the windows and returning the
// main frame
bool AudacityApp::OnInit()
{
   // Unused strings that we want to be translated, even though
   // we're not using them yet...
   wxString future1 = _("Master Gain Control");
   wxString future2 = _("Input Meter");
   wxString future3 = _("Output Meter");

   ::wxInitAllImageHandlers();

   wxFileSystem::AddHandler(new wxZipFSHandler);

   AColor::Init();
   InitPreferences();

	#if defined(__WXMSW__) && !defined(__WXUNIVERSAL__) && !defined(__CYGWIN__)
		this->AssociateFileTypes(); 
	#endif

   //
   // Paths: set search path and temp dir path
   //

   wxString home = wxGetHomeDir();
   mAppHomeDir = home;

   // On Unix systems, the default temp dir is in /tmp.
   // Search path (in this order):
   // * The AUDACITY_PATH environment variable
   // * The current directory
   // * The user's .audacity-files directory in their home directory
   // * The "share" and "share/doc" directories in their install path
   #ifdef __WXGTK__
   defaultTempDir.Printf(wxT("/tmp/audacity1.2-%s"), wxGetUserId().c_str());
   wxString pathVar = wxGetenv(wxT("AUDACITY_PATH"));
   if (pathVar != wxT(""))
      AddMultiPathsToPathList(pathVar, audacityPathList);
   AddUniquePathToPathList(FROMFILENAME(::wxGetCwd()), audacityPathList);
   AddUniquePathToPathList(wxString::Format(wxT("%s/.audacity-files"),
                                            home.c_str()),
                           audacityPathList);
   #ifdef AUDACITY_NAME
      AddUniquePathToPathList(wxString::Format(wxT("%s/share/%s"),
                                               wxT(INSTALL_PREFIX), wxT(AUDACITY_NAME)),
                              audacityPathList);
      AddUniquePathToPathList(wxString::Format(wxT("%s/share/doc/%s"),
                                               wxT(INSTALL_PREFIX), wxT(AUDACITY_NAME)),
                              audacityPathList);
   #else
      AddUniquePathToPathList(wxString::Format(wxT("%s/share/audacity"),
                                               wxT(INSTALL_PREFIX)),
                              audacityPathList);
      AddUniquePathToPathList(wxString::Format(wxT("%s/share/doc/audacity"),
                                               wxT(INSTALL_PREFIX)),
                              audacityPathList);
   #endif

   AddUniquePathToPathList(wxString::Format(wxT("%s/share/locale"),
                                            wxT(INSTALL_PREFIX)),
                           audacityPathList);

   #endif

   wxFileName tmpFile;
   tmpFile.AssignTempFileName(wxT("nn"));
   wxString tmpDirLoc = tmpFile.GetPath(wxPATH_GET_VOLUME);
   ::wxRemoveFile(FILENAME(tmpFile.GetFullPath()));

   // On Mac and Windows systems, use the directory which contains Audacity.
   #ifdef __WXMSW__
   // On Windows, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+wxT("\\Languages"), audacityPathList);
   defaultTempDir.Printf(wxT("%s\\audacity_1_2_temp"), tmpDirLoc.c_str());
   #endif
   #ifdef __MACOSX__
   // On Mac OS X, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);

   AddUniquePathToPathList(progPath, audacityPathList);
   // If Audacity is a "bundle" package, then the root directory is
   // the great-great-grandparent of the directory containing the executable.
   AddUniquePathToPathList(progPath+wxT("/../../../"), audacityPathList);

   AddUniquePathToPathList(progPath+wxT("/Languages"), audacityPathList);
   AddUniquePathToPathList(progPath+wxT("/../../../Languages"), audacityPathList);
   defaultTempDir.Printf(wxT("%s/audacity1.2-%s"),
                         tmpDirLoc.c_str(),
                         wxGetUserId().c_str());
   #endif
   #ifdef __MACOS9__
   // On Mac OS 9, the initial working directory is the one that
   // contains the program.
   wxString progPath = wxGetCwd();
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+wxT(":Languages"), audacityPathList);
   defaultTempDir.Printf(wxT("%s/audacity_1_2_temp"), tmpDirLoc.c_str());
   #endif

   // BG: Create a temporary window to set as the top window
   wxFrame *temporarywindow = new wxFrame(NULL, -1, wxT("temporarytopwindow"));
   SetTopWindow(temporarywindow);

   // Locale
   // wxWindows 2.3 has a much nicer wxLocale API.  We can make this code much
   // better once we move to wx 2.3/2.4.

   wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));

   // Pop up a dialog the first time the program is run
   if (lang == wxT(""))
      lang = ChooseLanguage(NULL);

#ifdef NOT_RQD
//TIDY-ME: (CleanSpeech) Language prompt??
// The prompt for language only happens ONCE on a system.
// I don't think we should disable it JKC
   wxString lang = gPrefs->Read(wxT("/Locale/Language"), "en");  //lda

// Pop up a dialog the first time the program is run
//lda   if (lang == "")
//lda      lang = ChooseLanguage(NULL);
#endif
   gPrefs->Write(wxT("/Locale/Language"), lang);

   if (lang != wxT("en")) {
      wxLogNull nolog;
      mLocale = new wxLocale(wxT(""), lang, wxT(""), true, true);

      for(unsigned int i=0; i<audacityPathList.GetCount(); i++)
         mLocale->AddCatalogLookupPathPrefix(audacityPathList[i]);

#ifdef AUDACITY_NAME
      mLocale->AddCatalog(wxT(AUDACITY_NAME));
#else
      mLocale->AddCatalog(wxT("audacity"));
#endif
   } else
      mLocale = NULL;

   // Initialize internationalisation (number formats etc.)
   //
   // This must go _after_ creating the wxLocale instance because
   // creating the wxLocale instance sets the application-wide locale.
   Internat::Init();

   // Init DirManager, which initializes the temp directory
   // If this fails, we must exit the program.

   if (!InitTempDir()) {
      FinishPreferences();
      return false;
   }

   // More initialization
   InitCleanSpeech();

   InitDitherers();
   InitAudioIO();

   LoadEffects();

#ifdef __WXMAC__

   // On the Mac, users don't expect a program to quit when you close the last window.
   // Create an offscreen frame with a menu bar.  The frame should never
   // be visible, but when all other windows are closed, this menu bar should
   // become visible.

   gParentFrame = new wxFrame(NULL, -1, wxT("invisible"), wxPoint(5000, 5000), wxSize(100, 100), wxFRAME_NO_TASKBAR);

   wxMenu *fileMenu = new wxMenu();
   fileMenu->Append(wxID_NEW, wxT("&New\tCtrl+N"));
   fileMenu->Append(wxID_OPEN, wxT("&Open...\tCtrl+O"));
   fileMenu->Append(wxID_ABOUT, _("&About Audacity..."));
   /* i18n-hint: Mac OS X shortcut should be Ctrl+, */
   fileMenu->Append(wxID_PREFERENCES, _("&Preferences...\tCtrl+,"));

   wxMenuBar *menuBar = new wxMenuBar();
   menuBar->Append(fileMenu, wxT("&File"));

   gParentFrame->SetMenuBar(menuBar);

   gParentFrame->Hide();

   SetTopWindow(gParentFrame);
#endif

   SetExitOnFrameDelete(true);

   AudacityProject *project = CreateNewAudacityProject(gParentWindow);
   SetTopWindow(project);

   delete temporarywindow;

#if !defined(__CYGWIN__)

   // Parse command-line arguments
   if (argc > 1) {
      for (int option = 1; option < argc; option++) {
         if (!argv[option])
            continue;
         bool handled = false;

         if (!wxString(wxT("-help")).CmpNoCase(argv[option])) {
            wxPrintf(/* i18n-hint: '-help', '-test' and
                      '-blocksize' need to stay in English. */
                   _("Command-line options supported:\n  -help (this message)\n  -test (run self diagnostics)\n  -blocksize ### (set max disk block size in bytes)\n\nIn addition, specify the name of an audio file or Audacity project\nto open it.\n\n"));
            exit(0);
         }

         if (option < argc - 1 &&
             argv[option + 1] &&
             !wxString(wxT("-blocksize")).CmpNoCase(argv[option])) {
            long theBlockSize;
            if (wxString(argv[option + 1]).ToLong(&theBlockSize)) {
               if (theBlockSize >= 256 && theBlockSize < 100000000) {
                  wxFprintf(stderr, _("Using block size of %ld\n"),
                          theBlockSize);
                  Sequence::SetMaxDiskBlockSize(theBlockSize);
               }
            }
            option++;
            handled = true;
         }

         if (!handled && !wxString(wxT("-test")).CmpNoCase(argv[option])) {
            RunBenchmark(NULL);
            exit(0);
         }

         if (argv[option][0] == wxT('-') && !handled) {
            wxPrintf(_("Unknown command line option: %s\n"), argv[option]);
            exit(0);
         }

         if (!handled)
            project->OpenFile(argv[option]);

      }                         // for option...
   }                            // if (argc>1)

#else
	
   // Cygwin command line parser (by Dave Fancella)
   if (argc > 1) {
      int optionstart = 1;
      bool startAtOffset = false;
		
      // Scan command line arguments looking for trouble
      for (int option = 1; option < argc; option++) {
         if (!argv[option])
            continue;
         // Check to see if argv[0] is copied across other arguments.
         // This is the reason Cygwin gets its own command line parser.
         if (wxString(argv[option]).Lower().Contains(wxString(wxT("audacity.exe")))) {
            startAtOffset = true;
            optionstart = option + 1;
         }
      }
		
      for (int option = optionstart; option < argc; option++) {
         if (!argv[option])
            continue;
         bool handled = false;
         bool openThisFile = false;
         wxString fileToOpen;
			
         if (!wxString(wxT("-help")).CmpNoCase(argv[option])) {
            wxPrintf(/* i18n-hint: '-help', '-test' and
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
             !wxString(wxT("-blocksize")).CmpNoCase(argv[option])) {
            long theBlockSize;
            if (wxString(argv[option + 1]).ToLong(&theBlockSize)) {
               if (theBlockSize >= 256 && theBlockSize < 100000000) {
                  wxFprintf(stderr, _("Using block size of %ld\n"),
                          theBlockSize);
                  Sequence::SetMaxDiskBlockSize(theBlockSize);
               }
            }
            option++;
            handled = true;
         }

         if (!handled && !wxString(wxT("-test")).CmpNoCase(argv[option])) {
            RunBenchmark(NULL);
            exit(0);
         }

         if (argv[option][0] == wxT('-') && !handled) {
            wxPrintf(_("Unknown command line option: %s\n"), argv[option]);
            exit(0);
         }
			
         if(handled)
            fileToOpen.Clear();
			
         if (!handled)
            fileToOpen = fileToOpen + wxT(" ") + argv[option];
         if(wxString(argv[option]).Lower().Contains(wxT(".aup")))
            openThisFile = true;
         if(openThisFile) {
            openThisFile = false;
            project->OpenFile(fileToOpen);
         }

      }                         // for option...
   }                            // if (argc>1)

#endif // Cygwin command-line parser

   gInited = true;

   return TRUE;
}

bool AudacityApp::InitCleanSpeech()
{
   wxString cwd = FROMFILENAME(::wxGetCwd());
   wxString presetsFromPrefs = gPrefs->Read(wxT("/Directories/PresetsDir"), wxT(""));
   wxString presets = wxT("");

   #ifdef __WXGTK__
   if (presetsFromPrefs.GetChar(0) != wxT('/'))
      presetsFromPrefs = wxT("");
   #endif

   #ifdef __WXMSW__
   wxString presetsDefaultLoc = cwd + wxT("\\presets");
   #else
   wxString presetsDefaultLoc = cwd + wxT("/presets");
   #endif

   // Stop wxWindows from printing its own error messages (not used ... does this really do anything?)
   wxLogNull logNo;

   // Try temp dir that was stored in prefs first
   if (presetsFromPrefs != wxT("")) {
      if (wxDirExists(FILENAME(presetsFromPrefs)))
         presets = presetsFromPrefs;
      else if (wxMkdir(FILENAME(presetsFromPrefs)))
         presets = presetsFromPrefs;
   }

   // If that didn't work, try the default location
   if ((presets == wxT("")) && (presetsDefaultLoc != wxT(""))) {
      if (wxDirExists(FILENAME(presetsDefaultLoc)))
         presets = presetsDefaultLoc;
      else if (wxMkdir(FILENAME(presetsDefaultLoc)))
         presets = presetsDefaultLoc;
   }

   if (presets == wxT("")) {
      // Failed
      wxMessageBox(_("Audacity could not find a place to store\n.csp CleanSpeech preset files\nAudacity is now going to exit. \nInstallation may be corrupt."));
      return false;
   }

   // The permissions don't always seem to be set on
   // some platforms.  Hopefully this fixes it...
   #ifdef __UNIX__
   chmod(FILENAME(presets).fn_str(), 0755);
   #endif

   gPrefs->Write(wxT("/Directories/PresetsDir"), presets);
   return true;
}

bool AudacityApp::InitTempDir()
{
   // We need to find a temp directory location.

   wxString tempFromPrefs = gPrefs->Read(wxT("/Directories/TempDir"), wxT(""));
   wxString tempDefaultLoc = wxGetApp().defaultTempDir;

   wxString temp = wxT("");

   #ifdef __WXGTK__
   if (tempFromPrefs.GetChar(0) != wxT('/'))
      tempFromPrefs = wxT("");
   #endif

   // Stop wxWindows from printing its own error messages

   wxLogNull logNo;

   // Try temp dir that was stored in prefs first

   if (tempFromPrefs != wxT("")) {
      if (wxDirExists(FILENAME(tempFromPrefs)))
         temp = tempFromPrefs;
      else if (wxMkdir(FILENAME(tempFromPrefs)))
         temp = tempFromPrefs;
   }

   // If that didn't work, try the default location

   if (temp==wxT("") && tempDefaultLoc != wxT("")) {
      if (wxDirExists(FILENAME(tempDefaultLoc)))
         temp = tempDefaultLoc;
      else if (wxMkdir(FILENAME(tempDefaultLoc)))
         temp = tempDefaultLoc;
   }

   if (temp == wxT("")) {
      // Failed
      wxMessageBox(_("Audacity could not find a place to store temporary files.\nPlease enter an appropriate directory in the preferences dialog."));

      PrefsDialog dialog(NULL);
      dialog.ShowTempDirPage();
      dialog.ShowModal();

      wxMessageBox(_("Audacity is now going to exit. Please launch Audacity again to use the new temporary directory."));
      return false;
   }

   // The permissions don't always seem to be set on
   // some platforms.  Hopefully this fixes it...
   #ifdef __UNIX__
   chmod(FILENAME(temp).fn_str(), 0755);
   #endif

   gPrefs->Write(wxT("/Directories/TempDir"), temp);
   DirManager::SetTempDir(temp);

   // Make sure the temp dir isn't locked by another process.
   if (!CreateSingleInstanceChecker(temp))
      return false;

   DirManager::CleanTempDir(true);

   return true;
}

// Return true if there are no other instances of Audacity running,
// false otherwise.
//
// Use "dir" for creating lockfiles (on OS X and Unix).

bool AudacityApp::CreateSingleInstanceChecker(wxString dir)
{
   wxLogNull dontLog;

   wxString name = wxString::Format(wxT("audacity-lock-%s"), wxGetUserId().c_str());
   mChecker = new wxSingleInstanceChecker();

   if (!mChecker->Create(FILENAME(name), FILENAME(dir))) {
      // Error initializing the wxSingleInstanceChecker.  We don't know
      // whether there is another instance running or not.

      wxString prompt =
         _("Audacity was not able to lock the temporary files directory.\nThis folder may be in use by another copy of Audacity.\nRunning two copies of Audacity simultaneously may cause\ndata loss or cause your system to crash.\n\nDo you still want to start Audacity?");
      int action = wxMessageBox(prompt,
                                _("Error locking temporary folder"),
                                wxYES_NO | wxICON_EXCLAMATION,
                                NULL);
      if (action == wxNO) {
         delete mChecker;
         return false;
      }
   }
   else if ( mChecker->IsAnotherRunning() ) {
      // There is another copy of Audacity running.  Force quit.
      
      wxString prompt =
         _("The system has detected that another copy of Audacity is running.\nRunning two copies of Audacity simultaneously may lead to\ndata loss or cause your system to crash, so is not allowed.\n\nUse the New or Open commands in the currently running Audacity\nprocess to open multiple projects simultaneously.\n");
      wxMessageBox(prompt, _("Audacity is already running"),
            wxOK | wxICON_ERROR);
      delete mChecker;
      return false;
   }

   return true;
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
   while (multiPathString != wxT("")) {
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

   if (pattern == wxT(""))
      return;

   for(unsigned i=0; i<pathList.GetCount(); i++) {
      wxString path = pathList[i];

      wxString fname = 
         wxFindFirstFile(path + wxFILE_SEP_PATH + pattern);

      while(fname != wxT("")) {
         results.Add(fname);
         fname = wxFindNextFile();
      }
   }
}

void AudacityApp::OnKeyDown(wxKeyEvent & event)
{
   // Remember it
   mLastKeyDown = event;

   // Not handled
   event.Skip(true);

   // Make sure this event is destined for a project window
   AudacityProject *prj = GetActiveProject();
   if (prj != wxGetTopLevelParent(wxWindow::FindFocus()))
      return;

   if (prj->HandleKeyDown(event))
      event.Skip(false);
}

void AudacityApp::OnChar(wxKeyEvent & event)
{
   // Default to being handled
   event.Skip(true);

   // Make sure this event is destined for a project window
   AudacityProject *prj = GetActiveProject();
   if (prj != wxGetTopLevelParent(wxWindow::FindFocus()))
      return;

   if (prj->HandleChar(mLastKeyDown))
      event.Skip(false);
}

void AudacityApp::OnKeyUp(wxKeyEvent & event)
{
   // Not handled
   event.Skip(true);

   // Make sure this event is destined for a project window
   AudacityProject *prj = GetActiveProject();
   if (prj != wxGetTopLevelParent(wxWindow::FindFocus()))
      return;

   if (prj->HandleKeyUp(event))
      event.Skip(false);
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
      if(gPrefs->Read(wxT("/QDeleteCmdCfgLocation"), &bFalse))
      {
         gPrefs->DeleteEntry(wxT("/QDeleteCmdCfgLocation"));
         gPrefs->Write(wxT("/DeleteCmdCfgLocation"), true);
      }
   }

   FinishPreferences();

   UnloadEffects();

   DeinitAudioIO();
   Internat::CleanUp();// JKC

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

//BG: On Windows, associate the aup file type with Audacity
/* We do this in the Windows installer now, 
	to avoid issues where user doesn't have admin privileges, but 
	in case that didn't work, allow the user to decide at startup.

	//v Should encapsulate this & allow access from Prefs, too, 
	//		if people want to manually change associations.
*/
#if defined(__WXMSW__) && !defined(__WXUNIVERSAL__) && !defined(__CYGWIN__)
void AudacityApp::AssociateFileTypes()
{
	wxRegKey associateFileTypes;
	associateFileTypes.SetName(wxT("HKCR\\.AUP"));
	bool bKeyExists = associateFileTypes.Exists();
	if (!bKeyExists) {
		// Not at HKEY_CLASSES_ROOT. Try HKEY_CURRENT_USER.
		associateFileTypes.SetName(wxT("HKCU\\.AUP"));
		bKeyExists = associateFileTypes.Exists();
	}
	if (!bKeyExists) {	
		// File types are not currently associated. 
		// Check pref in case user has already decided against it.
		bool bWantAssociateFiles = true;
		if (!gPrefs->Read(wxT("/WantAssociateFiles"), &bWantAssociateFiles) || 
				bWantAssociateFiles) {
			// Either there's no pref or user does want associations 
			// and they got stepped on, so ask.
			int wantAssoc = 
				wxMessageBox(
					_("Audacity project (.AUP) files are not currently \nassociated with Audacity. \n\nAssociate them, so they open on double-click?"), 
					_("Audacity Project Files"), 
					wxYES_NO | wxICON_QUESTION);
			if (wantAssoc == wxYES) {
				gPrefs->Write(wxT("/WantAssociateFiles"), true);

				wxString root_key = wxT("HKCR");
				associateFileTypes.SetName(wxT("HKCR\\.AUP")); // Start again with HKEY_CLASSES_ROOT.
				if (!associateFileTypes.Create(true)) {
					// Not at HKEY_CLASSES_ROOT. Try HKEY_CURRENT_USER.
					associateFileTypes.SetName(wxT("HKCU\\.AUP"));
					if (!associateFileTypes.Create(true)) { 
						// Actually, can't create keys. Empty root_key to flag failure.
						root_key.Empty();
					} else {
						// User must not have privileges to create at HKEY_CLASSES_ROOT.
						// Succeeded setting key for HKEY_CURRENT_USER. Continue that way.
						root_key = wxT("HKCU");
					}
				}
				if (root_key.IsEmpty()) {
					//v Warn that we can't set keys. Ask whether to set pref for no retry?
				} else {
					associateFileTypes = wxT("Audacity.Project"); // Finally set value for .AUP key

					associateFileTypes.SetName(root_key + wxT("\\Audacity.Project"));
					if(!associateFileTypes.Exists()) {
						associateFileTypes.Create(true);
						associateFileTypes = wxT("Audacity Project File");
					}

					associateFileTypes.SetName(root_key + wxT("\\Audacity.Project\\shell"));
					if(!associateFileTypes.Exists()) {
						associateFileTypes.Create(true);
						associateFileTypes = wxT("");
					}

					associateFileTypes.SetName(root_key + wxT("\\Audacity.Project\\shell\\open"));
					if(!associateFileTypes.Exists()) {
						associateFileTypes.Create(true);
					}

					associateFileTypes.SetName(root_key + wxT("\\Audacity.Project\\shell\\open\\command"));
					wxString tmpRegAudPath;
					if(associateFileTypes.Exists()) {
						tmpRegAudPath = wxString(associateFileTypes).Lower();
					}
					if (!associateFileTypes.Exists() || 
							(tmpRegAudPath.Find(wxT("\\audacity.exe")) >= 0)) {
						associateFileTypes.Create(true);
						associateFileTypes = (wxString)argv[0] + (wxString)wxT(" \"%1\"");
					}
				}
			} else {
				// User said no. Set a pref so we don't keep asking.
				gPrefs->Write(wxT("/WantAssociateFiles"), false);
			}
		}
	}
}
#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 49c2c7b5-6e93-4f33-83ab-ddac56ea598d

/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/app.h>
#include <wx/window.h>
#endif

#include <wx/fs_zip.h>

#include <wx/image.h>

#ifdef __WXGTK__
#include <unistd.h>
#endif

#ifdef __WXMAC__
# ifdef __UNIX__
#  include <ApplicationServices/ApplicationServices.h>
# else
#  include <AEDataModel.h>
#  include <AppleEvents.h>
# endif
#endif

#ifdef __WXMSW__
#endif

#include "AboutDialog.h"
#include "AudacityApp.h"
#include "AButton.h"
#include "ASlider.h"
#include "APalette.h"
#include "AudioIO.h"
#include "FreqWindow.h"
#include "Help.h"
#include "Prefs.h"
#include "Project.h"
#include "WaveTrack.h"
#include "effects/Amplify.h"
#include "effects/BassBoost.h"
#include "effects/Echo.h"
#include "effects/Fade.h"
#include "effects/Filter.h"
#include "effects/Invert.h"
#include "effects/NoiseRemoval.h"
#include "effects/Phaser.h"
#include "effects/Reverse.h"
#include "effects/Wahwah.h"
#include "prefs/PrefsDialog.h"

#define AUDACITY_MENUS_ENUM
#include "Menus.h"
#undef AUDACITY_MENUS_ENUM

#ifdef __WXMAC__
#include "effects/LoadVSTMac.h"
#endif

#ifdef __WXMSW__
#include "effects/LoadVSTWin.h"
#endif

#ifdef __WXGTK__
void wxOnAssert(const char *fileName, int lineNumber, const char *msg)
{
   if (msg)
      printf("ASSERTION FAILED: %s\n%s: %d\n", msg, fileName, lineNumber);
   else
      printf("ASSERTION FAILED!\n%s: %d\n", fileName, lineNumber);

   // Force core dump
//  int *i = 0;
//  if (*i)
//      exit(1);

//  exit(0);
}
#endif

wxFrame *gParentFrame = NULL;
wxWindow *gParentWindow = NULL;

void QuitAudacity()
{
   // Try to close and save each window.  If the user hits cancel, bail.
   int len = gAudacityProjects.Count();
   for (int i = 0; i < len; i++)
      if (!gAudacityProjects[i]->Close())
         return;

   if (gAPaletteFrame)
      gAPaletteFrame->Destroy();
   if (gFreqWindow)
      gFreqWindow->Destroy();
   if (gParentFrame)
      gParentFrame->Destroy();

   gAPaletteFrame = NULL;
   gFreqWindow = NULL;
   gParentFrame = NULL;

   QuitHelp();

   FinishPreferences();
}

IMPLEMENT_APP(AudacityApp)

BEGIN_EVENT_TABLE(AudacityApp, wxApp)
EVT_MENU(AboutID, AudacityApp::OnMenuAbout)
EVT_MENU(NewID, AudacityApp::OnMenuNew)
#ifdef __WXMAC__
EVT_MENU(OpenID, AudacityApp::OnMenuOpen)
#endif
EVT_MENU(PreferencesID, AudacityApp::OnMenuPreferences)
EVT_MENU(ExitID, AudacityApp::OnMenuExit)
END_EVENT_TABLE()

#if defined(__WXMAC__)

wxString wxMacFSSpec2MacFilename( const FSSpec *spec ) ;

pascal OSErr AEQuit(const AppleEvent * theAppleEvent, AppleEvent * theReply,
                    long Refcon)
{
   QuitAudacity();

   return noErr;
}

pascal OSErr AEOpenFiles(const AppleEvent * theAppleEvent, AppleEvent * theReply,
                         long Refcon)
{
   AEDescList docList;
   AEKeyword keywd;
   DescType returnedType;
   Size actualSize;
   long itemsInList;
   FSSpec theSpec;
   CInfoPBRec pb;
   Handle nameh;
   short namelen;
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

// The `main program' equivalent, creating the windows and returning the
// main frame
bool AudacityApp::OnInit()
{
#if 0
   mChecker = new wxSingleInstanceChecker(GetAppName());
   if ( mChecker->IsAnotherRunning() )
   {
      wxMessageBox("Audacity is already running...");

      return FALSE;
   }
#endif
#ifdef __WXMSW__
   // This is our own replacement for wxSingleInstanceChecker for Windows.
   mSingleInstanceMutex = (void *)::CreateMutex(NULL, FALSE, "AudacitySingleInstanceMutex");
   bool alreadyRunning = (GetLastError() == ERROR_ALREADY_EXISTS);
   if (alreadyRunning) {
      wxString prompt =
         "The system has detected that another copy of Audacity may be running.\n"
         "Running two copies of Audacity simultaneously may lead to data loss or\n"
         "cause your system to crash.\n\n"
         "Are you sure you want to launch Audacity now?";
         int action = wxMessageBox(prompt,
                                   "Audacity is already running",
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   NULL);
         if (action == wxNO) {
            CloseHandle((HANDLE)mSingleInstanceMutex);
            return false;
         }
   }
#endif
   
   
   ::wxInitAllImageHandlers();

   wxFileSystem::AddHandler(new wxZipFSHandler);

   InitPreferences();
   InitAudioIO();

#ifdef __WXMAC__
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

   Effect::RegisterEffect(new EffectAmplify());
   Effect::RegisterEffect(new EffectBassBoost());
   Effect::RegisterEffect(new EffectEcho());
   Effect::RegisterEffect(new EffectFadeIn());
   Effect::RegisterEffect(new EffectFadeOut());
   Effect::RegisterEffect(new EffectFilter());
   Effect::RegisterEffect(new EffectInvert());
   Effect::RegisterEffect(new EffectNoiseRemoval());
   Effect::RegisterEffect(new EffectPhaser());
   Effect::RegisterEffect(new EffectReverse());
   Effect::RegisterEffect(new EffectWahwah());

#if defined(__WXMAC__)
   LoadVSTPlugins(wxPathOnly(argv[0]));
#endif
#if defined(__WXMSW__)
   LoadVSTPlugins(wxPathOnly(argv[0]));
#endif

#if defined(__WXMAC__)
   AEInstallEventHandler(kCoreEventClass,
                         kAEOpenDocuments,
                         NewAEEventHandlerUPP(AEOpenFiles), 0, 0);
   AEInstallEventHandler(kCoreEventClass,
                         kAEQuitApplication,
                         NewAEEventHandlerUPP(AEQuit), 0, 0);
#endif

   SetExitOnFrameDelete(true);

   /*
      #ifdef __WXMSW__
      wxMDIParentFrame *parentFrame;
      parentFrame = new wxMDIParentFrame(NULL, 0, "Audacity",
      wxDefaultPosition,
      wxDefaultSize,
      wxDEFAULT_FRAME_STYLE | wxMAXIMIZE,
      "Audacity");
      parentFrame->Show(true);

      gParentFrame = parentFrame;
      gParentWindow = parentFrame->GetClientWindow();
      #endif
    */

   InitAPaletteFrame(gParentWindow);
   InitFreqWindow(gParentWindow);
   AudacityProject *project = CreateNewAudacityProject(gParentWindow);
   SetTopWindow(project);

#ifndef __MACOSX__

   // Parse command-line arguments

   if (argc > 1) {
      for (int option = 1; option < argc; option++) {
         if (!argv[option])
            continue;
         bool handled = false;

         if (!wxString("-help").CmpNoCase(argv[option])) {
            printf("Command-line options supported:\n");
            printf("  -help (this message)\n");
            printf("  -test (run self diagnostics)\n");
            printf
                ("  -blocksize ### (set max disk block size in bytes)\n");
            printf("\n");
            printf("In addition, specify the name of an audio file or "
                   "Audacity project\n");
            printf("to open it.\n");
            printf("\n");
            exit(0);
         }

         if (option < argc - 1 &&
             argv[option + 1] &&
             !wxString("-blocksize").CmpNoCase(argv[option])) {
            long theBlockSize;
            if (wxString(argv[option + 1]).ToLong(&theBlockSize)) {
               if (theBlockSize >= 256 && theBlockSize < 100000000) {
                  fprintf(stderr, "Using block size of %d\n",
                          theBlockSize);
                  WaveTrack::SetMaxDiskBlockSize(theBlockSize);
               }
            }
            option++;
            handled = true;
         }

         if (!handled && !wxString("-test").CmpNoCase(argv[option])) {
            RunTest();
            exit(0);
         }

         if (argv[option][0] == '-' && !handled) {
            printf("Unknown command line option: %s\n", argv[option]);
         }

         if (!handled)
            project->OpenFile(argv[option]);

      }                         // for option...
   }                            // if (argc>1)

#endif

   return TRUE;
}

void AudacityApp::OnMenuAbout(wxCommandEvent & event)
{
   AboutDialog dlog(NULL);
   dlog.ShowModal();
}

void AudacityApp::OnMenuNew(wxCommandEvent & event)
{
   CreateNewAudacityProject(gParentWindow);
}

void AudacityApp::OnMenuOpen(wxCommandEvent & event)
{
   wxString path = gPrefs->Read("/DefaultOpenPath",::wxGetCwd());

   wxString fileName = wxFileSelector("Select an audio file...",
                                      path,     // Path
                                      "",       // Name
                                      "",       // Extension
                                      "All files (*.*)|*.*|"
                                      "Audacity projects (*.aup)|*.aup|"
                                      "WAV files (*.wav)|*.wav|"
                                      "AIFF files (*.aif)|*.aif|"
                                      "AU files (*.au)|*.au|"
                                      "IRCAM files (*.snd)|*.snd|"
                                      "MP3 files (*.mp3)|*.mp3",
                                      0,        // Flags
                                      NULL);    // Parent

   if (fileName != "") {

      path =::wxPathOnly(fileName);
      gPrefs->Write("/DefaultOpenPath", path);

      // Make sure it isn't already open
      int numProjects = gAudacityProjects.Count();
      for (int i = 0; i < numProjects; i++)
         if (gAudacityProjects[i]->mFileName == fileName) {
            wxMessageBox("That project is already open in another window.");
            return;
         }

      AudacityProject *project = CreateNewAudacityProject(gParentWindow);
      project->OpenFile(fileName);
   }
}

void AudacityApp::OnMenuPreferences(wxCommandEvent & event)
{
   PrefsDialog dialog(NULL /* parent */ );
   dialog.ShowModal();
}

void AudacityApp::OnMenuExit(wxCommandEvent & event)
{
   QuitAudacity();
}

int AudacityApp::OnExit() {
   while (Pending())
      Dispatch();

#if 0
   delete mChecker;
#endif
#ifdef __WXMSW__
   if (mSingleInstanceMutex)
      CloseHandle((HANDLE)mSingleInstanceMutex);
   mSingleInstanceMutex = NULL;
#endif

   return 0;
}

void AudacityApp::RunTest()
{
#ifdef __WXGTK__
   DirManager *d = new DirManager();
   WaveTrack *t = new WaveTrack(d);
   VTrack *tmp = NULL;

   t->rate = 1.0;

   srand(234657);

   int len = 1024;
   int scale = 19500 + (rand() % 1000);
   int trials = 100;

   printf("scale: %d\n", scale);

   int ideal = t->GetIdealBlockSize();

   sampleType *small = new sampleType[len];
   sampleType *small2 = new sampleType[len];
   sampleType *block = new sampleType[scale];

   printf("Preparing...\n");

   int i, j, b, v;

   int m = 0;

   for (i = 0; i < len; i++) {
      v = sampleType(rand());
      small[i] = v;
      for (b = 0; b < scale; b++)
         block[b] = v;

      t->Append(block, scale);
   }

   if (t->numSamples != len * scale) {
      printf("Expected len %d, track len %d.\n", len * scale,
             t->numSamples);
      exit(0);
   }
   //t->Debug();

   int bad;

   printf("Running test...\n");

   int z;

   wxStartTimer();
   for (z = 0; z < trials; z++) {
      int x0 = rand() % len;
      int xlen = 1 + (rand() % (len - x0));
      printf("Cut: %d - %d \n", x0 * scale, (x0 + xlen) * scale);

      t->Cut(double (x0 * scale), double ((x0 + xlen) * scale), &tmp);
      if (!tmp) {
         printf("Trial %d\n", z);
         printf("Cut (%d, %d) failed.\n", (x0 * scale),
                (x0 + xlen) * scale);
         printf("Expected len %d, track len %d.\n", len * scale,
                t->numSamples);
         exit(0);
      }

      int y0 = rand() % (len - xlen);
      printf("Paste: %d\n", y0 * scale);

      t->Paste(double (y0 * scale), tmp);

      if (t->numSamples != len * scale) {
         printf("Trial %d\n", z);
         printf("Expected len %d, track len %d.\n", len * scale,
                t->numSamples);
         exit(0);
      }
      // Copy
      for (i = 0; i < xlen; i++)
         small2[i] = small[x0 + i];
      // Delete
      for (i = 0; i < (len - x0 - xlen); i++)
         small[x0 + i] = small[x0 + xlen + i];
      // Insert
      for (i = 0; i < (len - xlen - y0); i++)
         small[len - i - 1] = small[len - i - 1 - xlen];
      // Paste
      for (i = 0; i < xlen; i++)
         small[y0 + i] = small2[i];

      delete tmp;
   }

   long elapsed = wxGetElapsedTime();

   t->Debug();

   printf("Total time: %ld ms\n", elapsed);

#ifdef __WXGTK__
   printf("Checking file pointer leaks:\n");
   printf("Track # blocks: %d\n", t->GetBlockArray()->Count());
   printf("Disk # blocks: \n");
   system("ls .audacity_temp/* | wc --lines");
#endif

   printf("Correctness check: (blocks %d bytes %d)\n", len, scale);
   bad = 0;
   wxStartTimer();
   for (i = 0; i < len; i++) {
      v = small[i];
      t->Get(block, i * scale, scale);
      for (b = 0; b < scale; b++)
         if (block[b] != v) {
            bad++;
            if (bad < 10)
               printf("Bad: block %d byte %d\n", i, b);
            b = scale;
         }
   }
   if (bad == 0)
      printf("Passed!\n");
   else
      printf("Errors in %d/%d blocks\n", bad, len);

   elapsed = wxGetElapsedTime();

   printf("Time to check all data: %ld ms\n", elapsed);

   printf("Second measurement:\n");

   wxStartTimer();

   for (int j = 0; j < 2; j++) {
      for (i = 0; i < len; i++) {
         v = small[i];
         t->Get(block, i * scale + j, scale - j);
         for (b = 0; b < scale; b++)
            if (block[b] != v)
               bad++;
      }
   }

   elapsed = wxGetElapsedTime();

   printf("Done.\n");

   printf("Second measurement: %ld ms\n", elapsed / 2);

   delete t;

   delete[]small;
   delete[]small2;
   delete[]block;
#endif
}

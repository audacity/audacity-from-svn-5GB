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
#include <AEDataModel.h>
#include <AppleEvents.h>
#endif

#include "AudacityApp.h"
#include "AButton.h"
#include "ASlider.h"
#include "APalette.h"
#include "AudioIO.h"
#include "Benchmark.h"
#include "FreqWindow.h"
#include "Help.h"
#include "Prefs.h"
#include "Project.h"
#include "WaveTrack.h"
#include "effects/Amplify.h"
#include "effects/BassBoost.h"
#include "effects/Compressor.h"
#include "effects/Echo.h"
#include "effects/Fade.h"
#include "effects/Filter.h"
#include "effects/NoiseRemoval.h"
#include "effects/Phaser.h"
#include "effects/Wahwah.h"

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
  int *i = 0;
  if (*i)
      exit(1);

  exit(0);
}
#endif

wxFrame *gParentFrame = NULL;
wxWindow *gParentWindow = NULL;

void QuitAudacity()
{
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

   int len = gAudacityProjects.Count();
   for (int i = 0; i < len; i++)
      gAudacityProjects[i]->Destroy();
}

IMPLEMENT_APP(AudacityApp)
#ifdef __WXMAC__
pascal OSErr AEQuit(const AppleEvent * theAppleEvent, AppleEvent * theReply,
                    long Refcon)
{
   QuitAudacity();

   return noErr;
}

/* prototype of MoreFiles fn, included in wxMac already */
pascal OSErr FSpGetFullPath(const FSSpec * spec,
                            short *fullPathLength, Handle * fullPath);

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

      if (noErr == FSpGetFullPath(&theSpec, &namelen, &nameh)) {
         HLock(nameh);
         char *str = new char[namelen + 1];
         memcpy(str, (char *) *nameh, namelen);
         str[namelen] = 0;
         HUnlock(nameh);
         DisposeHandle(nameh);

         AudacityProject *project = GetActiveProject();

         if (project == NULL || !project->GetTracks()->IsEmpty()) {
            project = CreateNewAudacityProject(gParentWindow);
         }
         project->OpenFile(str);

         delete[]str;
      }
   }

   return noErr;
}
#endif

// The `main program' equivalent, creating the windows and returning the
// main frame
bool AudacityApp::OnInit()
{
 //  mChecker = new wxSingleInstanceChecker(GetAppName());
   ::wxInitAllImageHandlers();

   wxFileSystem::AddHandler(new wxZipFSHandler);

   InitPreferences();
   InitAudioIO();

   Effect::RegisterEffect(new EffectAmplify());
   Effect::RegisterEffect(new EffectBassBoost());
   Effect::RegisterEffect(new EffectCompressor());
   Effect::RegisterEffect(new EffectEcho());
   Effect::RegisterEffect(new EffectFadeIn());
   Effect::RegisterEffect(new EffectFadeOut());
   Effect::RegisterEffect(new EffectFilter());
   Effect::RegisterEffect(new EffectNoiseRemoval());
   Effect::RegisterEffect(new EffectPhaser());
   Effect::RegisterEffect(new EffectWahwah());

#if defined(__WXMAC__) || defined(__WXMSW__)
   LoadVSTPlugins();
#endif

#ifdef __WXMAC__
   AEInstallEventHandler(kCoreEventClass,
                         kAEOpenDocuments,
                         NewAEEventHandlerUPP(AEOpenFiles), 0, 0);
   AEInstallEventHandler(kCoreEventClass,
                         kAEQuitApplication,
                         NewAEEventHandlerUPP(AEQuit), 0, 0);
#endif

   SetExitOnFrameDelete(true);

   InitAPaletteFrame(gParentWindow);
   InitFreqWindow(gParentWindow);
   AudacityProject *project = CreateNewAudacityProject(gParentWindow);
   SetTopWindow(project);

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
                  fprintf(stderr, "Using block size of %ld\n",
                          theBlockSize);
                  WaveTrack::SetMaxDiskBlockSize(theBlockSize);
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
            printf("Unknown command line option: %s\n", argv[option]);
            exit(0);
         }

         if (!handled)
            project->OpenFile(argv[option]);

      }                         // for option...
   }                            // if (argc>1)

   return TRUE;
}


int AudacityApp::OnExit() {

//   delete mChecker;
   return 0;
}

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

#include <wx/image.h>

#ifdef __WXGTK__
#include <unistd.h>
#endif

#include "AudacityApp.h"
#include "AButton.h"
#include "ASlider.h"
#include "APalette.h"
#include "Play.h"
#include "Project.h"
#include "effects/Amplify.h"
#include "effects/Echo.h"
#include "effects/Fade.h"

#ifdef __WXMAC__
#include "effects/LoadVSTMac.h"
#endif

#ifdef __WXMSW__
#include "effects/LoadVSTWin.h"
#endif

#ifdef __WXGTK__
void wxOnAssert(const char* fileName, int lineNumber, const char* msg)
{
  if (msg)
	printf("ASSERTION FAILED: %s\n%s: %d\n",msg,fileName,lineNumber);
  else
	printf("ASSERTION FAILED!\n%s: %d\n",fileName,lineNumber);

  // Force core dump
  int *i = 0;
  if (*i)
	exit(1);

  exit(0);
}
#endif

IMPLEMENT_APP(AudacityApp)

// The `main program' equivalent, creating the windows and returning the
// main frame
bool AudacityApp::OnInit()
{
  ::wxInitAllImageHandlers();

  InitSoundPlayer();
  
  Effect::RegisterEffect(new EffectAmplify());
  Effect::RegisterEffect(new EffectEcho());
  Effect::RegisterEffect(new EffectFadeIn());
  Effect::RegisterEffect(new EffectFadeOut());

  #if defined(__WXMAC__) || defined(__WXMSW__)
  LoadVSTPlugins();
  #endif

  SetExitOnFrameDelete(true);

  InitAPalette(NULL);
  AudacityProject *project = CreateNewAudacityProject();
  SetTopWindow(gAPalette);

  // Parse command-line arguments

  if (argc > 1) {
	for(int option=1; option<argc; option++) {
	  if (!argv[option]) continue;
	  bool handled = false;

	  if (!wxString("-help").CmpNoCase(argv[option])) {
		printf("Command-line options supported:\n");
		printf("  -help (this message)\n");
		printf("  -test (run self diagnostics)\n");
		printf("  -blocksize ### (set max disk block size in bytes)\n");
		printf("\n");
		printf("In addition, specify the name of an audio file or "
			   "Audacity project\n");
		printf("to open it.\n");
		printf("\n");
		exit(0);
	  }

	  if (option<argc-1 &&
		  argv[option+1] &&
		  !wxString("-blocksize").CmpNoCase(argv[option])) {
		long theBlockSize;
		if (wxString(argv[option+1]).ToLong(&theBlockSize)) {
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
		printf("Unknown command line option: %s\n",
			   argv[option]);
		exit(0);
	  }
	  
	  if (!handled)
		project->OpenFile(argv[option]);

	} // for option...
  } // if (argc>1)

  return TRUE;
}

void AudacityApp::RunTest()
{
  DirManager *d = new DirManager();
  WaveTrack *t = new WaveTrack(d);
  VTrack *tmp = NULL;

  t->rate = 1.0;

  srand(234657);

  int len = 1024;
  int scale = 500 + (rand()%1000);
  int trials = 100;

  printf("scale: %d\n", scale);

  int ideal = t->GetIdealBlockSize();

  sampleType *small = new sampleType[len];
  sampleType *small2 = new sampleType[len];
  sampleType *block = new sampleType[scale];

  printf("Preparing...\n");

  int i, j, b, v;

  int m = 0;

  for(i=0; i<len; i++) {
	v = sampleType(rand());
	small[i] = v;
	for(b=0; b<scale; b++)
	  block[b] = v;

	t->Append(block, scale);
  }

  if (t->numSamples != len*scale) {
	printf("Expected len %d, track len %d.\n", len*scale, t->numSamples);
	exit(0);
  }

  //t->Debug();

  int bad;

  printf("Running test...\n");

  int z;
  
  wxStartTimer();
  for(z = 0; z < trials; z++) {
	int x0 = rand() % len;
	int xlen = 1 + (rand() % (len - x0 - 1));
	printf("Cut: %d - %d \n", x0*scale, (x0+xlen)*scale);

	t->Cut(double(x0*scale), double((x0 + xlen)*scale), &tmp);
	if (!tmp) {
	  printf("Trial %d\n", z);
	  printf("Cut (%d, %d) failed.\n", (x0*scale), (x0+xlen)*scale);
	  printf("Expected len %d, track len %d.\n", len*scale, t->numSamples);
	  exit(0);
	}

	int y0 = rand() % (len - xlen);
	printf("Paste: %d\n", y0*scale);

	t->Paste(double(y0*scale), tmp);

	if (t->numSamples != len*scale) {
	  printf("Trial %d\n", z);
	  printf("Expected len %d, track len %d.\n", len*scale, t->numSamples);
	  exit(0);
	}

	// Copy
	for(i=0; i<xlen; i++)
	  small2[i] = small[x0+i];
	// Delete
	for(i=0; i<(len-x0-xlen); i++)
	  small[x0+i] = small[x0+xlen+i];
	// Insert
	for(i=0; i<(len-xlen-y0); i++)
	  small[len-i-1] = small[len-i-1-xlen];
	// Paste
	for(i=0; i<xlen; i++)
	  small[y0+i] = small2[i];

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
  for(i=0; i<len; i++) {
	v = small[i];
	t->Get(block, i*scale, scale);
	for(b=0; b<scale; b++)
	  if (block[b] != v) {
		bad++;
		if (bad<10)
		  printf("Bad: block %d byte %d\n", i, b);
		b = scale;
	  }
  }
  if (bad==0)
	printf("Passed!\n");
  else
	printf("Errors in %d/%d blocks\n", bad, len);
  
  delete t;

  delete[] small;
  delete[] small2;
  delete[] block;
}

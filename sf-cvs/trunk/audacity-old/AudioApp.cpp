/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioApp.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include "wx/wx.h"
#endif

#include <wx/image.h>

#include "AudioApp.h"
#include "AudioFrame.h"
#include "AudioDoc.h"
#include "AudioView.h"

// For testing
#include <wx/file.h>

#include "Import.h"
#include "ImportRaw.h"
#include "DirManager.h"
#include "WaveTrack.h"
#include "GenericStream.h"
#include "ImportMP3.h"
#include "effects/Amplify.h"
#include "effects/Echo.h"
#include "effects/Fade.h"

#ifdef __WXMAC__
#include "effects/LoadVSTMac.h"
#endif

AudioFrame *frame = (AudioFrame *) NULL;

#if !wxUSE_DOC_VIEW_ARCHITECTURE
#error You must set wxUSE_DOC_VIEW_ARCHITECTURE to 1 in setup.h!
#endif

#ifdef __WXGTK__
void wxOnAssert(const char* fileName, int lineNumber, const char* msg)
{
  if (msg)
	printf("ASSERTION FAILED: %s\n%s: %d\n",msg,fileName,lineNumber);
  else
	printf("ASSERTION FAILED!\n%s: %d\n",fileName,lineNumber);
  exit(0);
}
#endif

void TestImportRaw(wxString fName)
{
  bool b16, sign, stereo, big, offset;

  GuessPCMFormat(fName, b16, sign, stereo, big, offset);
  printf("%30s: %s.%d.%s (%d)\n",
		 (const char *)fName,
		 stereo?"stereo":"mono",
		 b16?16:8,
		 sign?"s":"u",
		 offset);
}

void TestMP3(wxString fName)
{
  WaveTrack *left = 0;
  WaveTrack *right = 0;
  DirManager *dir = new DirManager();

  int startTime = clock();

  ::ImportMP3(fName, &left, &right, dir);

  int elapsedMS = clock() - startTime;
  double elapsedTime = elapsedMS * 0.000001;

  printf("Elapsed time: %lf sec\n",elapsedTime);
  printf("Decoded length: %lf sec\n",left->numSamples / 44100.0);

  exit(0);
}

void TestSuite()
{
  printf("Beginning test.\n");

  /*

  sampleType *buffer = new sampleType[1000];
  DirManager *dir = new DirManager();
  WaveTrack *t = new WaveTrack(dir);
  int i;
  
  for(i=0; i<1000; i++)
	buffer[i] = i;

  printf("Append\n");
  t->Append(buffer, 1000);
  t->Debug();

  printf("Delete\n");
  t->Delete(0, 100);
  t->Debug();

  printf("Delete\n");
  t->Delete(600, 100);
  t->Debug();

  for(i=0; i<100; i++)
	buffer[i] = 0;
  printf("Insert\n");
  t->Insert(buffer,200,100); 
  t->Debug();

  for(i=0; i<100; i++)
	buffer[i] = 1;
  printf("Insert\n");
  t->Insert(buffer,200,100); 
  t->Debug();

  for(i=0; i<100; i++)
	buffer[i] = 4;
  printf("Append\n");
  t->Append(buffer,100); 
  t->Debug();

  printf("Delete\n");
  t->Delete(950,100);
  t->Debug();

  printf("Finished modifications.\n");

  t->Get(buffer,0,1000);
  for(i=0; i<100; i++)
	printf("%4d %4d %4d %4d %4d %4d %4d %4d %4d %4d\n",
		   buffer[i*10+0], buffer[i*10+1],
		   buffer[i*10+2], buffer[i*10+3],
		   buffer[i*10+4], buffer[i*10+5],
		   buffer[i*10+6], buffer[i*10+7],
		   buffer[i*10+8], buffer[i*10+9]);

  printf("(Check summaries here)\n");

  wxString vaudio = ".";
  wxString foo1 = "foo1";
  wxString foo2 = "foo2";
  wxString foo3 = "foo3";

  t->Debug();

  printf("Saving foo1\n");
  dir->SetProject(vaudio,
				  foo1, true);
  GenericStream *out =
	new GenericStream("foo1.t");
  t->Save(out, false);
  delete out;

  t->Debug();

  printf("Set\n");
  for(i=0; i<100; i++)
	buffer[i] = 2;
  t->Set(buffer,100,100); 

  t->Debug();

  printf("Saving foo2\n");
  dir->SetProject(vaudio,
				  foo2, true);
  out = new GenericStream("foo2.t");
  t->Save(out, false);
  delete out;

  printf("Saving foo3\n");
  dir->SetProject(vaudio,
				  foo3, true);
  out = new GenericStream("foo3.t");
  t->Save(out, false);
  delete out;

  printf("Saving foo3 again\n");
  dir->SetProject(vaudio,
				  foo3, true);
  out = new GenericStream("foo3.t");
  t->Save(out, true);
  delete out;

  printf("Done.\n");

  delete[] buffer;
  delete t;
  delete dir;

  exit(0);

  */
}

void TestSuite2()
{
	wxFile f;
	
	int buffer[100];
	int i;
	
	for(i=0; i<100; i++)
		buffer[i] = 1000+i;
		
	f.Create("tempfile");
	f.Write(buffer,400);
	f.Close();
	
	f.Open("tempfile");
	f.Seek(200,wxFromStart);
	f.Read(buffer,200);
	f.Close();
	
	for(i=0; i<50; i++)
		buffer[i+50] = buffer[i];
}

AudioApp::AudioApp()
{
	m_docManager = 0;
}

AudioApp::~AudioApp()
{
}

bool AudioApp::OnInit()
{
  Effect::RegisterEffect(new EffectAmplify());
  Effect::RegisterEffect(new EffectEcho());
  Effect::RegisterEffect(new EffectFadeIn());
  Effect::RegisterEffect(new EffectFadeOut());
  
  #ifdef __WXMAC__
  LoadVSTPlugins();
  #endif
  
  /*
  Effect::RegisterEffect(new EffectEcho());
  Effect::RegisterEffect(new EffectCompressor());*/

  // Call this function to test WaveTrack and DirManager.
  // TestSuite();
  
  // Test wxFile
  // TestSuite2();
  /*
 	int x[10];
 	
 	for(int i=0; i<10; i++)
 		x[i] = 0xAABBCCDD;

 	for(int i=0; i<10; i++)
 	  x[i] = wxUINT32_SWAP_ON_BE(x[i]);//__lwbrx( &x[i] , 0 ) ;
 	  */


  // Allow us to import GIF, etc.
  #ifndef __WXMAC__
  ::wxInitAllImageHandlers();
  #endif
 	
  //// Create a document manager
  m_docManager = new wxDocManager;

  //// Create a template relating drawing documents to their views
  new wxDocTemplate(m_docManager,
						   "Audacity Project", "*.aup", "", "vap",
						   "Audio Doc", "Audio View",
          CLASSINFO(AudioDoc), CLASSINFO(AudioView));

  //// Create the main frame window
  frame = new AudioFrame(m_docManager, (wxFrame *) NULL, -1,
  			  "Audacity", wxPoint(0, 0), wxSize(300, 200),
  			  wxDEFAULT_FRAME_STYLE);
  
  //// Give it an icon (this is ignored in MDI mode: uses resources)
#ifdef __WXMSW__
  frame->SetIcon(wxIcon("doc_icn"));
#endif

  //// Make a menubar
  wxMenu *file_menu = new wxMenu;
  wxMenu *edit_menu = (wxMenu *) NULL;

  file_menu->Append(wxID_NEW, "&New...");
  file_menu->Append(wxID_OPEN, "&Open...");

  file_menu->AppendSeparator();
  file_menu->Append(wxID_EXIT, "E&xit");
  
  // Use this for a history menu
  // m_docManager->FileHistoryUseMenu(file_menu);

  wxMenu *help_menu = new wxMenu;
  help_menu->Append(wxID_ABOUT, "&About...");

  wxMenuBar *menu_bar = new wxMenuBar;

  menu_bar->Append(file_menu, "&File");
  if (edit_menu)
    menu_bar->Append(edit_menu, "&Edit");
  menu_bar->Append(help_menu, "&Help");

  // Associate the menu bar with the frame
  frame->SetMenuBar(menu_bar);

  frame->Centre(wxBOTH);
  //frame->Show(TRUE);

  SetTopWindow(frame);

  // Try to open any documents specified on the command line:

  bool openedFile = false;

  if (argc > 1) {

	for(int option=1; option<argc; option++) {
	  if (argv[option][0] == '-')
		printf("Unrecognized option: %s\n",argv[option]);
	  else {
		wxString fileName(argv[option]);

		//TestMP3(fileName);

		//TestImportRaw(fileName);

		wxDocument *d = m_docManager->CreateDocument(fileName, wxDOC_SILENT);

		if (d)
		  openedFile = true;
	  }
	}

  }

  if (!openedFile) {

	// Create new document (i.e. project)
	wxDocument *d = m_docManager->CreateDocument(wxString(""), wxDOC_NEW);

	// Import any audio files specified on command line
	DirManager *dirManager = &((AudioDoc *)d)->dirManager;
	TrackList *tracks = &((AudioDoc *)d)->tracks;
    for(int option=1; option<argc; option++) {
	  if (argv[option][0] != '-') {
		wxString fileName(argv[option]);

		WaveTrack *left;
		WaveTrack *right;
		if (ImportWAV(fileName, &left, &right, dirManager)) {
			if (left)
				tracks->Add(left);
			if (right)
				tracks->Add(right);
		}
			
	  }
	}
	((AudioView *)(d->GetFirstView()))->FixScrollbars();
	((AudioView *)(d->GetFirstView()))->SelectNone();
	//((AudioView *)(d->GetFirstView()))->Refresh();
  }

  return TRUE;
}

int AudioApp::MainLoop()
{
  return wxApp::MainLoop();
}

int AudioApp::OnExit(void)
{
  delete m_docManager;
  return 0;
}

wxFrame *AudioApp::CreateChildFrame(wxDocument *doc,
									wxView *view, bool isCanvas)
{
  //// Make a child frame
  wxDocChildFrame *subframe =
	new wxDocChildFrame(doc, view, GetMainFrame(), -1, "Child Frame",
        wxPoint(10, 10), wxSize(700, 400), wxDEFAULT_FRAME_STYLE);

  //#ifdef __WXMSW__
  //  subframe->SetIcon(wxString(isCanvas ? "chrt_icn" : "notepad_icn"));
  //#endif

//  subframe->Centre(wxBOTH);

  return subframe;
}

IMPLEMENT_APP(AudioApp)

AudioFrame *GetMainFrame(void)
{
  return frame;
}



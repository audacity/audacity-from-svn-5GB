/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioView.cpp

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

#ifdef __WXMSW__
#include "resource.h"
#endif

#include <wx/filedlg.h>

#include "AudioView.h"
#include "AudioApp.h"
#include "AudioDoc.h"
#include "WaveTrack.h"
#include "NoteTrack.h"
#include "LabelTrack.h"
#include "AudioFrame.h"
#include "Export.h"
#include "Import.h"
#include "ImportRaw.h"
#include "ImportMIDI.h"
#include "ImportMP3.h"
#include "effects/Effect.h"
#include "Pitch.h"
#include "Mix.h"
#include "Play.h"
#include "UndoManager.h"

enum {
  ZoomInButtonID = 1001,
  ZoomOutButtonID,
  
  PlayButtonID,
  StopButtonID,
  
  HSBarID,
  VSBarID,	  
  
  WaveTrackID,
  LabelTrackID,
  RemoveTrackID,
  
  ImportID,
  ImportRawID,
  ImportMIDIID,
  ImportMP3ID,
  
  ExportID,
  
  UndoID,
  RedoID,
  CutID,
  CopyID,
  PasteID,
  ClearID,
  SelectAllID,
  
  WaveDisplayID,
  SpectrumDisplayID,
  AutoCorrelateID,
  PitchID,
  QuickMixID,
  
  FirstEffectID  // Keep this the last enum please
};

int AudioView::labelWidth = 40;

// Global variable to determine when the application should quit.
int gNumViewsOpen = 0;

IMPLEMENT_DYNAMIC_CLASS(AudioView, wxView)

BEGIN_EVENT_TABLE(AudioView, wxView)
  EVT_MENU(AutoCorrelateID, AudioView::AutoCorrelate)
  EVT_MENU(PitchID, AudioView::Pitch)
  EVT_MENU(QuickMixID, AudioView::QuickMix)
  EVT_MENU(WaveTrackID, AudioView::NewWaveTrack)
  EVT_MENU(LabelTrackID, AudioView::NewLabelTrack)
  EVT_MENU(RemoveTrackID, AudioView::RemoveTracks)
  EVT_MENU(ImportID, AudioView::Import)
  EVT_MENU(ImportRawID, AudioView::ImportRaw)
  EVT_MENU(ImportMIDIID, AudioView::ImportMIDI)
  EVT_MENU(ImportMP3ID, AudioView::ImportMP3)
  EVT_MENU(ExportID, AudioView::Export)
  EVT_MENU(WaveDisplayID, AudioView::WaveDisplay)
  EVT_MENU(SpectrumDisplayID, AudioView::SpectrumDisplay)
  EVT_MENU(UndoID, AudioView::Undo)
  EVT_MENU(RedoID, AudioView::Redo)
  EVT_MENU(CutID, AudioView::Cut)
  EVT_MENU(CopyID, AudioView::Copy)
  EVT_MENU(PasteID, AudioView::Paste)
  EVT_MENU(ClearID, AudioView::Clear)
  EVT_MENU(SelectAllID, AudioView::SelectAll)
  EVT_BUTTON  (ZoomInButtonID,   AudioView::OnZoomInButton)
  EVT_BUTTON  (ZoomOutButtonID,   AudioView::OnZoomOutButton)
  EVT_BUTTON  (PlayButtonID,   AudioView::OnPlayButton)
  EVT_BUTTON  (StopButtonID,   AudioView::OnStopButton)
  EVT_COMMAND_SCROLL  (HSBarID,   AudioView::Scroll)
  EVT_COMMAND_SCROLL  (VSBarID,   AudioView::Scroll)
  EVT_SIZE (AudioView::OnSize)
  EVT_KEY_DOWN (AudioView::OnKeyDown)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(RulerPanel, wxPanel)
  EVT_MOUSE_EVENTS(RulerPanel::OnMouseEvent)
  EVT_PAINT(RulerPanel::OnPaint)
  EVT_ERASE_BACKGROUND(RulerPanel::OnEraseBackground)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(TrackPanel, wxPanel)
  EVT_MOUSE_EVENTS(TrackPanel::OnMouseEvent)
  EVT_PAINT(TrackPanel::OnPaint)
  EVT_ERASE_BACKGROUND(TrackPanel::OnEraseBackground)
END_EVENT_TABLE()

#ifdef __WXMAC__
  #define REDRAW(W)                            \
    if (1) {                                   \
		  wxPaintEvent *junk = new wxPaintEvent(); \
		  W->OnPaint(*junk);                       \
			delete junk;                             \
    }
#else
  #define REDRAW(W) W->Refresh(false)
#endif

#ifdef __WXMSW__
  #define AUDACITY_BITMAP_TYPE wxBITMAP_TYPE_BMP_RESOURCE
  #define BITMAP_PRE ""
  #define BITMAP_SUF ""
#else
  #ifdef __WXMAC
    #define AUDACITY_BITMAP_TYPE wxBITMAP_TYPE_PICT_RESOURCE
    #define BITMAP_PRE ""
    #define BITMAP_SUF ""
  #else
    #define AUDACITY_BITMAP_TYPE wxBITMAP_TYPE_XPM
    #define BITMAP_PRE "icons/"
    #define BITMAP_SUF ".XPM"
  #endif
#endif



AudioView::AudioView()
{
  frame = (wxFrame *) NULL;

  backgroundColor.Set(204,204,204);
  backgroundBrush.SetColour(204,204,204);
  backgroundPen.SetColour(204,204,204);

  // unselected
  lightBrush[0].SetColour(255,255,255);
  mediumBrush[0].SetColour(204,204,204);
  darkBrush[0].SetColour(170,170,170);
  lightPen[0].SetColour(255,255,255);
  mediumPen[0].SetColour(204,204,204);
  darkPen[0].SetColour(170,170,170);

  // selected
  lightBrush[1].SetColour(204,204,255);
  mediumBrush[1].SetColour(170,170,204);
  darkBrush[1].SetColour(148,148,170);
  lightPen[1].SetColour(204,204,255);
  mediumPen[1].SetColour(170,170,204);
  darkPen[1].SetColour(148,148,170);

  trackPanel = (TrackPanel *) NULL;

  hsbar = (wxScrollBar *) NULL;
  vsbar = (wxScrollBar *) NULL;
  
  numEffects = 0;

  myTimer.view = this;
  myTimer.Start(kAutoScrollDelay, FALSE); // Call timer every 50 ms

  clipboard = new TrackList();

  capturedTrack = NULL;

  mouseClickX = 0;
  mouseClickY = 0;
  
  clipLen = 0.0;

  sel0 = 0.0;
  sel1 = 0.0;

  // Horizontal scrollbar
  sinfo.total = 1.0;
  sinfo.screen = 1.0;
  sinfo.h = 0.0;
  sinfo.zoom = 40100.0 / 512.0;
  sinfo.lastZoom = sinfo.zoom;

  sinfo.scrollStep = 16;

  sinfo.sbarH = 0;
  sinfo.sbarScreen = 1;
  sinfo.sbarTotal = 1;

  // Vertical scrollbar
  vpos = 0;
  
  clipLen = 0.0;

  mainSizer = 0;
  topSizer = 0;
  bottomSizer = 0;
  trackSizer = 0;
}

AudioView::~AudioView()
{
}

// What to do when a view is created. Creates actual
// windows for displaying the view.
bool AudioView::OnCreate(wxDocument *doc, long WXUNUSED(flags) )
{
  gNumViewsOpen++;

  frame = wxGetApp().CreateChildFrame(doc, this, TRUE);

  //
  // Create Menu Bar
  //

  menuBar = new wxMenuBar();

  fileMenu = new wxMenu();
  fileMenu->Append(wxID_NEW, "&New...\tCtrl+N");
  fileMenu->Append(wxID_OPEN, "&Open...\tCtrl+O");
  fileMenu->Append(wxID_CLOSE, "&Close\tCtrl+W");
  fileMenu->Append(wxID_SAVE, "&Save\tCtrl+S");
  fileMenu->Append(wxID_SAVEAS, "Save As...");
  fileMenu->Append(ExportID, "&Export...");
  fileMenu->AppendSeparator();
  fileMenu->Append(wxID_PRINT, "&Print...\tCtrl+P");
  fileMenu->Append(wxID_PRINT_SETUP, "Print Setup...");
  fileMenu->Append(wxID_PREVIEW, "Print Preview");
  fileMenu->AppendSeparator();
  fileMenu->Append(wxID_EXIT, "E&xit");

  editMenu = new wxMenu();
  editMenu->Append(UndoID, "&Undo\tCtrl+Z");
  editMenu->Append(RedoID, "&Redo\tCtrl+R");
  editMenu->AppendSeparator();
  editMenu->Append(CutID, "Cut\tCtrl+X");
  editMenu->Append(CopyID, "Copy\tCtrl+C");
  editMenu->Append(PasteID, "Paste\tCtrl+V");
  editMenu->Append(ClearID, "Clear\tCtrl+B");
  editMenu->AppendSeparator();
  editMenu->Append(SelectAllID, "Select All\tCtrl+A");

  doc->GetCommandProcessor()->SetEditMenu(editMenu);

  projectMenu = new wxMenu();

  projectMenu->Append(ImportID, "&Import Audio...\tCtrl+I");
  projectMenu->Append(ImportRawID, "Import Raw Data...");
  projectMenu->Append(ImportMIDIID, "Import &MIDI...");
  projectMenu->Append(ImportMP3ID, "Import MP3...");
  projectMenu->AppendSeparator();
  projectMenu->Append(WaveTrackID, "New Audio Track");
  //  projectMenu->Append(LabelTrackID, "New Label Track");
  projectMenu->AppendSeparator();
  projectMenu->Append(RemoveTrackID, "&Remove Track(s)");

  trackMenu = new wxMenu();
  trackMenu->Append(QuickMixID, "Quick Mix");
  trackMenu->AppendSeparator();
  trackMenu->Append(WaveDisplayID, "Waveform Display");
  trackMenu->Append(SpectrumDisplayID, "Spectrum Display");
  trackMenu->AppendSeparator();
  //  trackMenu->Append(AutoCorrelateID, "AutoCorrelate");
  trackMenu->Append(PitchID, "Pitch Extract");
  
  effectMenu = new wxMenu();

  numEffects = Effect::GetNumEffects();
  for(int fi = 0; fi < numEffects; fi++)
    effectMenu->Append(FirstEffectID+fi,
		       (Effect::GetEffect(fi))->GetEffectName());

  helpMenu = new wxMenu;
  helpMenu->Append(wxID_ABOUT, "&About Audacity...");

  menuBar->Append(fileMenu, "&File");
  menuBar->Append(editMenu, "&Edit");
  menuBar->Append(projectMenu, "&Project");
  menuBar->Append(trackMenu, "&Track");
  menuBar->Append(effectMenu, "E&ffect");
  menuBar->Append(helpMenu, "&Help");

  frame->SetMenuBar(menuBar);  
  
  //
  // Make all child windows
  //

  int sbarWidth = 15;

  rulerPanel =
	new RulerPanel(this, frame, wxDefaultPosition,
				   wxSize(600 - labelWidth, 30), 0);

  trackPanel =
	new TrackPanel(this, frame, wxDefaultPosition,
				   wxSize(600, 300), 0);

  hsbar =
	new wxScrollBar(frame, HSBarID, wxDefaultPosition,
					wxSize(600, sbarWidth), wxSB_HORIZONTAL);

  vsbar =
	new wxScrollBar(frame, VSBarID, wxDefaultPosition,
					wxSize(sbarWidth, 300), wxSB_VERTICAL);

  status = new wxStaticText(frame, 0, "", wxDefaultPosition,
			  wxDefaultSize);
  status->SetLabel("");

  wxButton *b1;
  wxButton *b2;
  wxButton *b3;
  wxButton *b4;

  wxBitmap *zoomIn = new wxBitmap();
  wxBitmap *zoomOut = new wxBitmap();
  wxBitmap *play = new wxBitmap();
  wxBitmap *stop = new wxBitmap();

  if (zoomIn->LoadFile(BITMAP_PRE "ZoomIn" BITMAP_SUF,AUDACITY_BITMAP_TYPE) &&
      zoomOut->LoadFile(BITMAP_PRE "ZoomOut" BITMAP_SUF,AUDACITY_BITMAP_TYPE) &&
      play->LoadFile(BITMAP_PRE "Play" BITMAP_SUF,AUDACITY_BITMAP_TYPE) &&
      stop->LoadFile(BITMAP_PRE "Stop" BITMAP_SUF,AUDACITY_BITMAP_TYPE)) {
    
    b1 = (wxButton *)new wxBitmapButton
      (frame, ZoomInButtonID, *zoomIn,
       wxPoint(0, 0), wxSize(36, 36));
    b2 = (wxButton *)new wxBitmapButton
      (frame, ZoomOutButtonID, *zoomOut,
       wxPoint(30, 0), wxSize(36, 36));
    b3 = (wxButton *)new wxBitmapButton
      (frame, PlayButtonID, *play,
	   wxPoint(30, 0), wxSize(36, 36));
    b4 = (wxButton *)new wxBitmapButton
      (frame, StopButtonID, *stop,
       wxPoint(30, 0), wxSize(36, 36));

    b1->SetBackgroundColour(backgroundColor);
    b2->SetBackgroundColour(backgroundColor);
    b3->SetBackgroundColour(backgroundColor);
    b4->SetBackgroundColour(backgroundColor);
  }	  
  else {
	delete zoomIn;
	delete zoomOut;
	delete play;
	delete stop;
	
	b1 = new wxButton(frame, ZoomInButtonID, "<",
					  wxPoint(0, 0), wxSize(36,36));
	b2 = new wxButton(frame, ZoomOutButtonID, ">",
					  wxPoint(0, 0), wxSize(36,36));
	b3 = new wxButton(frame, PlayButtonID, "Play",
					  wxPoint(0, 0), wxSize(128,36));
	b4 = new wxButton(frame, StopButtonID, "Stop",
					  wxPoint(0, 0), wxSize(128,36));
  }

  wxBitmap *smallLogoBitmap = new wxBitmap();
  wxStaticBitmap *smallLogo = 0;
  if (smallLogoBitmap->LoadFile(BITMAP_PRE "AudacitySmall" BITMAP_SUF,
				AUDACITY_BITMAP_TYPE)) {
    smallLogo = new wxStaticBitmap(frame, 0, *smallLogoBitmap,
				   wxDefaultPosition, wxDefaultSize);
  }

  //
  // Lay them out using box sizers
  //

  mainSizer = new wxBoxSizer(wxVERTICAL);
  topSizer = new wxBoxSizer(wxHORIZONTAL);
  bottomSizer = new wxBoxSizer(wxHORIZONTAL);
  trackSizer = new wxBoxSizer(wxVERTICAL);

  mainSizer->Add(topSizer, 1, wxEXPAND, 0);
  mainSizer->Add(bottomSizer, 0, wxEXPAND | wxALL, 2);

  topSizer->Add(trackSizer, 1, wxEXPAND, 0);
  topSizer->Add(vsbar, 0, wxEXPAND | wxBOTTOM, sbarWidth);

  trackSizer->Add(rulerPanel, 0, wxEXPAND | wxLEFT, labelWidth);
  trackSizer->Add(trackPanel, 1, wxEXPAND, 0);
  trackSizer->Add(hsbar, 0, wxEXPAND, 0);

  bottomSizer->Add(b1, 0, wxEXPAND, 0);
  bottomSizer->Add(b2, 0, wxEXPAND, 0);
  bottomSizer->Add(b3, 0, wxEXPAND | wxLEFT, 24);
  bottomSizer->Add(b4, 0, wxEXPAND, 0);
  bottomSizer->Add(status, 1, wxEXPAND | wxLEFT, 24);
  if (smallLogo)
    bottomSizer->Add(smallLogo, 0, wxLEFT | wxRIGHT, 24);

  frame->SetAutoLayout(true);
  frame->SetSizer(mainSizer);

  mainSizer->Fit(frame);
  mainSizer->SetSizeHints(frame);

  //
  //
  //

  InitialState();

  FixScrollbars();
  
  frame->SetBackgroundColour(backgroundColor);

#ifdef __X__
  // X seems to require a forced resize
  int x, y;
  frame->GetSize(&x, &y);
  frame->SetSize(-1, -1, x, y);
#endif

  // Min size, max size
  frame->SetSizeHints(250,200,20000,20000);

  frame->Show(TRUE);

  #ifdef __WXMAC__
  
  // This (hack) tells various windows not to erase the background on update events.
  //frame->m_MacEraseBack = false;
  trackPanel->m_MacEraseBack = false;
  rulerPanel->m_MacEraseBack = false;
  hsbar->m_MacEraseBack = false;
  vsbar->m_MacEraseBack = false;
  #endif

  #ifdef DEBUG_PASTE_BUG  // probably can remove this - fixed

  WaveTrack *left = 0;
  WaveTrack *right = 0;
  ImportWAV("Mussorgsky1.WAV", &left, &right,
			&((AudioDoc *)GetDocument())->dirManager);

  selected->Clear();  
  GetTracks()->Add(left);
  selected->Add(left);

  PushState();

  sel0 = 2.0;
  sel1 = 4.0;

  Cut();

  left->Debug();

  sel0 = 4.0;
  sel1 = 4.0;

  Paste();

  left->Debug();

  FixScrollbars();
  REDRAW(trackPanel);
  REDRAW(rulerPanel);

  #endif
  
  return TRUE;
}

void AudioView::UpdateMenus()
{
  numEffects = Effect::GetNumEffects();

  if (undoManager.UndoAvailable())
    editMenu->Enable(editMenu->FindItem(wxString("Undo")), TRUE);
  else
    editMenu->Enable(editMenu->FindItem(wxString("Undo")), FALSE);    

  if (undoManager.RedoAvailable())
    editMenu->Enable(editMenu->FindItem(wxString("Redo")), TRUE);
  else
    editMenu->Enable(editMenu->FindItem(wxString("Redo")), FALSE);    

  if (clipboard->First() != 0) {
    editMenu->Enable(editMenu->FindItem(wxString("Paste")), TRUE);
  }
  else {
    editMenu->Enable(editMenu->FindItem(wxString("Paste")), FALSE);
  }
	
  TrackList *tracks = GetTracks();

  int numTracks=0;
  int numSelected=0;

  VTrack *t = tracks->First();
  while(t) {
	numTracks++;
	if (t->selected)
	  numSelected++;
	t = tracks->Next();
  }
  
  if (numSelected>0) {
    trackMenu->Enable(trackMenu->FindItem(wxString("Quick Mix")), TRUE);
    trackMenu->Enable(trackMenu->FindItem(wxString("Waveform Display")), TRUE);
    trackMenu->Enable(trackMenu->FindItem(wxString("Spectrum Display")), TRUE);
    trackMenu->Enable(trackMenu->FindItem(wxString("AutoCorrelate")), TRUE);
    trackMenu->Enable(trackMenu->FindItem(wxString("Pitch Extract")), TRUE);

    trackMenu->Enable(trackMenu->FindItem(wxString("Remove")), TRUE);
		
    fileMenu->Enable(projectMenu->FindItem(wxString("Export")), TRUE);
	
	for (int fi = 0; fi < numEffects; fi++)
	  effectMenu->Enable(effectMenu->FindItem(
        (Effect::GetEffect(fi))->GetEffectName()), TRUE);

	editMenu->Enable(editMenu->FindItem(wxString("Cut")), TRUE);
	editMenu->Enable(editMenu->FindItem(wxString("Copy")), TRUE);
	editMenu->Enable(editMenu->FindItem(wxString("Clear")), TRUE);
  }
  else {	
    for (int fi = 0; fi < numEffects; fi++)
      effectMenu->Enable(effectMenu->FindItem
			 ((Effect::GetEffect(fi))->GetEffectName()), FALSE);

    editMenu->Enable(editMenu->FindItem(wxString("Cut")), FALSE);
    editMenu->Enable(editMenu->FindItem(wxString("Copy")), FALSE);
    editMenu->Enable(editMenu->FindItem(wxString("Clear")), FALSE);

    trackMenu->Enable(trackMenu->FindItem(wxString("Quick Mix")), FALSE);
    trackMenu->Enable(trackMenu->FindItem(wxString("Waveform Display")), FALSE);
    trackMenu->Enable(trackMenu->FindItem(wxString("Spectrum Display")), FALSE);
    trackMenu->Enable(trackMenu->FindItem(wxString("AutoCorrelate")), FALSE);
    trackMenu->Enable(trackMenu->FindItem(wxString("Pitch Extract")), FALSE);

    trackMenu->Enable(trackMenu->FindItem(wxString("Remove")), FALSE);
		
    fileMenu->Enable(projectMenu->FindItem(wxString("Export")), FALSE);
  }
}

TrackList *AudioView::GetTracks()
{
  AudioDoc *doc = ((AudioDoc *)GetDocument());
  wxASSERT(doc);
  return &doc->tracks;
}

void AudioView::FixScrollbars()
{
  if (!hsbar || !vsbar)
    return;

  AudioDoc *doc = ((AudioDoc *)GetDocument());
  if (!doc)
    return;
    
  bool rescroll = false;

  TrackList *tracks = GetTracks();
  int totalHeight = (tracks->GetHeight() + 32);

  int panelWidth, panelHeight;
  trackPanel->GetSize(&panelWidth, &panelHeight);

  sinfo.total = tracks->GetMaxLen() + 1.0;
  sinfo.screen = ((double)panelWidth) / sinfo.zoom;

  if (sinfo.h > sinfo.total - sinfo.screen) {
  	sinfo.h = sinfo.total - sinfo.screen;
  	rescroll = true;
  }
  if (sinfo.h < 0.0) {
  	sinfo.h = 0.0;
  	rescroll = true;
  }

  sinfo.sbarTotal = (int)(sinfo.total * sinfo.zoom) / sinfo.scrollStep;
  sinfo.sbarScreen = (int)(sinfo.screen * sinfo.zoom) / sinfo.scrollStep;

  sinfo.sbarH = (int)(sinfo.h * sinfo.zoom) / sinfo.scrollStep;

  vpos =
	vsbar->GetThumbPosition() * sinfo.scrollStep;

  if (vpos >= totalHeight)
	vpos = totalHeight-1;
  if (vpos < 0)
	vpos = 0;

  hsbar->Show(sinfo.screen < sinfo.total);
  vsbar->Show(panelHeight < totalHeight);

  if (panelHeight >= totalHeight && vpos != 0) {
	vpos = 0;
	REDRAW(trackPanel);
	REDRAW(rulerPanel);
	rescroll = false;
  }
  if (sinfo.screen >= sinfo.total && sinfo.sbarH != 0) {
	sinfo.sbarH = 0;
	REDRAW(trackPanel);
	REDRAW(rulerPanel);
	rescroll = false;
  }

  hsbar->SetScrollbar(sinfo.sbarH, sinfo.sbarScreen,
					  sinfo.sbarTotal, sinfo.sbarScreen, TRUE);
  vsbar->SetScrollbar(vpos / sinfo.scrollStep,
					  panelHeight / sinfo.scrollStep,
					  totalHeight / sinfo.scrollStep,
					  panelHeight / sinfo.scrollStep,
					  TRUE);

  sinfo.lastZoom = sinfo.zoom;
  
  if (rescroll && sinfo.screen < sinfo.total) {
	  REDRAW(trackPanel);
		REDRAW(rulerPanel);
  }
}

void AudioView::OnSize(wxSizeEvent &evt)
{
  if (frame)
  {
    if (mainSizer)
      mainSizer->RecalcSizes();
    frame->Layout();
    FixScrollbars();
    frame->Clear();
    frame->Refresh(true);
  }
}

void AudioView::Scroll(wxScrollEvent &evt)
{
  int  hlast = sinfo.sbarH;
  int  vlast = vpos;
  int  hoffset = 0;
  int  voffset = 0;

  sinfo.sbarH = hsbar->GetThumbPosition();

  if (sinfo.sbarH != hlast) {
	  sinfo.h = (sinfo.sbarH * sinfo.scrollStep) / sinfo.zoom;
	  if (sinfo.h > sinfo.total - sinfo.screen)
		  sinfo.h = sinfo.total - sinfo.screen;
	  if (sinfo.h < 0.0)
		  sinfo.h = 0.0;
		hoffset = (sinfo.sbarH - hlast) * sinfo.scrollStep;
  }

  vpos = vsbar->GetThumbPosition() * sinfo.scrollStep;
  voffset = vpos - vlast;

  // Track panel is updated either way, but it is smart and only redraws
  // what is needed
  trackPanel->FastScroll(-hoffset, -voffset);

  // Ruler panel updated if we scroll horizontally
  if (hoffset) {
    REDRAW(rulerPanel);
  }
}

bool AudioView::ProcessEvent(wxEvent& event)
{
  if (event.GetEventType() == wxEVT_COMMAND_MENU_SELECTED &&
      event.GetId() >= FirstEffectID &&
      event.GetId() < FirstEffectID + numEffects) {
    Effect *f = Effect::GetEffect(event.GetId() - FirstEffectID);

    TrackList *tracks = GetTracks();
    VTrack *t = tracks->First();
    
    while(t) {
      if (t->selected && t->GetKind() == (VTrack::Wave)) {
	f->DoInPlaceEffect((WaveTrack *)t, sel0, sel1);
      }
      
      t = tracks->Next();
    }
    
    PushState();
    
    FixScrollbars();
    REDRAW(trackPanel);
    REDRAW(rulerPanel);
    
    // This indicates we handled the event.
    return true;
  }
  
  return wxView::ProcessEvent(event);
}

void AudioView::InitialState()
{
  undoManager.ClearStates();
  PushState();
}

void AudioView::PushState()
{
  TrackList *l = new TrackList(GetTracks());

  undoManager.PushState(l, sel0, sel1);
  delete l;
}

void AudioView::PopState(TrackList *l)
{
  TrackList *tracks = GetTracks();

  tracks->Clear();
  VTrack *t = l->First();
  while(t) {
	//    printf("Popping track with %d samples\n",
	//           ((WaveTrack *)t)->numSamples);
    //	((WaveTrack *)t)->Debug();
    tracks->Add(t->Duplicate());
    t = l->Next();
  }
}

void AudioView::Undo()
{
  wxASSERT(undoManager.UndoAvailable());

  TrackList *l = undoManager.Undo(&sel0, &sel1);
  PopState(l);

  FixScrollbars();
  REDRAW(trackPanel);
}

void AudioView::Redo()
{
  wxASSERT(undoManager.RedoAvailable());
  
  TrackList *l = undoManager.Redo(&sel0, &sel1);
  PopState(l);

  FixScrollbars();
  REDRAW(trackPanel);
}

void AudioView::ClearClipboard()
{
  VTrack *n = clipboard->First();
  while(n) {
    delete n;
    n = clipboard->Next();
  }
  
  clipLen = 0.0;
  
  clipboard->Clear();
}

void AudioView::Cut()
{
  ClearClipboard();

  TrackList *tracks = GetTracks();
  VTrack *n = tracks->First();
  VTrack *dest = 0;

  while(n) {
	if (n->selected) {
	  n->Cut(sel0, sel1, &dest);
	  if (dest)
		clipboard->Add(dest);
	}
	n = tracks->Next();
  }

  clipLen = (sel1 - sel0);

  sel1 = sel0;

  PushState();

  FixScrollbars();
  REDRAW(trackPanel);
  REDRAW(rulerPanel);
  UpdateMenus();
}

void AudioView::Copy()
{
  ClearClipboard();

  TrackList *tracks = GetTracks();
  VTrack *n = tracks->First();
  VTrack *dest = 0;

  while(n) {
	if (n->selected) {
	  n->Copy(sel0, sel1, &dest);
	  if (dest)
		clipboard->Add(dest);
	}
	n = tracks->Next();
  }

  clipLen = (sel1 - sel0);

  sel1 = sel0;
  UpdateMenus();

  //  PushState();
  //  Not an undoable operation
}

void AudioView::Paste()
{
  if (sel0 != sel1)
    Clear();
    
  wxASSERT(sel0 == sel1);

  double tsel = sel0;

  TrackList *tracks = GetTracks();
  VTrack *n = tracks->First();
  VTrack *c = clipboard->First();

  while(n && c) {
	if (n->selected) {
	  n->Paste(tsel, c);
	  c = clipboard->Next();
	}
	  
	n = tracks->Next();
  }

  // TODO: What if we clicked past the end of the track?

  sel0 = tsel;
  sel1 = tsel + clipLen;

  PushState();

  FixScrollbars();
  REDRAW(trackPanel);
  REDRAW(rulerPanel);
  UpdateMenus();
}

void AudioView::Clear()
{
  TrackList *tracks = GetTracks();
  VTrack *n = tracks->First();

  while(n) {
	if (n->selected)
	  n->Clear(sel0, sel1);
	n = tracks->Next();
  }

  sel1 = sel0;

  PushState();
  FixScrollbars();
  REDRAW(trackPanel);
  REDRAW(rulerPanel);
  UpdateMenus();
}

void AudioView::SelectAll()
{
  TrackList *tracks = GetTracks();

  VTrack *t = tracks->First();
  while(t) {
	t->selected = true;
	t = tracks->Next();
  }
  sel0 = 0.0;
  sel1 = tracks->GetMaxLen();
  
  REDRAW(trackPanel);
  REDRAW(rulerPanel);
  UpdateMenus();
}

void AudioView::SelectNone()
{
  TrackList *tracks = GetTracks();

  VTrack *t = tracks->First();
  while(t) {
	t->selected = false;
	t = tracks->Next();
  }
}

void AudioView::OnKeyDown(wxKeyEvent& keyEvent)
{
  if (keyEvent.KeyCode() == WXK_BACK ||
	  keyEvent.KeyCode() == WXK_DELETE)
	Clear();
}

void AudioView::WaveDisplay()
{
  TrackList *tracks = GetTracks();
  VTrack *n = tracks->First();

  while(n) {
	if (n->selected && n->GetKind() == VTrack::Wave)
	  ((WaveTrack *)n)->SetDisplay(WaveTrack::WaveDisplay);
	n = tracks->Next();
  }

  REDRAW(trackPanel);
  REDRAW(rulerPanel);
}

void AudioView::SpectrumDisplay()
{
  TrackList *tracks = GetTracks();
  VTrack *n = tracks->First();

  while(n) {
	if (n->selected && n->GetKind() == VTrack::Wave)
	  ((WaveTrack *)n)->SetDisplay(WaveTrack::SpectrumDisplay);
	n = tracks->Next();
  }

  REDRAW(trackPanel);
  REDRAW(rulerPanel);
}

void AudioView::RemoveTracks()
{
  TrackList *list = GetTracks();
  VTrack *t = list->First();

  while(t) {
	if (t->selected)
	  t = list->RemoveCurrent();
	else
	  t = list->Next();
  }

  PushState();

  REDRAW(trackPanel);
  REDRAW(rulerPanel);
  UpdateMenus();
}

void AudioView::Export()
{
  VTrack *left = 0;
  VTrack *right = 0;
  VTrack *t;
  int numSelected = 0;

  TrackList *tracks = GetTracks();
  t = tracks->First();
  while(t) {
	if (t->selected)
	  numSelected++;
	if (t->GetKind() != VTrack::Wave) {
	  wxMessageBox("Only audio tracks can be exported.");
	  return;
	}
	t = tracks->Next();
  }

  if (numSelected == 0) {
	wxMessageBox("Please select one or two tracks before trying to export.");
	return;
  }

  if (numSelected > 2) {
	wxMessageBox("Cannot export more than two tracks (stereo).  "
				 "Please select either one or two tracks.");
  }

  left = tracks->First();  
  while (left && (!left->selected))
	left = tracks->Next();
  
  do {
	right = tracks->Next();
  } while (right && (!right->selected));
  
  ::Export((WaveTrack *)left, (WaveTrack *)right);
}

void AudioView::ImportRaw()
{
  wxString fileName =
	wxFileSelector("Select a PCM File...",
				   "", // Path
				   "", // Name
				   "", // Extension
				   "", // Wildcard
				   0, // Flags
				   GetFrame()); // Parent

  if (fileName == "")
    return;

  WaveTrack *left = 0;
  WaveTrack *right = 0;
  
  if (::ImportRaw(fileName, &left, &right, &((AudioDoc *)GetDocument())->dirManager)) {

	SelectNone();

    if (left) {
      GetTracks()->Add(left);
	  left->selected = true;
    }

    if (right) {
      GetTracks()->Add(right);
	  right->selected = true;
    }

    PushState();

    FixScrollbars();
    REDRAW(trackPanel);
  	REDRAW(rulerPanel);
  }
  
}

void AudioView::NewWaveTrack()
{
  WaveTrack *t = new WaveTrack(&((AudioDoc *)GetDocument())->dirManager);

  SelectNone();

  GetTracks()->Add(t);
  t->selected = true;

  PushState();

  FixScrollbars();
    
  REDRAW(trackPanel);
  REDRAW(rulerPanel);
}

void AudioView::NewLabelTrack()
{
  LabelTrack *t = new LabelTrack(&((AudioDoc *)GetDocument())->dirManager);

  SelectNone();

  GetTracks()->Add(t);
  t->selected = true;

  PushState();

  FixScrollbars();
    
  REDRAW(trackPanel);
  REDRAW(rulerPanel);
}

void AudioView::Import()
{
  wxString fileName =
	wxFileSelector("Select an audio file...",
				   "", // Path
				   "", // Name
				   "", // Extension
				   "", // Wildcard
				   0, // Flags
				   GetFrame()); // Parent

  if (fileName == "")
    return;

  WaveTrack *left = 0;
  WaveTrack *right = 0;
  
  if (ImportWAV(fileName, &left, &right, &((AudioDoc *)GetDocument())->dirManager)) {

	if (left || right) {
	  SelectNone();
	}

    if (left) {
      GetTracks()->Add(left);
	  left->selected = true;
    }

    if (right) {
      GetTracks()->Add(right);
	  right->selected = true;
    }

    PushState();

    FixScrollbars();
    REDRAW(trackPanel);
    REDRAW(rulerPanel);
  }
  
}

void AudioView::ImportMIDI()
{
  wxString fileName =
	wxFileSelector("Select a MIDI File...",
				   "", // Path
				   "", // Name
				   ".mid", // Extension
				   "*.mid", // Wildcard
				   0, // Flags
				   GetFrame()); // Parent

  if (fileName == "")
    return;

  NoteTrack *newTrack =
	new NoteTrack(&((AudioDoc *)GetDocument())->dirManager);
  
  if (::ImportMIDI(fileName, newTrack)) {

    SelectNone();
    GetTracks()->Add(newTrack);
	newTrack->selected = true;
    
    PushState();

    FixScrollbars();
    REDRAW(trackPanel);
    REDRAW(rulerPanel);
  }
}
void AudioView::ImportMP3()
{
  #if 0 // seems to be working better now!
    static bool warned=false;

    if (!warned) {
	wxMessageBox("Warning: This feature is not stable and may crash.  "
				 "Proceed at your own risk.");
	warned = true;
    }
  #endif // 0

  wxString fileName =
	wxFileSelector("Select a MP3 File...",
				   "", // Path
				   "", // Name
				   ".mp3", // Extension
				   "*.mp3", // Wildcard
				   0, // Flags
				   GetFrame()); // Parent
  
  if (fileName == "")
    return;

  WaveTrack *left = 0;
  WaveTrack *right = 0;

  if (::ImportMP3(fileName, &left, &right,
		  &((AudioDoc *)GetDocument())->dirManager)) {  
    if (left || right) {
      SelectNone();
    }
    
    if (left) {
      GetTracks()->Add(left);
      left->selected = true;
    }
    
    if (right) {
      GetTracks()->Add(right);
      right->selected = true;
    }
    
    PushState();
    
    FixScrollbars();
    REDRAW(trackPanel);
    REDRAW(rulerPanel);
  }
}

void AudioView::DrawRuler(wxDC *dc, wxRect *visible)
{
  wxRect r = *visible;

  int sel = 0;

  dc->SetBrush(mediumBrush[sel]);
  dc->SetPen(mediumPen[sel]);
  
  dc->DrawRectangle(r);
  
  dc->SetPen(lightPen[sel]);
  
  dc->DrawLine(r.x, r.y, r.x + r.width, r.y);
  dc->DrawLine(r.x, r.y, r.x, r.y + r.height);
  dc->DrawLine(r.x+1, r.y+1, r.x + r.width, r.y+1);
  dc->DrawLine(r.x+1, r.y+1, r.x+1, r.y + r.height);	
  
  dc->SetPen(darkPen[sel]);
  
  dc->DrawLine(r.x, r.y + r.height-1, r.x + r.width, r.y + r.height-1);
  dc->DrawLine(r.x+1, r.y + r.height-2, r.x + r.width, r.y + r.height-2);
  
  dc->DrawLine(r.x+r.width-1,r.y,r.x+r.width-1,r.y+r.height);
  dc->DrawLine(r.x+r.width-2,r.y+1,r.x+r.width-2,r.y+r.height);  

  

  int minSpace = 60;  // min pixels between labels

  wxString unitStr;
  double unit = 1.0;
  double base;
  
  while(unit*sinfo.zoom < minSpace)
    unit *= 2.0;
  while(unit*sinfo.zoom > minSpace*2)
    unit /= 2.0;
	
  if (unit < 0.0005) {
    unitStr = "us"; // microseconds
    base = 0.000001;
  }
  else if (unit < 0.5) {
    unitStr = "ms"; // milliseconds
    base = 0.001;
  }
  else if (unit < 30.0) {
    unitStr = "s"; // seconds
    base = 1.0;  
  }
  else if (unit < 1800.0) {
    unitStr = "m"; // minutes
    base = 60.0;  
  }
  else {
    unitStr = "h"; // hours
    base = 3600.0;
  }
  
  unit = base;
  
  bool hand=true;
	
  while(unit*sinfo.zoom < minSpace) {
    unit *= (hand? 5.0 : 2.0);
    hand = !hand;
  }
  while(unit*sinfo.zoom > minSpace*(hand? 2.0 : 5.0)) {
    unit /= (hand? 2.0 : 5.0);
    hand = !hand;
  }
  
  unit /= 4;
  
  double pos = sinfo.h;
  int unitcount = (int)(pos / unit);

  dc->SetTextForeground(wxColour(0,0,204));
  
  int nextxpos = 0;

  for(int pixel=0; pixel<r.width; pixel++) {

	if (((int)(pos / unit)) > unitcount) {
	  unitcount = (int)(pos/unit);

	  switch(unitcount%4) {
	  case 0:
		dc->DrawLine(r.x+pixel,r.y+8,r.x+pixel,r.y+r.height);
				
		char str[100];
		sprintf(str,"%.1f%s",unitcount*unit/base, (const char *)unitStr);
		long textWidth, textHeight;
		dc->GetTextExtent(str, &textWidth, &textHeight);
		if (pixel >= nextxpos && pixel+2+textWidth < r.width) {
		  dc->DrawText(str, r.x+pixel+2, r.y+12);
		  
		  nextxpos = pixel + textWidth + 12;
		}
		break;

	  case 1:
	  case 3:
		dc->DrawLine(r.x+pixel,r.y+r.height-4,r.x+pixel,r.y+r.height);
		break;

	  case 2:
		dc->DrawLine(r.x+pixel,r.y+r.height-6,r.x+pixel,r.y+r.height);
		break;
	  }
	}
	pos += 1.0 / sinfo.zoom;
  }
}

VTrack *AudioView::FindTrack(int mouseX, int mouseY, bool label,
							 wxRect *trackRect, int *trackNum)
{
  wxRect r;
  if (label) {
	r.x = 0;
	r.width = labelWidth - 1;
  }
  else {
	int wid, ht;
	r.x = labelWidth;
	trackPanel->GetSize(&wid, &ht);
	r.width = wid - labelWidth;
  }
  r.y = -vpos;

  TrackList *tracks = GetTracks();
  VTrack *t = tracks->First();

  int n=1;
  
  while(t) {
	r.height = t->GetHeight();
	
  	if (r.Inside(mouseX, mouseY)) {
      if (trackRect)
        *trackRect = r;
      if (trackNum)
        *trackNum = n;
      return t;
    }

	r.y += r.height;
	n++;
	t = tracks->Next();
  }

  if (mouseY >= r.y && trackNum)
	*trackNum = n-1;
  
  return NULL;
}

bool AudioView::DClickLabel(wxMouseEvent &event)
{
  VTrack *t = FindTrack(event.m_x, event.m_y, true);
  
  if (t) {
    t->Toggle();
    REDRAW(trackPanel);
  	REDRAW(rulerPanel);
    return true;
  }
  else
    return false;
}

bool AudioView::ClickLabel(wxMouseEvent &event)
{
  VTrack *t = FindTrack(event.m_x, event.m_y, true);

  SelectNone();
  
  if (t) {
	t->selected = true;

	sel0 = 0.0;
	sel1 = t->GetMaxLen();
  }

  REDRAW(trackPanel);
  REDRAW(rulerPanel);
  
  return true;
}

void AudioView::TrackSetCursor(wxMouseEvent &event)
{
  if (event.ButtonDown())
	return;

  wxRect r;

  VTrack *t = FindTrack(event.m_x, event.m_y, false, &r);

  if (t) {
	if (event.m_y >= r.y + r.height - 12 && event.m_y < r.y + r.height)
	  trackPanel->SetCursor(*trackPanel->resizeCursor);
	else
	  trackPanel->SetCursor(*trackPanel->mainCursor);
  }
  else
	trackPanel->SetCursor(*trackPanel->noneCursor);
}

bool AudioView::ClickTrack(wxMouseEvent &event)
{
  wxRect r;
  int num;

  VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

  if (t) {
	capturedTrack = t;
	capturedRect = r;
	capturedNum = num;
	
	mouseClickX = event.m_x;
	mouseClickY = event.m_y;

	mouseMostRecentX = event.m_x;
	mouseMostRecentY = event.m_y;

	if (mouseClickY >= r.y + r.height - 12 && mouseClickY < r.y + r.height) {
	  resizing = true;
	  initialTrackHeight = t->GetHeight();
	}
	else {
	  resizing = false;

	  if (event.ShiftDown()) { // Extend selection
		double selend = sinfo.h + ((event.m_x - r.x) / sinfo.zoom);
		
		if (selend > sel1) {
		  sel1 = selend;
		}
		else if (selend < sel0) {
		  sel0 = selend;
		} else {
		  // This is not ideal.  Fix???
		  sel1 = selend;
		  selstart = sel0;
		}
	  } 
	  else if (event.ControlDown()) {// Drag Track
		selstart = sinfo.h + ((event.m_x - r.x) / sinfo.zoom);

		dragging = true;
	  }
	  else { // Selecting
		dragging = false;

		selstart = sinfo.h + ((event.m_x - r.x) / sinfo.zoom);
		
		SelectNone();
		t->selected = true;
		
		sel0 = selstart;
		sel1 = selstart;
	  }
	}
	
    REDRAW(trackPanel);
  	REDRAW(rulerPanel);
	
	return true;
  }
  else {
	// No track was selected

	SelectNone();
	resizing = false;
	dragging = false;
    REDRAW(trackPanel);
  	REDRAW(rulerPanel);
    
    return true;
  }
}

bool AudioView::DragTrack(wxMouseEvent &event)
{
  VTrack *t;
  wxRect r;
  int num;
  
  if (capturedTrack) {
	t = capturedTrack;
	r = capturedRect;
	num = capturedNum;
  }
  else
	t = FindTrack(event.m_x, event.m_y, false, &r, &num);
  
  if (t) {
	mouseMostRecentX = event.m_x;
	mouseMostRecentY = event.m_y;

	if (resizing) {
	  int delta = (event.m_y - mouseClickY);
	  int newTrackHeight = initialTrackHeight + delta;
	  if (newTrackHeight < 20)
		newTrackHeight = 20;
	  t->SetHeight(newTrackHeight);
	  REDRAW(trackPanel);
	}
    else if (dragging) {
      double selend = sinfo.h + ((event.m_x - r.x) / sinfo.zoom);

	  if (selend < 0.0)
		selend = 0.0;
      
      if (selend != selstart) {
        t->Offset(selend - selstart);
        REDRAW(trackPanel);
      }
    
      selstart = selend;
    }
    else {
	  // Selecting

      double selend = sinfo.h + ((event.m_x - r.x) / sinfo.zoom);
      
	  if (selend < 0.0)
		selend = 0.0;

      if (selend >= selstart) {
		sel0 = selstart;
		sel1 = selend;
	  }
	  else {
		sel0 = selend;
		sel1 = selstart;
	  }

	  // Handle which tracks are selected

	  int num2;
	  if (0 != FindTrack(event.m_x, event.m_y, false, NULL, &num2)) {
		// The tracks numbered num...num2 should be selected
		
		TrackList *tracks = GetTracks();
		VTrack *t = tracks->First();
		int i=1;
		while(t) {
		  t->selected = (i>=num && i<=num2) || (i>=num2 && i<=num);
		  t = tracks->Next();
		  i++;
		}
	  }

	  wxString str;
	  str.Printf("Selection: %lf - %lf seconds",sel0,sel1);
	  status->SetLabel(str);

      REDRAW(trackPanel);
    }
 
    return true;
  }
  else
    return false;
}

void AudioViewTimer::Notify()
{
  view->AutoScroll();
}

void AudioView::AutoScroll()
{
  if (!capturedTrack)
	return;

  int pos = hsbar->GetThumbPosition();
  int max = hsbar->GetRange() - hsbar->GetThumbSize();

  bool doauto = false;

  if (mouseMostRecentX > capturedRect.width && pos<max) {
	hsbar->SetThumbPosition(pos+1);
	wxScrollEvent *dummy = new wxScrollEvent();
	Scroll(*dummy);
	delete dummy;
	doauto = true;
  }
  else if (mouseMostRecentX < 0 && pos>0) {
	hsbar->SetThumbPosition(pos-1);
	wxScrollEvent *dummy = new wxScrollEvent();
	Scroll(*dummy);
	delete dummy;
	doauto = true;
  }

  if (doauto) {
	double selend =
	  sinfo.h + ((mouseMostRecentX - capturedRect.x) / sinfo.zoom);
	
	if (selend >= selstart) {
	  sel0 = selstart;
	  sel1 = selend;
	}
	else {
	  sel0 = selend;
	  sel1 = selstart;
	}
  }
}

void AudioView::ButtonUpTrack()
{
  if (dragging || resizing)
    FixScrollbars();
  capturedTrack = NULL;
}

void AudioView::FillBackgroundColor(wxDC *dc, wxRect *r)
{
  dc->SetBrush(backgroundBrush);
  dc->SetPen(backgroundPen);
  dc->DrawRectangle(*r);
}

// Draw all tracks
void AudioView::DrawTracks(wxDC *dc, wxRect *visible)
{
  wxRect r;
  
  r.x = visible->x;
  r.y = -vpos;
  r.width = visible->width;
  
  TrackList *tracks = GetTracks();
  VTrack *t;
  int num=0;
  
  t = tracks->First();
  while(t) {
	r.height = t->GetHeight();
	
	// If it's visible at all (vertically)
	if (r.y < (visible->y + visible->height) && (r.y + r.height) > visible->y)
	  {
	    double h = sinfo.h + (r.x / sinfo.zoom);
	    
		bool sel = t->selected;
		
		// Tell VTrack to draw itself

		wxRect innerRect = r;
		innerRect.x += labelWidth;
		innerRect.width -= labelWidth;
		innerRect.y += 4;
		innerRect.height -= 16;
		
		if (!t->IsCollapsed()) {
		  if (sel)
			t->Draw(*dc, innerRect, h, sinfo.zoom, sel0, sel1);
		  else
			t->Draw(*dc, innerRect, h, sinfo.zoom, 0.0, 0.0);
		}
		else {
		  dc->SetBrush(backgroundBrush);
		  dc->SetPen(backgroundPen);
		  
		  dc->DrawRectangle(innerRect);
		}
		
		// Draw label

		wxRect labelRect = r;
		  
		labelRect.width = labelWidth;
		  
		dc->SetBrush(mediumBrush[sel]);
		dc->SetPen(mediumPen[sel]);
		
		dc->DrawRectangle(labelRect);
		
		dc->SetPen(lightPen[sel]);
		
		dc->DrawLine(labelRect.x, labelRect.y,
					 labelRect.x + labelRect.width, labelRect.y);
		dc->DrawLine(labelRect.x, labelRect.y,
					 labelRect.x, labelRect.y + labelRect.height);
		dc->DrawLine(labelRect.x+1, labelRect.y+1,
					 labelRect.x + labelRect.width, labelRect.y+1);
		dc->DrawLine(labelRect.x+1, labelRect.y+1,
					 labelRect.x+1, labelRect.y + labelRect.height);	
		
		dc->SetPen(darkPen[sel]);
		
		dc->DrawLine(labelRect.x, labelRect.y + labelRect.height,
					 labelRect.x + labelRect.width, labelRect.y + labelRect.height);
		dc->DrawLine(labelRect.x+1, labelRect.y + labelRect.height-1,
					 labelRect.x + labelRect.width, labelRect.y + labelRect.height-1);
		
		dc->DrawLine(labelRect.x+labelRect.width-1,labelRect.y+4,
					 labelRect.x+labelRect.width-1,labelRect.y+labelRect.height-10);
		dc->DrawLine(labelRect.x+labelRect.width-2,labelRect.y+4,
					 labelRect.x+labelRect.width-2,labelRect.y+labelRect.height-10);
		
		int ctr = labelRect.y + (labelRect.height/2);
		dc->SetTextForeground(wxColour(0,0,0));
		char str[100];
		sprintf(str,"%d",num+1);		
		dc->DrawText(str, labelRect.x+15, ctr-8);		
		if (t->GetKind()==VTrack::Wave) {
		  sprintf(str,"%dk",(int)(((WaveTrack *)t)->rate/1000));
		  dc->DrawText(str, labelRect.x+5, ctr+8);
		}

		r.x += labelWidth;

		// Draw top bevels

		dc->SetPen(lightPen[sel]);
		dc->DrawLine(r.x, r.y, r.x + r.width, r.y);
		dc->DrawLine(r.x, r.y+1, r.x + r.width, r.y+1);
		dc->SetPen(mediumPen[sel]);
		dc->DrawLine(r.x, r.y+2, r.x + r.width, r.y+2);
		dc->DrawLine(r.x, r.y+3, r.x + r.width, r.y+3);
		dc->SetPen(darkPen[sel]);
		dc->DrawLine(r.x, r.y+4, r.x + r.width, r.y+4);
		dc->DrawLine(r.x, r.y+5, r.x + r.width, r.y+5);

		// Draw bottom bevels

		dc->SetPen(lightPen[sel]);
		dc->DrawLine(r.x, r.y+r.height-12, r.x + r.width, r.y+r.height-12);
		dc->DrawLine(r.x, r.y+r.height-11, r.x + r.width, r.y+r.height-11);
		dc->SetPen(darkPen[sel]);
		dc->DrawLine(r.x, r.y+r.height-1, r.x + r.width, r.y+r.height-1);
		dc->DrawLine(r.x, r.y+r.height-2, r.x + r.width, r.y+r.height-2);

		dc->SetPen(mediumPen[sel]);

		int j;
		for(j=3; j<=10; j++)
		  dc->DrawLine(r.x, r.y+r.height-j, r.x + r.width, r.y+r.height-j);

		dc->SetPen(lightPen[sel]);
		for(j=r.width/2-40; j<r.width/2+40; j+=4)
		  dc->DrawLine(r.x+j, r.y+r.height-9, r.x+j, r.y+r.height-5);
		dc->SetPen(darkPen[sel]);
		for(j=r.width/2-39; j<r.width/2+41; j+=4)
		  dc->DrawLine(r.x+j, r.y+r.height-9, r.x+j, r.y+r.height-5);

		r.x -= labelWidth;
	  }
  
    r.y += r.height;
	num++;
    t = tracks->Next();
  }

  r.height = visible->height;
  dc->SetBrush(backgroundBrush);
  dc->SetPen(backgroundPen);
  dc->DrawRectangle(r);
}

void AudioView::OnZoomInButton()
{
  sinfo.zoom = sinfo.zoom*2.0;

  FixScrollbars();
  REDRAW(trackPanel);
  REDRAW(rulerPanel);
}

void AudioView::OnZoomOutButton()
{
  sinfo.zoom = sinfo.zoom/2.0;

  FixScrollbars();
  REDRAW(trackPanel);
  REDRAW(rulerPanel);
}

void AudioView::OnPlayButton()
{
  SoundPlayer player;
  double plays0 = sel0;
  double plays1 = sel1;

  if (sel0 == sel1) {
	plays0 = 0.0;
	plays1 = 10000000000.0;
  }

  TrackList *tracks = GetTracks();
  VTrack *t = tracks->First();  
  while(t) {
	if (t->selected && t->GetKind() == (VTrack::Wave)) {
	  player.Begin((WaveTrack *)t, plays0, plays1);
	  return;
	}
	
	t = tracks->Next();
  }
}

void AudioView::OnStopButton()
{
}

void AudioView::OnUpdate(wxView *WXUNUSED(sender), wxObject *WXUNUSED(hint))
{
  if (trackPanel)
    REDRAW(trackPanel);
  if (rulerPanel)
    REDRAW(rulerPanel);
}

// Clean up windows used for displaying the view.
bool AudioView::OnClose(bool deleteWindow)
{
  if (!GetDocument()->Close())
    return FALSE;

  SetFrame((wxFrame *) NULL);

  Activate(FALSE);
  
  if (deleteWindow)
  {
    delete frame;
    return TRUE;
  }

  gNumViewsOpen--;

  if (gNumViewsOpen == 0)
	exit(0);

  return TRUE;
}

void AudioView::OnDraw(wxDC *dc)
{
  // Not used
}

void RulerPanel::OnEraseBackground(wxEraseEvent &ignore)
{
}

void TrackPanel::OnEraseBackground(wxEraseEvent &ignore)
{
}

void RulerPanel::OnPaint(wxPaintEvent& event)
{
  wxPaintDC dc(this);

  int width, height;
  GetSize(&width, &height);
  if (width != bitWidth || height != bitHeight || !bitmap) {
	bitWidth = width;
	bitHeight = height;

	if (bitmap)
	  delete bitmap;

	bitmap = new wxBitmap(width, height);
  }

  wxMemoryDC memDC;

  memDC.SelectObject(*bitmap);

  wxRect r(0,0,width,height);
  view->DrawRuler(&memDC, &r);

  dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);
}

void TrackPanel::FastScroll(int hoffset, int voffset)
{
#ifdef __WXMSW__
  REDRAW(this);
  return;
#endif

  int labelWidth = view->labelWidth;

  wxRect r;
  GetSize(&r.width, &r.height);
  r.width -= labelWidth;

  if (hoffset && voffset) {
    // Tricks don't work if we scroll horizontally AND vertically
    // at the same time
    REDRAW(this);
    return;
  }
  
  if (r.width != bitWidth || r.height != bitHeight || !bitmap) {
    // Don't try any tricks if we got resized, too
    REDRAW(this);
    return;
  }
  
  r.x = 0;
  r.y = 0;

  if (hoffset > 0)
    r.width = hoffset;
  else if (hoffset < 0) {
    r.x = (r.width + hoffset);
    r.width = -hoffset;
  }
  else if (voffset > 0)
    r.height = voffset;
  else if (voffset < 0) {
    r.y = (r.height + voffset);
    r.height = -voffset;
  }
  else
    return;

  r.x += labelWidth;
  r.y += 4;
  r.height -= 16;

  wxPaintDC dc(this);

  wxMemoryDC memDC;

  memDC.SelectObject(*bitmap);

  view->DrawTracks(&memDC, &r);

  ScrollWindow(hoffset, voffset);
  dc.Blit(r.x, r.y, r.width, r.height, &memDC, r.x, r.y, wxCOPY, FALSE);
    
}

void TrackPanel::OnPaint(wxPaintEvent& event)
{
  view->UpdateMenus();

  wxPaintDC dc(this);

  wxRect r;
  r.x = 0;
  r.y = 0;
  GetSize(&r.width, &r.height);
  if (r.width != bitWidth || r.height != bitHeight || !bitmap) {
	bitWidth = r.width;
	bitHeight = r.height;
	
	if (bitmap)
	  delete bitmap;
	
	bitmap = new wxBitmap(r.width, r.height);
  }

  wxMemoryDC memDC;

  memDC.SelectObject(*bitmap);

  view->DrawTracks(&memDC, &r);

  dc.Blit(0, 0, r.width, r.height, &memDC, 0, 0, wxCOPY, FALSE);
}

void RulerPanel::OnMouseEvent(wxMouseEvent& event)
{
  
}

void TrackPanel::OnMouseEvent(wxMouseEvent& event)
{
  if (event.m_x < view->labelWidth) {
	//	Group panel stuff

	if (event.ButtonDClick())
	  view->DClickLabel(event);  
	else if (event.ButtonDown())
	  view->ClickLabel(event);
	else if (event.ButtonUp())
	  SetCursor(*mainCursor);
  }
  else {
	if (event.ButtonDown()) {
	  CaptureMouse();
	  if (event.ControlDown())
		SetCursor(*dragCursor);
	  view->ClickTrack(event);
	}
	else if (event.Dragging())
	  view->DragTrack(event);
	else if (event.ButtonUp()) {
	  ReleaseMouse();
	  SetCursor(*mainCursor);
	  view->ButtonUpTrack();
	}
	else if (event.Moving()) {
	  view->TrackSetCursor(event);
	}
  }
}

RulerPanel::RulerPanel(wxView *v, wxFrame *frame, 
					   const wxPoint& pos, const wxSize& size, 
					   const long style):
 wxPanel(frame, -1, pos, size, style)
{
  view = (AudioView *)v;
  
  mainCursor = new wxCursor(wxCURSOR_ARROW);
  
  SetCursor(*mainCursor);

  bitWidth = bitHeight = 0;
  bitmap = 0;
}

TrackPanel::TrackPanel(wxView *v, wxFrame *frame, 
					   const wxPoint& pos, const wxSize& size, 
					   const long style):
  wxPanel(frame, -1, pos, size, style)
{
  view = (AudioView *)v;
  
  mainCursor = new wxCursor(wxCURSOR_IBEAM);
  dragCursor = new wxCursor(wxCURSOR_HAND);
  noneCursor = new wxCursor(wxCURSOR_ARROW);
  resizeCursor = new wxCursor(wxCURSOR_SIZENS);

  SetCursor(*mainCursor);

  bitWidth = bitHeight = 0;
  bitmap = 0;
}

void AudioView::AutoCorrelate()
{
/****************

  Waveform *wave = &((AudioDoc *)GetDocument())->wave[0];
  Waveform *wave2 = &((AudioDoc *)GetDocument())->wave[((AudioDoc *)GetDocument())->numWaves++];
  
  double *dest = new double[wave->numSamples];
  wxASSERT(dest);

  wave2->numSamples = wave->numSamples;
  wave2->sample = dest;
  wave2->rate = wave->rate;
  wave2->skip = 1;

  ::AutoCorrelate(wave->numSamples, wave->sample, dest);

  Refresh(FALSE);

****************/

}

void AudioView::Pitch()
{
  VTrack *t;
  bool success = false;

  TrackList *tracks = GetTracks();
  t = tracks->First();
  while(t) {
    if (t->selected && t->GetKind() == VTrack::Wave) {
      NoteTrack *note = PitchExtract((WaveTrack *)t,
				     &((AudioDoc *)GetDocument())->dirManager);
      if (note) {
		success = true;
		tracks->Add(note);
      }
    }

    t = tracks->Next();
  }

  if (success) {
    PushState();
    
    FixScrollbars();
    REDRAW(trackPanel);
    REDRAW(rulerPanel);
  }
}

void AudioView::QuickMix()
{
  WaveTrack **waveArray;
  VTrack *t;
  int numWaves = 0;
  int w;

  TrackList *tracks = GetTracks();
  t = tracks->First();
  while(t) {
    if (t->selected && t->GetKind() == VTrack::Wave)
      numWaves++;
    t = tracks->Next();
  }
  
  if (numWaves == 0)
	return;

  waveArray = new WaveTrack*[numWaves];
  w = 0;
  t = tracks->First();
  while(t) {
    if (t->selected && t->GetKind() == VTrack::Wave)
      waveArray[w++] = (WaveTrack *)t;
    t = tracks->Next();
  }  

  WaveTrack *mix = ::QuickMix(numWaves, waveArray,
			    &((AudioDoc *)GetDocument())->dirManager);
  if (mix) {
    tracks->Add(mix);

    PushState();
    
    FixScrollbars();
    REDRAW(trackPanel);
    REDRAW(rulerPanel);
  }
}



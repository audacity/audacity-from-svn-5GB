/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.cpp

  Dominic Mazzoni

  In Audacity, the main window you work in is called a project.
  AudacityProjects can contain an arbitrary number of tracks of many
  different types, but if a project contains just one or two
  tracks then it can be saved in standard formats like WAV or AIFF.
  This window is the one that contains the menu bar (except on
  the Mac).

**********************************************************************/

#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/menu.h>
#include <wx/string.h>
#endif

#include <wx/textfile.h>

#include "Project.h"
#include "AudacityApp.h"
#include "AColor.h"
#include "APalette.h"
#include "Import.h"
#include "ImportRaw.h"
#include "ImportMIDI.h"
#include "ImportMP3.h"
#include "Mix.h"
#include "Export.h"
#include "AboutDialog.h"
#include "Track.h"
#include "WaveTrack.h"
#include "NoteTrack.h"
#include "TrackPanel.h"
#include "effects/Effect.h"

TrackList *AudacityProject::msClipboard = new TrackList();
double     AudacityProject::msClipLen = 0.0;

const int sbarWidth = 15;
int gAudacityDocNum = 0;
AudacityProject *gActiveProject;

AudacityProject *CreateNewAudacityProject()
{
  wxPoint where;
  where.x = 10;
  where.y = 85;

  #ifdef __WXMAC__
  where.y += 50;
  #endif

  where.x += gAudacityDocNum*25;
  where.y += gAudacityDocNum*25;

  AudacityProject *p =
	new AudacityProject((wxFrame *) NULL, -1, 
						where, wxSize(600, 300));

  p->Show(true);

  gAudacityDocNum = (gAudacityDocNum+1)%10;

  gActiveProject = p;

  return p;
}

AudacityProject *GetActiveProject()
{
  return gActiveProject;
}

enum {
  FirstID = 1000,

  // Window controls

  HSBarID,
  VSBarID,
  TrackPanelID,

  // File Menu

  NewID,
  OpenID,
  CloseID,
  SaveID,
  SaveAsID,
  ExportID,
  ExitID,

  // Edit Menu

  UndoID,
  RedoID,
  CutID,
  CopyID,
  PasteID,
  ClearID,
  SelectAllID,

  // Project Menu
  
  ImportID,
  ImportRawID,
  ImportMIDIID,
  ImportMP3ID,
  
  // Track Menu

  NewWaveTrackID,
  NewLabelTrackID,
  RemoveTracksID,
  
  WaveDisplayID,
  SpectrumDisplayID,
  AutoCorrelateID,
  PitchID,
  QuickMixID,

  // Help Menu

  AboutID,

  // Effect Menu
  
  FirstEffectID  // Keep this the last enum please
};

BEGIN_EVENT_TABLE(AudacityProject, wxWindow)
  EVT_MOUSE_EVENTS(AudacityProject::OnMouseEvent)
  EVT_PAINT(AudacityProject::OnPaint)
  EVT_CLOSE(AudacityProject::OnCloseWindow)
  EVT_SIZE (AudacityProject::OnSize)
  EVT_ACTIVATE (AudacityProject::OnActivate)
  EVT_COMMAND_SCROLL  (HSBarID,   AudacityProject::OnScroll)
  EVT_COMMAND_SCROLL  (VSBarID,   AudacityProject::OnScroll)
  EVT_COMMAND_SCROLL  (TrackPanelID,   AudacityProject::OnScrollUpdate)
  // File menu
  EVT_MENU(NewID, AudacityProject::OnNew)
  EVT_MENU(OpenID, AudacityProject::OnOpen)
  EVT_MENU(CloseID, AudacityProject::OnCloseWindow)
  EVT_MENU(SaveID, AudacityProject::OnSave)
  EVT_MENU(SaveAsID, AudacityProject::OnSaveAs)
  EVT_MENU(ExportID, AudacityProject::OnExport)
  EVT_MENU(ExitID, AudacityProject::OnExit)
  // Edit menu
  EVT_MENU(UndoID, AudacityProject::Undo)
  EVT_MENU(RedoID, AudacityProject::Redo)
  EVT_MENU(CutID, AudacityProject::Cut)
  EVT_MENU(CopyID, AudacityProject::Copy)
  EVT_MENU(PasteID, AudacityProject::Paste)
  EVT_MENU(ClearID, AudacityProject::Clear)
  EVT_MENU(SelectAllID, AudacityProject::SelectAll)
  // Project menu
  EVT_MENU(ImportID, AudacityProject::OnImport)
  EVT_MENU(ImportMIDIID, AudacityProject::OnImportMIDI)
  EVT_MENU(ImportRawID, AudacityProject::OnImportRaw)
  EVT_MENU(ImportMP3ID, AudacityProject::OnImportMP3)
  EVT_MENU(QuickMixID, AudacityProject::OnQuickMix)
  EVT_MENU(NewWaveTrackID, AudacityProject::OnNewWaveTrack)
  EVT_MENU(RemoveTracksID, AudacityProject::OnRemoveTracks)
  // Help menu
  EVT_MENU(AboutID, AudacityProject::OnAbout)
END_EVENT_TABLE()

int gOpenProjects = 0;

AudacityProject::AudacityProject(wxWindow *parent, wxWindowID id,
								 const wxPoint& pos, const wxSize& size) :
  wxFrame(parent, id, "Audacity", pos, size),
  mDirty(false),
  mTrackPanel(NULL),
  mRate(44100.0)
{
  CreateStatusBar(1);

  //
  // Create track list
  //
  
  mTracks = new TrackList();

  //
  // Initialize view info (shared with TrackPanel)
  //

  // Selection
  mViewInfo.sel0 = 0.0;
  mViewInfo.sel1 = 0.0;

  // Horizontal scrollbar
  mViewInfo.total = 1.0;
  mViewInfo.screen = 1.0;
  mViewInfo.h = 0.0;
  mViewInfo.zoom = 44100.0 / 512.0;
  mViewInfo.lastZoom = mViewInfo.zoom;

  // Vertical scrollbar
  mViewInfo.vpos = 0;

  mViewInfo.scrollStep = 16;

  mViewInfo.sbarH = 0;
  mViewInfo.sbarScreen = 1;
  mViewInfo.sbarTotal = 1;


  //
  // Create Menu Bar
  //

  mMenuBar = new wxMenuBar();

  mFileMenu = new wxMenu();
  mFileMenu->Append(NewID, "&New...\tCtrl+N");
  mFileMenu->Append(OpenID, "&Open...\tCtrl+O");
  mFileMenu->Append(CloseID, "&Close\tCtrl+W");
  mFileMenu->Append(SaveID, "&Save\tCtrl+S");
  mFileMenu->Append(SaveAsID, "Save As...");
  mFileMenu->Append(ExportID, "&Export...");
  mFileMenu->AppendSeparator();
  mFileMenu->Append(ExitID, "E&xit");

  mEditMenu = new wxMenu();
  mEditMenu->Append(UndoID, "&Undo\tCtrl+Z");
  mEditMenu->Append(RedoID, "&Redo\tCtrl+R");
  mEditMenu->AppendSeparator();
  mEditMenu->Append(CutID, "Cut\tCtrl+X");
  mEditMenu->Append(CopyID, "Copy\tCtrl+C");
  mEditMenu->Append(PasteID, "Paste\tCtrl+V");
  mEditMenu->Append(ClearID, "Clear\tCtrl+B");
  mEditMenu->AppendSeparator();
  mEditMenu->Append(SelectAllID, "Select All\tCtrl+A");

  mProjectMenu = new wxMenu();
  mProjectMenu->Append(ImportID, "&Import Audio...\tCtrl+I");
  mProjectMenu->Append(ImportRawID, "Import Raw Data...");
  mProjectMenu->Append(ImportMIDIID, "Import &MIDI...");
  mProjectMenu->Append(ImportMP3ID, "Import MP3...");
  mProjectMenu->AppendSeparator();
  mProjectMenu->Append(QuickMixID, "Quick Mix");
  mProjectMenu->AppendSeparator();
  mProjectMenu->Append(NewWaveTrackID, "New Audio Track");
  //  mProjectMenu->Append(NewLabelTrackID, "New Label Track");
  mProjectMenu->AppendSeparator();
  mProjectMenu->Append(RemoveTracksID, "&Remove Track(s)");

  /*
  mTrackMenu = new wxMenu();
  mTrackMenu->Append(QuickMixID, "Quick Mix");
  mTrackMenu->AppendSeparator();
  mTrackMenu->Append(WaveDisplayID, "Waveform Display");
  mTrackMenu->Append(SpectrumDisplayID, "Spectrum Display");
  mTrackMenu->AppendSeparator();
  //  mTrackMenu->Append(AutoCorrelateID, "AutoCorrelate");
  mTrackMenu->Append(PitchID, "Pitch Extract");
  */

  mEffectMenu = new wxMenu();

  int numEffects = Effect::GetNumEffects();
  for(int fi = 0; fi < numEffects; fi++)
    mEffectMenu->Append(FirstEffectID+fi,
						(Effect::GetEffect(fi))->GetEffectName());
  
  mHelpMenu = new wxMenu;
  mHelpMenu->Append(AboutID, "&About Audacity...");

  mMenuBar->Append(mFileMenu, "&File");
  mMenuBar->Append(mEditMenu, "&Edit");
  mMenuBar->Append(mProjectMenu, "&Project");
  //  mMenuBar->Append(mTrackMenu, "&Track");
  mMenuBar->Append(mEffectMenu, "E&ffect");
  mMenuBar->Append(mHelpMenu, "&Help");

  SetMenuBar(mMenuBar);

  //
  // Create the TrackPanel and the scrollbars
  //

  int width, height;
  GetClientSize(&width, &height);

  mTrackPanel = new TrackPanel(this, TrackPanelID,
							   wxPoint(0, 0),
							   wxSize(width-sbarWidth, height-sbarWidth),
							   mTracks, &mViewInfo);

  int hoffset = mTrackPanel->GetLabelOffset()-1;
  int voffset = mTrackPanel->GetRulerHeight();

  mHsbar =
	new wxScrollBar(this, HSBarID, 
					wxPoint(hoffset, height-sbarWidth),
					wxSize(width-hoffset-sbarWidth, sbarWidth),
					wxSB_HORIZONTAL);

  mVsbar =
	new wxScrollBar(this, VSBarID,
					wxPoint(width-sbarWidth, voffset),
					wxSize(sbarWidth, height-sbarWidth-voffset),
					wxSB_VERTICAL);

  InitialState();
  FixScrollbars();

  // Min size, max size
  SetSizeHints(250,200,20000,20000);

  gOpenProjects++;
}

AudacityProject::~AudacityProject()
{
  // TODO delete mTracks;

  gOpenProjects--;

  if (gOpenProjects <= 0) {
	gAPalette->Destroy();
  }
}

double AudacityProject::GetRate()
{
  return mRate;
}

double AudacityProject::GetSel0()
{
  return mViewInfo.sel0;
}

double AudacityProject::GetSel1()
{
  return mViewInfo.sel1;
}

TrackList *AudacityProject::GetTracks()
{
  return mTracks;
}

void AudacityProject::FixScrollbars()
{
  bool rescroll = false;

  int totalHeight = (mTracks->GetHeight() + 32);

  int panelWidth, panelHeight;
  mTrackPanel->GetTracksUsableArea(&panelWidth, &panelHeight);

  mViewInfo.total = mTracks->GetMaxLen() + 1.0;
  mViewInfo.screen = ((double)panelWidth) / mViewInfo.zoom;

  if (mViewInfo.h > mViewInfo.total - mViewInfo.screen) {
  	mViewInfo.h = mViewInfo.total - mViewInfo.screen;
  	rescroll = true;
  }
  if (mViewInfo.h < 0.0) {
  	mViewInfo.h = 0.0;
  	rescroll = true;
  }

  mViewInfo.sbarTotal = (int)(mViewInfo.total * mViewInfo.zoom)
	/ mViewInfo.scrollStep;
  mViewInfo.sbarScreen = (int)(mViewInfo.screen * mViewInfo.zoom)
	/ mViewInfo.scrollStep;

  mViewInfo.sbarH = (int)(mViewInfo.h * mViewInfo.zoom)
	/ mViewInfo.scrollStep;

  mViewInfo.vpos =
	mVsbar->GetThumbPosition() * mViewInfo.scrollStep;

  if (mViewInfo.vpos >= totalHeight)
	mViewInfo.vpos = totalHeight-1;
  if (mViewInfo.vpos < 0)
	mViewInfo.vpos = 0;

  #ifdef __WXGTK__
  mHsbar->Show(mViewInfo.screen < mViewInfo.total);
  mVsbar->Show(panelHeight < totalHeight);
  #else
  mHsbar->Enable(mViewInfo.screen < mViewInfo.total);
  mVsbar->Enable(panelHeight < totalHeight);
  #endif

  if (panelHeight >= totalHeight && mViewInfo.vpos != 0) {
	mViewInfo.vpos = 0;
	mTrackPanel->Refresh();
	//REDRAW(trackPanel);
	//REDRAW(rulerPanel);
	rescroll = false;
  }
  if (mViewInfo.screen >= mViewInfo.total && mViewInfo.sbarH != 0) {
	mViewInfo.sbarH = 0;
	mTrackPanel->Refresh();
	//REDRAW(trackPanel);
	//REDRAW(rulerPanel);
	rescroll = false;
  }

  mHsbar->SetScrollbar(mViewInfo.sbarH, mViewInfo.sbarScreen,
					   mViewInfo.sbarTotal, mViewInfo.sbarScreen, TRUE);
  mVsbar->SetScrollbar(mViewInfo.vpos / mViewInfo.scrollStep,
					   panelHeight / mViewInfo.scrollStep,
					   totalHeight / mViewInfo.scrollStep,
					   panelHeight / mViewInfo.scrollStep,
					   TRUE);

  mViewInfo.lastZoom = mViewInfo.zoom;
  
  if (rescroll && mViewInfo.screen < mViewInfo.total) {
	mTrackPanel->Refresh();
	//REDRAW(trackPanel);
	//REDRAW(rulerPanel);
  }
}

void AudacityProject::OnSize(wxSizeEvent &event)
{
  if (mTrackPanel) {

    int width, height;
    GetClientSize(&width, &height);

    mTrackPanel->SetSize(0, 0,
  					     width-sbarWidth, height-sbarWidth);

    int hoffset = mTrackPanel->GetLabelOffset()-1;
    int voffset = mTrackPanel->GetRulerHeight();

    mHsbar->SetSize(hoffset, height-sbarWidth,
				    width-hoffset-sbarWidth, sbarWidth);
    mVsbar->SetSize(width-sbarWidth, voffset,
				    sbarWidth, height-sbarWidth-voffset);

    FixScrollbars();
  }
}

void AudacityProject::OnScrollUpdate(wxScrollEvent &event)
{
  // We get this message from our TrackPanel when we need
  // to recalculate our scrollbars

  FixScrollbars();
  gActiveProject = this;
}

void AudacityProject::OnScroll(wxScrollEvent &event)
{
  int  hlast = mViewInfo.sbarH;
  int  vlast = mViewInfo.vpos;
  int  hoffset = 0;
  int  voffset = 0;

  mViewInfo.sbarH = mHsbar->GetThumbPosition();

  if (mViewInfo.sbarH != hlast) {
	  mViewInfo.h = (mViewInfo.sbarH * mViewInfo.scrollStep) / mViewInfo.zoom;
	  if (mViewInfo.h > mViewInfo.total - mViewInfo.screen)
		  mViewInfo.h = mViewInfo.total - mViewInfo.screen;
	  if (mViewInfo.h < 0.0)
		  mViewInfo.h = 0.0;
		hoffset = (mViewInfo.sbarH - hlast) * mViewInfo.scrollStep;
  }

  mViewInfo.vpos = mVsbar->GetThumbPosition() * mViewInfo.scrollStep;
  voffset = mViewInfo.vpos - vlast;

  /*
	TODO: add back fast scrolling code

	// Track panel is updated either way, but it is smart and only redraws
	// what is needed
	trackPanel->FastScroll(-hoffset, -voffset);
	
	// Ruler panel updated if we scroll horizontally
	if (hoffset) {
      REDRAW(rulerPanel);
	}
  */

  gActiveProject = this;
  mTrackPanel->Refresh(false);
  #ifdef __WXMAC__
  mTrackPanel->MacUpdateImmediately();
  #endif
}

bool AudacityProject::ProcessEvent(wxEvent& event)
{
  gActiveProject = this;

  int numEffects = Effect::GetNumEffects();

  if (event.GetEventType() == wxEVT_COMMAND_MENU_SELECTED &&
      event.GetId() >= FirstEffectID &&
      event.GetId() < FirstEffectID + numEffects) {
    Effect *f = Effect::GetEffect(event.GetId() - FirstEffectID);
	
    VTrack *t = mTracks->First();
    
    while(t) {
      if (t->selected && t->GetKind() == (VTrack::Wave)) {
		f->DoInPlaceEffect((WaveTrack *)t, mViewInfo.sel0, mViewInfo.sel1);
      }
      
      t = mTracks->Next();
    }
    
    PushState();
    
    FixScrollbars();
	mTrackPanel->Refresh(false);
    
    // This indicates we handled the event.
    return true;
  }
  
  return wxWindow::ProcessEvent(event);
}

void AudacityProject::OnPaint(wxPaintEvent& event)
{
  // Draw a colored strip on the right and bottom edges of
  // the window to fill in the small area not covered by
  // the TrackPanel or the scrollbars.

  wxPaintDC dc(this);
  int width, height;
  GetClientSize(&width, &height);
    
  AColor::Medium(&dc, false);
  
  wxRect r;
  r.x = width-sbarWidth;
  r.y = 0;
  r.width = sbarWidth;
  r.height = height;
  dc.DrawRectangle(r);
  
  r.x = 0;
  r.y = height-sbarWidth;
  r.width = width;
  r.height = sbarWidth;
  dc.DrawRectangle(r);
}

void AudacityProject::OnActivate(wxActivateEvent& event)
{
  gActiveProject = this;
}

void AudacityProject::OnMouseEvent(wxMouseEvent& event)
{

}

void AudacityProject::OnAbout()
{
  AboutDialog dlog;
  dlog.ShowModal();
}

void AudacityProject::OnCloseWindow()
{
  Destroy();
}

void AudacityProject::OpenFile(wxString fileName)
{
  wxTextFile f(fileName);
  f.Open();
  if (!f.IsOpened()) {
	wxMessageBox("Couldn't open "+mFileName);
	return;
  }

  if (f.GetFirstLine() != "AudacityProject") {
	f.Close();
	if (mDirty) {
	  AudacityProject *project = CreateNewAudacityProject();
	  project->ImportFile(fileName);
	}
	else {
	  ImportFile(fileName);
	  return;
	}
  }

  mFileName = fileName;
  mName = wxFileNameFromPath(mFileName);
  SetTitle(mName);

  ///
  /// Parse project file
  ///

  wxString projName;
  wxString projPath;
  long longVpos;

  if (f.GetNextLine() != "Version") goto openFileError;
  if (f.GetNextLine() != AUDACITY_FILE_FORMAT_VERSION) {
	wxMessageBox("This project was saved by a different version of "
				 "Audacity and is no longer supported.");
	return;
  }

  if (f.GetNextLine() != "projName") goto openFileError;
  projName = f.GetNextLine();
  projPath = wxPathOnly(mFileName);
  if (!mDirManager.SetProject(projPath, projName, false)) return;

  if (f.GetNextLine() != "sel0") goto openFileError;
  if (!(f.GetNextLine().ToDouble(&mViewInfo.sel0))) goto openFileError;
  if (f.GetNextLine() != "sel1") goto openFileError;
  if (!(f.GetNextLine().ToDouble(&mViewInfo.sel1))) goto openFileError;
  if (f.GetNextLine() != "vpos") goto openFileError;
  if (!(f.GetNextLine().ToLong(&longVpos))) goto openFileError;
  mViewInfo.vpos = longVpos;
  if (f.GetNextLine() != "h") goto openFileError;
  if (!(f.GetNextLine().ToDouble(&mViewInfo.h))) goto openFileError;
  if (f.GetNextLine() != "zoom") goto openFileError;
  if (!(f.GetNextLine().ToDouble(&mViewInfo.zoom))) goto openFileError;

  mTracks->Clear();
  InitialState();

  mTracks->Load(&f, &mDirManager);

  f.Close();

  FixScrollbars();
  mTrackPanel->Refresh(false);

  return;

openFileError:
  wxMessageBox(wxString::Format(
    "Error reading Audacity Project %s in line %d",
	(const char *)mFileName,
	f.GetCurrentLine()));
  f.Close();
  return;
}

void AudacityProject::OnNew()
{
  CreateNewAudacityProject();
}

void AudacityProject::OnOpen()
{
  wxString fileName =
	wxFileSelector("Select an audio file...",
				   "", // Path
				   "", // Name
				   "", // Extension
				   "", // Wildcard
				   0, // Flags
				   this); // Parent
  
  if (fileName != "") {
	if (mDirty) {
	  AudacityProject *project = CreateNewAudacityProject();
	  project->OpenFile(fileName);
	}
	else
	  OpenFile(fileName);
  }
}

void AudacityProject::OnSave(bool overwrite /* = true */)
{
  if (mName == "" || mFileName == "") {
	OnSaveAs();
	return;
  }

  if (wxFileExists(mFileName)) {
	// TODO: Should do this safely, by renaming the old
	// file and then removing it only once the new one
	// is closed and flushed.
	wxRemoveFile(mFileName);
  }

  wxString project = mFileName;
  if (project.Len() > 4 && project.Mid(project.Len()-4)==".aup")
	project = project.Mid(0, project.Len()-4);
  wxString projName = wxFileNameFromPath(project) + "_data";
  wxString projPath = wxPathOnly(project);
  bool success = mDirManager.SetProject(projPath, projName, !overwrite);

  if (!success) {
	wxMessageBox(wxString::Format(
	  "Could not save project because the directory %s could not be created.",
      (const char *)project));
	return;
  }

  wxTextFile f(mFileName);
  f.Create();
  f.Open();
  if (!f.IsOpened()) {
	wxMessageBox("Couldn't write to "+mFileName);
	return;
  }

  f.AddLine("AudacityProject");
  f.AddLine("Version");
  f.AddLine(AUDACITY_FILE_FORMAT_VERSION);
  f.AddLine("projName");
  f.AddLine(projName);
  f.AddLine("sel0");
  f.AddLine(wxString::Format("%g", mViewInfo.sel0));
  f.AddLine("sel1");
  f.AddLine(wxString::Format("%g", mViewInfo.sel1));
  f.AddLine("vpos");
  f.AddLine(wxString::Format("%d", mViewInfo.vpos));
  f.AddLine("h");
  f.AddLine(wxString::Format("%g", mViewInfo.h));
  f.AddLine("zoom");
  f.AddLine(wxString::Format("%g", mViewInfo.zoom));

  f.AddLine("BeginTracks");

  mTracks->Save(&f, overwrite);

  f.Write();
  f.Close();
}

void AudacityProject::OnSaveAs()
{
  wxString fName = wxSaveFileSelector("", "aup");

  if (fName == "")
	return;

  mFileName = fName;
  mName = wxFileNameFromPath(mFileName);
  SetTitle(mName);

  OnSave(false);
}

void AudacityProject::OnExit()
{
  // TODO: The proper way to exit is to destroy all frames

  wxExit();
}

void AudacityProject::ImportFile(wxString fileName)
{
  WaveTrack *left = 0;
  WaveTrack *right = 0;
  
  if (ImportWAV(fileName, &left, &right, &mDirManager)) {

	if (left || right) {
	  // TODO SelectNone();
	}

	if (left) {
	  if (right) {
		left->channel = VTrack::LeftChannel;
		right->channel = VTrack::RightChannel;
	  }
	  else
		left->channel = VTrack::MonoChannel;
	}
	else
	  right->channel = VTrack::MonoChannel;

    if (left) {
      mTracks->Add(left);
	  left->selected = true;
    }

    if (right) {
      mTracks->Add(right);
	  right->selected = true;
    }

    PushState();

    FixScrollbars();
	mTrackPanel->Refresh(false);
  }
}

void AudacityProject::OnImport()
{
  wxString fileName =
	wxFileSelector("Select an audio file...",
				   "", // Path
				   "", // Name
				   "", // Extension
				   "", // Wildcard
				   0, // Flags
				   this); // Parent

  if (fileName != "")
	ImportFile(fileName);
}

void AudacityProject::OnExport()
{
  VTrack *left = 0;
  VTrack *right = 0;
  VTrack *t;
  int numSelected = 0;

  t = mTracks->First();
  while(t) {
	if (t->selected)
	  numSelected++;
	if (t->GetKind() != VTrack::Wave) {
	  wxMessageBox("Only audio tracks can be exported.");
	  return;
	}
	t = mTracks->Next();
  }

  if (numSelected == 0) {
	wxMessageBox("Please select one or two tracks before trying to export.");
	return;
  }

  if (numSelected > 2) {
	wxMessageBox("Cannot export more than two tracks (stereo).  "
				 "Please select either one or two tracks.");
  }

  left = mTracks->First();  
  while (left && (!left->selected))
	left = mTracks->Next();
  
  do {
	right = mTracks->Next();
  } while (right && (!right->selected));
  
  ::Export((WaveTrack *)left, (WaveTrack *)right);
}

void AudacityProject::OnImportRaw()
{
  wxString fileName =
	wxFileSelector("Select a PCM File...",
				   "", // Path
				   "", // Name
				   "", // Extension
				   "", // Wildcard
				   0, // Flags
				   this); // Parent

  if (fileName == "")
    return;

  WaveTrack *left = 0;
  WaveTrack *right = 0;
  
  if (::ImportRaw(fileName, &left, &right, &mDirManager)) {

	SelectNone();

    if (left) {
      mTracks->Add(left);
	  left->selected = true;
    }

    if (right) {
      mTracks->Add(right);
	  right->selected = true;
    }

    PushState();

    FixScrollbars();
	mTrackPanel->Refresh(false);
  }
  
}

void AudacityProject::OnQuickMix()
{
  WaveTrack **waveArray;
  VTrack *t;
  int numWaves = 0;
  int w;

  t = mTracks->First();
  while(t) {
    if (t->selected && t->GetKind() == VTrack::Wave)
      numWaves++;
    t = mTracks->Next();
  }
  
  if (numWaves == 0)
	return;

  waveArray = new WaveTrack*[numWaves];
  w = 0;
  t = mTracks->First();
  while(t) {
    if (t->selected && t->GetKind() == VTrack::Wave)
      waveArray[w++] = (WaveTrack *)t;
    t = mTracks->Next();
  }  

  WaveTrack *mix = ::QuickMix(numWaves, waveArray, &mDirManager);
  if (mix) {
    mTracks->Add(mix);

    PushState();
    
    FixScrollbars();
	mTrackPanel->Refresh(false);
  }
}

void AudacityProject::OnNewWaveTrack()
{
  WaveTrack *t = new WaveTrack(&mDirManager);

  SelectNone();

  mTracks->Add(t);
  t->selected = true;

  PushState();

  FixScrollbars();
  mTrackPanel->Refresh(false);
}

void AudacityProject::OnRemoveTracks()
{
  VTrack *t = mTracks->First();

  while(t) {
	if (t->selected)
	  t = mTracks->RemoveCurrent();
	else
	  t = mTracks->Next();
  }

  PushState();

  mTrackPanel->Refresh(false);
  UpdateMenus();
}

void AudacityProject::OnImportMIDI()
{
  wxString fileName =
	wxFileSelector("Select a MIDI File...",
				   "", // Path
				   "", // Name
				   ".mid", // Extension
				   "*.mid", // Wildcard
				   0, // Flags
				   this); // Parent

  if (fileName == "")
    return;

  NoteTrack *newTrack =	new NoteTrack(&mDirManager);
  
  if (::ImportMIDI(fileName, newTrack)) {

    SelectNone();
    mTracks->Add(newTrack);
	newTrack->selected = true;
    
    PushState();

    FixScrollbars();
	mTrackPanel->Refresh(false);
  }
}

void AudacityProject::OnImportMP3()
{
  wxString fileName =
	wxFileSelector("Select a MP3 File...",
				   "", // Path
				   "", // Name
				   ".mp3", // Extension
				   "*.mp3", // Wildcard
				   0, // Flags
				   this); // Parent
  
  if (fileName == "")
    return;

  WaveTrack *left = 0;
  WaveTrack *right = 0;

  if (::ImportMP3(fileName, &left, &right, &mDirManager)) {

    if (left || right) {
      SelectNone();
    }
    
    if (left) {
      mTracks->Add(left);
      left->selected = true;
    }
    
    if (right) {
      mTracks->Add(right);
      right->selected = true;
    }
    
    PushState();
    
    FixScrollbars();
	mTrackPanel->Refresh(false);
  }
}

void AudacityProject::UpdateMenus()
{
  
}

//
// Undo/History methods
//

void AudacityProject::InitialState()
{
  mUndoManager.ClearStates();
  PushState(false);
}

void AudacityProject::PushState(bool makeDirty /* = true */)
{
  TrackList *l = new TrackList(mTracks);

  mUndoManager.PushState(l, mViewInfo.sel0, mViewInfo.sel1);
  delete l;

  if (makeDirty)
	mDirty = true;
}

void AudacityProject::PopState(TrackList *l)
{
  mTracks->Clear();
  VTrack *t = l->First();
  while(t) {
	//    printf("Popping track with %d samples\n",
	//           ((WaveTrack *)t)->numSamples);
    //	((WaveTrack *)t)->Debug();
    mTracks->Add(t->Duplicate());
    t = l->Next();
  }
}

void AudacityProject::Undo()
{
  if (!mUndoManager.UndoAvailable()) {
	wxMessageBox("Nothing to undo");
	return;
  }

  TrackList *l = mUndoManager.Undo(&mViewInfo.sel0, &mViewInfo.sel1);
  PopState(l);

  FixScrollbars();
  UpdateMenus();
  mTrackPanel->Refresh(false);
}

void AudacityProject::Redo()
{
  if (!mUndoManager.RedoAvailable()) {
	wxMessageBox("Nothing to redo");
	return;
  }
  
  TrackList *l = mUndoManager.Redo(&mViewInfo.sel0, &mViewInfo.sel1);
  PopState(l);

  FixScrollbars();
  UpdateMenus();
  mTrackPanel->Refresh(false);
}

//
// Clipboard methods
//

void AudacityProject::ClearClipboard()
{
  VTrack *n = msClipboard->First();
  while(n) {
    delete n;
    n = msClipboard->Next();
  }
  
  msClipLen = 0.0;
  
  msClipboard->Clear();
}

void AudacityProject::Cut()
{
  ClearClipboard();

  VTrack *n = mTracks->First();
  VTrack *dest = 0;

  while(n) {
	if (n->selected) {
	  n->Cut(mViewInfo.sel0, mViewInfo.sel1, &dest);
	  if (dest)
		msClipboard->Add(dest);
	}
	n = mTracks->Next();
  }

  msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);

  mViewInfo.sel1 = mViewInfo.sel0;

  PushState();

  FixScrollbars();
  mTrackPanel->Refresh(false);
  UpdateMenus();
}

void AudacityProject::Copy()
{
  ClearClipboard();

  VTrack *n = mTracks->First();
  VTrack *dest = 0;

  while(n) {
	if (n->selected) {
	  n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
	  if (dest)
		msClipboard->Add(dest);
	}
	n = mTracks->Next();
  }

  msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);

  mViewInfo.sel1 = mViewInfo.sel0;
  UpdateMenus();

  //  PushState();
  //  Not an undoable operation
}

void AudacityProject::Paste()
{
  if (mViewInfo.sel0 != mViewInfo.sel1)
    Clear();
    
  wxASSERT(mViewInfo.sel0 == mViewInfo.sel1);

  double tsel = mViewInfo.sel0;

  VTrack *n = mTracks->First();
  VTrack *c = msClipboard->First();

  while(n && c) {
	if (n->selected) {
	  n->Paste(tsel, c);
	  c = msClipboard->Next();
	}
	  
	n = mTracks->Next();
  }

  // TODO: What if we clicked past the end of the track?

  mViewInfo.sel0 = tsel;
  mViewInfo.sel1 = tsel + msClipLen;

  PushState();

  FixScrollbars();
  mTrackPanel->Refresh(false);
  UpdateMenus();
}

void AudacityProject::Clear()
{
  VTrack *n = mTracks->First();

  while(n) {
	if (n->selected)
	  n->Clear(mViewInfo.sel0, mViewInfo.sel1);
	n = mTracks->Next();
  }

  mViewInfo.sel1 = mViewInfo.sel0;

  PushState();
  FixScrollbars();
  mTrackPanel->Refresh(false);
  UpdateMenus();
}

void AudacityProject::SelectAll()
{
  VTrack *t = mTracks->First();
  while(t) {
	t->selected = true;
	t = mTracks->Next();
  }
  mViewInfo.sel0 = 0.0;
  mViewInfo.sel1 = mTracks->GetMaxLen();
  
  mTrackPanel->Refresh(false);
  UpdateMenus();
}

void AudacityProject::SelectNone()
{
  VTrack *t = mTracks->First();
  while(t) {
	t->selected = false;
	t = mTracks->Next();
  }
  mTrackPanel->Refresh(false);
}


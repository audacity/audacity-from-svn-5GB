/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.h

  Dominic Mazzoni

  In Audacity, the main window you work in is called a project.
  Projects can contain an arbitrary number of tracks of many
  different types, but if a project contains just one or two
  tracks then it can be saved in standard formats like WAV or AIFF.
  This window is the one that contains the menu bar (except on
  the Mac).

**********************************************************************/

#ifndef __AUDACITY_PROJECT__
#define __AUDACITY_PROJECT__

#include <wx/frame.h>
#include <wx/window.h>
#include <wx/scrolbar.h>
#include <wx/statusbr.h>

#include "DirManager.h"
#include "TrackPanel.h"
#include "UndoManager.h"

class wxBoxSizer;
class TrackList;

class AudacityProject;

AudacityProject *CreateNewAudacityProject();
AudacityProject *GetActiveProject();
void RedrawAllProjects();

class AudacityProject: public wxFrame
{
public:

  // Constructor and Destructor

  AudacityProject(wxWindow *parent, wxWindowID id,
				  const wxPoint& pos, const wxSize& size);

  virtual ~AudacityProject();

  // Accessors

  double      GetRate();
  TrackList  *GetTracks();
  double      GetSel0();
  double      GetSel1();

  // File I/O

  void OpenFile(wxString fileName);
  void ImportFile(wxString fileName);

  // File Menu

  void OnNew();
  void OnOpen();
  void OnSave(bool overwrite = true);
  void OnSaveAs();

  void OnExport();

  // Edit Menu

  void Cut();
  void Copy();
  void Paste();
  void Clear();  
  void SelectAll();
  void SelectNone();

  void Undo();
  void Redo();

  // View Menu

  void OnZoomIn();
  void OnZoomOut();
  void OnZoomNormal();
  void OnZoomFit();

  void OnPlotSpectrum();

  // Project Menu

  void OnImport();
  void OnImportMIDI();
  void OnImportMP3();
  void OnImportRaw();

  void OnQuickMix();

  void OnNewWaveTrack();
  void OnRemoveTracks();

  // Help Menu

  void OnAbout();

  void UpdateMenus();

  // Message Handlers

  virtual bool ProcessEvent(wxEvent& event);

  void OnActivate(wxActivateEvent& event);
  void OnPaint(wxPaintEvent& event);
  void OnMouseEvent(wxMouseEvent& event);  
  void OnSize(wxSizeEvent &event);
  void OnScroll(wxScrollEvent &event);
  void OnScrollUpdate(wxScrollEvent &event);
  void OnCloseWindow();
  void OnExit();

  // Other commands

  void RedrawProject();

  // Scrollbars

  void OnScrollLeft();
  void OnScrollRight();
  void FinishAutoScroll();
  void FixScrollbars();

private:

  // Private Methods

  void ClearClipboard();  
  void InitialState();
  void PushState(bool makeDirty = true);
  void PopState(TrackList *l);

  // The project's name and file info

  wxString    mName;
  wxString    mFileName;
  DirManager  mDirManager;
  double      mRate;

  // List of tracks and display info

  TrackList   *mTracks;
  ViewInfo    mViewInfo;

  TrackList   *mLastSavedTracks;

  // Clipboard (static because it is shared by all projects)

  static TrackList *msClipboard;
  static double     msClipLen;

  // History/Undo manager

  UndoManager mUndoManager;
  bool        mDirty;

  // Menus

  wxMenuBar   *mMenuBar;
  wxMenu      *mFileMenu;
  wxMenu      *mEditMenu;
  wxMenu      *mViewMenu;
  wxMenu      *mProjectMenu;
  wxMenu      *mTrackMenu;
  wxMenu      *mEffectMenu;
  wxMenu      *mHelpMenu;

  // Window elements

  TrackPanel  *mTrackPanel;

  wxStatusBar *mStatusBar;

  wxScrollBar *mHsbar;
  wxScrollBar *mVsbar;

  bool        mAutoScrolling;

public:

  DECLARE_EVENT_TABLE()
};

#endif

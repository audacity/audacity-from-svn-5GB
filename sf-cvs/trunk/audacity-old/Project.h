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

#include "AStatus.h"
#include "DirManager.h"
#include "TrackPanel.h"
#include "UndoManager.h"

class wxBoxSizer;
class wxDragImage;

class TrackList;
class APalette;

class AudacityProject;

AudacityProject *CreateNewAudacityProject(wxWindow *parentFrame);
AudacityProject *GetActiveProject();
void RedrawAllProjects();

WX_DEFINE_ARRAY(AudacityProject *, AProjectArray);

extern AProjectArray gAudacityProjects;

class AudacityProject: public wxFrame,
					   public TrackPanelListener,
					   public AStatusListener
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
  APalette   *GetAPalette();
  wxString    GetName();
  DirManager *GetDirManager();

  // File I/O

  void OpenFile(wxString fileName);
  void Save(bool overwrite = true, bool fromSaveAs = false);
  void SaveAs();
  void ImportFile(wxString fileName);
  void ImportMP3(wxString fileName);
  void ImportOGG(wxString fileName);

  // File Menu

  void OnNew(wxCommandEvent& event);
  void OnOpen(wxCommandEvent& event);
  void OnClose(wxCommandEvent& event);
  void OnSave(wxCommandEvent& event);
  void OnSaveAs(wxCommandEvent& event);

  void OnExportLabels(wxCommandEvent& event);
  void OnExportMix(wxCommandEvent& event);
  void OnExportSelection(wxCommandEvent& event);

  void OnExit(wxCommandEvent& event);

  // Edit Menu

  void Cut(wxCommandEvent& event);
  void Copy(wxCommandEvent& event);
  void Paste(wxCommandEvent& event);
  void Clear();
  void OnClear(wxCommandEvent& event);  
  void SelectAll(wxCommandEvent& event);

  void Undo(wxCommandEvent& event);
  void Redo(wxCommandEvent& event);

  // View Menu

  void OnZoomIn(wxCommandEvent& event);
  void OnZoomOut(wxCommandEvent& event);
  void OnZoomNormal(wxCommandEvent& event);
  void OnZoomFit(wxCommandEvent& event);

  void ZoomFit();

  void OnPlotSpectrum(wxCommandEvent& event);

  void OnFloatPalette(wxCommandEvent& event);

  // Project Menu

  void OnPreferences(wxCommandEvent& event);
  void OnImport(wxCommandEvent& event);
  void OnImportLabels(wxCommandEvent& event);
  void OnImportMIDI(wxCommandEvent& event);
  void OnImportMP3(wxCommandEvent& event);
  void OnImportOGG(wxCommandEvent& event);
  void OnImportRaw(wxCommandEvent& event);

  void OnQuickMix(wxCommandEvent& event);
  
  void OnAlignZero(wxCommandEvent& event);
  void OnAlign(wxCommandEvent& event);

  void OnNewWaveTrack(wxCommandEvent& event);
  void OnNewLabelTrack(wxCommandEvent& event);
  void OnRemoveTracks(wxCommandEvent& event);

  // Help Menu

  void OnAbout(wxCommandEvent& event);

  void UpdateMenus();

  // Message Handlers

  virtual bool ProcessEvent(wxEvent& event);

  void OnActivate(wxActivateEvent& event);
  void OnPaint(wxPaintEvent& event);
  void OnMouseEvent(wxMouseEvent& event);  
  void OnSize(wxSizeEvent &event);
  void OnScroll(wxScrollEvent &event);
  void OnCloseWindow(wxCloseEvent &event);

  void HandleResize();

  // Other commands

  void RedrawProject();
  void SelectNone();

  // Scrollbars

  void OnScrollLeft();
  void OnScrollRight();
  void FinishAutoScroll();
  void FixScrollbars();

  // TrackPanel callback methods

  virtual void TP_DisplayStatusMessage(const char *msg, int fieldNum);
  virtual int  TP_GetCurrentTool();
  virtual void TP_OnPlayKey();
  virtual void TP_PushState();
  virtual void TP_RedrawScrollbars();
  virtual void TP_ScrollLeft();
  virtual void TP_ScrollRight();
  virtual void TP_HasMouse();

  // APalette

  void ShowPalette();
  void HidePalette();

  // AStatus callback methods

  virtual void AS_SetRate(double rate);

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

  APalette    *mAPalette;
  AStatus     *mStatus;
  wxPoint      mPaletteHotspot;
  wxDragImage *mDrag;
  TrackPanel  *mTrackPanel;
  wxScrollBar *mHsbar;
  wxScrollBar *mVsbar;

  bool        mAutoScrolling;

public:

  DECLARE_EVENT_TABLE()
};

#endif

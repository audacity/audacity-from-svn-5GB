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
#include <wx/intl.h>

#include "AStatus.h"
#include "DirManager.h"
#include "TrackPanel.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "ToolBar.h"
#include "ControlToolBar.h"
#include "EditToolBar.h"

class wxWindow;
class wxBoxSizer;
class wxDragImage;
class wxScrollEvent;
class wxScrollBar;

class Toolbar;
class TrackList;
class Tags;
class ControlToolBar;

class AudacityProject;

AudacityProject *CreateNewAudacityProject(wxWindow * parentFrame);
AudacityProject *GetActiveProject();
void RedrawAllProjects();
void CloseAllProjects();

WX_DEFINE_ARRAY(AudacityProject *, AProjectArray);

extern AProjectArray gAudacityProjects;

class HistoryWindow;
class ToolBar;

WX_DEFINE_ARRAY(ToolBar *, ToolBarArray);

class AudacityProject:public wxFrame,
    public TrackPanelListener, public AStatusListener {
 public:

   // Constructor and Destructor

   AudacityProject(wxWindow * parent, wxWindowID id,
                   const wxPoint & pos, const wxSize & size);

    virtual ~ AudacityProject();

   // Accessors

   double GetRate();
   TrackList *GetTracks();
   double GetSel0();
   double GetSel1();

   wxString GetName();
   DirManager *GetDirManager();
   Tags *GetTags();

   // File I/O

   void OpenFile(wxString fileName);
   void Import(wxString fileName);
   bool Save(bool overwrite = true, bool fromSaveAs = false);
   bool SaveAs();
   void Clear();

   // Methods associated with menu items are in Menus.h

#define AUDACITY_MENUS_METHODS
#include "Menus.h"
#undef AUDACITY_MENUS_METHODS

   // Message Handlers

   virtual bool ProcessEvent(wxEvent & event);

   void OnActivate(wxActivateEvent & event);
   void OnDropFiles(wxDropFilesEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void OnSize(wxSizeEvent & event);
   void OnScroll(wxScrollEvent & event);
   void OnCloseWindow(wxCloseEvent & event);

   void HandleResize();

   // Other commands

   void RedrawProject();
   void SelectNone();
   void Rewind(bool shift);
   void SkipEnd(bool shift);
   void ReReadSettings();
   void SetStop(bool bStopped);

   // Scrollbars

   void OnScrollLeft();
   void OnScrollRight();
   void FinishAutoScroll();
   void FixScrollbars();

   // TrackPanel callback methods

   virtual void TP_DisplayStatusMessage(const char *msg, int fieldNum);
   virtual int TP_GetCurrentTool();
   virtual void TP_OnPlayKey();
   virtual void TP_PushState(wxString desc =
                             wxString(_("Not SPECIFIED YET!")));
   virtual void TP_RedrawScrollbars();
   virtual void TP_ScrollLeft();
   virtual void TP_ScrollRight();
   virtual void TP_HasMouse();
   virtual void TP_ScrollWindow(double scrollto);
   virtual void TP_HandleResize();

   // ToolBar

   void LoadToolBar(enum ToolBarType);
   void UnloadToolBar(enum ToolBarType);
   void MakeToolBarMenuEntriesCorrect();
   ControlToolBar *GetControlToolBar();
   bool IsToolBarLoaded(enum ToolBarType);

   // AStatus callback methods

   virtual void AS_SetRate(double rate);

   void SetStateTo(unsigned int n);

 private:


   // Private Methods

   void ClearClipboard();
   void InitialState();
   void PushState(wxString desc, bool makeDirty = true);
   void PopState(TrackList * l);

   // The project's name and file info

   wxString mFileName;
   DirManager mDirManager;
   double mRate;

   // Tags (artist name, song properties, MP3 ID3 info, etc.)

   Tags *mTags;

   // List of tracks and display info

   TrackList *mTracks;
   ViewInfo mViewInfo;

   TrackList *mLastSavedTracks;

   // Clipboard (static because it is shared by all projects)

   static TrackList *msClipboard;
   static AudacityProject *msClipProject;
   static double msClipLen;

   // History/Undo manager

   UndoManager mUndoManager;
   bool mDirty;

   // Menus

   wxMenuBar *mMenuBar;
   wxMenu *mFileMenu;
   wxMenu *mEditMenu;
   wxMenu *mViewMenu;
   wxMenu *mProjectMenu;
   wxMenu *mTrackMenu;
   wxMenu *mEffectMenu;
   wxMenu *mPluginMenu;
   wxMenu *mHelpMenu;

   // Window elements


   AStatus *mStatus;
   wxPoint mToolBarHotspot;
   wxDragImage *mDrag;
   TrackPanel *mTrackPanel;
   wxScrollBar *mHsbar;
   wxScrollBar *mVsbar;
   bool mAutoScrolling;
   HistoryWindow *mHistoryWindow;

   ToolBarArray mToolBarArray;
   int mTotalToolBarHeight;
   enum ToolBarType mDraggingToolBar;

 public:

    DECLARE_EVENT_TABLE()
};

#endif

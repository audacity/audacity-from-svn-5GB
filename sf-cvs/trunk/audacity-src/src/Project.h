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

#include "Audacity.h"

#if defined(__MACOS9__)
#include <Files.h>
#elif defined(__MACOSX__)

#else
// Work around bugs in <wx/dragimag.h>, wxGTK 2.2.9.2
// (bad use of preprocessor, failure to forward-declare class names)
// Bug report forwarded to wxWindows team.
class wxRect;
class wxMemoryDC;
class wxDC;
class wxTimer;
#include <wx/defs.h>
#include <wx/log.h>
#include <wx/dragimag.h>
#include <wx/generic/dragimgg.h>
#endif

#include <wx/frame.h>
#include <wx/intl.h>

#include "AStatus.h"
#include "DirManager.h"
#include "effects/Effect.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "ToolBar.h"
#include "TrackPanel.h"
#include "xml/XMLTagHandler.h"

#include "commands/CommandManager.h"

const int AudacityProjectTimerID = 5200;

class wxWindow;
class wxBoxSizer;
class wxScrollEvent;
class wxScrollBar;
class wxProgressDialog;

class Toolbar;
class TrackList;
class Tags;
class ControlToolBar;
class MixerToolBar;
class HistoryWindow;
class Importer;

class AudacityProject;

AudacityProject *CreateNewAudacityProject(wxWindow * parentFrame);
AudacityProject *GetActiveProject();
void RedrawAllProjects();
void CloseAllProjects();

void GetDefaultWindowRect(wxRect *defRect);
void GetNextWindowPlacement(wxRect *nextRect, bool *bMaximized);

WX_DEFINE_ARRAY(AudacityProject *, AProjectArray);

extern AProjectArray gAudacityProjects;


WX_DEFINE_ARRAY(ToolBar *, ToolBarArray);
WX_DEFINE_ARRAY(wxMenu *, MenuArray);


class AudacityProject:public wxFrame,
                      public TrackPanelListener,
                      public AStatusListener,
                      public XMLTagHandler
{
 public:

   // Constructor and Destructor

   AudacityProject(wxWindow * parent, wxWindowID id,
                   const wxPoint & pos, const wxSize & size);

   virtual ~ AudacityProject();

   TrackList *GetTracks() { return mTracks; };
   UndoManager *GetUndoManager() { return &mUndoManager; }

   double GetRate() { return mRate; }
   double GetZoom() { return mViewInfo.zoom; }
   double GetSel0() { return mViewInfo.sel0; }
   double GetSel1() { return mViewInfo.sel1; }

   bool Clipboard() { return msClipLen > 0.0; }

   wxString GetName();
   DirManager *GetDirManager();
   TrackFactory *GetTrackFactory();
   Tags *GetTags();
   int GetAudioIOToken();
   void SetAudioIOToken(int token);

   bool IsActive();

   // File I/O

   static void ShowOpenDialog(AudacityProject *proj);
   void OpenFile(wxString fileName);
   void Import(wxString fileName);
   void AddImportedTracks(wxString fileName,
                          Track **newTracks, int numTracks);
   bool Save(bool overwrite = true, bool fromSaveAs = false);
   bool SaveAs();
   void Clear();

#include "Menus.h"

 public:

   // Message Handlers

   virtual bool ProcessEvent(wxEvent & event);
   void OnUpdateMenus(wxUpdateUIEvent & event);

   void OnActivate(wxActivateEvent & event);
   void OnDropFiles(wxDropFilesEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void OnIconize(wxIconizeEvent &event);
   void OnSize(wxSizeEvent & event);
   void OnScroll(wxScrollEvent & event);
   void OnCloseWindow(wxCloseEvent & event);
   void OnTimer(wxTimerEvent & event);

   bool HandleKeyEvent(wxKeyEvent & event);

   void HandleResize();

   // Other commands

   static void DeleteClipboard();

   void UpdateMenus();
   void UpdatePrefs();
   void RedrawProject();
   void SelectNone();
   void Zoom(double level);
   void Rewind(bool shift);
   void SkipEnd(bool shift);
   void SetStop(bool bStopped);

   // Scrollbars

   void OnScrollLeft();
   void OnScrollRight();

   void OnScrollLeftButton(wxScrollEvent & event);
   void OnScrollRightButton(wxScrollEvent & event);

   void FinishAutoScroll();
   void FixScrollbars();

   // TrackPanel callback methods

   virtual void TP_DisplayStatusMessage(const char *msg, int fieldNum);
   virtual void TP_DisplaySelection();
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
   virtual ControlToolBar * TP_GetControlToolBar();

   // ToolBar

   void LoadToolBar(enum ToolBarType);
   void UnloadToolBar(enum ToolBarType);
   ControlToolBar *GetControlToolBar();
   MixerToolBar *GetMixerToolBar();
   bool IsToolBarLoaded(enum ToolBarType);

 private:
   void DecorateToolBar( wxPaintDC & dc, int iToolBar );
   int FlowLayout( int i, int x, int y, int width, int height );
   void BoxLayout( int width );
   void LayoutToolBars();
   int GetGrabberFromEvent(wxMouseEvent & event);
 public:


   // AStatus callback methods

   virtual void AS_SetRate(double rate);

   void SetStateTo(unsigned int n);

   // XMLTagHandler callback methods

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

 private:


   // Private Methods

   void ClearClipboard();
   void InitialState();
   void PushState(wxString desc, bool makeDirty = true);
   void PopState(TrackList * l);

   // Callbacks for backend operations

   static bool ImportProgressCallback(void *self, float percent);

   wxProgressDialog *mImportProgressDialog;

   // The project's name and file info

   wxString mFileName;
   DirManager *mDirManager; // MM: DirManager now created dynamically
   double mRate;
   sampleFormat mDefaultFormat;

   // Tags (artist name, song properties, MP3 ID3 info, etc.)

   Tags *mTags;

   // List of tracks and display info

   TrackList *mTracks;
   ViewInfo mViewInfo;

   int mSelectionFormat;
   int mSnapTo;

   TrackList *mLastSavedTracks;

   // Clipboard (static because it is shared by all projects)

   static TrackList *msClipboard;
   static AudacityProject *msClipProject;
   static double msClipLen;

   // History/Undo manager

   UndoManager mUndoManager;
   bool mDirty;

   // Commands

   CommandManager mCommandManager;

   bool mFirstTimeUpdateMenus;
   bool mLastNonZeroRegionSelected;
   int mLastNumTracks;
   int mLastNumTracksSelected;
   int mLastNumWaveTracks;
   int mLastNumWaveTracksSelected;
   int mLastNumLabelTracks;
   double mLastZoomLevel;
   int mLastToolBarCheckSum;   //This finds the state of the toolbars:
                               //Base three for ControlToolBar, EditToolBar, etc
                               // 0: unloaded, 1: docked, 2: floating (* 3 for EditToolBar)
   bool mLastUndoState;
   bool mLastRedoState;
   bool mLastClipboardState;   // true: clipboard full; false: clipboard empty.

   // Window elements

   wxTimer *mTimer;
   long mLastStatusUpdateTime;
   long mLastUpdateUITime;

   AStatus *mStatus;
   wxPoint mToolBarHotspot;

#ifndef __WXMAC__
   wxGenericDragImage *mDrag;
#endif

   TrackPanel *mTrackPanel;
   TrackFactory *mTrackFactory;
   Importer *mImporter;
   wxScrollBar *mHsbar;
   wxScrollBar *mVsbar;
   bool mAutoScrolling;
   bool mActive;
   bool mImportingRaw;
   bool mIconized;
   HistoryWindow *mHistoryWindow;

   ToolBarArray mToolBarArray;
   int mTotalToolBarHeight;
   enum ToolBarType mDraggingToolBar;
   int  mAudioIOToken;

 public:

    DECLARE_EVENT_TABLE()
};

#endif

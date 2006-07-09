/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.cpp

  Dominic Mazzoni

*******************************************************************//**

\file Project.cpp
\brief Implements AudacityProject, AdornedRulerPanel and 
AudacityDropTarget.  Includes Menus.cpp.

*//****************************************************************//**

\class AudacityProject
\brief AudacityProject provides the main window, with tools and 
tracks contained within it.

  In Audacity, the main window you work in is called a project.
  AudacityProjects can contain an arbitrary number of tracks of many
  different types, but if a project contains just one or two
  tracks then it can be saved in standard formats like WAV or AIFF.
  This window is the one that contains the menu bar (except on
  the Mac).

\attention The menu functions for AudacityProject, those for creating 
the menu bars and acting on clicks, are found in file Menus.cpp

*//****************************************************************//**

\class AdornedRulerPanel
\brief AdornedRulerPanel provides the Audacity Ruler, including 
the cursor for play back and record.

*//****************************************************************//**

\class AudacityDropTarget
\brief AudacityDropTarget, derived from wxFileDropTarget gives 
drag and drop functionality for audio files.

*//****************************************************************//**

\class ViewInfo
\brief ViewInfo is used mainly to hold the zooming, selection and
scroll information.  It also has some status flags.

*//*******************************************************************/

#include "Audacity.h"

#include <stdio.h>
#include <iostream>
#include <wx/wxprec.h>

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/docview.h>
#include <wx/intl.h>
#include <wx/string.h>
#include <wx/filefn.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/timer.h>
#include <wx/sizer.h>
#include <wx/statusbr.h>
#include <wx/notebook.h>

#ifdef __MACOSX__
#include <CoreServices/CoreServices.h>
#endif

#ifdef __MACOS9__
#include <Files.h>
#endif

#ifdef __WXMAC__
#define __MOVIES__  /* Apple's Movies.h not compatible with Audacity */
#include <wx/mac/private.h>
#endif

#include <wx/dragimag.h>
#include <wx/generic/dragimgg.h>

#include <wx/event.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/scrolbar.h>
#include <wx/textfile.h>
#include <wx/menu.h>
#include <wx/progdlg.h>
#include <wx/arrimpl.cpp>       // this allows for creation of wxObjArray

#include "Project.h"

#include "AudacityApp.h"
#include "AColor.h"
#include "SelectionBar.h"
#include "AudioIO.h"
#include "ControlToolBar.h"
#include "ToolsToolBar.h"
#include "EditToolBar.h"
#include "MeterToolBar.h"
#include "TranscriptionToolBar.h"
#include "FreqWindow.h"
#include "HistoryWindow.h"
#include "Internat.h"
#include "import/Import.h"
#include "LabelTrack.h"
#include "Legacy.h"
#include "Mix.h"
#include "MixerToolBar.h"
#include "NoteTrack.h"
#include "Prefs.h"
#include "Tags.h"
#include "ToolBar.h"
#include "Track.h"
#include "TrackPanel.h"
#include "WaveTrack.h"
#include "DirManager.h"
#include "effects/Effect.h"
#include "prefs/PrefsDialog.h"
#include "widgets/Ruler.h"
#include "widgets/Warning.h"
#include "xml/XMLFileReader.h"
#include "PlatformCompatibility.h"
#include "Experimental.h"
#include "export/Export.h"

#include "Theme.h"
#include "AllThemeResources.h"


using std::cout;

TrackList *AudacityProject::msClipboard = new TrackList();
double AudacityProject::msClipLen = 0.0;
AudacityProject *AudacityProject::msClipProject = NULL;


const int grabberWidth=10;

#ifdef __WXMAC__
# ifndef __UNIX__
#  include <Files.h>
# endif

const int sbarSpaceWidth = 15;
const int sbarControlWidth = 16;
const int sbarExtraLen = 1;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs
#elif defined(__WXMSW__)
const int sbarSpaceWidth = 16;
const int sbarControlWidth = 16;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs
#else // wxGTK, wxMOTIF, wxX11
const int sbarSpaceWidth = 15;
const int sbarControlWidth = 15;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs
//#include "../images/AudacityLogo48x48.xpm"
#include "Theme.h"
#include "AllThemeResources.h"

#endif

/* Define Global Variables */
//The following global counts the number of documents that have been opened
//for the purpose of project placement (not to keep track of the number)
//It is only accurate modulo ten, and does not decrement when a project is
//closed.
static int gAudacityOffsetInc = 0;
static int gAudacityPosInc = 0;
//This is a pointer to the currently-active project.
static AudacityProject *gActiveProject;
//This array holds onto all of the projects currently open
AProjectArray gAudacityProjects;

/* Declare Static functions */
static void SetActiveProject(AudacityProject * project);

AudacityProject *GetActiveProject()
{
   return gActiveProject;
}

void SetActiveProject(AudacityProject * project)
{
   gActiveProject = project;
   wxTheApp->SetTopWindow(project);
}

#if wxUSE_DRAG_AND_DROP
AudacityDropTarget::AudacityDropTarget(AudacityProject *proj)
   : mProject(proj)
{
}

AudacityDropTarget::~AudacityDropTarget()
{
}

bool AudacityDropTarget::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames)
{
   for (unsigned int i = 0; i < filenames.GetCount(); i++)
      mProject->Import(filenames[i]);
   return true;
}
#endif

AudacityProject *CreateNewAudacityProject(wxWindow * parentWindow)
{
   bool bMaximized;
   wxRect wndRect;

#if defined(__WXMAC__)
   if (gParentFrame->IsShown()) {
      gParentFrame->Hide();
   }
#endif
   
   GetNextWindowPlacement(&wndRect, &bMaximized);

   //Create and show a new project
   AudacityProject *p = new AudacityProject(parentWindow, -1,
                                            wxPoint(wndRect.x, wndRect.y), wxSize(wndRect.width, wndRect.height));

   gAudacityProjects.Add(p);
   
   if(bMaximized)
      p->Maximize(true);

   p->Show(true);

   //Set the new project as active:
   SetActiveProject(p);

   return p;
}

void RedrawAllProjects()
{
   size_t len = gAudacityProjects.GetCount();
   for (size_t i = 0; i < len; i++)
      gAudacityProjects[i]->RedrawProject();
}

void RefreshCursorForAllProjects()
{
   size_t len = gAudacityProjects.GetCount();
   for (size_t i = 0; i < len; i++)
      gAudacityProjects[i]->RefreshCursor();
}

void CloseAllProjects()
{
   size_t len = gAudacityProjects.GetCount();
   for (size_t i = 0; i < len; i++)
      gAudacityProjects[i]->Close();

   //Set the Offset and Position increments to 0
   gAudacityOffsetInc = 0;
   gAudacityPosInc = 0;
}

// BG: The default size and position of the first window
void GetDefaultWindowRect(wxRect *defRect)
{
   //the point that a new window should open at.
   defRect->x = 10;
   defRect->y = 10;

   defRect->width = 600;
   defRect->height = 400;

   //These conditional values assist in improving placement and size
   //of new windows on different platforms.
#ifdef __WXMAC__
   defRect->y += 50;
#endif

#ifdef __WXGTK__
   defRect->height += 20;
#endif

#ifdef __WXMSW__
   defRect->height += 40;
#endif
}

// BG: Calculate where to place the next window (could be the first window)
// BG: Does not store X and Y in prefs. This is intentional.
void GetNextWindowPlacement(wxRect *nextRect, bool *bMaximized)
{
   wxRect defWndRect;

   GetDefaultWindowRect(&defWndRect);

   *bMaximized = false;

   if(gAudacityProjects.IsEmpty())
   {
      //Read the values from the registry, or use the defaults
      nextRect->SetWidth(gPrefs->Read(wxT("/Window/Width"), defWndRect.GetWidth()));
      nextRect->SetHeight(gPrefs->Read(wxT("/Window/Height"), defWndRect.GetHeight()));

      gPrefs->Read(wxT("/Window/Maximized"), bMaximized);
   }
   else
   {
      //Base the values on the previous Window
      *nextRect = gAudacityProjects[gAudacityProjects.GetCount()-1]->GetRect();

      *bMaximized = gAudacityProjects[gAudacityProjects.GetCount()-1]->IsMaximized();
   }

   nextRect->x = defWndRect.x;
   nextRect->y = defWndRect.y;

   //Placement depends on the increments
   nextRect->x += (gAudacityPosInc * 25) + (gAudacityOffsetInc * 25);
   nextRect->y += gAudacityPosInc * 25;

   //Make sure that the Window will be completely visible

   //Get the size of the screen
   wxRect screenRect;
   wxClientDisplayRect(&screenRect.x, &screenRect.y, &screenRect.width, &screenRect.height);

   //First check if we need to reset the increments

   //Have we hit the bottom of the screen?
   if( (nextRect->y+nextRect->height > screenRect.y+screenRect.height) )
   {
      //Reset Position increment
      gAudacityPosInc = 0;

      //Increment Offset increment
      gAudacityOffsetInc++;

      //Recalculate the position on the screen
      nextRect->x = defWndRect.x;
      nextRect->y = defWndRect.y;

      nextRect->x += (gAudacityPosInc * 25) + (gAudacityOffsetInc * 25);
      nextRect->y += gAudacityPosInc * 25;
   }

   //Have we hit the right side of the screen?
   if( (nextRect->x+nextRect->width > screenRect.x+screenRect.width) )
   {
      //Reset both Position and Offset increments
      gAudacityPosInc = 0;
      gAudacityOffsetInc = 0;

      //Recalculate the position on the screen
      nextRect->x = defWndRect.x;
      nextRect->y = defWndRect.y;
      //No need to compute the offset and position, just use the defaults
   }

   //Next check if the screen is too small for the default Audacity width and height
   //Uses both comparisons from above
   if( (nextRect->x+nextRect->width > screenRect.x+screenRect.width) ||
       (nextRect->y+nextRect->height > screenRect.y+screenRect.height) )
   {
      //Resize the Audacity window to fit in the screen
      nextRect->width = screenRect.width-nextRect->x;
      nextRect->height = screenRect.height-nextRect->y;
   }

   //Increment Position increment
   gAudacityPosInc++;
}

enum {
   FirstID = 1000,

   // Window controls

   HSBarID,
   VSBarID,
   TrackPanelID
};

BEGIN_EVENT_TABLE(AudacityProject, wxFrame)
    EVT_MENU_OPEN(AudacityProject::OnMenuEvent)
    EVT_MENU_CLOSE(AudacityProject::OnMenuEvent)
    EVT_MENU(wxID_ANY, AudacityProject::OnMenu)
    EVT_MOUSE_EVENTS(AudacityProject::OnMouseEvent)
    EVT_PAINT(AudacityProject::OnPaint)
    EVT_CLOSE(AudacityProject::OnCloseWindow)
    EVT_SIZE(AudacityProject::OnSize)
    EVT_ACTIVATE(AudacityProject::OnActivate)
    EVT_COMMAND_SCROLL_LINEUP(HSBarID, AudacityProject::OnScrollLeftButton)
    EVT_COMMAND_SCROLL_LINEDOWN(HSBarID, AudacityProject::OnScrollRightButton)
    EVT_COMMAND_SCROLL(HSBarID, AudacityProject::OnScroll)
    EVT_COMMAND_SCROLL(VSBarID, AudacityProject::OnScroll)
    EVT_TIMER(AudacityProjectTimerID, AudacityProject::OnTimer)
    // Update menu method
    EVT_UPDATE_UI(1, AudacityProject::OnUpdateMenus)
    EVT_ICONIZE(AudacityProject::OnIconize)
    EVT_COMMAND(wxID_ANY, EVT_TOOLBAR_UPDATED, AudacityProject::OnToolBarUpdate)
END_EVENT_TABLE()



AudacityProject::AudacityProject(wxWindow * parent, wxWindowID id,
                                 const wxPoint & pos,
                                 const wxSize & size)
   : wxFrame(parent, id, wxT("Audacity"), pos, size),
     mLastPlayMode(normalPlay),
     mImportProgressDialog(NULL),
     mRate((double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate())),
     mDefaultFormat((sampleFormat) gPrefs->
           Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample)),
     mSnapTo(0),
     mDirty(false),
     mTrackPanel(NULL),
     mTrackFactory(NULL),
     mImporter(NULL),
     mAutoScrolling(false),
     mActive(true),
     mHistoryWindow(NULL),
     mToolBarDock(NULL),
     mAudioIOToken(-1),
     mIsDeleting(false),
     mTracksFitVerticallyZoomed(false),  //lda
     mCleanSpeechMode(false),            //lda
     mShowId3Dialog(false)              //lda
{
   mStatusBar = CreateStatusBar();

   mDrag = NULL;

#if wxUSE_DRAG_AND_DROP
   SetDropTarget(new AudacityDropTarget(this));
#endif

   // MM: DirManager is created dynamically, freed on demand via ref-counting
   // MM: We don't need to Ref() here because it start with refcount=1
   mDirManager = new DirManager();

   // Create track list
   mTracks = new TrackList();
   mLastSavedTracks = NULL;

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

   UpdatePrefs();
   // Some extra information
   mViewInfo.bIsPlaying = false;
   mViewInfo.bRedrawWaveform = false;

   mLockPlayRegion = false;
   mLastUpdateUITime = ::wxGetUTCTime();

   CreateMenusAndCommands();

   // LLL:  Read this!!!
   //
   // Until the time (and cpu) required to refresh the track panel is
   // reduced, leave the following window creations in the order specified.
   // This will place the refresh of the track panel last, allowing all
   // the others to get done quickly.
   //
   // Near as I can tell, this is only a problem under Windows.
   //

   //
   // Create the ToolBarDock
   //
   mToolBarDock = new ToolBarDock( this );
   mToolBarDock->LayoutToolBars();

   // Fix the sliders on the mixer toolbar so that the tip windows
   // actually pop-up on top of everything else.  Sorry for the hack -
   // it's necessary to do it this way to avoid flicker.
#if 0
   MixerToolBar *mtb = GetMixerToolBar();
   if (mtb)
      mtb->RecreateTipWindows();
#endif 

   //
   // Create the horizontal ruler
   //
   mRuler = new AdornedRulerPanel( this,
                                   wxID_ANY,
                                   wxDefaultPosition,
                                   wxSize( -1, AdornedRulerPanel::GetRulerHeight() ),
                                   &mViewInfo );

   //
   // Create the selection bar
   //
   mSelectionBar = new SelectionBar(this,
                                    wxID_ANY, 
                                    wxDefaultPosition,
                                    wxDefaultSize,
                                    mRate,
                                    this);

   //
   // Create the TrackPanel and the scrollbars
   //
   wxWindow    * pPage;

#ifdef EXPERIMENTAL_NOTEBOOK
   // We are using a notebook (tabbed panel), so we create the notebook and add pages.
   GuiFactory Factory;
   wxNotebook  * pNotebook;
   mMainPanel = Factory.AddPanel( 
      this, wxPoint( left, top ), wxSize( width, height ) );
   pNotebook  = Factory.AddNotebook( mMainPanel );
   pPage = Factory.AddPage( pNotebook, _("Main Mix"));
#else
   // Not using a notebook, so we place the track panel inside another panel, 
   // this keeps the notebook code and normal code consistant and also
   // paves the way for adding additional windows inside the track panel.
   mMainPanel = new wxPanel(this, -1, 
      wxDefaultPosition, 
      wxDefaultSize,
      wxNO_BORDER);
   mMainPanel->SetSizer( new wxBoxSizer(wxVERTICAL) );
   pPage = mMainPanel;
#endif

   wxBoxSizer *bs = new wxBoxSizer( wxVERTICAL );
   bs->Add( mToolBarDock, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP );
   bs->Add( mRuler, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP );
   bs->Add( pPage, 1, wxEXPAND | wxALIGN_LEFT );
   bs->Add( mSelectionBar, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_BOTTOM );
   SetAutoLayout( true );
   SetSizer( bs );
   bs->Layout();

   mHsbar = new wxScrollBar(pPage,
                            HSBarID,
                            wxDefaultPosition,
                            wxDefaultSize,
                            wxSB_HORIZONTAL);
   mVsbar = new wxScrollBar(pPage,
                            VSBarID,
                            wxDefaultPosition,
                            wxDefaultSize,
                            wxSB_VERTICAL);

   mTrackPanel = new TrackPanel(pPage,
                                TrackPanelID,
                                wxDefaultPosition,
                                wxDefaultSize,
                                mTracks,
                                &mViewInfo,
                                this,
                                mRuler);

   bs = (wxBoxSizer *) pPage->GetSizer();

   wxBoxSizer *hs;
   wxBoxSizer *vs;

   // Top horizontal grouping
   hs = new wxBoxSizer( wxHORIZONTAL );

   // Track panel
   hs->Add( mTrackPanel, 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP );

   // Vertical grouping
   vs = new wxBoxSizer( wxVERTICAL );

   // Vertical scroll bar
   vs->Add( mVsbar, 1, wxEXPAND | wxALIGN_RIGHT | wxALIGN_TOP );
   hs->Add( vs, 0, wxEXPAND | wxALIGN_RIGHT | wxALIGN_TOP );
   bs->Add( hs, 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP );

   // Bottom horizontal grouping
   hs = new wxBoxSizer( wxHORIZONTAL );

   // Bottom scrollbar
   hs->Add( mTrackPanel->GetLeftOffset() - 1, 0 );
   hs->Add( mHsbar, 1, wxALIGN_BOTTOM );
   hs->Add( mVsbar->GetSize().GetWidth(), 0 );
   bs->Add( hs, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_BOTTOM );

   // Lay it out
   pPage->SetAutoLayout( true );
   pPage->Layout();

#ifdef EXPERIMENTAL_NOTEBOOK
   AddPages(this, Factory, pNotebook);
#endif   

   mMainPanel->Layout();

   wxASSERT( mTrackPanel->GetProject()==this);

   // MM: Give track panel the focus to ensure keyboard commands work
   mTrackPanel->SetFocus();

   InitialState();
   FixScrollbars();
   mRuler->SetLeftOffset( mTrackPanel->GetLeftOffset() );
   mRuler->SetTrackPanel(mTrackPanel);

   //
   // Set the Icon
   //

   // loads either the XPM or the windows resource, depending on the platform
#if !defined(__WXMAC__) && !defined(__WXX11__)
   #ifdef __WXMSW__
      wxIcon ic(wxICON(AudacityLogo));
   #else
      wxIcon ic;
      ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
   #endif
   SetIcon(ic);
#endif
   mIconized = false;

   // Create tags object
   mTags = new Tags();

   mTrackFactory = new TrackFactory(mDirManager);
   mImporter = new Importer;
   mImportingRaw = false;

   wxString msg = wxString::Format(wxT("Welcome to Audacity version %s"),
                                   wxT(AUDACITY_VERSION_STRING));
   mStatusBar->SetStatusText(msg);
   mLastStatusUpdateTime = ::wxGetUTCTime();

   mTimer = new wxTimer(this, AudacityProjectTimerID);
   mTimer->Start(200);
}

AudacityProject::~AudacityProject()
{
   // DMM: Save the size of the last window the user closes
   // (unless we're quitting - then the Quit routine will
   // do it for us).
   if (gAudacityProjects.GetCount() == 1 &&
       !gIsQuitting)
      SaveWindowSize();

   mIsDeleting = true;

   delete mTimer;
   mTimer=NULL;


// JKC: For Win98 and Linux do not detach the menu bar.
// We want wxWindows to clean it up for us.
// TODO: Is there a Mac issue here??
// SetMenuBar(NULL);

   if (gAudioIO->IsStreamActive(mAudioIOToken)) {
      gAudioIO->StopStream();

      while(gAudioIO->IsBusy()) {
         wxMilliSleep(100);
      }
   }

   mTrackPanel->Destroy();

   delete mImporter;
   mImporter = NULL;

   delete mTrackFactory;
   mTrackFactory = NULL;

   // Lock all blocks in all tracks of the last saved version, so that
   // the blockfiles aren't deleted on disk when we delete the blockfiles
   // in memory.  After it's locked, delete the data structure so that
   // there's no memory leak.
   if (mLastSavedTracks) {
      TrackListIterator iter(mLastSavedTracks);
      Track *t = iter.First();
      while (t) {
         if (t->GetKind() == Track::Wave)
            ((WaveTrack *) t)->Lock();
         t = iter.Next();
      }

      mLastSavedTracks->Clear(true);
      delete mLastSavedTracks;
      mLastSavedTracks = NULL;
   }


   delete mTags;
   mTags = NULL;

   mTracks->Clear(true);
   delete mTracks;
   mTracks = NULL;

   delete mRecentFiles;
   mRecentFiles = NULL;
//   delete mRecentProjects;
//   mRecentProjects = NULL;

   // MM: Tell the DirManager it can now delete itself
   // if it finds it is no longer needed. If it is still
   // used (f.e. by the clipboard), it will recognize this
   // and will destroy itself later.
   mDirManager->Deref();

#ifdef EXPERIMENTAL_VOCAL_STUDIO
   ButtonWindow::ClearStripesAndButtons();
#endif
}

void AudacityProject::UpdateGuiPrefs()
{
   gPrefs->Read(wxT("/GUI/UpdateSpectrogram"), &mViewInfo.bUpdateSpectrogram, true);
   gPrefs->Read(wxT("/GUI/AutoScroll"), &mViewInfo.bUpdateTrackIndicator, true);
   gPrefs->Read(wxT("/GUI/TracksFitVerticallyZoomed"), &mTracksFitVerticallyZoomed, false);
}

void AudacityProject::UpdateBatchPrefs()
{
   gPrefs->Read(wxT("/Batch/EmptyCanBeDirty"), &mEmptyCanBeDirty, false );
   gPrefs->Read(wxT("/Batch/CleanSpeechMode"), &mCleanSpeechMode, false);
   gPrefs->Read(wxT("/Batch/ShowId3Dialog"), &mShowId3Dialog, false);
   gPrefs->Read(wxT("/Batch/NormalizeOnLoad"),&mNormalizeOnLoad, false);

   if( GetControlToolBar() )
   {
      GetControlToolBar()->UpdatePrefs();
   }
}

void AudacityProject::UpdatePrefs()
{
   UpdateGuiPrefs();
   UpdateBatchPrefs();
   SetProjectTitle( );

   if( mTrackPanel )
      mTrackPanel->UpdatePrefs();
// TODO: Do we need to update the new status bar after a prefs change?
//   if( mStatus )
//      mStatus->UpdateRates();

   if( GetMixerToolBar() )
      GetMixerToolBar()->UpdateControls();
}

void AudacityProject::RedrawProject()
{
   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::RefreshCursor()
{
   mTrackPanel->HandleCursorForLastMouseEvent();
}

void AudacityProject::SetSel0(double newSel0)
{
   //Bound checking should go on here
   
   mViewInfo.sel0 = newSel0;
}

void AudacityProject::SetSel1(double newSel1)
{
   //Bound checking should go on here
   
   mViewInfo.sel1 = newSel1;
}



DirManager *AudacityProject::GetDirManager()
{
   return mDirManager;
}

TrackFactory *AudacityProject::GetTrackFactory()
{
   return mTrackFactory;
}

AdornedRulerPanel *AudacityProject::GetRulerPanel()
{
   return mRuler;
}

int AudacityProject::GetAudioIOToken()
{
   return mAudioIOToken;
}

void AudacityProject::SetAudioIOToken(int token)
{
   mAudioIOToken = token;
}

Tags *AudacityProject::GetTags()
{
   return mTags;
}

wxString AudacityProject::GetName()
{
   wxString name = wxFileNameFromPath(mFileName);

   // Chop off the extension
   size_t len = name.Len();
   if (len > 4 && name.Mid(len - 4) == wxT(".aup"))
      name = name.Mid(0, len - 4);

   return name;
}

void AudacityProject::SetProjectTitle()
{
   wxString name = GetName();
   if( name.IsEmpty() )
   {
      name = mCleanSpeechMode ? wxT("Audacity CleanSpeech") : wxT("Audacity");
   }
   SetTitle( name );
}

void AudacityProject::AS_SetRate(double rate)
{
   mRate = rate;
}

void AudacityProject::AS_ModifySelection(double &start, double &end)
{
   mViewInfo.sel0 = start;
   mViewInfo.sel1 = end;
   mTrackPanel->Refresh(false);
   ModifyState();
}

void AudacityProject::FinishAutoScroll()
{
   // Set a flag so we don't have to generate two update events
   mAutoScrolling = true;

   // Call our Scroll method which updates our ViewInfo variables
   // to reflect the positions of the scrollbars
   wxScrollEvent *dummy = new wxScrollEvent();
   OnScroll(*dummy);
   delete dummy;

   mAutoScrolling = false;
}


///
/// This method handles general left-scrolling, either for drag-scrolling
/// or when the scrollbar is clicked to the left of the thumb
///
void AudacityProject::OnScrollLeft()
{
   int pos = mHsbar->GetThumbPosition();
   pos = (pos > 0) ? pos : 0;   //Set to larger of pos and 0

   if (pos > 0) {
      mHsbar->SetThumbPosition(pos - sbarHjump);        //Jump sbarHjump pixels to the left
      FinishAutoScroll();
   }
}
///
/// This method handles general right-scrolling, either for drag-scrolling
/// or when the scrollbar is clicked to the right of the thumb
///

void AudacityProject::OnScrollRight()
{
   int pos = mHsbar->GetThumbPosition();
   int max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = (pos < max) ? pos : max;       //Set to smaller of pos and max

   if (pos < max) {
      mHsbar->SetThumbPosition(pos + sbarHjump);        //Jump sbarHjump pixels to the right
      FinishAutoScroll();
   }
}

///
///  This handles the event when the left direction button on the scrollbar is depresssed
///
void AudacityProject::OnScrollLeftButton(wxScrollEvent & event)
{
   int pos = mHsbar->GetThumbPosition();
   pos = (pos > 0) ? pos : 0;   //Set to larger of pos and 0

   if (pos > 0) {
      mHsbar->SetThumbPosition(pos - sbarHjump);        //Jump sbarHjump pixels to the left
      OnScroll(event);
   }
}

///
///  This handles  the event when the right direction button on the scrollbar is depresssed
///
void AudacityProject::OnScrollRightButton(wxScrollEvent & event)
{
   int pos = mHsbar->GetThumbPosition();
   int max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = (pos < max) ? pos : max;       //Set to smaller of pos and max

   if (pos < max) {
      mHsbar->SetThumbPosition(pos + sbarHjump);        //Jump sbarHjump pixels to the right
      OnScroll(event);
   }
}


//
// This method, like the other methods prefaced with TP, handles TrackPanel
// 'callback'.
//
void AudacityProject::TP_ScrollWindow(double scrollto)
{
   int pos = (int) (scrollto * mViewInfo.zoom);
   int max = mHsbar->GetRange() - mHsbar->GetThumbSize();

   if (pos > max)
      pos = max;
   else if (pos < 0)
      pos = 0;

   mHsbar->SetThumbPosition(pos);

   // Call our Scroll method which updates our ViewInfo variables
   // to reflect the positions of the scrollbars
   wxScrollEvent *dummy = new wxScrollEvent();
   OnScroll(*dummy);
   delete dummy;
}

//
// Scroll vertically. This is called for example by the mouse wheel
// handler in Track Panel. A positive argument makes the window
// scroll down, while a negative argument scrolls up.
//
void AudacityProject::TP_ScrollUpDown(int delta)
{
   int oldPos = mVsbar->GetThumbPosition();
   int pos = oldPos + delta;
   int max = mVsbar->GetRange() - mVsbar->GetThumbSize();

   // Can be negative in case of only one track
   if (max < 0)
      max = 0;

   if (pos > max)
      pos = max;
   else if (pos < 0)
      pos = 0;
 
   if (pos != oldPos)
   {
      mVsbar->SetThumbPosition(pos);

      wxScrollEvent dummy;
      OnScroll(dummy);
   }
}

void AudacityProject::FixScrollbars()
{
   if(!mTracks)
      return;

   bool rescroll = false;

   int totalHeight = (mTracks->GetHeight() + 32);

   int panelWidth, panelHeight;
   mTrackPanel->GetTracksUsableArea(&panelWidth, &panelHeight);

   // Add 1/4 of a screen of blank space to the end of the longest track
   mViewInfo.screen = ((double) panelWidth) / mViewInfo.zoom;
   mViewInfo.total = mTracks->GetEndTime() + mViewInfo.screen / 4;

   if (mViewInfo.h > mViewInfo.total - mViewInfo.screen) {
      mViewInfo.h = mViewInfo.total - mViewInfo.screen;
      rescroll = true;
   }
   if (mViewInfo.h < 0.0) {
      mViewInfo.h = 0.0;
      rescroll = true;
   }

   mViewInfo.sbarTotal = (int) (mViewInfo.total * mViewInfo.zoom);
   mViewInfo.sbarScreen = (int) (mViewInfo.screen * mViewInfo.zoom);
   mViewInfo.sbarH = (int) (mViewInfo.h * mViewInfo.zoom);

   mViewInfo.vpos = mVsbar->GetThumbPosition() * mViewInfo.scrollStep;

   if (mViewInfo.vpos >= totalHeight)
      mViewInfo.vpos = totalHeight - 1;
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

      mTrackPanel->Refresh(false);
      rescroll = false;
   }
   if (mViewInfo.screen >= mViewInfo.total && mViewInfo.sbarH != 0) {
      mViewInfo.sbarH = 0;

      mTrackPanel->Refresh(false);
      rescroll = false;
   }

   mHsbar->SetScrollbar(mViewInfo.sbarH, mViewInfo.sbarScreen,
                        mViewInfo.sbarTotal, mViewInfo.sbarScreen, TRUE);
   mHsbar->Refresh();
   mVsbar->SetScrollbar(mViewInfo.vpos / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep,
                        totalHeight / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep, TRUE);
   mVsbar->Refresh();
   mViewInfo.lastZoom = mViewInfo.zoom;

   if (rescroll && mViewInfo.screen < mViewInfo.total){
      mTrackPanel->Refresh(false);
   }

   UpdateMenus();
}

void AudacityProject::HandleResize()
{
   if (!mTrackPanel) 
      return;

   FixScrollbars();

   mToolBarDock->LayoutToolBars();
   Layout();

   mSelectionBar->Refresh( false );

   // Retrieve size of this projects window
   wxSize mainsz = GetSize();

   // Retrieve position of the track panel to use as the size of the top
   // third of the window
   wxPoint tppos = ClientToScreen( mTrackPanel->GetParent()->GetPosition() );

   // Retrieve position of selection bar to use as the size of the bottom
   // third of the window
   wxPoint sbpos = ClientToScreen( mSelectionBar->GetPosition() );

   // The "+ 50" is the minimum height of the TrackPanel
   SetSizeHints( 250, ( mainsz.y - sbpos.y ) + tppos.y + 50, 20000, 20000 );
}


void AudacityProject::OnIconize(wxIconizeEvent &event)
{
   int VisibleProjectCount = 0;

   //JKC: On Iconizing we get called twice.  Don't know
   // why but it does no harm.
   // Should we be returning true/false rather than 
   // void return?  I don't know.
   mIconized = event.Iconized();

   unsigned int i;

   for(i=0;i<gAudacityProjects.Count();i++){
      if(gAudacityProjects[i]){
         if( !gAudacityProjects[i]->mIconized )
            VisibleProjectCount++;
      }
   }

   event.Skip();
}

void AudacityProject::OnSize(wxSizeEvent & event)
{
   HandleResize();

   event.Skip();
}

///
///  A toolbar has been updated, so handle it like a sizing event.
///
void AudacityProject::OnToolBarUpdate(wxCommandEvent & event)
{
   HandleResize();

   event.Skip( false );             /* No need to propagate any further */
}

void AudacityProject::OnScroll(wxScrollEvent & event)
{
   int hlast = mViewInfo.sbarH;
   int vlast = mViewInfo.vpos;
   int hoffset = 0;
   int voffset = 0;

   mViewInfo.sbarH = mHsbar->GetThumbPosition();

   if (mViewInfo.sbarH != hlast) {
      mViewInfo.h = mViewInfo.sbarH / mViewInfo.zoom;

      if (mViewInfo.h > mViewInfo.total - mViewInfo.screen)
         mViewInfo.h = mViewInfo.total - mViewInfo.screen;
      if (mViewInfo.h < 0.0)
         mViewInfo.h = 0.0;
      hoffset = (mViewInfo.sbarH - hlast);
   }

   mViewInfo.vpos = mVsbar->GetThumbPosition() * mViewInfo.scrollStep;
   voffset = mViewInfo.vpos - vlast;

   /*   TODO: add back fast scrolling code

      // Track panel is updated either way, but it is smart and only redraws
      // what is needed
      TrackPanel->FastScroll(-hoffset, -voffset);

      // Ruler panel updated if we scroll horizontally
      if (hoffset) {
      REDRAW(rulerPanel);
      }
    */

   SetActiveProject(this);

   if (!mAutoScrolling) {
      mTrackPanel->Refresh(false);
   }
}

bool AudacityProject::HandleKeyDown(wxKeyEvent & event)
{
   // Allow the Play button to change to a Loop button,
   // and the zoom cursor to change to a zoom out cursor
   if (event.GetKeyCode() == WXK_SHIFT) {
      ControlToolBar *tb = GetControlToolBar();
      if (tb)
         tb->OnShiftDown(event);

      mTrackPanel->HandleShiftKey(true);
   }

   if (event.GetKeyCode() == WXK_CONTROL)
      mTrackPanel->HandleControlKey(true);

   return false;
}

bool AudacityProject::HandleChar(wxKeyEvent & event)
{
   return mCommandManager.HandleKey(event, GetUpdateFlags(), 0xFFFFFFFF);
}

bool AudacityProject::HandleKeyUp(wxKeyEvent & event)
{
   // Allow the Loop button to change back to a Play button,
   // and Zoom Out cursor back to Zoom In
   if (event.GetKeyCode() == WXK_SHIFT) {
      ControlToolBar *tb = GetControlToolBar();
      if (tb)
         tb->OnShiftUp(event);

      mTrackPanel->HandleShiftKey(false);
   }

   if (event.GetKeyCode() == WXK_CONTROL)
      mTrackPanel->HandleControlKey(false);

   return false;
}

void AudacityProject::OnMenuEvent(wxMenuEvent & event)
{
   if (event.GetEventType() == wxEVT_MENU_OPEN) {
      mCommandManager.HandleMenuOpen(event);
   }
   else if (event.GetEventType() == wxEVT_MENU_CLOSE) {
      mCommandManager.HandleMenuClose(event);
   }
}

void AudacityProject::OnMenu(wxCommandEvent & event)
{
   bool handled = mCommandManager.HandleMenuID(event.GetId(),
                                               GetUpdateFlags(),
                                               0xFFFFFFFF);

   if (handled)
      event.Skip(false);
   else
      event.Skip(true);
}

//TODO: This function is still kinda hackish, clean up
void AudacityProject::OnUpdateMenus(wxUpdateUIEvent & event)
{
 #if 0
   if (::wxGetUTCTime() - mLastUpdateUITime < 3)
      return;

   mLastUpdateUITime = ::wxGetUTCTime();
 #endif

   UpdateMenus();
}

void AudacityProject::OnPaint(wxPaintEvent & /*event*/)
{
   // Unfortunately some of the code called in our destructor
   // can trigger evens like Paint events...
   if (mIsDeleting)
      return;

   wxPaintDC dc(this);
}

void AudacityProject::OnActivate(wxActivateEvent & event)
{
   mActive = event.GetActive();
   if (mActive)
      SetActiveProject(this);
   event.Skip();
}

bool AudacityProject::IsActive()
{
   return mActive;
}

void AudacityProject::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown())
      SetActiveProject(this);
}

void AudacityProject::OnCloseWindow(wxCloseEvent & event)
{
   if (gPrefsDialogVisible) {
      event.Veto();
      return;
   }

   // Check to see if we were playing or recording
   // audio, and if so, make sure Audio I/O is completely finished.
   // The main point of this is to properly push the state
   // and flush the tracks once we've completely finished
   // recording new state.
   // This code is derived from similar code in 
   // AudacityProject::~AudacityProject() and TrackPanel::OnTimer().
   if (this->GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(this->GetAudioIOToken())) {

      wxBusyCursor busy;
      gAudioIO->StopStream();
      while(gAudioIO->IsBusy()) {
         wxMilliSleep(100);
      }
      
      // We were playing or recording audio, but we've stopped the stream.
      wxCommandEvent dummyEvent;
      this->GetControlToolBar()->OnStop(dummyEvent);      
         
      if (gAudioIO->GetNumCaptureChannels() > 0) {
         // Tracks are buffered during recording.  This flushes
         // them so that there's nothing left in the append
         // buffers.
         TrackListIterator iter(mTracks);
         for (Track * t = iter.First(); t; t = iter.Next()) {
            if (t->GetKind() == Track::Wave) {
               ((WaveTrack *)t)->Flush();
            }
         }
         this->PushState(_("Recorded Audio"), _("Record"));
      }

      this->FixScrollbars();
      this->SetAudioIOToken(0);
      this->RedrawProject();
   }

	// These two lines test for an 'empty' project.
   // of course it could still have a history at this stage.
   TrackListIterator iter2(mTracks);
	bool bHasTracks = (iter2.First() != NULL);

	// We may not bother to prompt the user to save, if the 
   // project is now empty.
	if (event.CanVeto() && (mEmptyCanBeDirty || bHasTracks)) {
      if (mUndoManager.UnsavedChanges()) {
         int result = wxMessageBox(_("Save changes before closing?"),
                                   _("Save changes?"),
                                   wxYES_NO | wxCANCEL | wxICON_QUESTION,
                                   this);

         if (result == wxCANCEL || (result == wxYES && !Save())) {
            event.Veto();
            return;
         }
		}
   }

   // LL:  Moved here from destructor since the object isn't necessarily deleted
   // right away (Destroy() can queue the deletion) and, during QuitAudacity(), a
   // check is made for active projects.  If left in the destructor, there
   // is a chance that the project will not have been removed from the list
   // and it will be "closed" twice.
   gAudacityProjects.Remove(this);

   if (gActiveProject == this) {
      // Find a new active project
      if (gAudacityProjects.Count() > 0) {
         gActiveProject = gAudacityProjects[0];
      }
      else {
         gActiveProject = NULL;
      }
   }
   
   if (gAudacityProjects.IsEmpty() && !gIsQuitting) {
      bool quitOnClose;
#ifdef __WXMAC__
      bool defaultQuitOnClose = false;
#else
      bool defaultQuitOnClose = true;
#endif
      
      gPrefs->Read(wxT("/GUI/QuitOnClose"), &quitOnClose, defaultQuitOnClose);
      
      if (quitOnClose)
         QuitAudacity();
      else {
#ifdef __WXMAC__
         gParentFrame->Show();
         wxGetApp().SetTopWindow(gParentFrame);
#else
         CreateNewAudacityProject(gParentWindow);
#endif
      }
   }

   //BG: Process messages before we destroy the window
   //
   //LL: On 2006-06-18, I had a failure in Meter::OnMouse()
   //    when quitting.  The wxSafeYield() was allowing some
   //    messages to get processed and Meter::OnMouse() failed
   //    when it attepted to use the result returned from
   //    GetActiveProject().  I was unable to reproduce it and
   //    couldn't figure out how to correct it, so the exposure
   //    is still there.
   //
   wxSafeYield(NULL, true);
   Destroy();
}

// static method, can be called outside of a project
void AudacityProject::ShowOpenDialog(AudacityProject *proj)
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),
                                FROMFILENAME(::wxGetCwd()));
   // Beware, some compilers let you access mCleanSpeechMode
   // here, even though it is not valid for a static method call,
   // so we must go via prefs.
   bool bCleanSpeechMode;
   gPrefs->Read(wxT("/Batch/CleanSpeechMode"), &bCleanSpeechMode, false );
   wxFileDialog dlog(NULL, _("Select one or more audio files..."),
                     path, wxT(""),
	                  bCleanSpeechMode ? 
                        _("Music files (*.wav;*.mp3)|*.wav;*.mp3|WAV files (*.wav)|*.wav|MP3 files (*.mp3)|*.mp3")
                     :
                        _("All files (*.*)|*.*|Audacity projects (*.aup)|*.aup|WAV files (*.wav)|*.wav|AIFF files (*.aif)|*.aif|AU files (*.au)|*.au|MP3 files (*.mp3)|*.mp3|Ogg Vorbis files (*.ogg)|*.ogg|FLAC files (*.flac)|*.flac|List of Files (*.lof)|*.lof"),
                     wxOPEN | wxMULTIPLE);

   int result = dlog.ShowModal();

   if (result != wxID_OK)
      return;

   wxArrayString selectedFiles;
   unsigned int ff;

   dlog.GetPaths(selectedFiles);

   for(ff=0; ff<selectedFiles.GetCount(); ff++) {
      wxString fileName = selectedFiles[ff];
      wxFileName newFileName(fileName);

      gPrefs->Write(wxT("/DefaultOpenPath"), wxPathOnly(fileName));
      
      // Make sure it isn't already open
      size_t numProjects = gAudacityProjects.Count();
      for (size_t i = 0; i < numProjects; i++) {
         if (newFileName.SameAs(gAudacityProjects[i]->mFileName)) {
            wxMessageBox(wxString::Format(_("%s is already open in another window."),
                                          newFileName.GetName().c_str()),
                         _("Error opening project"),
                         wxOK | wxCENTRE);
            continue;
         }
      }

      // DMM: If the project is dirty, that means it's been touched at
      // all, and it's not safe to open a new project directly in its
      // place.  Only if the project is brand-new clean and the user
      // hasn't done any action at all is it safe for Open to take place
      // inside the current project.
      //
      // If you try to Open a new project inside the current window when
      // there are no tracks, but there's an Undo history, etc, then
      // bad things can happen, including data files moving to the new
      // project directory, etc.
      if (!proj || proj->mDirty || !proj->mTracks->IsEmpty()) {
         // Open in a new window
         proj = CreateNewAudacityProject(gParentWindow);
      }
      // This project is clean; it's never been touched.  Therefore
      // all relevant member variables are in their initial state,
      // and it's okay to open a new project inside this window.
      proj->OpenFile(fileName);

      proj->mRecentFiles->AddFileToHistory(fileName);
      gPrefs->SetPath(wxT("/RecentFiles"));
      proj->mRecentFiles->Save(*gPrefs);
      gPrefs->SetPath(wxT(".."));
   }
}

void AudacityProject::OpenFile(wxString fileName)
{
   // On Win32, we may be given a short (DOS-compatible) file name on rare
   // occassions (e.g. stuff like "C:\PROGRA~1\AUDACI~1\PROJEC~1.AUP"). We
   // convert these to long file name first.
   fileName = PlatformCompatibility::GetLongFileName(fileName);

   // We want to open projects using wxTextFile, but if it's NOT a project
   // file (but actually a WAV file, for example), then wxTextFile will spin
   // for a long time searching for line breaks.  So, we look for our
   // signature at the beginning of the file first:

   wxString firstLine = wxT("AudacityProject");

   if (!::wxFileExists(FILENAME(fileName))) {
      wxMessageBox(_("Could not open file: ") + fileName,
                   _("Error opening file"),
                   wxOK | wxCENTRE, this);
      return;
   }

   wxFFile *ff = new wxFFile(FILENAME(fileName).c_str(), wxT("rb"));
   if (!ff->IsOpened()) {
      wxMessageBox(_("Could not open file: ") + fileName,
                   _("Error opening file"),
                   wxOK | wxCENTRE, this);
   }
   char buf[16];
   int numRead = ff->Read(buf, 15);
   if (numRead != 15) {
      wxMessageBox(wxString::Format(_("File may be invalid or corrupted: \n%s"), 
                   (const wxChar*)fileName), _("Error opening file or project"),
                   wxOK | wxCENTRE, this);
     ff->Close();
     delete ff;
     return;
   }
   buf[15] = 0;
   ff->Close();
   delete ff;

   wxString temp = LAT1CTOWX(buf);
   if (temp == wxT("AudacityProject")) {
      // It's an Audacity 1.0 (or earlier) project file.
      // Convert to the new format.
      bool success = ConvertLegacyProjectFile(wxFileName(fileName));
      if (!success) {
         wxMessageBox(_("Audacity was unable to convert an Audacity 1.0 project to the new project format."),
                      _("Error opening project"),
                      wxOK | wxCENTRE, this);
         return;
      }
      else {
         temp = wxT("<?xml ");
      }
   }
   
   if (temp.Mid(0, 6) != wxT("<?xml ")) {
      // If it's not XML, try opening it as any other form of audio
      Import(fileName);
      return;
   }

   ///
   /// Parse project file
   ///

   mFileName = fileName;
   SetProjectTitle();

   XMLFileReader xmlFile;

   if (xmlFile.Parse(this, fileName)) {
      // By making a duplicate set of pointers to the existing blocks
      // on disk, we add one to their reference count, guaranteeing
      // that their reference counts will never reach zero and thus
      // the version saved on disk will be preserved until the
      // user selects Save().
      
      bool err = false;
      Track *t;
      TrackListIterator iter(mTracks);
      mLastSavedTracks = new TrackList();

      t = iter.First();
      while (t) {
         if (t->GetErrorOpening())
            err = true;
         mLastSavedTracks->Add(t->Duplicate());
         t = iter.Next();
      }

      InitialState();
      mTrackPanel->SetFocusedTrack(iter.First());
      HandleResize();
      mTrackPanel->Refresh(false);
      mTrackPanel->Update(); // force any repaint to happen now,
      // else any asynch calls into the blockfile code will not have
      // finished logging errors (if any) before the call to ProjectFSCK()

      int status=GetDirManager()->ProjectFSCK(err);

      if(status & FSCKstatus_CLOSEREQ){
         // there was an error in the load/check and the user
         // explictly opted to close the project

         mTracks->Clear(true);
         
         mFileName = wxT("");
         SetProjectTitle();
         mTrackPanel->Refresh(true);

      }else if (status & FSCKstatus_CHANGED){
         
         t = iter.First();
         while (t) {
            if (t->GetKind() == Track::Wave)
            {
               // Only wave tracks have a notion of "changed"
               for (WaveClipList::Node* it=((WaveTrack*)t)->GetClipIterator(); it; it=it->GetNext())
                  it->GetData()->MarkChanged();
            }
            t = iter.Next();
         }
         mTrackPanel->Refresh(true);
         this->PushState(_("Project checker repaired file"), _("Repair"));

      }
   } else {
      mTracks->Clear(true);

      mFileName = wxT("");
      SetProjectTitle();

      wxMessageBox(xmlFile.GetErrorStr(),
                   _("Error opening project"),
                   wxOK | wxCENTRE, this);
   }
}

bool AudacityProject::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   wxString fileVersion;
   wxString audacityVersion;
   int requiredTags = 0;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while(*attrs) {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value)
         break;

      if (!wxStrcmp(attr, wxT("version"))) {
         fileVersion = value;
         requiredTags++;
      }

      if (!wxStrcmp(attr, wxT("audacityversion"))) {
         audacityVersion = value;
         requiredTags++;
      }

      if (!wxStrcmp(attr, wxT("projname"))) {
         wxString projName = value;
         wxString projPath = wxPathOnly(mFileName);
         
         if (!mDirManager->SetProject(projPath, projName, false)) {

            wxMessageBox(wxString::Format(_("Couldn't find the project data folder: \"%s\""),
                                          projName.c_str()),
                         _("Error opening project"),
                         wxOK | wxCENTRE, this);

            return false;
         }

         requiredTags++;
      }

      if (!wxStrcmp(attr, wxT("sel0")))
         Internat::CompatibleToDouble(value, &mViewInfo.sel0);

      if (!wxStrcmp(attr, wxT("sel1")))
         Internat::CompatibleToDouble(value, &mViewInfo.sel1);

      long longVpos = 0;
      if (!wxStrcmp(attr, wxT("vpos")))
         wxString(value).ToLong(&longVpos);
      mViewInfo.vpos = longVpos;

      if (!wxStrcmp(attr, wxT("h")))
         Internat::CompatibleToDouble(value, &mViewInfo.h);

      if (!wxStrcmp(attr, wxT("zoom")))
         Internat::CompatibleToDouble(value, &mViewInfo.zoom);

      if (!wxStrcmp(attr, wxT("rate"))) {
         Internat::CompatibleToDouble(value, &mRate);
         mSelectionBar->SetRate(mRate);
      }
   } // while

   // Specifically detect newer versions of Audacity
   if (fileVersion.Length() != 5 || // expecting '1.1.0', for example
       fileVersion > wxT(AUDACITY_FILE_FORMAT_VERSION)) {
      wxString msg;
      msg.Printf(_("This file was saved using Audacity %s.\nYou are using Audacity %s - you need to upgrade to\na newer version to open this file."),
                 audacityVersion.c_str(),
                 wxT(AUDACITY_VERSION_STRING));
      wxMessageBox(msg,
                   _("Can't open project file"),
                   wxOK | wxICON_EXCLAMATION | wxCENTRE, this);
      return false;
   }

   // Specifically detect older versions of Audacity
   if (fileVersion < wxT(AUDACITY_FILE_FORMAT_VERSION)) {
      wxString msg;
      msg.Printf(_("This file was saved by Audacity %s and the format\nhas changed.  This version of Audacity can try to\nopen it, but there may be problems.  You should back up\nyour project first, to be safe.\n\nWould you like to open this file right now anyway?"),
                 audacityVersion.c_str());
      int action;
      action = wxMessageBox(msg,
                            _("Opening old project file"),
                            wxYES_NO | wxICON_EXCLAMATION | wxCENTRE,
                            this);
      if (action == wxNO)
         return false;
   }

   if (wxStrcmp(tag, wxT("audacityproject")) &&
       wxStrcmp(tag, wxT("project"))) {
      // If the tag name is not one of these two (the new name is
      // "project" with an Audacity namespace, but we don't detect
      // the namespace yet), then we don't know what the error is
      return false;
   }

   if (requiredTags < 3)
      return false;

   // All other tests passed, so we succeed
   return true;
}

XMLTagHandler *AudacityProject::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("tags"))) {
      return mTags;
   }

   if (!wxStrcmp(tag, wxT("wavetrack"))) {
      WaveTrack *newTrack = mTrackFactory->NewWaveTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!wxStrcmp(tag, wxT("notetrack"))) {
      NoteTrack *newTrack = mTrackFactory->NewNoteTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!wxStrcmp(tag, wxT("labeltrack"))) {
      LabelTrack *newTrack = mTrackFactory->NewLabelTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!wxStrcmp(tag, wxT("timetrack"))) {
      TimeTrack *newTrack = mTrackFactory->NewTimeTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   return NULL;
}

void AudacityProject::WriteXML(int depth, FILE *fp)
{
   int i;

   // Warning: This block of code is duplicated in Save, for now...
   wxString project = mFileName;
   if (project.Len() > 4 && project.Mid(project.Len() - 4) == wxT(".aup"))
      project = project.Mid(0, project.Len() - 4);
   wxString projName = wxFileNameFromPath(project) + wxT("_data");
   // End Warning -DMM

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<project ");
   fprintf(fp, "xmlns=\"http://audacity.sourceforge.net/xml/\" ");
   fprintf(fp, "projname=\"%s\" ", (const char *)XMLEsc(projName).mb_str());
   fprintf(fp, "version=\"%s\" ", AUDACITY_FILE_FORMAT_VERSION);
   fprintf(fp, "audacityversion=\"%s\" ", AUDACITY_VERSION_STRING);
   fprintf(fp, "sel0=\"%s\" ", (const char *)Internat::ToString(mViewInfo.sel0, 10).mb_str());
   fprintf(fp, "sel1=\"%s\" ", (const char *)Internat::ToString(mViewInfo.sel1, 10).mb_str());
   fprintf(fp, "vpos=\"%d\" ", mViewInfo.vpos);
   fprintf(fp, "h=\"%s\" ", (const char *)Internat::ToString(mViewInfo.h, 10).mb_str());
   fprintf(fp, "zoom=\"%s\" ", (const char *)Internat::ToString(mViewInfo.zoom, 10).mb_str());
   fprintf(fp, "rate=\"%s\" ", (const char *)Internat::ToString(mRate).mb_str());
   fprintf(fp, ">\n");

   mTags->WriteXML(depth+1, fp);

   Track *t;
   TrackListIterator iter(mTracks);
   t = iter.First();
   while (t) {
      t->WriteXML(depth+1, fp);
      t = iter.Next();
   }

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "</project>\n");
}

bool AudacityProject::Save(bool overwrite /* = true */ ,
                           bool fromSaveAs /* = false */ )
{
   if (!fromSaveAs && mDirManager->GetProjectName() == wxT(""))
      return SaveAs();

   //TIDY-ME: CleanSpeechMode could be split into a number of prefs?
   // For example, this could be a preference to only work
   // with wav files.
   //
   // CleanSpeechMode tries hard to ignore project files
   // and just work with .Wav, so does an export on a save.
   if( mCleanSpeechMode )
   {
      double endTime = mTracks->GetEndTime();
      bool flag = ::Export(this, false, 0.0, endTime);
      return flag;
   }

   //
   // Always save a backup of the original project file
   //

   wxString safetyFileName = wxT("");
   if (wxFileExists(FILENAME(mFileName))) {

#ifdef __WXGTK__
      safetyFileName = mFileName + wxT("~");
#else
      safetyFileName = mFileName + wxT(".bak");
#endif

      if (wxFileExists(FILENAME(safetyFileName)))
         wxRemoveFile(FILENAME(safetyFileName));

      wxRename(FILENAME(mFileName), FILENAME(safetyFileName));
   }

   if (fromSaveAs || mDirManager->GetProjectName() == wxT("")) {

      // This block of code is duplicated in WriteXML, for now...
      wxString project = mFileName;
      if (project.Len() > 4 && project.Mid(project.Len() - 4) == wxT(".aup"))
         project = project.Mid(0, project.Len() - 4);
      wxString projName = wxFileNameFromPath(project) + wxT("_data");
      wxString projPath = wxPathOnly(project);
      
      // We are about to move files from the current directory to
      // the new directory.  We need to make sure files that belonged
      // to the last saved project don't get erased, so we "lock" them.
      // (Otherwise the new project would be fine, but the old one would
      // be empty of all of its files.)
      
      // Lock all blocks in all tracks of the last saved version
      if (mLastSavedTracks && !overwrite) {
         TrackListIterator iter(mLastSavedTracks);
         Track *t = iter.First();
         while (t) {
            if (t->GetKind() == Track::Wave)
               ((WaveTrack *) t)->Lock();
            t = iter.Next();
         }
      }
      // This renames the project directory, and moves or copies
      // all of our block files over
      bool success = mDirManager->SetProject(projPath, projName, !overwrite);
      
      // Unlock all blocks in all tracks of the last saved version
      if (mLastSavedTracks && !overwrite) {
         TrackListIterator iter(mLastSavedTracks);
         Track *t = iter.First();
         while (t) {
            if (t->GetKind() == Track::Wave)
               ((WaveTrack *) t)->Unlock();
            t = iter.Next();
         }
      }
      
      if (!success) {
         wxMessageBox(wxString::Format(_("Could not save project. Perhaps %s is not writeable,\nor the disk is full."),
                                       project.c_str()),
                      _("Error saving project"),
                      wxOK | wxCENTRE, this);
         if (safetyFileName)
            wxRename(FILENAME(safetyFileName), FILENAME(mFileName));
         
         return false;
      }
   }

   wxFFile saveFile(FILENAME(mFileName).c_str(), wxT("wb"));
   if (!saveFile.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + mFileName,
                   _("Error saving project"),
                   wxOK | wxCENTRE, this);

      if (safetyFileName)
         wxRename(FILENAME(safetyFileName), FILENAME(mFileName));
      
      return false;
   }

   fprintf(saveFile.fp(), "<?xml ");
   fprintf(saveFile.fp(), "version=\"1.0\" ");
   fprintf(saveFile.fp(), "standalone=\"no\" ");
   fprintf(saveFile.fp(), "?>\n");

   wxString dtdName = wxT("-//audacityproject-1.3.0//DTD//EN");
   wxString dtdURI =
      wxT("http://audacity.sourceforge.net/xml/audacityproject-1.3.0.dtd");

   fprintf(saveFile.fp(), "<!DOCTYPE ");
   fprintf(saveFile.fp(), "project ");
   fprintf(saveFile.fp(), "PUBLIC ");
   fprintf(saveFile.fp(), "\"%s\" ", (const char*)dtdName.mb_str());
   fprintf(saveFile.fp(), "\"%s\" ", (const char*)dtdURI.mb_str());
   fprintf(saveFile.fp(), ">\n");

   WriteXML(0, saveFile.fp());

   saveFile.Close();

#ifdef __WXMAC__
   FSSpec spec;

   wxMacFilename2FSSpec(FILENAME(mFileName), &spec);
   FInfo finfo;
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      finfo.fdType = AUDACITY_PROJECT_TYPE;
      finfo.fdCreator = AUDACITY_CREATOR;
      FSpSetFInfo(&spec, &finfo);
   }
#endif

   if (mLastSavedTracks) {
      mLastSavedTracks->Clear(true);
      delete mLastSavedTracks;
   }

   mLastSavedTracks = new TrackList();

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      mLastSavedTracks->Add(t->Duplicate());
      t = iter.Next();
   }

   mStatusBar->SetStatusText(wxString::Format(_("Saved %s"),
                                              mFileName.c_str()));
   
   mUndoManager.StateSaved();
   return true;
}

bool AudacityProject::ImportProgressCallback(void *_self, float percent)
{
   AudacityProject *self = (AudacityProject*)_self;
   const int progressDialogGranularity = 1000;

   if (self->mImportProgressDialog) {
      bool keepGoing =
         self->mImportProgressDialog->Update((int)(percent *
                                                   progressDialogGranularity));

      if (!keepGoing)
         self->mUserCanceledProgress = true;

      return !keepGoing;
   }
   else if (wxGetElapsedTime(false) > 500) {
      wxString description;

      if (self->mImportingRaw)
         /* i18n-hint: This refers to files that are opened directly
            without looking at the file header.  Same as "Import Raw" */
         description = _("Raw");
      else
         description = self->mImporter->GetFileDescription();
         
      wxString dialogMessage;
      dialogMessage.Printf(_("Importing %s File..."),
                           description.c_str());

      self->mImportProgressDialog = new wxProgressDialog(_("Import"),
                                         dialogMessage,
                                         progressDialogGranularity,
                                         self,
                                         wxPD_CAN_ABORT |
                                         wxPD_REMAINING_TIME |
                                         wxPD_AUTO_HIDE);
      return !self->mImportProgressDialog->Update((int)(percent * progressDialogGranularity));
   }
   else {
      return 0;
   }
}

void AudacityProject::AddImportedTracks(wxString fileName,
                                        Track **newTracks, int numTracks)
{
   SelectNone();

   bool initiallyEmpty = mTracks->IsEmpty();
   double newRate = 0;
   wxString trackNameBase = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');

   for (int i = 0; i < numTracks; i++) {
      if (newRate == 0 && newTracks[i]->GetKind() == Track::Wave) {
         newRate = ((WaveTrack *)newTracks[i])->GetRate();
      }
      mTracks->Add(newTracks[i]);
      newTracks[i]->SetSelected(true);
      if (numTracks > 1)
         newTracks[i]->SetName(trackNameBase + wxString::Format(wxT(" %d" ), i + 1));
      else
         newTracks[i]->SetName(trackNameBase);
   }

   delete[]newTracks;

   // Automatically assign rate of imported file to whole project,
   // if this is the first file that is imported
   if (initiallyEmpty && newRate > 0) {
      // msmeyer: Before changing rate, check if rate is supported
      // by current sound card. If it is not, don't change it,
      // otherwise playback won't work.
      if (AudioIO::GetSupportedSampleRates().Index((int)newRate) != wxNOT_FOUND)
      {
         mRate = newRate;
         mSelectionBar->SetRate(mRate);
      }
   }

   PushState(wxString::Format(_("Imported '%s'"), fileName.c_str()),
             _("Import"));

   OnZoomFit();

   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());
   mTrackPanel->Refresh(false);

   if (initiallyEmpty && mDirManager->GetProjectName() == wxT("")) {
      wxString name = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast(wxT('.'));
      mFileName =::wxPathOnly(fileName) + wxFILE_SEP_PATH + name + wxT(".aup");
      SetProjectTitle();
   }

   HandleResize();   
}

void AudacityProject::Import(wxString fileName)
{
   Track **newTracks;
   int numTracks;
   wxString errorMessage;

   wxStartTimer();

   wxASSERT(!mImportProgressDialog);

   mUserCanceledProgress = false;
   numTracks = mImporter->Import(fileName, mTrackFactory, &newTracks,
                                 errorMessage,
                                 AudacityProject::ImportProgressCallback,
                                 this);

   if(mImportProgressDialog) {
      delete mImportProgressDialog;
      mImportProgressDialog = NULL;
   }

   if (mUserCanceledProgress)
      return;

   if (numTracks <= 0) {
      wxMessageBox(errorMessage,
                   _("Error importing"),
                   wxOK | wxCENTRE, this);
      return;
   }

   // If the project is empty, import ID3 tags from the file.
   // Yes, these are normally only in MP3 files, but they could
   // be in another file, why not?
   if (mTracks->IsEmpty()) {
      mTags->ImportID3(fileName);
   }

   // for LOF ("list of files") files, do not import the file as if it
   // were an audio file itself
   if (fileName.AfterLast('.').IsSameAs(wxT("lof"), false))
   {
      return;
   }

   AddImportedTracks(fileName, newTracks, numTracks);

   int mode = gPrefs->Read(wxT("/Batch/NormalizeOnLoad"), 0L);
   if (mode == 1) {
      //TODO: All we want is a SelectAll()
      SelectNone();
      SelectAllIfNone();
      OnEffect(ALL_EFFECTS | CONFIGURED_EFFECT, mNormalizeIndex ); // gNormalize);
   }
}

bool AudacityProject::SaveAs()
{
   wxString path = wxPathOnly(mFileName);
   wxString fName;
	wxString ext = mCleanSpeechMode ? wxT(".wav") : wxT(".aup");

	fName = GetName().Len()? GetName() + ext : wxString(wxT(""));
	if( mCleanSpeechMode )
	{
	   fName = wxFileSelector(_("Save Speech As:"),
                          path, fName, wxT(""),
                          _("Windows PCM Audio file *.wav)|*.wav"),  //lda
                          wxSAVE | wxOVERWRITE_PROMPT, this);
	}
	else
	{
 	  ShowWarningDialog(this, wxT("FirstProjectSave"),
                     _("Audacity project files (.aup) let you save everything you're working on exactly as it\nappears on the screen, but most other programs can't open Audacity project files.\n\nWhen you want to save a file that can be opened by other programs, select one of the\nExport commands."));
 	  fName = wxFileSelector(_("Save Project As:"),
                          path, fName, wxT(""),
                          _("Audacity projects (*.aup)|*.aup"),
                          wxSAVE | wxOVERWRITE_PROMPT, this);
	}

   if (fName == wxT(""))
      return false;

   size_t len = fName.Len();
   if (len > 4 && fName.Mid(len - 4) == wxT(".aup"))
      fName = fName.Mid(0, len - 4);

   mFileName = fName + ext;
   SetProjectTitle();

   bool success = Save(false, true);

   if (success) {
      mRecentFiles->AddFileToHistory(mFileName);
      gPrefs->SetPath(wxT("/RecentFiles"));
      mRecentFiles->Save(*gPrefs);
      gPrefs->SetPath(wxT(".."));
   }

   return(success);
}

//
// Undo/History methods
//

void AudacityProject::InitialState()
{
   mUndoManager.ClearStates();

   TrackList *l = new TrackList(mTracks);

   mUndoManager.PushState(l, mViewInfo.sel0, mViewInfo.sel1,
                          _("Created new project"), wxT(""));
   delete l;

   mUndoManager.StateSaved();

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenus();

   UpdateMenus();
}

void AudacityProject::PushState(wxString desc,
                                wxString shortDesc,
                                bool consolidate)
{
   TrackList *l = new TrackList(mTracks);

	//ANSWER-ME: What's the zoom-fit calc for?  
   if (GetTracksFitVerticallyZoomed() == true) {
      OnZoomFitV_Calc();
   }
   mUndoManager.PushState(l, mViewInfo.sel0, mViewInfo.sel1,
                          desc, shortDesc, consolidate);
   delete l;

   mDirty = true;

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenus();

   UpdateMenus();
}

void AudacityProject::ModifyState()
{
   TrackList *l = new TrackList(mTracks);

   mUndoManager.ModifyState(l, mViewInfo.sel0, mViewInfo.sel1);

   delete l;
}

void AudacityProject::PopState(TrackList * l)
{
   mTracks->Clear(true);
   TrackListIterator iter(l);
   Track *t = iter.First();
   while (t) {
      //    printf("Popping track with %d samples\n",
      //           ((WaveTrack *)t)->numSamples);
      //  ((WaveTrack *)t)->Debug();
      mTracks->Add(t->Duplicate());
      t = iter.Next();
   }

   HandleResize();

   UpdateMenus();
}

void AudacityProject::SetStateTo(unsigned int n)
{
   TrackList *l =
       mUndoManager.SetStateTo(n, &mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   HandleResize();
   mTrackPanel->SetFocusedTrack(NULL);
   mTrackPanel->Refresh(false);
   ModifyUndoMenus();
}

//
// Clipboard methods
//

//static
void AudacityProject::DeleteClipboard()
{
   if (msClipboard) {
      msClipboard->Clear( true );
      delete msClipboard;
      msClipboard = NULL;
   }
}

void AudacityProject::ClearClipboard()
{
   TrackListIterator iter(msClipboard);
   Track *n = iter.First();
   while (n) {
      delete n;
      n = iter.Next();
   }

   msClipLen = 0.0;
   msClipProject = NULL;
   msClipboard->Clear();
}

void AudacityProject::Clear()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected())
         n->Clear(mViewInfo.sel0, mViewInfo.sel1);
      n = iter.Next();
   }

   double seconds = mViewInfo.sel1 - mViewInfo.sel0;

   mViewInfo.sel1 = mViewInfo.sel0;

   PushState(wxString::Format(_("Deleted %.2f seconds at t=%.2f"),
                              seconds,
                              mViewInfo.sel0),
             _("Delete"));

   RedrawProject();
}

void AudacityProject::SelectNone()
{
   TrackListIterator iter(mTracks);

   Track *t = iter.First();
   while (t) {
      t->SetSelected(false);
      t = iter.Next();
   }
   mTrackPanel->Refresh(false);
}

// Utility function called by other zoom methods
void AudacityProject::Zoom(double level)
{
   if (level > gMaxZoom)
      level = gMaxZoom;
   if (level <= gMinZoom)
      level = gMinZoom;

   mViewInfo.zoom = level;
   FixScrollbars();
}

///////////////////////////////////////////////////////////////////
// This method 'rewinds' the track, by setting the cursor to 0 and
// scrolling the window to fit 0 on the left side of it 
// (maintaining  current zoom).  
// If shift is held down, it will extend the left edge of the 
// selection to 0 (holding right edge constant), otherwise it will
// move both left and right edge of selection to 0 
///////////////////////////////////////////////////////////////////
void AudacityProject::Rewind(bool shift)
{
   mViewInfo.sel0 = 0;
   if (!shift || mViewInfo.sel1 < mViewInfo.sel0)
      mViewInfo.sel1 = 0;

   TP_ScrollWindow(0);
}


///////////////////////////////////////////////////////////////////
// This method 'fast-forwards' the track, by setting the cursor to
// the end of the samples on the selected track and  scrolling the
//  window to fit the end on its right side (maintaining  current zoom).  
// If shift is held down, it will extend the right edge of the 
// selection to the end (holding left edge constant), otherwise it will
// move both left and right edge of selection to the end 
///////////////////////////////////////////////////////////////////
void AudacityProject::SkipEnd(bool shift)
{
   double len = mTracks->GetEndTime();

   mViewInfo.sel1 = len;
   if (!shift || mViewInfo.sel0 > mViewInfo.sel1)
      mViewInfo.sel0 = len;

   //STM: Determine wisely where to position the viewport
   // There are two conditions:
   //
   // (1) If the total width of the sample is larger than the viewport
   //     is wide, sets the viewport so that the end of the sample will 
   //     have about 5% empty space after it
   // (2) If the total width of the sample is less than the viewport is
   //     wide, set the viewport's left edge to be 0.

   //Calculates viewstart: End of sample  - 95% of a screen width
   double viewstart = len - mViewInfo.screen * .95;
   viewstart = viewstart > 0 ? viewstart : 0.0;

   TP_ScrollWindow(viewstart);
}


////////////////////////////////////////////////////////////
//  This fetches a pointer to the control toolbar.  It may
//  either be embedded in the current window or floating out
//  in the open.
////////////////////////////////////////////////////////////
ControlToolBar *AudacityProject::GetControlToolBar()
{
   return mToolBarDock ? mToolBarDock->GetControlToolBar() : NULL;
}

//JKC: same as above *except* this a virtual function that
//can be called from the track panel callback.
//It seems a little crazy doing this but TrackArtist 
//needs to get information about the tool bar state and 
//I don't currently see a cleaner way.
ControlToolBar * AudacityProject::TP_GetControlToolBar()
{
   return GetControlToolBar();
}

ToolsToolBar * AudacityProject::TP_GetToolsToolBar()
{
   return GetToolsToolBar();
}

EditToolBar *AudacityProject::GetEditToolBar()
{
   return mToolBarDock ? mToolBarDock->GetEditToolBar() : NULL;
}

MeterToolBar *AudacityProject::GetMeterToolBar()
{
   return mToolBarDock ? mToolBarDock->GetMeterToolBar() : NULL;
}

MixerToolBar *AudacityProject::GetMixerToolBar()
{
   return mToolBarDock ? mToolBarDock->GetMixerToolBar() : NULL;
}

ToolsToolBar *AudacityProject::GetToolsToolBar()
{
   return mToolBarDock ? mToolBarDock->GetToolsToolBar() : NULL;
}

TranscriptionToolBar *AudacityProject::GetTranscriptionToolBar()
{
   return mToolBarDock ? mToolBarDock->GetTranscriptionToolBar() : NULL;
}

void AudacityProject::SetStop(bool bStopped)
{
   mTrackPanel->SetStop(bStopped);
}

void AudacityProject::OnTimer(wxTimerEvent& event)
{
   MixerToolBar *mixerToolBar = GetMixerToolBar();
   if( mixerToolBar )
      mixerToolBar->UpdateControls();

   if (::wxGetUTCTime() - mLastStatusUpdateTime < 3)
      return;


   // gAudioIO->GetNumCaptureChannels() should only be positive 
   // when we are recording.
   if (gAudioIO->GetNumCaptureChannels() > 0) {
      wxLongLong freeSpace = mDirManager->GetFreeDiskSpace();
      if (freeSpace >= 0) {
         wxString msg;
         double recTime;
         int recMins;

         recTime = freeSpace.GetHi() * 4294967296.0 + freeSpace.GetLo();
         recTime /= SAMPLE_SIZE(gAudioIO->GetCaptureFormat());
         recTime /= gAudioIO->GetNumCaptureChannels();
         recTime /= GetRate();
         recMins = (int)(recTime / 60.0);

         if (recMins >= 120)
            msg.Printf(_("Disk space remains for recording %d hours and %d minutes."),
                       recMins/60, recMins%60);
         else if (recMins >= 60)
            msg.Printf(_("Disk space remains for recording 1 hour and %d minutes."),
                       recMins-60);
         else if (recMins > 3)
            msg.Printf(_("Disk space remains for recording %d minutes."),
                       recMins);
         else if (recTime >= 2)
            msg.Printf(_("Disk space remains for recording %d seconds."),
                       (int)recTime);
         else
            msg.Printf(_("Out of disk space"));

         mStatusBar->SetStatusText(msg);
      }
   }
}

//get regions selected by selected labels
//removes unnecessary regions, overlapping regions are merged
//regions memory need to be deleted by the caller
void AudacityProject::GetRegionsByLabel( Regions &regions )
{
   TrackListIterator iter( mTracks );
   Track *n;
   
   //determine labelled regions
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Label )
      {
         LabelTrack *lt = ( LabelTrack* )n;
         for( int i = 0; i < lt->GetNumLabels(); i++ ) 
         {
            const LabelStruct *ls = lt->GetLabel( i );
            if( ls->t >= mViewInfo.sel0 && ls->t1 <= mViewInfo.sel1 )
            {
               Region *region = new Region;
               region->start = ls->t;
               region->end = ls->t1;
               regions.Add( region );
            }
         }
      }

   //anything to do ?
   if( regions.GetCount() == 0 )
      return;
   
   //sort and remove unnecessary regions
   regions.Sort( Region::cmp );
   unsigned int selected = 1;
   while( selected < regions.GetCount() )
   {
      Region *cur = regions.Item( selected );
      Region *last = regions.Item( selected - 1 );
      if( cur->start <= last->end )
      {
         if( cur->end > last->end )
            last->end = cur->end;
         delete cur;
         regions.RemoveAt( selected );
      }
      else
         selected++;
   }
}

//Executes the edit function on all selected wave tracks with
//regions specified by selected labels
//If No tracks selected, function is applied on all tracks
void AudacityProject::EditByLabel( WaveTrack::EditFunction action )
{ 
   Regions regions;
   
   GetRegionsByLabel( regions );
   if( regions.GetCount() == 0 )
      return;

   TrackListIterator iter( mTracks );
   Track *n;
   bool allTracks = true;

   // if at least one wave track is selected
   // apply only on the selected track
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Wave && n->GetSelected() )
      {
         allTracks = false;
         break;
      }
  
   //Apply action on wavetracks starting from
   //labeled regions in the end. This is to correctly perform
   //actions like 'Delete' which collapse the track area.
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Wave && ( allTracks || n->GetSelected() ) )
      {
         WaveTrack *wt = ( WaveTrack* )n;
         for( int i = ( int )regions.GetCount() - 1; i >= 0; i-- )
            ( wt->*action )( regions.Item( i )->start, regions.Item( i )->end );
      }

   //delete label regions
   for( unsigned int i = 0; i < regions.GetCount(); i++ )
      delete regions.Item( i );
}

//Executes the edit function on all selected wave tracks with
//regions specified by selected labels
//If No tracks selected, function is applied on all tracks
//functions copy the edited regions to clipboard, possibly in multiple tracks
void AudacityProject::EditClipboardByLabel( WaveTrack::EditDestFunction action )
{ 
   Regions regions;
   
   GetRegionsByLabel( regions );
   if( regions.GetCount() == 0 )
      return;

   TrackListIterator iter( mTracks );
   Track *n;
   bool allTracks = true;

   // if at least one wave track is selected
   // apply only on the selected track
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Wave && n->GetSelected() )
      {
         allTracks = false;
         break;
      }
 
   ClearClipboard(); 
   //Apply action on wavetracks starting from
   //labeled regions in the end. This is to correctly perform
   //actions like 'Cut' which collapse the track area.
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Wave && ( allTracks || n->GetSelected() ) )
      {
         WaveTrack *wt = ( WaveTrack* )n;
         WaveTrack *merged = NULL;
         for( int i = ( int )regions.GetCount() - 1; i >= 0; i-- )
         {
            Track *dest = NULL;
            ( wt->*action )( regions.Item( i )->start, regions.Item( i )->end, 
                             &dest );
            if( dest )
            {
               dest->SetChannel( wt->GetChannel() );
               dest->SetLinked( wt->GetLinked() );
               dest->SetName( wt->GetName() );
               ( ( WaveTrack* )dest )->SetRate( wt->GetRate() );
               if( !merged )
                  merged = ( WaveTrack* )dest;
               else
               {
                  //since we are doing this from the end, next region
                  //has to go in the beginning
                  merged->Paste( merged->GetStartTime(), dest );
                  delete dest;
               }
               msClipLen += ( regions.Item( i )->end - regions.Item( i )->start );
            }
         }
         if( merged )
            msClipboard->Add( merged );
      }

   //delete label regions
   for( unsigned int i = 0; i < regions.GetCount(); i++ )
      delete regions.Item( i );
}

// TrackPanel callback method
void AudacityProject::TP_DisplayStatusMessage(wxString msg)
{
   mStatusBar->SetStatusText(msg);
   mLastStatusUpdateTime = ::wxGetUTCTime();
}

void AudacityProject::TP_DisplaySelection()
{
   double audioTime;

   if (gAudioIO->IsBusy())
      audioTime = gAudioIO->GetStreamTime();
   else {
      audioTime = 0;
      if (!mLockPlayRegion)
         mRuler->SetPlayRegion(mViewInfo.sel0, mViewInfo.sel1);
   }

   mSelectionBar->SetTimes(mViewInfo.sel0, mViewInfo.sel1, audioTime);
}

// TrackPanel callback method
int AudacityProject::TP_GetCurrentTool()
{
   //ControlToolBar might be NULL--especially on shutdown.
   //Make sure it isn't and if it is, return a reasonable value
   ToolsToolBar *ctb = GetToolsToolBar();
   if (ctb)
      return GetToolsToolBar()->GetCurrentTool();
   else
      return 0;
}




// TrackPanel callback method
void AudacityProject::TP_OnPlayKey()
{
   OnPlayStop();
}

// TrackPanel callback method
void AudacityProject::TP_PushState(wxString desc, wxString shortDesc,
                                   bool consolidate)
{
   PushState(desc, shortDesc, consolidate);
}

// TrackPanel callback method
void AudacityProject::TP_ModifyState()
{
   ModifyState();
}

// TrackPanel callback method
void AudacityProject::TP_ScrollLeft()
{
   OnScrollLeft();
}

// TrackPanel callback method
void AudacityProject::TP_ScrollRight()
{
   OnScrollRight();
}

// TrackPanel callback method
void AudacityProject::TP_RedrawScrollbars()
{
   FixScrollbars();
}

void AudacityProject::TP_HandleResize()
{
   HandleResize();
}

void AudacityProject::GetPlayRegion(double* playRegionStart,
                                    double *playRegionEnd)
{
   mRuler->GetPlayRegion(playRegionStart, playRegionEnd);
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: da1685fe-8c99-4407-a71b-cf52576ff11a


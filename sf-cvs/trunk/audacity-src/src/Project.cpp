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

#include "Audacity.h"

#include <stdio.h>

#include <wx/wxprec.h>

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>
#include <wx/string.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/timer.h>

#ifdef __MACOSX__
#include <CoreServices/CoreServices.h>
#endif

#ifdef __MACOS9__
#include <Files.h>
#endif

#ifdef __WXMAC__
#define __MOVIES__        /* Apple's Movies.h not compatible with Audacity */
/*#define __MACHELP__*/

#include <wx/mac/private.h>
#else
#include <wx/dragimag.h>
#include <wx/generic/dragimgg.h>
#endif

#include <wx/event.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/scrolbar.h>
#include <wx/textfile.h>
#include <wx/menu.h>
#include <wx/progdlg.h>

#include "Project.h"

#include "AudacityApp.h"
#include "AColor.h"
#include "AStatus.h"
#include "AudioIO.h"
#include "ControlToolBar.h"
#include "EditToolBar.h"
#include "FormatSelection.h"
#include "FreqWindow.h"
#include "HistoryWindow.h"
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
#include "effects/Effect.h"
#include "xml/XMLFileReader.h"

#include <wx/arrimpl.cpp>       // this allows for creation of wxObjArray


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
#endif
#ifdef __WXMSW__
const int sbarSpaceWidth = 16;
const int sbarControlWidth = 16;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs
#endif
#ifdef __WXGTK__
const int sbarSpaceWidth = 15;
const int sbarControlWidth = 15;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs
#endif

#if defined(__WXGTK__) || defined(__WXMOTIF__)
#include "../images/AudacityLogo.xpm"
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

AudacityProject *CreateNewAudacityProject(wxWindow * parentWindow)
{
   bool bMaximized;
   wxRect wndRect;
   GetNextWindowPlacement(&wndRect, &bMaximized);

   //Create and show a new project
   AudacityProject *p = new AudacityProject(parentWindow, -1,
                                            wxPoint(wndRect.x, wndRect.y), wxSize(wndRect.width, wndRect.height));

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
      nextRect->SetWidth(gPrefs->Read("/Window/Width", defWndRect.GetWidth()));
      nextRect->SetHeight(gPrefs->Read("/Window/Height", defWndRect.GetHeight()));

      gPrefs->Read("/Window/Maximized", bMaximized);
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
    EVT_MOUSE_EVENTS(AudacityProject::OnMouseEvent)
    EVT_PAINT(AudacityProject::OnPaint)
    EVT_CLOSE(AudacityProject::OnCloseWindow)
    EVT_SIZE(AudacityProject::OnSize)
    EVT_ACTIVATE(AudacityProject::OnActivate)
    EVT_COMMAND_SCROLL_LINEUP(HSBarID, AudacityProject::OnScrollLeftButton)
    EVT_COMMAND_SCROLL_LINEDOWN(HSBarID, AudacityProject::OnScrollRightButton)
    EVT_COMMAND_SCROLL(HSBarID, AudacityProject::OnScroll)
    EVT_COMMAND_SCROLL(VSBarID, AudacityProject::OnScroll)
    EVT_DROP_FILES(AudacityProject::OnDropFiles)
    EVT_TIMER(AudacityProjectTimerID, AudacityProject::OnTimer)
    // Update menu method
    EVT_UPDATE_UI(1, AudacityProject::OnUpdateMenus)
    EVT_ICONIZE(  AudacityProject::OnIconize)

END_EVENT_TABLE()

AudacityProject::AudacityProject(wxWindow * parent, wxWindowID id,
                                 const wxPoint & pos,
                                 const wxSize & size)
   : wxFrame(parent, id, "Audacity", pos, size),
     mLastPlayMode(normalPlay),
     mImportProgressDialog(NULL),
     mRate((double) gPrefs->Read("/SamplingRate/DefaultProjectSampleRate", AudioIO::GetOptimalSupportedSampleRate())),
     mDefaultFormat((sampleFormat) gPrefs->
           Read("/SamplingRate/DefaultProjectSampleFormat", floatSample)),
     mSelectionFormat(SELECTION_FORMAT_RULER_MIN_SEC),
     mSnapTo(0),
     mDirty(false),
     mFirstTimeUpdateMenus(true),
     mTrackPanel(NULL),
     mTrackFactory(NULL),
     mImporter(NULL),
     mAutoScrolling(false),
     mActive(true),
     mHistoryWindow(NULL),
     mTotalToolBarHeight(0),
     mDraggingToolBar(NoneID)
{
   #ifndef __WXMAC__
   mDrag = NULL;
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

   // Some GUI prefs
   gPrefs->Read("/GUI/UpdateSpectrogram", &mViewInfo.bUpdateSpectrogram,
                true);
   gPrefs->Read("/GUI/AutoScroll", &mViewInfo.bUpdateTrackIndicator, true);

   // Some extra information
   mViewInfo.bIsPlaying = false;
   mViewInfo.bRedrawWaveform = false;

   CreateMenusAndCommands();

   int left = 0, top = 0, width, height;
   GetClientSize(&width, &height);

   // Create the Control Toolbar (if we're not using a windowed toolbar)
   // The control toolbar should be automatically loaded--other toolbars
   // are optional.

   if (!gControlToolBarStub->GetWindowedStatus()) {
      int h = gControlToolBarStub->GetHeight();

      ToolBar *tb = new ControlToolBar(this, 0, wxPoint(10, top),
                                       wxSize(width - 10 - sbarSpaceWidth, h));
      mToolBarArray.Add((ToolBar *) tb);

//      top += h + 1;
//      height -= h + 1;
//      mTotalToolBarHeight += h;
   }

#if USE_PORTMIXER
   if (gMixerToolBarStub) {
      if (gMixerToolBarStub->GetLoadedStatus()
          && !gMixerToolBarStub->GetWindowedStatus()) {
         int h = gMixerToolBarStub->GetHeight();
         ToolBar *etb = new MixerToolBar(this, 0, wxPoint(10, top),
                                         wxSize(width - 10 - sbarSpaceWidth, h));
         mToolBarArray.Add((ToolBar *) etb);

//         top += h + 1;
//         height -= h + 1;
//         mTotalToolBarHeight += h;
      }
   }
#endif

   if (gEditToolBarStub) {
      if (gEditToolBarStub->GetLoadedStatus()
          && !gEditToolBarStub->GetWindowedStatus()) {
         int h = gEditToolBarStub->GetHeight();
         ToolBar *etb = new EditToolBar(this, 0, wxPoint(10, top),
                                        wxSize(width - 10 - sbarSpaceWidth, h));
         mToolBarArray.Add((ToolBar *) etb);

//         top += h + 1;
//         height -= h + 1;
//         mTotalToolBarHeight += h;
      }
   }

   LayoutToolBars();
   height -= mTotalToolBarHeight;
   top     = mTotalToolBarHeight;

   // Fix the sliders on the mixer toolbar so that the tip windows
   // actually pop-up on top of everything else.  Sorry for the hack -
   // it's necessary to do it this way to avoid flicker.

   MixerToolBar *mtb = GetMixerToolBar();
   if (mtb)
      mtb->RecreateTipWindows();

   //
   // Create the status bar
   //

   int sh = GetStatusHeight();

   mStatus = new AStatus(this, 0,
                         wxPoint(0, height - sh),
                         wxSize(width, sh), mRate, this);
   height -= sh;

   mStatus->SetField(wxString::Format("Welcome to Audacity version %s",
                                      AUDACITY_VERSION_STRING), 0);

   mLastStatusUpdateTime = ::wxGetUTCTime();
   mTimer = new wxTimer(this, AudacityProjectTimerID);
   mTimer->Start(200);

   //
   // Create the TrackPanel and the scrollbars
   //

   mTrackPanel = new TrackPanel(this, TrackPanelID,
                                wxPoint(left, top),
                                wxSize(width - sbarSpaceWidth,
                                       height - sbarSpaceWidth), mTracks,
                                &mViewInfo, this);

   int hoffset = mTrackPanel->GetLeftOffset() - 1;
   int voffset = mTrackPanel->GetRulerHeight();

#if defined __WXMAC__ 
   width++;
   height++;
#endif

   mHsbar =
       new wxScrollBar(this, HSBarID,
                       wxPoint(hoffset, top + height - sbarSpaceWidth),
                       wxSize(width - hoffset - sbarSpaceWidth +
                              sbarExtraLen, sbarControlWidth),
                       wxSB_HORIZONTAL);

   mVsbar =
       new wxScrollBar(this, VSBarID,
                       wxPoint(width - sbarSpaceWidth, top + voffset),
                       wxSize(sbarControlWidth,
                              height - sbarSpaceWidth - voffset +
                              sbarExtraLen), wxSB_VERTICAL);

   mLastUpdateUITime = ::wxGetUTCTime();

   InitialState();
   FixScrollbars();

   //
   // Set the Icon
   //

   // loads either the XPM or the windows resource, depending on the platform
#ifndef __WXMAC__
   wxIcon ic(wxICON(AudacityLogo));
   SetIcon(ic);
#endif
   mIconized = false;

   // Min size, max size
   SetSizeHints(250, 200, 20000, 20000);

   // Create tags object
   mTags = new Tags();

   mTrackFactory = new TrackFactory(mDirManager);
   mImporter = new Importer;
   mImportingRaw = false;

   mAudioIOToken = -1;

#ifdef __WXMSW__
   // Accept drag 'n' drop files
   DragAcceptFiles(true);
#endif

   gAudacityProjects.Add(this);
}

AudacityProject::~AudacityProject()
{
   delete mTimer;
   mTimer=NULL;

// JKC: For Win98 and Linux do not detach the menu bar.
// We want wxWindows to clean it up for us.
// TODO: Is there a Mac issue here??
// SetMenuBar(NULL);

   if (gAudioIO->IsStreamActive(mAudioIOToken)) {
      gAudioIO->StopStream();

      while(gAudioIO->IsBusy()) {
         wxUsleep(100);
      }
   }

   mTrackPanel->Destroy();
   WX_CLEAR_ARRAY(mToolBarArray);

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

   // MM: Tell the DirManager it can now delete itself
   // if it finds it is no longer needed. If it is still
   // used (f.e. by the clipboard), it will recognize this
   // and will destroy itself later.
   mDirManager->Deref();

   gAudacityProjects.Remove(this);

   #ifdef __WXMAC__
   if (gAudacityProjects.IsEmpty())
      wxGetApp().SetTopWindow(gParentFrame);
   #else
   if (gAudacityProjects.IsEmpty())
      QuitAudacity();
   #endif

   if (gActiveProject == this) {
      // Find a new active project
      if (gAudacityProjects.Count() > 0) {
         gActiveProject = gAudacityProjects[0];
         wxGetApp().SetTopWindow(gParentFrame);
      }
      else
         gActiveProject = NULL;
   }
}

void AudacityProject::UpdatePrefs()
{
   mTrackPanel->UpdatePrefs();
   ControlToolBar *controltoolbar = (ControlToolBar *)mToolBarArray[0];
   controltoolbar->UpdatePrefs();
   mStatus->UpdateRates();
}

void AudacityProject::RedrawProject()
{
   FixScrollbars();
   mTrackPanel->Refresh(false);
}

DirManager *AudacityProject::GetDirManager()
{
   return mDirManager;
}

TrackFactory *AudacityProject::GetTrackFactory()
{
   return mTrackFactory;
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
   if (len > 4 && name.Mid(len - 4) == ".aup")
      name = name.Mid(0, len - 4);

   return name;
}

void AudacityProject::AS_SetRate(double rate)
{
   mRate = rate;
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
   mVsbar->SetScrollbar(mViewInfo.vpos / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep,
                        totalHeight / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep, TRUE);

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

   int h;
   int left = 0, top = 0;
   int width, height;
   GetClientSize(&width, &height);
     
	int ptop = 0;

   LayoutToolBars();
   h = mTotalToolBarHeight;

   top += h + ptop;
   height -= h + ptop;
   int sh = GetStatusHeight();

   mStatus->SetSize(0, top + height - sh, width, sh);
   height -= sh;

   mTrackPanel->SetSize(left, top,
                        width - sbarSpaceWidth,
                        height - sbarSpaceWidth);

   int hoffset = mTrackPanel->GetLeftOffset() - 1;
   int voffset = mTrackPanel->GetRulerHeight();

   mHsbar->SetSize(hoffset, top + height - sbarSpaceWidth,
                   width - hoffset - sbarSpaceWidth + sbarExtraLen,
                   sbarControlWidth);
   mVsbar->SetSize(width - sbarSpaceWidth, top + voffset - sbarExtraLen,
                   sbarControlWidth,
                   height - sbarSpaceWidth - voffset +
                   2 * sbarExtraLen);
   FixScrollbars();
}


void AudacityProject::OnIconize(wxIconizeEvent &event)
{
   int VisibleProjectCount = 0;

   //JKC: On Iconizing we get called twice.  Don't know
   // why but it does no harm.
   // Should we be returning true/false rather than 
   // void return?  I don't know.
   mIconized = event.Iconized();

   for(unsigned int i=0;i<gAudacityProjects.Count();i++){
      if(gAudacityProjects[i]){
         if( !gAudacityProjects[i]->mIconized )
            VisibleProjectCount++;
      }
   }

   //Only do anything to the tool windows if we've just iconized and there
   //are no more projects visible OR
   //We've just un-iconized and there is only one project visible.
   bool bToolBarIconizationChange = VisibleProjectCount == (mIconized ? 0 : 1);

   if( bToolBarIconizationChange )
   {
      if (gControlToolBarStub) {
         gControlToolBarStub->Iconize( mIconized );
      }
      if (gMixerToolBarStub) {
         gMixerToolBarStub->Iconize( mIconized );
      }
      if (gEditToolBarStub) {
         gEditToolBarStub->Iconize( mIconized );
      }
   }

   event.Skip();
}

void AudacityProject::OnSize(wxSizeEvent & event)
{
   HandleResize();
   event.Skip();
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
#ifdef __WXMAC__
      mTrackPanel->MacUpdateImmediately();
#endif
   }
}

bool AudacityProject::HandleKeyDown(wxKeyEvent & event)
{
   // Allow the Play button to change to a Loop button
   if (event.GetKeyCode() == WXK_SHIFT) {
      ControlToolBar *tb = GetControlToolBar();
      if (tb)
         tb->OnShiftDown(event);
   }

   // If there is a selected label track and the event did not
   // involve the control key, the label track gets dibs.
   // By returning "false", that says that we want someone else
   // to have a crack at it.

   if (!event.ControlDown()) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      while (t) {
         if (t->GetKind() == Track::Label && t->GetSelected())
            return false;
         t = iter.Next();
      }   
   }
   
   return mCommandManager.HandleKey(event);
}

bool AudacityProject::HandleKeyUp(wxKeyEvent & event)
{
   // Allow the Play button to change to a Loop button
   if (event.GetKeyCode() == WXK_SHIFT) {
      ControlToolBar *tb = GetControlToolBar();
      if (tb)
         tb->OnShiftUp(event);
   }

   return false;
}

bool AudacityProject::ProcessEvent(wxEvent & event)
{
   if (event.GetEventType() == wxEVT_COMMAND_MENU_SELECTED) {
      if (mCommandManager.HandleMenuID(event.GetId()))
         return true;
   }

   return wxFrame::ProcessEvent(event);
}

//TODO: This function is still kinda hackish, clean up
void AudacityProject::OnUpdateMenus(wxUpdateUIEvent & event)
{
   if (::wxGetUTCTime() - mLastUpdateUITime < 3)
      return;

   mLastUpdateUITime = ::wxGetUTCTime();

   UpdateMenus();
}

/// DecorateToolBar draws the grabber handle to the left of the toolbar.
/// It also draws a line under each toolbar.
/// @param dc - the device context to draw to
/// @param iToolBar - index in toolbar array of toolbar to decorate.
void AudacityProject::DecorateToolBar( wxPaintDC & dc, int iToolBar )
{
   AColor::Medium(&dc, false);
   unsigned int j;

   int toolbarwidth;
   int toolbarheight;
   int toolbarleft;
   int toolbartop;
   int toolbarbottom;

   mToolBarArray[iToolBar]->GetSize(&toolbarwidth, &toolbarheight);
   mToolBarArray[iToolBar]->GetPosition(&toolbarleft, &toolbartop );
   toolbarbottom = toolbartop + toolbarheight;

   //Draw a rectangle around the "grab-bar"
   wxRect r;
   r.x = toolbarleft-grabberWidth;
   r.y = toolbartop;
   r.width = grabberWidth;
   r.height = toolbarheight;
   dc.DrawRectangle(r);

   // Draw little bumps to the left of the toolbar to
   // make it a "grab-bar".

   //adjust min and max so that they aren't too close to the edges
   unsigned int minbump = (toolbarheight % 2 == 0) ? 3 : 4;
   unsigned int maxbump =
       (toolbarheight % 2 == 0) ? toolbarheight - 3 : toolbarheight - 4;

   AColor::Light(&dc, false);
   for (j = minbump; j < maxbump; j += 4)
      dc.DrawLine(r.x+3, toolbartop + j, r.x+6, toolbartop + j);

   AColor::Dark(&dc, false);
   for (j = minbump + 1; j < maxbump + 1; j += 4)
      dc.DrawLine(r.x+3, toolbartop + j, r.x+6, toolbartop + j);

   //Draw a black line to the right of the grab-bar
   dc.SetPen(*wxBLACK_PEN);
   dc.DrawLine(r.x+9, toolbartop, r.x+9, toolbarbottom);

   //Draw some more lines for Windows (tm), along the top and left side 
   //of the grab-bar
#ifdef __WXMSW__
   dc.DrawLine(r.x, toolbartop, r.x+grabberWidth, toolbartop);
   dc.DrawLine(r.x, toolbartop, r.x, toolbarbottom);
#endif
   dc.DrawLine(r.x, toolbarbottom, r.x+toolbarwidth+grabberWidth, toolbarbottom);
}

void AudacityProject::OnPaint(wxPaintEvent & /*event*/)
{
   wxPaintDC dc(this);

   int top = 0;
   int h = 0;
   unsigned int i;

   //wxRect r;

   int width, height;
   GetClientSize(&width, &height);

   //Deal with the ToolBars 
   for (i = 0; i < mToolBarArray.GetCount(); i++) {
      DecorateToolBar( dc, i );
   }

   h=mTotalToolBarHeight; 
   //Now, h is equal to the total height of all the toolbars
   top += h;
   height -= h;

   int sh = GetStatusHeight();
   height -= sh;

   // Fill in space on sides of scrollbars

   dc.SetPen(*wxBLACK_PEN);
   dc.DrawLine(width - sbarSpaceWidth, top,
               width - sbarSpaceWidth, top + height - sbarSpaceWidth + 1);
   dc.DrawLine(0, top + height - sbarSpaceWidth,
               width - sbarSpaceWidth, top + height - sbarSpaceWidth);

   wxRect f;
   f.x = 0;
   f.y = top + height - sbarSpaceWidth + 1;
   f.width = mTrackPanel->GetLeftOffset() - 2;
   f.height = sbarSpaceWidth - 2;
   AColor::Medium(&dc, false);
   dc.DrawRectangle(f);
   AColor::Bevel(dc, true, f);

   //This makes the TrackPanel refresh properly, so that
   //it doesn't leave a little trail of indicator cursors
   #ifndef __WXMAC__
   mTrackPanel->Refresh(false);
   #endif
}

void AudacityProject::OnActivate(wxActivateEvent & event)
{
   SetActiveProject(this);
   mActive = event.GetActive();
   event.Skip();
}

bool AudacityProject::IsActive()
{
   return mActive;
}



#if defined __WXMSW__
	const int extraSpace = 1;
#else
	const int extraSpace = 0;
#endif

/// FlowLayout places toolbars and returns the number of the first unplaced toolbar.
/// This function calls itself recursively to fill-in space if possible.
///  @param i - the index of the first toolbar to place.
///  @param x - top left x of region to place toolbars in.
///  @param y - top left y of region to place toolbars in.
///  @param width  - width  of region to palce toolbars in.
///  @param height - height of region to palce toolbars in.
int AudacityProject::FlowLayout( int i, int x, int y, int width, int height )
{
   int lastToolBarInRow;

//   wxLogDebug("FlowLayout( i=%i, x=%i, y=%i, width=%i, height=%i",i,x,y,width,height );
   while( true) {

      wxSize s;
      bool bFinishedSection;
      // IF no more toolbars, THEN finished.
      if( i >= (int)mToolBarArray.GetCount() )
         bFinishedSection=true;
      // ELSE IF not enough space, THEN finished.
      else {
         s = mToolBarArray[i]->GetIdealSize();
         bFinishedSection = ( (s.GetWidth()+ grabberWidth) > width) || ( s.GetHeight() > height);
      }

      //IF finished, THEN (may adjust last toolbar size) and return.
      if( bFinishedSection ) {
         // ---- Start-height-adjustment
         // JKC: The next bit of logic adjust toolbar height and is purest HACKery.
         // The problem is we don't want the darker gray background to show,
         // when toolbars having different heights.
         // The fully 'correct' solution would be to create objects for the
         // space-fillers which themselves get drawn.
         // What we instead do is to increase the height of toolbars.
         // We spot a possible need for this when we reach the end of a section
         // and there is unused height in that section.
         // One crazy 'feature' of the code here is that the same toolbar
         // may get its height adjusted more than once.
         if((x>0) && ( i>0 )){
            int barX, barY;
            int barWidth, barHeight;
            mToolBarArray[i-1]->GetPosition( &barX, &barY );
            mToolBarArray[i-1]->GetSize( &barWidth, &barHeight );
            mToolBarArray[i-1]->SetSize( barWidth,  barHeight + height +extraSpace);
//            wxLogDebug("a: At %i,%i for %i Toolbar %i has adjusted height %i", x,y,barY, i-1, barHeight + height+extraSpace );
            // This adjusts the height of the preceding toolbar as well, provided it has the same y position.
            if( i>1 ){
               int bar2X, bar2Y;
               mToolBarArray[i-2]->GetPosition( &bar2X, &bar2Y );
               if( bar2Y == barY ){
                  int dummy;
                  mToolBarArray[i-2]->GetSize( &barWidth, &dummy );
                  mToolBarArray[i-2]->SetSize( barWidth,  barHeight + height+extraSpace);
//                  wxLogDebug("b: At %i,%i for %i Toolbar %i has adjusted height %i", x,y,bar2Y, i-2, barHeight + height + extraSpace );
               }
            }
         }
         // ---- End-height-adjustment
         return i; //return the index of the first unplaced toolbar.
      }

      // Thank goodness, there is room for the toolbar!
      // So, place this tool bar.
      mToolBarArray[i]->SetSize(x+grabberWidth,y,s.GetWidth(),s.GetHeight());
      // Move on to next toolbar and attempt to place it in the space to the right 
      // of the one just placed.
      lastToolBarInRow = i;
      i++;
      // Here comes a recursive call, doing layout in a smaller region to the right.
      i = FlowLayout( i, x+grabberWidth+s.GetWidth(), y, 
            width - (grabberWidth+s.GetWidth()), s.GetHeight());
      // Adjust the width of the last toolbar in each row to take up the remaining area.
      if( i==(lastToolBarInRow+1)){
         mToolBarArray[lastToolBarInRow]->SetSize(x+grabberWidth,y,width-grabberWidth, s.GetHeight());
      }

      y+=s.GetHeight()+extraSpace;
      height-=s.GetHeight()+extraSpace;
      // mTotalToolBarHeight will end up with the next free y position.
      mTotalToolBarHeight = y;
   }
   return i;
}

/// BoxLayout for the toolbars is simpler code than FlowLayout.
/// This is the 'classic' style where the toolbars are one above the other.
void AudacityProject::BoxLayout( int width )
{
   int i=0;
   int x = 0;
   int y = extraSpace;
   while(i < (int)mToolBarArray.GetCount() ){
      wxSize s = mToolBarArray[i]->GetIdealSize();
      // Place this tool bar.
      mToolBarArray[i]->SetSize(x+grabberWidth, y, width-grabberWidth,s.GetHeight());
      i++;
      y+=s.GetHeight()+extraSpace;
   }
   mTotalToolBarHeight = y;
}

/// LayoutToolBars decides where to put the toolbars.
void AudacityProject::LayoutToolBars()
{
   //Get the size of the current project window
   int width, height;
   GetSize(&width, &height);
   mTotalToolBarHeight = extraSpace;

// wxLogDebug("Toolbar Layout..." );
   // Start from coordinate (0,extraspace) to avoid drawing over the 
   // extra line under the menu bar in windoze.
   int nPlaced=FlowLayout( 0, 0, extraSpace, width, height );

   // FlowLayout will fail if any of the toolbars is too wide 
   // for the window.
   // IF we couldn't place all toolbars using flow layout, 
   // THEN use BoxLayout.
   if( nPlaced < (int)mToolBarArray.GetCount())
      BoxLayout( width );
}

///LoadToolBar creates a toolbar of type t in the ToolBars array
void AudacityProject::LoadToolBar(enum ToolBarType t)
{
   //First, go through ToolBarArray and determine the current 
   //combined height of all toolbars.
   int tbheight = 0;
   size_t len = mToolBarArray.GetCount();
   for (size_t i = 0; i < len; i++)
      tbheight += mToolBarArray[i]->GetHeight();

   //Get the size of the current project window
   int width, height;
   GetSize(&width, &height);

   //Create a toolbar of the proper type
   ToolBar *toolbar;
   int h;
   switch (t) {
   case ControlToolBarID:
      h = gControlToolBarStub->GetHeight();

      toolbar =
          new ControlToolBar(this, -1, wxPoint(10, tbheight),
                             wxSize(width - 10, h));
      #ifndef __WXMAC__
      mCommandManager.Modify("FloatControlTB", _("Float Control Toolbar"));
      #endif
      mToolBarArray.Insert(toolbar, 0);
      break;

   case EditToolBarID:

      if (!gEditToolBarStub) {
         gEditToolBarStub = new ToolBarStub(gParentWindow, EditToolBarID);
      }

      h = gEditToolBarStub->GetHeight();
      toolbar =
          new EditToolBar(this, -1, wxPoint(10, tbheight),
                          wxSize(width - 10, h));


      mToolBarArray.Add(toolbar);
      break;

   case MixerToolBarID:

     if (!gMixerToolBarStub) {
       gMixerToolBarStub = new ToolBarStub(gParentWindow, MixerToolBarID);
     }
     
     h = gMixerToolBarStub->GetHeight();
     toolbar =
       new MixerToolBar(this, -1, wxPoint(10, tbheight),
			wxSize(width - 10, h));
     
     
     mToolBarArray.Add(toolbar);
     break;
      
     
   case NoneID:
   default:
     toolbar = NULL;
     break;
   }
   
   //Add the new toolbar to the ToolBarArray and redraw screen
   mTotalToolBarHeight += toolbar->GetHeight() +1;
   HandleResize();

   Refresh();
}

void AudacityProject::UnloadToolBar(enum ToolBarType t)
{
   //Go through all of the toolbars (from the bottom up)
   //And delete it if it is type T

   size_t i = mToolBarArray.GetCount();
   while (i > 0) {
      i--;   //decrement i right away, because toolbararray is 0-based.
      
      //Remove a toolbar if it is of the correct type.
      if (mToolBarArray[i]->GetType() == t) {
         
         mTotalToolBarHeight -= mToolBarArray[i]->GetHeight();
         delete mToolBarArray[i];
         mToolBarArray.RemoveAt(i);
         
         //Now, do any changes specific to different toolbar types
         switch (t) {
         case ControlToolBarID:
            
#ifndef __WXMAC__
            //If the ControlToolBar is being unloaded from this project, you
            //should change the menu entry of this project
            mCommandManager.Modify("FloatControlTB", _("Dock Control Toolbar"));
#endif
            break;
            
         case EditToolBarID:
            break;
            
         case MixerToolBarID:
            break;


         case NoneID:
         default:
            break;
         }
      }
   }
   HandleResize();
   Refresh();
}

bool AudacityProject::IsToolBarLoaded(enum ToolBarType t)
{
   size_t len = mToolBarArray.GetCount();
   for (size_t i = 0; i < len; i++) {

      if (mToolBarArray[i]->GetType() == t) {
         return true;
      }
   }
   return false;
}


/// GetGrabberFromEvent converts an x,y coordinate into
/// a toolbar/toolbar-grabber number or -1 if hit-test fails.
int AudacityProject::GetGrabberFromEvent(wxMouseEvent & event)
{
   if( event.m_y > mTotalToolBarHeight )
      return -1;

   for(int i=0;i<(int)mToolBarArray.GetCount();i++)
   {
      wxRect r = mToolBarArray[i]->GetRect();
      if(( r.y < event.m_y )  && ( event.m_y < (r.y+r.height) ) &&
         ( (r.x- grabberWidth) < event.m_x) && (event.m_x < r.x ))
         return i;
   }
   return -1;
}


void AudacityProject::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown())
      SetActiveProject(this);

   //Initial hotspot is relative to TrackPanel window (right below the menu
   wxPoint hotspot;
   hotspot.x = event.m_x;
   hotspot.y = event.m_y;

#ifndef __WXMAC__

   //mouse is relative to the screen
   wxPoint mouse = ClientToScreen(hotspot);

   int i;
   //See if we need to drag a toolbar off the window
   if (event.ButtonDown() && !mDrag && ((i=GetGrabberFromEvent( event )) >=0 ))
      {
      //You want the hotspot to be relative to the toolbar being clicked.
      int x,y;
      mToolBarArray[i]->GetPosition( &x, &y );
     
      int width, height;
      wxSize s = mToolBarArray[i]->GetIdealSize();

      mToolBarArray[i]->GetSize(&width, &height);

      //To enhance performance, these toolbar bitmaps could be pre-loaded
      //Right now, they are not.

      //Only get as much of the toolbar image as the ideal size is
      width = (width > s.x) ? s.x : width;
      height = (height > s.y ) ? s.y : height;

      wxClientDC dc(this);
      //Make the new bitmap a bit bigger
      wxBitmap *bitmap = new wxBitmap((width + 2), (height + 2));

      wxMemoryDC *memDC = new wxMemoryDC();
      memDC->SelectObject(*bitmap);

      //Draw a black box on perimeter
      memDC->SetPen(*wxBLACK_PEN);
      memDC->DrawRectangle(0, 0, width + 2, height + 2);

      //copy an image of the toolbar into the box
      memDC->Blit(1, 1, width, height, &dc, x, y - 1);
      delete memDC;

      mDrag = new wxGenericDragImage(*bitmap);
      delete bitmap;

      hotspot = hotspot - wxPoint(x,y);

	   mDrag->BeginDrag(hotspot, this, true);
      mDrag->Move(hotspot);
      mDrag->Show();
      mToolBarHotspot = hotspot;

      mDraggingToolBar = mToolBarArray[i]->GetType();
   }

   else if (event.Dragging() && mDrag) {

     mDrag->Move(hotspot - wxPoint(1, 1) );//+ mToolBarHotspot);

   } else if (event.ButtonUp() && mDrag) {


      mDrag->Hide();
      mDrag->EndDrag();
      delete mDrag;
      mDrag = NULL;

      mouse -= mToolBarHotspot;

      switch (mDraggingToolBar) {
      case ControlToolBarID:
         gControlToolBarStub->ShowWindowedToolBar(&mouse);
         gControlToolBarStub->UnloadAll();
         break;
      case EditToolBarID:
         gEditToolBarStub->ShowWindowedToolBar(&mouse);
         gEditToolBarStub->UnloadAll();
         break;
      case MixerToolBarID:
         gMixerToolBarStub->ShowWindowedToolBar(&mouse);
         gMixerToolBarStub->UnloadAll();
         break;

      case NoneID:
      default:
         break;
      }

      mDraggingToolBar = NoneID;
      HandleResize();
   }
#endif
}

void AudacityProject::OnCloseWindow(wxCloseEvent & event)
{
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

   //BG: Process messages before we destroy the window
   wxSafeYield();
   Destroy();
}

void AudacityProject::OnDropFiles(wxDropFilesEvent & event)
{
   int numFiles = event.GetNumberOfFiles();

   if (numFiles > 0) {
      wxString *files = event.GetFiles();

      int i;
      for (i = 0; i < numFiles; i++)
         Import(files[i]);
   }
}

// static method, can be called outside of a project
void AudacityProject::ShowOpenDialog(AudacityProject *proj)
{
   wxString path = gPrefs->Read("/DefaultOpenPath",::wxGetCwd());

   wxString fileName = wxFileSelector(_("Select an audio file..."),
                                      path,     // Path
                                      "",       // Name
                                      "",       // Extension
                                      _("All files (*.*)|*.*|"
                                        "Audacity projects (*.aup)|*.aup|"
                                        "WAV files (*.wav)|*.wav|"
                                        "AIFF files (*.aif)|*.aif|"
                                        "AU files (*.au)|*.au|"
                                        "MP3 files (*.mp3)|*.mp3|"
                                        "Ogg Vorbis files (*.ogg)|*.ogg|"
                                        "List of Files (*.lof)|*.lof"),
                                      0,        // Flags
                                      proj);    // Parent

   if (fileName != "") {
      gPrefs->Write("/DefaultOpenPath", wxPathOnly(fileName));

      wxFileName newFileName(fileName);

      // Make sure it isn't already open
      size_t numProjects = gAudacityProjects.Count();
      for (size_t i = 0; i < numProjects; i++) {
         if (newFileName.SameAs(gAudacityProjects[i]->mFileName)) {
            wxMessageBox(_("That project is already open in another window."),
                         _("Error opening project"),
                         wxOK | wxCENTRE);
            return;
         }
      }

      // STM: Removing check of IsDirty(): as long as it's empty, why should we care?
      // (alternately, if its dirty and empty, we should clean it)
      if (!proj ||  !proj->mTracks->IsEmpty()) {
         // Open in a new window if this one is in use or doesn't exist (on a Mac when no window is open).
         AudacityProject *newProject =
            CreateNewAudacityProject(gParentWindow);
         newProject->OpenFile(fileName);
      } else {
         proj->OpenFile(fileName);
      }
   }
}

void AudacityProject::OpenFile(wxString fileName)
{
   // We want to open projects using wxTextFile, but if it's NOT a project
   // file (but actually a WAV file, for example), then wxTextFile will spin
   // for a long time searching for line breaks.  So, we look for our
   // signature at the beginning of the file first:

   wxString firstLine = "AudacityProject";
   char temp[16];

   if (!::wxFileExists(fileName)) {
      wxMessageBox(_("Could not open file: ") + fileName,
                   _("Error opening file"),
                   wxOK | wxCENTRE, this);
      return;
   }

   wxFFile *ff = new wxFFile(fileName);
   if (!ff->IsOpened()) {
      wxMessageBox(_("Could not open file: ") + fileName,
                   _("Error opening file"),
                   wxOK | wxCENTRE, this);
   }
   ff->Read(temp, 15);
   temp[15] = 0;
   ff->Close();
   delete ff;

   if (!strcmp(temp, "AudacityProject")) {
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
         strcpy(temp, "<?xml ");
      }
   }
   
   temp[6] = 0;
   if (strcmp(temp, "<?xml ")) {
      // If it's not XML, try opening it as any other form of audio
      Import(fileName);
      return;
   }

   ///
   /// Parse project file
   ///

   mFileName = fileName;
   SetTitle(GetName());

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
      HandleResize();
      mTrackPanel->Refresh(false);

      if (err) {
         ::wxMessageBox(_("An error occurred while opening the project file.\nSome audio may not have been retrieved."),
                        _("Error opening project"),
                        wxOK | wxCENTRE, this);
      }
   }
   else {
      mTracks->Clear(true);

      mFileName = "";
      SetTitle("Audacity");

      wxMessageBox(xmlFile.GetErrorStr(),
                   _("Error opening project"),
                   wxOK | wxCENTRE, this);
   }
}

bool AudacityProject::HandleXMLTag(const char *tag, const char **attrs)
{
   if (strcmp(tag, "audacityproject"))
      return false;

   int requiredTags = 0;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while(*attrs) {
      const char *attr = *attrs++;
      const char *value = *attrs++;

      if (!value)
         break;

      if (!strcmp(attr, "projname")) {
         wxString projName = value;
         wxString projPath = wxPathOnly(mFileName);
         
         if (!mDirManager->SetProject(projPath, projName, false)) {

            wxMessageBox(wxString::Format(_("Couldn't find the project data folder: \"%s\""),
                                          (const char *)projName),
                         _("Error opening project"),
                         wxOK | wxCENTRE, this);

            return false;
         }

         requiredTags++;
      }

      if (!strcmp(attr, "sel0"))
         wxString(value).ToDouble(&mViewInfo.sel0);

      if (!strcmp(attr, "sel1"))
         wxString(value).ToDouble(&mViewInfo.sel1);

      long longVpos;
      if (!strcmp(attr, "vpos"))
         wxString(value).ToLong(&longVpos);
      mViewInfo.vpos = longVpos;

      if (!strcmp(attr, "h"))
         wxString(value).ToDouble(&mViewInfo.h);

      if (!strcmp(attr, "zoom"))
         wxString(value).ToDouble(&mViewInfo.zoom);

      if (!strcmp(attr, "rate")) {
         wxString(value).ToDouble(&mRate);
         mStatus->SetRate(mRate);
      }
   } // while

   if (requiredTags >= 1)
      return true;
   else
      return false;
}

XMLTagHandler *AudacityProject::HandleXMLChild(const char *tag)
{
   if (!strcmp(tag, "tags")) {
      return mTags;
   }

   if (!strcmp(tag, "wavetrack")) {
      WaveTrack *newTrack = mTrackFactory->NewWaveTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!strcmp(tag, "notetrack")) {
      NoteTrack *newTrack = mTrackFactory->NewNoteTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!strcmp(tag, "labeltrack")) {
      LabelTrack *newTrack = mTrackFactory->NewLabelTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!strcmp(tag, "timetrack")) {
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
   if (project.Len() > 4 && project.Mid(project.Len() - 4) == ".aup")
      project = project.Mid(0, project.Len() - 4);
   wxString projName = wxFileNameFromPath(project) + "_data";
   // End Warning -DMM

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<audacityproject ");
   fprintf(fp, "projname=\"%s\" ", (const char *)projName);
   fprintf(fp, "version=\"%s\" ", AUDACITY_FILE_FORMAT_VERSION);
   fprintf(fp, "audacityversion=\"%s\" ", AUDACITY_VERSION_STRING);
   fprintf(fp, "sel0=\"%.10g\" ", mViewInfo.sel0);
   fprintf(fp, "sel1=\"%.10g\" ", mViewInfo.sel1);
   fprintf(fp, "vpos=\"%d\" ", mViewInfo.vpos);
   fprintf(fp, "h=\"%.10g\" ", mViewInfo.h);
   fprintf(fp, "zoom=\"%.10g\" ", mViewInfo.zoom);
   fprintf(fp, "rate=\"%g\" ", mRate);
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
   fprintf(fp, "</audacityproject>\n");
}

bool AudacityProject::Save(bool overwrite /* = true */ ,
                           bool fromSaveAs /* = false */ )
{
   if (!fromSaveAs && mDirManager->GetProjectName() == "")
      return SaveAs();

   //
   // Always save a backup of the original project file
   //

   wxString safetyFileName = "";
   if (wxFileExists(mFileName)) {

#ifdef __WXGTK__
      safetyFileName = mFileName + "~";
#else
      safetyFileName = mFileName + ".bak";
#endif

      if (wxFileExists(safetyFileName))
         wxRemoveFile(safetyFileName);

      wxRename(mFileName, safetyFileName);
   }

   if (fromSaveAs || mDirManager->GetProjectName() == "") {

      // This block of code is duplicated in WriteXML, for now...
      wxString project = mFileName;
      if (project.Len() > 4 && project.Mid(project.Len() - 4) == ".aup")
         project = project.Mid(0, project.Len() - 4);
      wxString projName = wxFileNameFromPath(project) + "_data";
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
         wxMessageBox(wxString::Format(_("Could not save project.  "
                                         "Perhaps %s is not writeable,\n"
                                         "or the disk is full."),
                                       (const char *) project),
                      _("Error saving project"),
                      wxOK | wxCENTRE, this);
         if (safetyFileName)
            wxRename(safetyFileName, mFileName);
         
         return false;
      }
   }

   FILE *fp = fopen(mFileName, "wb");
   if (!fp || ferror(fp)) {
      wxMessageBox(_("Couldn't write to file: ") + mFileName,
                   _("Error saving project"),
                   wxOK | wxCENTRE, this);

      if (safetyFileName)
         wxRename(safetyFileName, mFileName);
      
      return false;
   }

   fprintf(fp, "<?xml version=\"1.0\"?>\n");
   WriteXML(0, fp);

   fclose(fp);

#ifdef __WXMAC__
   FSSpec spec;

   wxMacFilename2FSSpec(mFileName, &spec);
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

   mStatus->SetField(wxString::Format(_("Saved %s"),
                                      (const char *) mFileName), 0);

   mUndoManager.StateSaved();
   return true;
}

bool AudacityProject::ImportProgressCallback(void *_self, float percent)
{
   AudacityProject *self = (AudacityProject*)_self;
   const int progressDialogGranularity = 1000;

   if (self->mImportProgressDialog) {
      return !self->mImportProgressDialog->Update((int)(percent * progressDialogGranularity));
   }
   else if (wxGetElapsedTime(false) > 500) {
      wxString description;

      if (self->mImportingRaw)
         description = _("Raw");
      else
         description = self->mImporter->GetFileDescription();
         
      wxString dialogMessage;
      dialogMessage.Printf(_("Importing %s File..."),
                           (const char *)description);

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

   for (int i = 0; i < numTracks; i++) {
      if (newRate == 0 && newTracks[i]->GetKind() == Track::Wave) {
         newRate = ((WaveTrack *)newTracks[i])->GetRate();
      }
      mTracks->Add(newTracks[i]);
      newTracks[i]->SetSelected(true);
      newTracks[i]->SetName(fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.'));
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
         mStatus->SetRate(mRate);
      }
   }

   PushState(wxString::Format(_("Imported '%s'"), fileName.c_str()),
             _("Import"));

   OnZoomFit();

   mTrackPanel->Refresh(false);

   if (initiallyEmpty && mDirManager->GetProjectName() == "") {
      wxString name = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
      mFileName =::wxPathOnly(fileName) + wxFILE_SEP_PATH + name + ".aup";
      SetTitle(GetName());
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

   numTracks = mImporter->Import(fileName, mTrackFactory, &newTracks,
                                 errorMessage,
                                 AudacityProject::ImportProgressCallback,
                                 this);

   // for LOF ("list of files") files, do not import the file as if it
   // were an audio file itself
   if (fileName.AfterLast('.').IsSameAs("lof", false))
   {
      return;
   }

   if(mImportProgressDialog) {
      delete mImportProgressDialog;
      mImportProgressDialog = NULL;
   }

   if (numTracks <= 0) {
      wxMessageBox(errorMessage,
                   _("Error importing"),
                   wxOK | wxCENTRE, this);
      return;
   }

   AddImportedTracks(fileName, newTracks, numTracks);
}

bool AudacityProject::SaveAs()
{
   wxString path = wxPathOnly(mFileName);
   wxString fName = GetName().Len()? GetName() + ".aup" : wxString("");

   fName = wxFileSelector(_("Save Project As:"),
                          path,
                          fName,
                          "",
                          _("Audacity projects (*.aup)|*.aup"), wxSAVE,
                          this);

   if (fName == "")
      return false;

   size_t len = fName.Len();
   if (len > 4 && fName.Mid(len - 4) == ".aup")
      fName = fName.Mid(0, len - 4);

   mFileName = fName + ".aup";
   SetTitle(GetName());

   return Save(false, true);
}

//
// Undo/History methods
//

void AudacityProject::InitialState()
{
   mUndoManager.ClearStates();

   TrackList *l = new TrackList(mTracks);
   mUndoManager.PushState(l, mViewInfo.sel0, mViewInfo.sel1,
                          _("Created new project"), _(""));
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
   mTrackPanel->Refresh(false);
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

   mViewInfo.sel1 = mViewInfo.sel0;

   PushState(wxString::Format(_("Deleted %.2f seconds at t=%.2f"),
                              mViewInfo.sel0 - mViewInfo.sel1,
                              mViewInfo.sel0),
             _("Delete"));
   FixScrollbars();
   mTrackPanel->Refresh(false);
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
   ToolBar *tb = NULL;

   if (mToolBarArray.GetCount() > 0)
      if ((mToolBarArray[0]->GetType()) == ControlToolBarID)
         tb = mToolBarArray[0];

   if (!tb && gControlToolBarStub)
      tb = gControlToolBarStub->GetToolBar();

   return (ControlToolBar *) tb;
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

MixerToolBar *AudacityProject::GetMixerToolBar()
{
   for(unsigned int i=0; i<mToolBarArray.GetCount(); i++)
      if ((mToolBarArray[i]->GetType()) == MixerToolBarID)
         return (MixerToolBar *)mToolBarArray[i];

   if (gMixerToolBarStub)
      return (MixerToolBar *)gMixerToolBarStub->GetToolBar();

   return NULL;
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
            msg.Printf(_("Recording time remaining: %d hours and %d minutes"),
                       recMins/60, recMins%60);
         else if (recMins >= 60)
            msg.Printf(_("Recording time remaining: 1 hour and %d minutes"),
                       recMins-60);
         else if (recMins > 3)
            msg.Printf(_("Recording time remaining: %d minutes"),
                       recMins);
         else if (recTime >= 2)
            msg.Printf(_("Recording time remaining: %d seconds"),
                       (int)recTime);
         else
            msg.Printf(_("Out of disk space"));

         mStatus->SetField(msg, 0);
      }
   }
}

// TrackPanel callback method
void AudacityProject::TP_DisplayStatusMessage(const char *msg,
                                              int fieldNum)
{
   mStatus->SetField(msg, fieldNum);
   if (fieldNum == 0)
      mLastStatusUpdateTime = ::wxGetUTCTime();
}

void AudacityProject::TP_DisplaySelection()
{
   wxString formatting = FormatSelection(mSelectionFormat, mSnapTo,
                                         mRate, &mViewInfo);
   TP_DisplayStatusMessage(formatting, 1);   
}

// TrackPanel callback method
int AudacityProject::TP_GetCurrentTool()
{
   //ControlToolBar might be NULL--especially on shutdown.
   //Make sure it isn't and if it is, return a reasonable value
   ControlToolBar *ctb = GetControlToolBar();
   if (ctb)
      return GetControlToolBar()->GetCurrentTool();
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

// TrackPanel callback method
void AudacityProject::TP_HasMouse()
{
   // This is unneccesary, I think, because it already gets done in our own ::OnMouseEvent
   //SetActiveProject(this);
   //mTrackPanel->SetFocus();
}

void AudacityProject::TP_HandleResize()
{
   HandleResize();
}

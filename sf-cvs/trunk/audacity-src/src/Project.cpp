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
#include "FreqWindow.h"
#include "HistoryWindow.h"
#include "import/Import.h"
#include "LabelTrack.h"
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
     mImportProgressDialog(NULL),
     mRate((double) gPrefs->Read("/SamplingRate/DefaultProjectSampleRate", 44100)),
     mDefaultFormat((sampleFormat) gPrefs->
           Read("/SamplingRate/DefaultProjectSampleFormat", floatSample)),
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

   //Initialize Commands
   if(!GetCommands()->Initialize())
   {
      wxMessageBox("Error: Failed to initialize Audacity's commands!");
   }

   wxMenuBar * MainMenuBar = GetCommands()->GetMenuBar("appmenu");
   if(!MainMenuBar)
   {
      wxMessageBox("Error: Failed to clone the 'appmenu' menubar!");
   }

   SetMenuBar(MainMenuBar);

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

      top += h + 1;
      height -= h + 1;
      mTotalToolBarHeight += h;
   }

#if USE_PORTMIXER
   if (gMixerToolBarStub) {
      if (gMixerToolBarStub->GetLoadedStatus()
          && !gMixerToolBarStub->GetWindowedStatus()) {
         int h = gMixerToolBarStub->GetHeight();
         ToolBar *etb = new MixerToolBar(this, 0, wxPoint(10, top),
                                         wxSize(width - 10 - sbarSpaceWidth, h));
         mToolBarArray.Add((ToolBar *) etb);

         top += h + 1;
         height -= h + 1;
         mTotalToolBarHeight += h;
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

         top += h + 1;
         height -= h + 1;
         mTotalToolBarHeight += h;
      }
   }

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

   //the commands code is responsible for deleting the menu bar, not Audacity's frame
   SetMenuBar(NULL);

   if (gAudioIO->IsStreamActive(mAudioIOToken))
      gAudioIO->StopStream();

   mTrackPanel->Destroy();

   //Go through the toolbar array and delete all the toolbars
   //Do this from the bottom, to avoid too much popping forward in the array
   // that would be obtained if you keep deleting the 0th item from the front

   size_t i = mToolBarArray.GetCount();
   while (i > 0) {
      delete mToolBarArray[--i];
      mToolBarArray.RemoveAt(i);
   }
   mToolBarArray.Clear();
   WX_CLEAR_ARRAY(mToolBarArray)

   delete mImporter;
   mImporter = NULL;

   delete mTrackFactory;
   mTrackFactory = NULL;

   if (mLastSavedTracks) {
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
   if (mTrackPanel) {
      int left = 0, top = 0;
      int width, height;
      GetClientSize(&width, &height);
      //Deal with the ToolBars 
      int toolbartop, toolbarbottom, toolbarheight;
      unsigned int i;

      
      //This adjust the initial toolbar offset, allowing
      //the horizontal lines between toolbars and menus to show up.
#if defined __WXMSW__
	int h = 1;
#else
	int h = 0;
#endif
	  
	  int ptop = 0;

      for (i = 0; i < mToolBarArray.GetCount(); i++) {
         toolbartop = h;
         toolbarheight = mToolBarArray[i]->GetHeight();
         toolbarbottom = toolbartop + toolbarheight;
         h = toolbarbottom;

         mToolBarArray[i]->SetSize(10, toolbartop, width - 10,
                                   toolbarheight);
         h += 1;
      }

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
}


void AudacityProject::OnIconize(wxIconizeEvent &event)
{
   int VisibleProjectCount = 0;

   //JKC: On Iconizing we get called twice.  Don't know
   // why but it does no harm.
   // Should we be returning true/false rather than 
   // void return?  I don't know.
   mIconized = event.Iconized();

   for(int i=0;i<gAudacityProjects.Count();i++){
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

bool AudacityProject::ProcessEvent(wxEvent & event)
{
   if (event.GetEventType() == wxEVT_COMMAND_MENU_SELECTED) {
      if(GetCommands()->HandleMenuEvent(event))
         return true;
   }

   return wxFrame::ProcessEvent(event);
}

bool AudacityProject::ProcessEffectEvent(int nEffectIndex)
{
   Effect *f = NULL;

   f = Effect::GetEffect(nEffectIndex);

   if (f) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      double prevEndTime = mTracks->GetEndTime();
      int count = 0;

      while (t) {
         if (t->GetSelected() && t->GetKind() == (Track::Wave))
            count++;
         t = iter.Next();
      }

      if (count == 0) {
         // No tracks were selected...
         if (f->GetEffectFlags() & INSERT_EFFECT) {
            // Create a new track for the generated audio...
            WaveTrack *newTrack = mTrackFactory->NewWaveTrack();
            mTracks->Add(newTrack);
            newTrack->SetSelected(true);
         }
         else {
            wxMessageBox(_("You must select a track first."));
            return true;
         }
      }

      if (f->DoEffect(this, mTracks, mTrackFactory,
                      &mViewInfo.sel0, &mViewInfo.sel1)) {
         PushState(f->GetEffectDescription()); 
         if (mTracks->GetEndTime() > prevEndTime)
            OnZoomFit();
         FixScrollbars();

         mTrackPanel->Refresh(false);
      } else {
         // TODO: undo the effect if necessary?
      }

      // This indicates we handled the event.
      return true;
   }

   return false;
}

void AudacityProject::UpdateMenus()
{
   if (!gControlToolBarStub)
      return;

   GetCommands()->EnableItemsByFunction("appmenu", "OnSave", mUndoManager.UnsavedChanges());

   bool nonZeroRegionSelected = (mViewInfo.sel1 > mViewInfo.sel0);

   int numTracks = 0;
   int numTracksSelected = 0;
   int numWaveTracks = 0;
   int numWaveTracksSelected = 0;
   int numLabelTracks = 0;
   int numLabelTracksSelected = 0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      numTracks++;
      // JH: logically, we only want to count a stereo pair as one track. Right??
      // I'm changing it and hoping I don't break anything
      if (t->GetKind() == Track::Wave && t->GetLinked() == false)
         numWaveTracks++;
      if (t->GetKind() == Track::Label)
         numLabelTracks++;
      if (t->GetSelected()) {
         numTracksSelected++;
         // JH: logically, we only want to count a stereo pair as one track. Right??
         // I'm changing it and hoping I don't break anything
         if (t->GetKind() == Track::Wave && t->GetLinked() == false)
            numWaveTracksSelected++;
         else if(t->GetKind() == Track::Label)
            numLabelTracksSelected++;

      }
      t = iter.Next();
   }

   GetCommands()->EnableItemsByFunction("appmenu", "Paste", numTracksSelected > 0 && msClipLen > 0.0);
   GetCommands()->EnableItemsByFunction("appmenu", "PasteOver", numTracksSelected > 0 && msClipLen > 0.0);

   //Calculate the ToolBarCheckSum (uniquely specifies state of all toolbars):
   int toolBarCheckSum = 0;
   toolBarCheckSum += gControlToolBarStub->GetWindowedStatus() ? 2 : 1;
   if (gEditToolBarStub) {
      if (gEditToolBarStub->GetLoadedStatus()) {
         if(gEditToolBarStub->GetWindowedStatus())
            toolBarCheckSum += 6;
         else
            toolBarCheckSum += 3;
      }
   }
   if (gMixerToolBarStub) {
      if (gMixerToolBarStub->GetLoadedStatus()) {
         if(gMixerToolBarStub->GetWindowedStatus())
            toolBarCheckSum += 12;
         else
            toolBarCheckSum += 24;
      }
   }
   
   
   
   // Get ahold of the clipboard status
   bool clipboardStatus = static_cast<bool>(GetActiveProject()->Clipboard());

   // Return from this function if nothing's changed since
   // the last time we were here.
 
   if (!mFirstTimeUpdateMenus &&
       mLastNonZeroRegionSelected == nonZeroRegionSelected &&
       mLastNumTracks == numTracks &&
       mLastNumTracksSelected == numTracksSelected &&
       mLastNumWaveTracks == numWaveTracks &&
       mLastNumWaveTracksSelected == numWaveTracksSelected &&
       mLastNumLabelTracks == numLabelTracks &&
       mLastZoomLevel == mViewInfo.zoom &&
       mLastToolBarCheckSum == toolBarCheckSum &&
       mLastUndoState == mUndoManager.UndoAvailable() &&
       mLastRedoState == mUndoManager.RedoAvailable() &&
       mLastClipboardState == clipboardStatus  ) 
      return;
   
   // Otherwise, save state and then update all of the menus

   mFirstTimeUpdateMenus = false;
   mLastNonZeroRegionSelected = nonZeroRegionSelected;
   mLastNumTracks = numTracks;
   mLastNumTracksSelected = numTracksSelected;
   mLastNumWaveTracks = numWaveTracks;
   mLastNumWaveTracksSelected = numWaveTracksSelected;
   mLastNumLabelTracks = numLabelTracks;
   mLastZoomLevel = mViewInfo.zoom;
   mLastToolBarCheckSum = toolBarCheckSum;
   mLastUndoState = mUndoManager.UndoAvailable();
   mLastRedoState = mUndoManager.RedoAvailable();  
   mLastClipboardState = clipboardStatus;

   bool anySelection = numTracksSelected > 0 && nonZeroRegionSelected;

   GetCommands()->EnableItemsByFunction("appmenu", "OnExportMix", numTracks > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnExportSelection", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "OnExportLossyMix", numTracks > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnExportLossySelection", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "OnExportLabels", numLabelTracks > 0);

   GetCommands()->EnableItemsByFunction("appmenu", "Cut", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "Copy", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "Trim", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "OnDelete", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "OnSilence", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "OnSplit", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "OnSplitLabels", numLabelTracksSelected == 1 && numWaveTracksSelected == 1);
   GetCommands()->EnableItemsByFunction("appmenu", "OnDuplicate", anySelection);
   GetCommands()->EnableItemsByFunction("appmenu", "OnSelectAll", numTracks > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnSelectCursorEnd", numWaveTracksSelected > 0 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnSelectStartCursor", numWaveTracksSelected > 0 && !nonZeroRegionSelected);

   GetCommands()->EnableItemsByFunction("appmenu", "Undo", mUndoManager.UndoAvailable());
   GetCommands()->EnableItemsByFunction("appmenu", "Redo", mUndoManager.RedoAvailable());


   GetCommands()->EnableItemsByFunction("appmenu", "OnPlotSpectrum", numWaveTracksSelected > 0
                     && nonZeroRegionSelected);

#ifndef __WXMAC__
   //Modify toolbar-specific Menus

   if (gEditToolBarStub) {

     // Loaded or unloaded?
     GetCommands()->EnableItemsByFunction("appmenu", "OnFloatEditToolBar", gEditToolBarStub->GetLoadedStatus());

     // Floating or docked?
     if (gEditToolBarStub->GetWindowedStatus())
       GetCommands()->ChangeText("appmenu", "OnFloatEditToolBar", _("Dock Edit Toolbar"));
     else
       GetCommands()->ChangeText("appmenu", "OnFloatEditToolBar", _("Float Edit Toolbar"));
   }
   else
     {
       GetCommands()->EnableItemsByFunction("appmenu", "OnFloatEditToolBar", false);
     }


   if (gMixerToolBarStub) {
     
     // Loaded or unloaded?
     GetCommands()->EnableItemsByFunction("appmenu", "OnFloatMixerToolBar", gMixerToolBarStub->GetLoadedStatus());
     
     // Floating or docked?
     if (gMixerToolBarStub->GetWindowedStatus())
       GetCommands()->ChangeText("appmenu", "OnFloatMixerToolBar", _("Dock Mixer Toolbar"));
     else
       GetCommands()->ChangeText("appmenu", "OnFloatMixerToolBar", _("Float Mixer Toolbar"));
   }
   else
     {
       GetCommands()->EnableItemsByFunction("appmenu", "OnFloatMixerToolBar", false);
     }

#endif

   GetCommands()->EnableItemsByFunction("appmenu", "OnQuickMix", numWaveTracksSelected > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnSelectionSave", numWaveTracksSelected > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnSelectionRestore", numWaveTracksSelected > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnCursorTrackStart", numWaveTracksSelected > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnCursorTrackEnd", numWaveTracksSelected > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnCursorSelStart", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnCursorSelEnd", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlign", numWaveTracksSelected > 1);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignZero", numWaveTracksSelected > 0);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignZeroMoveSel", numWaveTracksSelected == 1);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupZero", numWaveTracksSelected > 1);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupZeroMoveSel", numWaveTracksSelected > 1);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignCursor", numWaveTracksSelected > 0 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignCursorMoveSel", numWaveTracksSelected == 1 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignSelStart", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignSelStartMoveSel", numWaveTracksSelected == 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignSelEnd", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignSelEndMoveSel", numWaveTracksSelected == 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignEndCursor", numWaveTracksSelected > 0 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignEndCursorMoveSel", numWaveTracksSelected == 1 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignEndSelStart", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignEndSelStartMoveSel", numWaveTracksSelected == 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignEndSelEnd", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignEndSelEndMoveSel", numWaveTracksSelected == 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupCursor", numWaveTracksSelected > 1 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupCursorMoveSel", numWaveTracksSelected > 1 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupSelStart", numWaveTracksSelected > 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupSelStartMoveSel", numWaveTracksSelected > 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupSelEnd", numWaveTracksSelected > 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupSelEndMoveSel", numWaveTracksSelected > 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupEndCursor", numWaveTracksSelected > 1 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupEndCursorMoveSel", numWaveTracksSelected > 1 && !nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupEndSelStart", numWaveTracksSelected > 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupEndSelStartMoveSel", numWaveTracksSelected > 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupEndSelEnd", numWaveTracksSelected > 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnAlignGroupEndSelEndMoveSel", numWaveTracksSelected > 1 && nonZeroRegionSelected);
   GetCommands()->EnableItemsByFunction("appmenu", "OnRemoveTracks", numTracksSelected > 0);

   // Effects menus

   EffectArray *effs;
   unsigned int e;

#if 0 // dmazzoni: Generate menu is now always enabled
   effs = Effect::GetEffects(BUILTIN_EFFECT | PLUGIN_EFFECT | INSERT_EFFECT);
   for(e=0; e<effs->GetCount(); e++)
      GetCommands()->EnableItemsByFunction("appmenu", wxString::Format("%i@GeneratePlugins@Effect", (*effs)[e]->GetID()),
                          numWaveTracksSelected > 0);
   delete effs;
#endif

   effs = Effect::GetEffects(BUILTIN_EFFECT | PLUGIN_EFFECT | PROCESS_EFFECT);
   for(e=0; e<effs->GetCount(); e++)
      GetCommands()->EnableItemsByFunction("appmenu", wxString::Format("%i@EffectPlugins@Effect", (*effs)[e]->GetID()),
                          numWaveTracksSelected > 0
                          && nonZeroRegionSelected);
   delete effs;

   effs = Effect::GetEffects(BUILTIN_EFFECT | PLUGIN_EFFECT | ANALYZE_EFFECT);
   for(e=0; e<effs->GetCount(); e++)
      GetCommands()->EnableItemsByFunction("appmenu", wxString::Format("%i@AnalyzePlugins@Effect", (*effs)[e]->GetID()),
                          numWaveTracksSelected > 0
                           && nonZeroRegionSelected);
   delete effs;

   //Now, go through each toolbar, and and call EnableDisableButtons()
   unsigned int i;
   for (i = 0; i < mToolBarArray.GetCount(); i++) {
      mToolBarArray[i]->EnableDisableButtons();
   }

   //Now, do the same thing for the (possibly invisible) floating toolbars
   gControlToolBarStub->GetToolBar()->EnableDisableButtons();

   //gEditToolBarStub might be null:
   if(gEditToolBarStub){
      gEditToolBarStub->GetToolBar()->EnableDisableButtons();
   }
}

//TODO: This function is still kinda hackish, clean up
void AudacityProject::OnUpdateMenus(wxUpdateUIEvent & event)
{
   if (::wxGetUTCTime() - mLastUpdateUITime < 3)
      return;

   mLastUpdateUITime = ::wxGetUTCTime();

   UpdateMenus();
}


void AudacityProject::OnPaint(wxPaintEvent & /*event*/)
{
   wxPaintDC dc(this);

   int top = 0;
   int h = 0;
   unsigned int i, j;
   int toolbartop, toolbarbottom, toolbarheight;
   wxRect r;

   int width, height;
   GetClientSize(&width, &height);

   //Deal with the ToolBars 
   for (i = 0; i < mToolBarArray.GetCount(); i++) {

   
	  AColor::Medium(&dc, false);
      toolbartop = h;
	  toolbarheight = mToolBarArray[i]->GetHeight();
      h += toolbarheight;
      toolbarbottom = h;

      //Draw a rectangle beneath the "grab-bar"
      r.x = 0;
      r.y = toolbartop;
      r.width = 10;
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
         dc.DrawLine(3, toolbartop + j, 6, toolbartop + j);

      AColor::Dark(&dc, false);
      for (j = minbump + 1; j < maxbump + 1; j += 4)
         dc.DrawLine(3, toolbartop + j, 6, toolbartop + j);

      //Draw a black line to the right of the grab-bar
      dc.SetPen(*wxBLACK_PEN);
      dc.DrawLine(9, toolbartop, 9, toolbarbottom);
	
	  //Draw some more lines for Windows (tm), along the top and left side 
	  //of the grab-bar
#ifdef __WXMSW__
	  dc.DrawLine(0, toolbartop, 10, toolbartop);
	  dc.DrawLine(0, toolbartop, 0, toolbarbottom);
#endif
      dc.DrawLine(0, toolbarbottom, width, toolbarbottom);
      h++;
   }

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

//This creates a toolbar of type t in the ToolBars array
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
      GetCommands()->ChangeText("appmenu", "OnFloatControlToolBar", _("Float Control Toolbar"));
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
            GetCommands()->ChangeText("appmenu", "OnFloatControlToolBar", _("Dock Control Toolbar"));
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

   //See if we need to drag a toolbar off the window
   if (event.ButtonDown()
       && !mDrag && event.m_x < 10 && event.m_y < mTotalToolBarHeight) {

      //Now, see which toolbar we need to drag
      int h = 0;
      unsigned int i = 0;
      while (i < mToolBarArray.GetCount()) {
         h += mToolBarArray[i]->GetHeight() + 1;

         if (event.m_y < h)
            break;
         i++;
      }

      //You want the hotspot to be relative to the toolbar being clicked.
      //So, subtract from the y coordinate the height of the toolbars above it.
      //But, to do this you need to subtract the height of the last toolbar
      //you looked at in the above loop

      hotspot.y -= (h - mToolBarArray[i]->GetHeight() -1);
      
      if (i >= mToolBarArray.GetCount()) {
         //This shouldn't really happen, except if the click is on
         //the border--which might occur for some platform-specific formatting
      } 
      else 
	{

         int width, height;
         wxSize s = mToolBarArray[i]->GetIdealSize();

         mToolBarArray[i]->GetSize(&width, &height);


         //To enhance performance, these toolbar bitmaps could be pre-loaded
         //Right now, they are not.

         //Only get as much of the toolbar image as the ideal size is
         width = (width > s.x) ? s.x : width;

         wxClientDC dc(this);
         //Make the new bitmap a bit bigger
         wxBitmap *bitmap = new wxBitmap((width + 2), (height + 2));

         wxMemoryDC *memDC = new wxMemoryDC();
         memDC->SelectObject(*bitmap);

         //Draw a black box on perimeter
         memDC->SetPen(*wxBLACK_PEN);
         memDC->DrawRectangle(0, 0, width + 2, height + 2);

         //copy an image of the toolbar into the box
         memDC->Blit(1, 1, width, height, &dc, 0, h - height - 1);
         delete memDC;

         mDrag = new wxDragImage(*bitmap);
         delete bitmap;


         hotspot = hotspot - wxPoint(1,1-h+height);

	 mDrag->BeginDrag(mouse - hotspot, this, true);
         //Adjust a little because the bitmap is bigger than the toolbar

         mDrag->Move(mouse);
         mDrag->Show();
         mToolBarHotspot = hotspot;

         mDraggingToolBar = mToolBarArray[i]->GetType();
      }
   }

   else if (event.Dragging() && mDrag) {

     mDrag->Move(mouse - wxPoint(1, 1) );//+ mToolBarHotspot);

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
                                        "IRCAM files (*.snd)|*.snd|"
                                        "MP3 files (*.mp3)|*.mp3"),
                                      0,        // Flags
                                      proj);    // Parent

   if (fileName != "") {
      gPrefs->Write("/DefaultOpenPath", wxPathOnly(fileName));

      // Make sure it isn't already open
      size_t numProjects = gAudacityProjects.Count();
      for (size_t i = 0; i < numProjects; i++)
         if (gAudacityProjects[i]->mFileName == fileName) {
            wxMessageBox
                (_("That project is already open in another window."));
            return;
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
      wxMessageBox(_("Could not open file: ") + mFileName);
      return;
   }

   wxFFile ff(fileName);
   if (!ff.IsOpened()) {
      wxMessageBox(_("Could not open file: ") + mFileName);
      return;
   }
   ff.Read(temp, 15);
   temp[15] = 0;
   ff.Close();

   if (!strcmp(temp, "AudacityProject")) {
      wxMessageBox("Old Audacity project files can't be opened by "
                   "this newer version of Audacity (yet).");
      return;
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
      
      Track *t;
      TrackListIterator iter(mTracks);
      mLastSavedTracks = new TrackList();
      t = iter.First();
      while (t) {
         mLastSavedTracks->Add(t->Duplicate());
         t = iter.Next();
      }

      InitialState();
      HandleResize();
      mTrackPanel->Refresh(false);
   }
   else {
      mTracks->Clear(true);

      mFileName = "";
      SetTitle("Audacity");

      wxMessageBox(xmlFile.GetErrorStr());
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

#if LEGACY_PROJECT_FILE_SUPPORT

void AudacityProject::OpenLegacyProjectFile()
{
 TODO: This function is not complete; the old code is
       just reproduced here as a guide ... -DMM

   wxTextFile f;

   f.Open(fileName);
   if (!f.IsOpened()) {
      wxMessageBox(_("Could not open file: ") + mFileName);
      return;
   }

   wxString projName;
   wxString projPath;
   wxString version;
   long longVpos;

   f.GetFirstLine();            // This should say "AudacityProject"

   if (f.GetNextLine() != "Version")
      goto openFileError;
   version = f.GetNextLine();
   if (version != AUDACITY_FILE_FORMAT_VERSION) {
      wxMessageBox(_("This project was saved by a different version of "
                     "Audacity and is no longer supported."));
      return;
   }

   if (f.GetNextLine() != "projName")
      goto openFileError;
   projName = f.GetNextLine();
   projPath = wxPathOnly(mFileName);

   if (!mDirManager->SetProject(projPath, projName, false))
      return;

   if (f.GetNextLine() != "sel0")
      goto openFileError;
   if (!(f.GetNextLine().ToDouble(&mViewInfo.sel0)))
      goto openFileError;
   if (f.GetNextLine() != "sel1")
      goto openFileError;
   if (!(f.GetNextLine().ToDouble(&mViewInfo.sel1)))
      goto openFileError;
   if (f.GetNextLine() != "vpos")
      goto openFileError;
   if (!(f.GetNextLine().ToLong(&longVpos)))
      goto openFileError;
   mViewInfo.vpos = longVpos;
   if (f.GetNextLine() != "h")
      goto openFileError;
   if (!(f.GetNextLine().ToDouble(&mViewInfo.h)))
      goto openFileError;
   if (f.GetNextLine() != "zoom")
      goto openFileError;
   if (!(f.GetNextLine().ToDouble(&mViewInfo.zoom)))
      goto openFileError;
   if (version != "0.9") {
      if (f.GetNextLine() != "rate")
         goto openFileError;
      if (!(f.GetNextLine().ToDouble(&mRate)))
         goto openFileError;
      mStatus->SetRate(mRate);
   }

   mTracks->Clear();
   mTracks->Load(&f, mDirManager);

   // By making a duplicate set of pointers to the existing blocks
   // on disk, we add one to their reference count, guaranteeing
   // that their reference counts will never reach zero and thus
   // the version saved on disk will be preserved until the
   // user selects Save().

   Track *t;
   TrackListIterator iter(mTracks);
   mLastSavedTracks = new TrackList();
   t = iter.First();
   while (t) {
      mLastSavedTracks->Add(t->Duplicate());
      t = iter.Next();
   }

 openFileError:
   wxMessageBox(wxString::
                Format(_("Error reading Audacity Project %s in line %d"),
                       (const char *) mFileName, f.GetCurrentLine()));
   f.Close();
   return;
}

#endif // LEGACY_PROJECT_FILE_SUPPORT

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
                                       (const char *) project));
         if (safetyFileName)
            wxRename(safetyFileName, mFileName);
         
         return false;
      }
   }

   FILE *fp = fopen(mFileName, "wb");
   if (!fp || ferror(fp)) {
      wxMessageBox(_("Couldn't write to file: ") + mFileName);

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

   // TODO: detect which (if any) were WaveTracks and get the rate from them
   bool initiallyEmpty = mTracks->IsEmpty();
   //double newRate = newTracks[0]->GetRate();

   for (int i = 0; i < numTracks; i++) {
      mTracks->Add(newTracks[i]);
      newTracks[i]->SetSelected(true);
      newTracks[i]->SetName(fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.'));
   }

   delete[]newTracks;

   if (initiallyEmpty) {
      //mRate = newRate;
      //mStatus->SetRate(mRate);
   }

   PushState(wxString::Format(_("Imported '%s'"), fileName.c_str()));

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

   if(mImportProgressDialog) {
      delete mImportProgressDialog;
      mImportProgressDialog = NULL;
   }

   if (numTracks <= 0) {
      wxMessageBox(errorMessage);
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
   PushState(_("Created new project"), false);
   mUndoManager.StateSaved();
}

void AudacityProject::PushState(wxString desc,
                                bool makeDirty /* = true */ )
{
   TrackList *l = new TrackList(mTracks);

   mUndoManager.PushState(l, mViewInfo.sel0, mViewInfo.sel1, desc);
   delete l;

   if (makeDirty)
      mDirty = true;

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   UpdateMenus();
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
                              mViewInfo.sel0));
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
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   //If busy, stop playing, make sure everything is unpaused.
   if (gAudioIO->IsStreamActive()) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->OnStop(evt);
   } else {
      //Otherwise, start playing
      toolbar->SetPlay(true);
      toolbar->SetStop(false);
      toolbar->OnPlay(evt);
   }
}

// TrackPanel callback method
void AudacityProject::TP_PushState(wxString desc)
{
   PushState(desc);
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

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

#include "Project.h"

#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>
#include <wx/string.h>
#endif

#ifdef __WXMAC__
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# endif

void wxMacFilename2FSSpec( const char *path , FSSpec *spec ) ;

#else
# include <wx/dragimag.h>
#endif

#include <wx/event.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/scrolbar.h>
#include <wx/textfile.h>
#include <wx/menu.h>

#include "Audacity.h"
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

void wxMacFilename2FSSpec( const char *path , FSSpec *spec ) ;

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


int gAudacityDocNum = 0;
AProjectArray gAudacityProjects;
AudacityProject *gActiveProject;

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

   wxPoint where;
   where.x = 10;
   where.y = 10;

   int width = 600;
   int height = 400;


#ifdef __WXMAC__
   where.y += 50;
#endif

#ifdef __WXGTK__
   height += 20;
#endif

   where.x += gAudacityDocNum * 25;
   where.y += gAudacityDocNum * 25;

   AudacityProject *p = new AudacityProject(parentWindow, -1,
                                            where, wxSize(width, height));

   p->Show(true);

   gAudacityDocNum = (gAudacityDocNum + 1) % 10;

   SetActiveProject(p);
   return p;
}

void RedrawAllProjects()
{
   int len = gAudacityProjects.GetCount();
   for (int i = 0; i < len; i++)
      gAudacityProjects[i]->RedrawProject();
}

void CloseAllProjects()
{
   int len = gAudacityProjects.GetCount();
   for (int i = 0; i < len; i++)
      gAudacityProjects[i]->Close();
}

enum {
   FirstID = 1000,

   // Window controls

   HSBarID,
   VSBarID,
   TrackPanelID
};

#define AUDACITY_MENUS_GLOBALS
#include "Menus.h"
#undef AUDACITY_MENUS_GLOBALS

BEGIN_EVENT_TABLE(AudacityProject, wxFrame)
    EVT_CHAR_HOOK(AudacityProject::OnKey)
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
    // Update menu method
    EVT_UPDATE_UI(UndoID, AudacityProject::OnUpdateMenus)
END_EVENT_TABLE()

AudacityProject::AudacityProject(wxWindow * parent, wxWindowID id,
                                 const wxPoint & pos,
                                 const wxSize & size)
   : wxFrame(parent, id, "Audacity", pos, size),
     mRate((double) gPrefs->Read("/SamplingRate/DefaultProjectSampleRate",
                                 44100)),
     mDefaultFormat((sampleFormat) gPrefs->
           Read("/SamplingRate/DefaultProjectSampleFormat", floatSample)),
     mDirty(false),
     mTrackPanel(NULL), mAutoScrolling(false), mHistoryWindow(NULL),
     mTotalToolBarHeight(0), mDraggingToolBar(NoneID)
{
   #ifndef __WXMAC__
   mDrag = NULL;
   #endif

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

   mMenuBar = NULL;
   CreateMenuBar();

   int left = 0, top = 0, width, height;
   GetClientSize(&width, &height);

   // Create the Control Toolbar (if we're not using a windowed toolbar)
   // The control toolbar should be automatically loaded--other toolbars
   // are optional.

   if (!gControlToolBarStub->GetWindowedStatus()) {
      int h = gControlToolBarStub->GetHeight();
      ToolBar *tb = new ControlToolBar(this, 0, wxPoint(10, top),
                                       wxSize(width - 10, h));
      mToolBarArray.Add((ToolBar *) tb);

      top += h + 1;
      height -= h + 1;
      mTotalToolBarHeight += h;
   }

   if (gEditToolBarStub) {
      if (gEditToolBarStub->GetLoadedStatus()
          && !gEditToolBarStub->GetWindowedStatus()) {
         int h = gEditToolBarStub->GetHeight();
         ToolBar *etb = new EditToolBar(this, 0, wxPoint(10, top),
                                        wxSize(width - 10, h));
         mToolBarArray.Add((ToolBar *) etb);

         top += h + 1;
         height -= h + 1;
         mTotalToolBarHeight += h;
      }
   }
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

#ifdef __WXMAC__
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

   // Min size, max size
   SetSizeHints(250, 200, 20000, 20000);

   // Create tags object
   mTags = new Tags();

#ifdef __WXMSW__
   // Accept drag 'n' drop files
   DragAcceptFiles(true);
#endif

   gAudacityProjects.Add(this);
}

AudacityProject::~AudacityProject()
{
   if (gAudioIO->IsBusy() && gAudioIO->GetProject() == this)
      gAudioIO->HardStop();

   //Go through the toolbar array and delete all the toolbars
   //Do this from the bottom, to avoid too much popping forward in the array
   // that would be obtained if you keep deleting the 0th item from the front

   int i;
   for (i = mToolBarArray.GetCount() - 1; i >= 0; i--) {
      delete mToolBarArray[i];
      mToolBarArray.RemoveAt(i);
   }
   mToolBarArray.Clear();
   WX_CLEAR_ARRAY(mToolBarArray)

   delete mTags;
   mTags = NULL;

   mTracks->Clear(true);
   delete mTracks;
   mTracks = NULL;

   mCommandMenuItem.Clear();

   gAudacityProjects.Remove(this);

   if (gAudacityProjects.IsEmpty())
      QuitAudacity();
   else if (gActiveProject == this)
      gActiveProject = gAudacityProjects[0];
}

void AudacityProject::RedrawProject()
{
   FixScrollbars();
   mTrackPanel->Refresh(false);
}

DirManager *AudacityProject::GetDirManager()
{
   return &mDirManager;
}

Tags *AudacityProject::GetTags()
{
   return mTags;
}

wxString AudacityProject::GetName()
{
   wxString name = wxFileNameFromPath(mFileName);

   // Chop off the extension
   int len = name.Len();
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

void AudacityProject::OnScrollLeft()
{
   int pos = mHsbar->GetThumbPosition();
   pos = (pos > 0) ? pos : 0;   //Set to larger of pos and 0

   if (pos > 0) {
      mHsbar->SetThumbPosition(pos - sbarHjump);        //Jump 30 pixels to the left
      FinishAutoScroll();
   }
}

void AudacityProject::OnScrollRight()
{
   int pos = mHsbar->GetThumbPosition();
   int max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = (pos < max) ? pos : max;       //Set to smaller of pos and max

   if (pos < max) {
      mHsbar->SetThumbPosition(pos + sbarHjump);        //Jump 30 pixels to the right
      FinishAutoScroll();
   }
}

void AudacityProject::OnScrollLeftButton(wxScrollEvent & event)
{
   int pos = mHsbar->GetThumbPosition();
   pos = (pos > 0) ? pos : 0;   //Set to larger of pos and 0

   if (pos > 0) {
      mHsbar->SetThumbPosition(pos - sbarHjump);        //Jump 30 pixels to the left
      OnScroll(event);
   }
}


void AudacityProject::OnScrollRightButton(wxScrollEvent & event)
{
   int pos = mHsbar->GetThumbPosition();
   int max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = (pos < max) ? pos : max;       //Set to smaller of pos and max

   if (pos < max) {
      mHsbar->SetThumbPosition(pos + sbarHjump);        //Jump 30 pixels to the right
      OnScroll(event);
   }
}



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
   bool rescroll = false;

   int totalHeight = (mTracks->GetHeight() + 32);

   int panelWidth, panelHeight;
   mTrackPanel->GetTracksUsableArea(&panelWidth, &panelHeight);

   // Add 1/4 of a screen of blank space to the end of the longest track
   mViewInfo.screen = ((double) panelWidth) / mViewInfo.zoom;
   mViewInfo.total = mTracks->GetMaxLen() + mViewInfo.screen / 4;

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
      mTrackPanel->Refresh();
      rescroll = false;
   }
   if (mViewInfo.screen >= mViewInfo.total && mViewInfo.sbarH != 0) {
      mViewInfo.sbarH = 0;
      mTrackPanel->Refresh();
      rescroll = false;
   }

   mHsbar->SetScrollbar(mViewInfo.sbarH, mViewInfo.sbarScreen,
                        mViewInfo.sbarTotal, mViewInfo.sbarScreen, TRUE);
   mVsbar->SetScrollbar(mViewInfo.vpos / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep,
                        totalHeight / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep, TRUE);

   mViewInfo.lastZoom = mViewInfo.zoom;

   if (rescroll && mViewInfo.screen < mViewInfo.total)
      mTrackPanel->Refresh();
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
      int h = 0;
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

void AudacityProject::OnSize(wxSizeEvent & event)
{
   HandleResize();
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
   int numEffects = Effect::GetNumEffects(false);
   int numPlugins = Effect::GetNumEffects(true);
   Effect *f = NULL;

   if (event.GetEventType() == wxEVT_COMMAND_MENU_SELECTED) {
      // Builtin Effects
      if (event.GetId() >= FirstEffectID &&
          event.GetId() < FirstEffectID + numEffects) {
         f = Effect::GetEffect(event.GetId() - FirstEffectID, false);
      } else if (event.GetId() >= FirstPluginID &&
                 event.GetId() < FirstPluginID + numPlugins) {
         f = Effect::GetEffect(event.GetId() - FirstPluginID, true);
      } else if (HandleMenuEvent(event))
         return true;
   }
   /*
   else if ((event.GetEventType() == wxEVT_CHAR) || (event.GetEventType() == wxEVT_CHAR_HOOK) || (event.GetEventType() == wxEVT_NAVIGATION_KEY) || (event.GetEventType() == wxEVT_KEY_DOWN))
   {
      return false;
   }
   */

   if (f) {
      TrackListIterator iter(mTracks);
      VTrack *t = iter.First();
      int count = 0;

      while (t) {
         if (t->GetSelected() && t->GetKind() == (VTrack::Wave))
            count++;
         t = iter.Next();
      }

      if (count == 0 || mViewInfo.sel0 == mViewInfo.sel1) {
         wxMessageBox(_("No audio data is selected."));
         return true;
      }

      if (f->DoEffect(this, mTracks, mViewInfo.sel0, mViewInfo.sel1)) {
         PushState(_("Applied an effect."));    // maybe more specific?
         FixScrollbars();
         mTrackPanel->Refresh(false);
      } else {
         // TODO: undo the effect if necessary?
      }

      // This indicates we handled the event.
      return true;
   }

   return wxFrame::ProcessEvent(event);
}

void AudacityProject::OnPaint(wxPaintEvent & event)
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
      h += mToolBarArray[i]->GetHeight();
      toolbarbottom = h;
      toolbarheight = toolbarbottom - toolbartop;

      //Adjust a little for Windows (tm) 
#ifdef __WXMSW__
      h++;
#endif

      //Draw a rectangle the space of scrollbar
      r.x = width - sbarSpaceWidth;
      r.y = toolbartop;
      r.width = sbarSpaceWidth;
      r.height = toolbarheight;
      dc.DrawRectangle(r);

      //Draw a rectangle around the "grab-bar"
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

      //Draw some more lines for Windows (tm)
#ifdef __WXMSW__
      dc.DrawLine(0, toolbartop, width, toolbartop);
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
}

void AudacityProject::OnKey(wxKeyEvent& event)
{
   wxString newStr = "";

   long key = event.GetKeyCode();

   if(event.ControlDown())
      newStr += "Ctrl+";

   if(event.AltDown())
      newStr += "Alt+";

   if(event.ShiftDown())
      newStr += "Shift+";

   if (event.ControlDown() && key >= 1 && key <= 26)
      newStr += (char)(64 + key);
   else if (key >= 33 && key <= 126)
      newStr += (char)key;
   else if (key == WXK_BACK)
      newStr = "Backspace";
   else if (key == WXK_DELETE)
      newStr = "Delete";
   else if (key == WXK_SPACE)
      newStr = "Spacebar";
   else
      return; // Don't change it if we don't recognize the key

   (this->*((wxEventFunction) (GetCommandFunc(FindCommandByCombos(newStr)))))(event);
}

void AudacityProject::OnActivate(wxActivateEvent & event)
{
   SetActiveProject(this);
   event.Skip();
}

//This creates a toolbar of type t in the ToolBars array
void AudacityProject::LoadToolBar(enum ToolBarType t)
{
   //First, go through ToolBarArray and determine the current 
   //combined height of all toolbars.
   int tbheight = 0;
   int len = mToolBarArray.GetCount();
   for (int i = 0; i < len; i++)
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
      ((wxMenuItemBase *) mViewMenu->FindItem(FloatControlToolBarID))->
          SetName(_("Float Control Toolbar"));
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

   case NoneID:
   default:
      toolbar = NULL;
      break;
   }

   //Add the new toolbar to the ToolBarArray and redraw screen
   mTotalToolBarHeight += toolbar->GetHeight() + 1;

   HandleResize();
}

void AudacityProject::UnloadToolBar(enum ToolBarType t)
{
   //Go through all of the toolbars (from the bottom up)
   //And delete it if it is type T

   for (int i = mToolBarArray.GetCount() - 1; i >= 0; i--) {

      if (mToolBarArray[i]->GetType() == t) {

         mTotalToolBarHeight -= mToolBarArray[i]->GetHeight();
         delete mToolBarArray[i];
         mToolBarArray.RemoveAt(i);

         //Now, do any changes specific to different toolbar types
         switch (t) {
         case ControlToolBarID:

            //If the ControlToolBar is being unloaded from this project, you
            //should change the menu entry of this project
            ((wxMenuItemBase *) mViewMenu->
             FindItem(FloatControlToolBarID))->
               SetName(_("Dock Control Toolbar"));
            break;

         case EditToolBarID:
            break;

         case NoneID:
         default:
            break;
         }
      }
   }
   HandleResize();
}

bool AudacityProject::IsToolBarLoaded(enum ToolBarType t)
{
   int len = mToolBarArray.GetCount();
   for (int i = 0; i < len; i++) {

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

   wxPoint hotspot;
   hotspot.x = event.m_x;
   hotspot.y = event.m_y;

   wxPoint mouse = ClientToScreen(hotspot);

#ifndef __WXMAC__
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

      hotspot.y -= h - mToolBarArray[i]->GetHeight();

      if (i >= mToolBarArray.GetCount()) {
         //This shouldn't really happen, except if the click is on
         //the border--which might occur for some platform-specific formatting
      } else {

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

         //Adjust a little because the bitmap is bigger than the toolbar
         mDrag->BeginDrag(hotspot, this, true);
         mDrag->Move(mouse - wxPoint(1, 2));
         mDrag->Show();
         mToolBarHotspot = hotspot;

         mDraggingToolBar = mToolBarArray[i]->GetType();
      }
   }

   else if (event.Dragging() && mDrag) {

      mDrag->Move(mouse);

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

      case NoneID:
      default:
         break;
      }

      mDraggingToolBar = NoneID;
      HandleResize();
   }
   #endif
}

void AudacityProject::OnClose(wxEvent & event)
{
   Close();
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

   wxFile ff(fileName);
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
      
      VTrack *t;
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
         
         if (!mDirManager.SetProject(projPath, projName, false)) {
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
      WaveTrack *newTrack = new WaveTrack(&mDirManager);
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!strcmp(tag, "notetrack")) {
      NoteTrack *newTrack = new NoteTrack(&mDirManager);
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!strcmp(tag, "labeltrack")) {
      LabelTrack *newTrack = new LabelTrack(&mDirManager);
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

   VTrack *t;
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

   if (!mDirManager.SetProject(projPath, projName, false))
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
   mTracks->Load(&f, &mDirManager);

   // By making a duplicate set of pointers to the existing blocks
   // on disk, we add one to their reference count, guaranteeing
   // that their reference counts will never reach zero and thus
   // the version saved on disk will be preserved until the
   // user selects Save().

   VTrack *t;
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
   if (!fromSaveAs && mDirManager.GetProjectName() == "")
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
      VTrack *t = iter.First();
      while (t) {
         if (t->GetKind() == VTrack::Wave)
            ((WaveTrack *) t)->Lock();
         t = iter.Next();
      }
   }
   // This renames the project directory, and moves or copies
   // all of our block files over
   bool success = mDirManager.SetProject(projPath, projName, !overwrite);

   // Unlock all blocks in all tracks of the last saved version
   if (mLastSavedTracks && !overwrite) {
      TrackListIterator iter(mLastSavedTracks);
      VTrack *t = iter.First();
      while (t) {
         if (t->GetKind() == VTrack::Wave)
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
   VTrack *t = iter.First();
   while (t) {
      mLastSavedTracks->Add(t->Duplicate());
      t = iter.Next();
   }

   mStatus->SetField(wxString::Format(_("Saved %s"),
                                      (const char *) mFileName), 0);

   mUndoManager.StateSaved();
   return true;
}

void AudacityProject::Import(wxString fileName)
{
   WaveTrack **newTracks;
   int numTracks;

   numTracks =::Import(this, fileName, &newTracks);

   if (numTracks <= 0)
      return;

   SelectNone();

   bool initiallyEmpty = mTracks->IsEmpty();
   double newRate = newTracks[0]->GetRate();

   for (int i = 0; i < numTracks; i++) {
      mTracks->Add(newTracks[i]);
      newTracks[i]->SetSelected(true);
   }

   delete[]newTracks;

   if (initiallyEmpty) {
      mRate = newRate;
      mStatus->SetRate(mRate);
   }

   PushState(wxString::Format(_("Imported '%s'"), fileName.c_str()));

   #if !defined(__WXMAC__) || !defined(__UNIX__)
   wxEvent e;
   OnZoomFit(e);
   #endif

   mTrackPanel->Refresh(false);

   if (initiallyEmpty) {
      wxString name =::TrackNameFromFileName(fileName);
      mFileName =::wxPathOnly(fileName) + wxFILE_SEP_PATH + name + ".aup";
      SetTitle(GetName());
   }

   HandleResize();
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

   int len = fName.Len();
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
}

void AudacityProject::PopState(TrackList * l)
{
   mTracks->Clear(true);
   TrackListIterator iter(l);
   VTrack *t = iter.First();
   while (t) {
      //    printf("Popping track with %d samples\n",
      //           ((WaveTrack *)t)->numSamples);
      //  ((WaveTrack *)t)->Debug();
      mTracks->Add(t->Duplicate());
      t = iter.Next();
   }

   HandleResize();
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

void AudacityProject::ClearClipboard()
{
   TrackListIterator iter(msClipboard);
   VTrack *n = iter.First();
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

   VTrack *n = iter.First();

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

   VTrack *t = iter.First();
   while (t) {
      t->SetSelected(false);
      t = iter.Next();
   }
   mTrackPanel->Refresh(false);
}

void AudacityProject::Rewind(bool shift)
{
   mViewInfo.sel0 = 0;
   if (!shift || mViewInfo.sel1 < mViewInfo.sel0)
      mViewInfo.sel1 = 0;

   TP_ScrollWindow(0);
}

void AudacityProject::SkipEnd(bool shift)
{
   double len = mTracks->GetMaxLen();

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


void AudacityProject::ReReadSettings()
{
   mTrackPanel->ReReadSettings();
}

void AudacityProject::SetStop(bool bStopped)
{
   mTrackPanel->SetStop(bStopped);
}

// TrackPanel callback method
void AudacityProject::TP_DisplayStatusMessage(const char *msg,
                                              int fieldNum)
{
   mStatus->SetField(msg, fieldNum);
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

   if (gAudioIO->IsBusy()) {
      toolbar->SetPlay(false);
      toolbar->SetStop(true);
      toolbar->OnStop();
   } else {
      toolbar->SetPlay(true);
      toolbar->SetStop(false);
      toolbar->OnPlay();
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

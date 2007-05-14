/**********************************************************************

  Audacity: A Digital Audio Editor

  Screenshot.cpp
  
  Dominic Mazzoni

*******************************************************************/

#include "Screenshot.h"

#include <wx/defs.h>
#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dcmemory.h>
#include <wx/dcscreen.h>
#include <wx/dirdlg.h>
#include <wx/frame.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/panel.h>
#include <wx/settings.h>
#include <wx/statusbr.h>
#include <wx/textctrl.h>
#include <wx/timer.h>
#include <wx/window.h>

#include "Project.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "TrackPanel.h"
#include "toolbars/ToolManager.h"
#include "toolbars/ToolBar.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/DeviceToolBar.h"
#include "toolbars/EditToolBar.h"
#include "toolbars/MeterToolBar.h"
#include "toolbars/MixerToolBar.h"
#include "toolbars/SelectionBar.h"
#include "toolbars/ToolsToolBar.h"
#include "toolbars/TranscriptionToolBar.h"
#include "widgets/Ruler.h"

class StaticColorPanel : public wxWindow {
 public:
   StaticColorPanel(wxWindow* parent) :
      wxWindow(parent, -1)
   {
   }

   void SetColour(wxColour c) {
      mColour = c;
   }

   void OnPaint(wxPaintEvent& evt) {
      int width, height;
      GetClientSize(&width, &height);
      wxPaintDC dc(this);
      dc.SetBrush(wxBrush(mColour, wxSOLID));
      dc.SetPen(*wxTRANSPARENT_PEN);
      dc.DrawRectangle(0, 0, width, height);
   }
   DECLARE_EVENT_TABLE();

   wxColour mColour;
};

BEGIN_EVENT_TABLE(StaticColorPanel, wxWindow)
   EVT_PAINT(StaticColorPanel::OnPaint)
END_EVENT_TABLE();

class ScreenFrame : public wxFrame {
 public:
   // constructors and destructors
   ScreenFrame(wxWindow *parent, wxWindowID id);
   virtual ~ScreenFrame();

   virtual bool ProcessEvent(wxEvent& event);

 private:
   wxTopLevelWindow *GetFrontWindow();

   void Capture(wxString basename, wxDC& src,
                wxTopLevelWindow *window,
                int x, int y, int width, int height);
   void CaptureToolbar(int type, wxString name);

   void Populate();
   void PopulateOrExchange(ShuttleGui &S);

   void OnCloseWindow(wxCloseEvent& evt);

   void OnMainWindowSmall(wxCommandEvent& evt);
   void OnMainWindowLarge(wxCommandEvent& evt);
   void OnToggleBackgroundBlue(wxCommandEvent& evt);
   void OnToggleBackgroundWhite(wxCommandEvent& evt);
   void OnCaptureWindowContents(wxCommandEvent& evt);
   void OnCaptureWindowPlus(wxCommandEvent& evt);
   void OnCaptureFullScreen(wxCommandEvent& evt);

   void OnCaptureToolbars(wxCommandEvent& evt);
   void OnCaptureSelectionBar(wxCommandEvent& evt);
   void OnCaptureTools(wxCommandEvent& evt);
   void OnCaptureControl(wxCommandEvent& evt);
   void OnCaptureMixer(wxCommandEvent& evt);
   void OnCaptureMeter(wxCommandEvent& evt);
   void OnCaptureEdit(wxCommandEvent& evt);
   void OnCaptureDevice(wxCommandEvent& evt);
   void OnCaptureTranscription(wxCommandEvent& evt);

   void OnOneSec(wxCommandEvent& evt);
   void OnTenSec(wxCommandEvent& evt);
   void OnOneMin(wxCommandEvent& evt);
   void OnFiveMin(wxCommandEvent& evt);
   void OnOneHour(wxCommandEvent& evt);

   void OnShortTracks(wxCommandEvent& evt);
   void OnMedTracks(wxCommandEvent& evt);
   void OnTallTracks(wxCommandEvent& evt);

   void OnCaptureTrackPanel(wxCommandEvent& evt);
   void OnCaptureRuler(wxCommandEvent& evt);
   void OnCaptureTracks(wxCommandEvent& evt);
   void OnCaptureFirstTrack(wxCommandEvent& evt);
   void OnCaptureSecondTrack(wxCommandEvent& evt);

   void OnDirChoose(wxCommandEvent& evt);

   void AdjustBackground();
   void AddDockedToolbar(AudacityProject *proj,
                         int id, int index);

   wxCheckBox *mDelayCheckBox;
   wxTextCtrl *mDirectoryTextBox;
   wxStatusBar *mStatus;
   wxFrame *mBackground;
   StaticColorPanel *mStaticColorPanel;

   DECLARE_EVENT_TABLE()
};

static ScreenFrame *mFrame = NULL;

void OpenScreenshotTools() {

   if (!mFrame) {
      mFrame = new ScreenFrame(NULL, -1);
   }
   mFrame->Show();
   mFrame->Raise();
}

void CloseScreenshotTools() {

   if (mFrame) {
      mFrame->Destroy();
      mFrame = NULL;
   }
}

enum {
   IdMainWindowSmall = 19200,
   IdMainWindowLarge,

   IdDirectory,
   IdDirChoose,

   IdOneSec,
   IdTenSec,
   IdOneMin,
   IdFiveMin,
   IdOneHour,

   IdShortTracks,
   IdMedTracks,
   IdTallTracks,

   IdDelayCheckBox,

   IdCaptureToolbars,
   IdCaptureSelectionBar,
   IdCaptureTools,
   IdCaptureControl,
   IdCaptureMixer,
   IdCaptureMeter,
   IdCaptureEdit,
   IdCaptureDevice,
   IdCaptureTranscription,

   IdCaptureTrackPanel,
   IdCaptureRuler,
   IdCaptureTracks,
   IdCaptureFirstTrack,
   IdCaptureSecondTrack,

   IdToggleBackgroundBlue,
   IdToggleBackgroundWhite,

   // Put all events that might need delay below:
   IdAllDelayedEvents,

   IdCaptureWindowContents,
   IdCaptureWindowPlus,
   IdCaptureFullScreen,

   IdLastDelayedEvent,
};

ScreenFrame::ScreenFrame(wxWindow * parent, wxWindowID id):
   wxFrame(parent, id, wxT("Screen Capture Frame"),
           wxDefaultPosition, wxDefaultSize,
#if !defined(__WXMSW__)
           wxFRAME_TOOL_WINDOW)|
#endif
           wxSYSTEM_MENU|wxCAPTION|wxCLOSE_BOX)
{
   mDelayCheckBox = NULL;
   mDirectoryTextBox = NULL;

   mStatus = CreateStatusBar();
   mBackground = new wxFrame(NULL, -1, wxT(""),
                             wxPoint(-100, -100),
                             wxSize(1, 1),
                             wxFRAME_NO_TASKBAR);
   mStaticColorPanel = new StaticColorPanel(mBackground);

   Populate();
}

ScreenFrame::~ScreenFrame() {
   delete mBackground;
}

class ScreenFrameTimer : public wxTimer {
public:
   ScreenFrameTimer(ScreenFrame *screenFrame,
                    wxEvent& evt) {
      this->screenFrame = screenFrame;
      this->evt = evt.Clone();
   }

   virtual void Notify() {
      evt->SetEventObject(NULL);
      screenFrame->ProcessEvent(*evt);
      delete evt;
      delete this;
   }

   ScreenFrame *screenFrame;
   wxEvent *evt;
};

bool ScreenFrame::ProcessEvent(wxEvent& event) {
   int id = event.GetId();
   if (mDelayCheckBox &&
       mDelayCheckBox->GetValue() &&
       event.IsCommandEvent() &&
       event.GetEventType() == wxEVT_COMMAND_BUTTON_CLICKED &&
       id >= IdAllDelayedEvents && id <= IdLastDelayedEvent &&
       event.GetEventObject() != NULL) {
      ScreenFrameTimer *timer = new ScreenFrameTimer(this, event);
      timer->Start(5000, true);
      return true;
   }
   else {
      if (event.IsCommandEvent() &&
          event.GetEventObject() == NULL) {
         event.SetEventObject(this);
      }
      return wxFrame::ProcessEvent(event);
   }
}

wxTopLevelWindow *ScreenFrame::GetFrontWindow() {
   wxWindow *front = NULL;
   wxWindow *proj = wxGetTopLevelParent(GetActiveProject());

   // This is kind of an odd hack.  There's no method to enumerate all
   // possible windows, so we search the whole screen for any windows
   // that are not this one and not the frontmost Audacity project and
   // if we find anything, we assume that's the dialog the user wants
   // to capture.

   int width, height, x, y;
   wxDisplaySize(&width, &height);
   for(x = 0; x < width; x += 50) {
      for(y = 0; y < height; y += 50) {
         wxWindow *win = wxFindWindowAtPoint(wxPoint(x, y));
         if (win) {
            win = wxGetTopLevelParent(win);
            if (win != this && win != proj && win != mBackground) {
               front = win;
               break;
            }
         }
      }
   }

   if (!front || !front->IsTopLevel()) {
      return (wxTopLevelWindow *)proj;
   }
   return (wxTopLevelWindow *)front;
}

void ScreenFrame::Capture(wxString basename, wxDC& src,
                          wxTopLevelWindow *window,
                          int x, int y, int width, int height)
{
   wxFileName prefixPath;
   prefixPath.AssignDir(mDirectoryTextBox->GetValue());
   wxString prefix = prefixPath.GetPath
      (wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR);

   wxString filename;
   int i = 0;
   do {
      filename.Printf(wxT("%s%s%03d.png"),
                      prefix.c_str(), basename.c_str(), i);
      i++;
   } while (::wxFileExists(filename));

   // Try to make sure the window raises and activates
   window->Raise();
   wxYield();

   wxBitmap bitmap(width, height);
   wxMemoryDC memDC;
   memDC.SelectObject(bitmap);
   memDC.Blit(0, 0, width, height, &src, x, y, wxCOPY);
   wxImage image = bitmap.ConvertToImage();
   if (image.SaveFile(filename)) {
      mStatus->SetStatusText(_("Saved ") + filename);
   }
   else {
      wxMessageBox(_("Error trying to save file: ") + filename);
   }

   ::wxBell();

   Raise();
}

void ScreenFrame::CaptureToolbar(int type, wxString name)
{
   AudacityProject *proj = GetActiveProject();
   ToolManager *man = proj->mToolManager;

   bool visible = man->IsVisible(type);
   if (!visible) {
      man->ShowHide(type);
   }

   wxWindow *w = man->GetToolBar(type);
   int width, height;
   w->GetClientSize(&width, &height);

   wxClientDC dc(w);
   Capture(name, dc, proj, 0, 0, width, height);

   if (!visible) {
      man->ShowHide(type);
      Raise();
   }
}

void ScreenFrame::Populate()
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

void ScreenFrame::PopulateOrExchange(ShuttleGui &S)
{
   S.StartPanel(0)->SetBackgroundColour(
      wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
   S.SetBorder(2);
   S.StartVerticalLay(1);

   S.StartStatic(wxT("Choose location to save files"), true);
   S.StartMultiColumn(2, wxEXPAND);
   S.SetStretchyCol( 1 );// Column 1 is stretchy...
   {
      wxString dir =
         gPrefs->Read(wxT("/ScreenshotPath"),
                      wxFileName::GetHomeDir());
      mDirectoryTextBox =
         S.Id(IdDirectory).AddTextBox(wxT("Save images here:"),
                                      dir, 30);
      S.Id(IdDirChoose).AddButton(wxT("Choose Directory"));
   }
   S.EndMultiColumn();
   S.EndStatic();


   S.StartStatic(wxT("Capture entire window or screen"), true);
   S.StartHorizontalLay();
   {
      S.Id(IdMainWindowSmall).AddButton(wxT("Resize Small"));
      S.Id(IdMainWindowLarge).AddButton(wxT("Resize Large"));
      S.Id(IdToggleBackgroundBlue).AddButton(wxT("Blue Bkgnd"));
      S.Id(IdToggleBackgroundWhite).AddButton(wxT("White Bkgnd"));
   }
   S.EndHorizontalLay();
   S.StartHorizontalLay();
   {
      S.Id(IdCaptureWindowContents).AddButton(wxT("Capture Window Only"));
      S.Id(IdCaptureWindowPlus).AddButton(wxT("Capture Window Plus"));
      S.Id(IdCaptureFullScreen).AddButton(wxT("Capture Full Screen"));
   }
   S.EndHorizontalLay();
   S.StartHorizontalLay();
   {
      mDelayCheckBox = S.Id(IdDelayCheckBox).AddCheckBox
         (wxT("Wait 5 seconds and capture frontmost window/dialog"),
          wxT("false"));
   }
   S.EndHorizontalLay();
   S.EndStatic();

   S.StartStatic(wxT("Capture part of a project window"), true);
   S.StartHorizontalLay();
   {
      S.Id(IdCaptureToolbars).AddButton(wxT("All Toolbars"));
      S.Id(IdCaptureSelectionBar).AddButton(wxT("SelectionBar"));
      S.Id(IdCaptureTools).AddButton(wxT("Tools"));
      S.Id(IdCaptureControl).AddButton(wxT("Control"));
   }
   S.EndHorizontalLay();
   S.StartHorizontalLay();
   {
      S.Id(IdCaptureMixer).AddButton(wxT("Mixer"));
      S.Id(IdCaptureMeter).AddButton(wxT("Meter"));
      S.Id(IdCaptureEdit).AddButton(wxT("Edit"));
      S.Id(IdCaptureDevice).AddButton(wxT("Device"));
      S.Id(IdCaptureTranscription).AddButton(wxT("Transcription"));
   }
   S.EndHorizontalLay();
   S.StartHorizontalLay();
   {
      S.Id(IdCaptureTrackPanel).AddButton(wxT("Track Panel"));
      S.Id(IdCaptureRuler).AddButton(wxT("Ruler"));
      S.Id(IdCaptureTracks).AddButton(wxT("Tracks"));
      S.Id(IdCaptureFirstTrack).AddButton(wxT("First Track"));
      S.Id(IdCaptureSecondTrack).AddButton(wxT("Second Track"));
   }
   S.EndHorizontalLay();

   S.EndStatic();

   S.StartStatic(wxT("Scale"), true);
   S.StartHorizontalLay();
   {
      S.Id(IdOneSec).AddButton(wxT("One Sec"));
      S.Id(IdTenSec).AddButton(wxT("Ten Sec"));
      S.Id(IdOneMin).AddButton(wxT("One Min"));
      S.Id(IdFiveMin).AddButton(wxT("Five Min"));
      S.Id(IdOneHour).AddButton(wxT("One Hour"));
   }
   S.EndHorizontalLay();
   S.StartHorizontalLay();
   {
      S.Id(IdShortTracks).AddButton(wxT("Short Tracks"));
      S.Id(IdMedTracks).AddButton(wxT("Medium Tracks"));
      S.Id(IdTallTracks).AddButton(wxT("Tall Tracks"));
   }
   S.EndHorizontalLay();
   S.EndStatic();

   S.EndVerticalLay();
   S.EndPanel();

   Layout();
   Fit();
   SetMinSize(GetSize());

   int top = 0;
   #ifdef __WXMAC__
   top += 20;
   #endif

   int width, height;
   GetSize(&width, &height);
   int displayWidth, displayHeight;
   wxDisplaySize(&displayWidth, &displayHeight);

   if (width > 100)
      Move(displayWidth - width - 16, top + 16);
   else
      CentreOnParent(); 
}

BEGIN_EVENT_TABLE(ScreenFrame, wxFrame)
   EVT_CLOSE(ScreenFrame::OnCloseWindow)

   EVT_BUTTON(IdMainWindowSmall,        ScreenFrame::OnMainWindowSmall)
   EVT_BUTTON(IdMainWindowLarge,        ScreenFrame::OnMainWindowLarge)
   EVT_BUTTON(IdToggleBackgroundBlue,   ScreenFrame::OnToggleBackgroundBlue)
   EVT_BUTTON(IdToggleBackgroundWhite,  ScreenFrame::OnToggleBackgroundWhite)
   EVT_BUTTON(IdCaptureWindowContents,  ScreenFrame::OnCaptureWindowContents)
   EVT_BUTTON(IdCaptureWindowPlus,      ScreenFrame::OnCaptureWindowPlus)

   EVT_BUTTON(IdCaptureFullScreen,      ScreenFrame::OnCaptureFullScreen)

   EVT_BUTTON(IdCaptureToolbars,        ScreenFrame::OnCaptureToolbars)
   EVT_BUTTON(IdCaptureSelectionBar,    ScreenFrame::OnCaptureSelectionBar)
   EVT_BUTTON(IdCaptureTools,           ScreenFrame::OnCaptureTools)
   EVT_BUTTON(IdCaptureControl,         ScreenFrame::OnCaptureControl)
   EVT_BUTTON(IdCaptureMixer,           ScreenFrame::OnCaptureMixer)
   EVT_BUTTON(IdCaptureMeter,           ScreenFrame::OnCaptureMeter)
   EVT_BUTTON(IdCaptureEdit,            ScreenFrame::OnCaptureEdit)
   EVT_BUTTON(IdCaptureDevice,          ScreenFrame::OnCaptureDevice)
   EVT_BUTTON(IdCaptureTranscription,   ScreenFrame::OnCaptureTranscription)

   EVT_BUTTON(IdOneSec,                 ScreenFrame::OnOneSec)
   EVT_BUTTON(IdTenSec,                 ScreenFrame::OnTenSec)
   EVT_BUTTON(IdOneMin,                 ScreenFrame::OnOneMin)
   EVT_BUTTON(IdFiveMin,                ScreenFrame::OnFiveMin)
   EVT_BUTTON(IdOneHour,                ScreenFrame::OnOneHour)

   EVT_BUTTON(IdShortTracks,            ScreenFrame::OnShortTracks)
   EVT_BUTTON(IdMedTracks,              ScreenFrame::OnMedTracks)
   EVT_BUTTON(IdTallTracks,             ScreenFrame::OnTallTracks)

   EVT_BUTTON(IdCaptureTrackPanel,      ScreenFrame::OnCaptureTrackPanel)
   EVT_BUTTON(IdCaptureRuler,           ScreenFrame::OnCaptureRuler)
   EVT_BUTTON(IdCaptureTracks,          ScreenFrame::OnCaptureTracks)
   EVT_BUTTON(IdCaptureFirstTrack,      ScreenFrame::OnCaptureFirstTrack)
   EVT_BUTTON(IdCaptureSecondTrack,     ScreenFrame::OnCaptureSecondTrack)

   EVT_BUTTON(IdDirChoose,              ScreenFrame::OnDirChoose)

END_EVENT_TABLE();

void ScreenFrame::OnCloseWindow(wxCloseEvent & evt)
{
   mFrame = NULL;
   Destroy();
}

void ScreenFrame::AddDockedToolbar(AudacityProject *proj,
                                   int id, int index) {
   ToolManager *man = proj->mToolManager;
   ToolDock *dock = man->GetTopDock();
   ToolBar *bar = man->GetToolBar(id);

   man->ShowHide(id);
   if (!bar->IsDocked()) {
      wxWindow *parent = bar->GetParent();
      dock->Dock(bar, index);
      parent->Destroy();
   }
}

void ScreenFrame::OnMainWindowSmall(wxCommandEvent& evt)
{
   int top = 20;

   AudacityProject *proj = GetActiveProject();
   proj->SetSize(16, 16 + top, 680, 450);
   ToolManager *man = proj->mToolManager;
   ToolDock *dock = man->GetTopDock();

   int i;
   for(i = 0; i < ToolBarCount; i++) {
      if (man->IsVisible(i))
         man->ShowHide(i);
   }

   dock->LayoutToolBars();

   ToolBar *meter = man->GetToolBar(MeterBarID);
   int width, height;
   meter->GetSize(&width, &height);

   #ifdef __WXMAC__
   meter->SetSize(334, height);
   #else
   meter->SetSize(260, height);
   #endif

   AddDockedToolbar(proj, ToolsBarID, 0);
   AddDockedToolbar(proj, ControlBarID, 1);
   AddDockedToolbar(proj, MeterBarID, 2);
   AddDockedToolbar(proj, MixerBarID, 3);
   AddDockedToolbar(proj, EditBarID, 4);

   dock->LayoutToolBars();

   man->ShowHide(SelectionBarID);

   AdjustBackground();
}

void ScreenFrame::OnMainWindowLarge(wxCommandEvent& evt)
{
   int top = 20;

   AudacityProject *proj = GetActiveProject();
   proj->SetSize(16, 16 + top, 900, 600);
   ToolManager *man = proj->mToolManager;
   ToolDock *dock = man->GetTopDock();
   int i;
   for(i = 0; i < ToolBarCount; i++) {
      if (man->IsVisible(i))
         man->ShowHide(i);
   }

   dock->LayoutToolBars();

   ToolBar *meter = man->GetToolBar(MeterBarID);
   int width, height;
   meter->GetSize(&width, &height);

   #ifdef __WXMAC__
   meter->SetSize(554, height);
   #else
   meter->SetSize(480, height);
   #endif

   AddDockedToolbar(proj, ToolsBarID, 0);
   AddDockedToolbar(proj, ControlBarID, 1);
   AddDockedToolbar(proj, MeterBarID, 2);
   AddDockedToolbar(proj, EditBarID, 3);
   AddDockedToolbar(proj, MixerBarID, 4);
   AddDockedToolbar(proj, DeviceBarID, 5);

   dock->LayoutToolBars();

   man->ShowHide(SelectionBarID);

   AdjustBackground();
}

void ScreenFrame::AdjustBackground()
{
   int x, y;
   int width, height;
   GetFrontWindow()->GetPosition(&x, &y);
   GetFrontWindow()->GetSize(&width, &height);

   #ifdef __WXMAC__
   int b = 20;
   int extra = 0;
   #else
   int b = 12;
   int extra = 20;
   #endif

   int b2 = b * 2;

   mBackground->SetSize(x - b, y - b - extra, width + b2, height + b2 + extra);
   mStaticColorPanel->SetSize(0, 0, width + b2, height + b2 + extra);
   mStaticColorPanel->Show();
}

void ScreenFrame::OnToggleBackgroundBlue(wxCommandEvent& evt)
{
   if (mBackground->IsShown()) {
      mBackground->Hide();
      return;
   }

   mStaticColorPanel->SetColour(wxColour(51, 102, 153));
   wxTopLevelWindow *front = GetFrontWindow();
   AdjustBackground();
   mBackground->Show();
   front->Raise();
   Raise();
}

void ScreenFrame::OnToggleBackgroundWhite(wxCommandEvent& evt)
{
   if (mBackground->IsShown()) {
      mBackground->Hide();
      return;
   }

   mStaticColorPanel->SetColour(wxColour(255, 255, 255));
   wxTopLevelWindow *front = GetFrontWindow();
   AdjustBackground();
   mBackground->Show();
   front->Raise();
   Raise();
}

void ScreenFrame::OnCaptureWindowContents(wxCommandEvent& evt)
{
   wxTopLevelWindow *w = GetFrontWindow();
   int width, height;
   w->GetClientSize(&width, &height);

   wxString basename = wxT("window");
   if (w != GetActiveProject() && w->GetTitle() != wxT("")) {
      basename += (wxT("-") + w->GetTitle() + wxT("-"));
   }

   wxClientDC dc(w);
   Capture(basename, dc, w, 0, 0, width, height);
}

void ScreenFrame::OnCaptureWindowPlus(wxCommandEvent& evt)
{
   wxTopLevelWindow *w = GetFrontWindow();
   int x, y;
   int width, height;
   w->GetPosition(&x, &y);
   w->GetSize(&width, &height);

   wxScreenDC screenDC;

   wxString basename = wxT("windowplus");
   if (w != GetActiveProject() && w->GetTitle() != wxT("")) {
      basename += (wxT("-") + w->GetTitle() + wxT("-"));
   }

   if (mBackground->IsShown()) {
      AdjustBackground();
      mBackground->Raise();
      wxYield();
      w->Raise();
      wxYield();
      wxMilliSleep(200);
      wxYield();      
   }

   int extraheight = 0;
   #ifndef __WXMAC__
   extraheight += 20;
   #endif

   if (x > 16 && y > 16) {
      Capture(basename, screenDC, w,
              x - 16, y - 16, width + 32, height + 32 + extraheight);
   }
   else {
      Capture(basename, screenDC, w,
              0, 0, width + x + 16, height + y + 16 + extraheight);
   }
}

void ScreenFrame::OnCaptureFullScreen(wxCommandEvent& evt)
{
   int width, height;
   wxDisplaySize(&width, &height);

   wxScreenDC screenDC;
   Capture(wxT("fullscreen"), screenDC, GetFrontWindow(),
           0, 0, width, height);
}

void ScreenFrame::OnCaptureToolbars(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   wxRect r = proj->mToolManager->GetTopDock()->GetRect();

   wxClientDC dc(proj);
   Capture(wxT("toolbars"), dc, proj, r.x, r.y, r.width, r.height);
}

void ScreenFrame::OnCaptureSelectionBar(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   wxRect r = proj->mToolManager->GetBotDock()->GetRect();

   wxClientDC dc(proj);
   Capture(wxT("selectionbar"), dc, proj, r.x, r.y, r.width, r.height);
}

void ScreenFrame::OnCaptureTools(wxCommandEvent& evt)
{
   CaptureToolbar(ToolsBarID, wxT("tools"));
}

void ScreenFrame::OnCaptureControl(wxCommandEvent& evt)
{
   CaptureToolbar(ControlBarID, wxT("control"));
}

void ScreenFrame::OnCaptureMixer(wxCommandEvent& evt)
{
   CaptureToolbar(MixerBarID, wxT("mixer"));
}

void ScreenFrame::OnCaptureMeter(wxCommandEvent& evt)
{
   CaptureToolbar(MeterBarID, wxT("meter"));
}

void ScreenFrame::OnCaptureEdit(wxCommandEvent& evt)
{
   CaptureToolbar(EditBarID, wxT("edit"));
}

void ScreenFrame::OnCaptureDevice(wxCommandEvent& evt)
{
   CaptureToolbar(DeviceBarID, wxT("device"));
}

void ScreenFrame::OnCaptureTranscription(wxCommandEvent& evt)
{
   CaptureToolbar(TranscriptionBarID, wxT("transcription"));
}

void ScreenFrame::OnOneSec(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 1.0;
   proj->RedrawProject();
}

void ScreenFrame::OnTenSec(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 10.0;
   proj->RedrawProject();
}

void ScreenFrame::OnOneMin(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 60.0;
   proj->RedrawProject();
}

void ScreenFrame::OnFiveMin(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 300.0;
   proj->RedrawProject();
}

void ScreenFrame::OnOneHour(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 3600.0;
   proj->RedrawProject();
}

void ScreenFrame::OnShortTracks(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackListIterator iter(proj->GetTracks());
   for (Track * t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Wave) {
         if (proj->GetTracks()->GetLink(t)) {
            t->SetHeight(48);
         }
         else {
            t->SetHeight(24);
         }
      }
   }
   proj->RedrawProject();
}

void ScreenFrame::OnMedTracks(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackListIterator iter(proj->GetTracks());
   for (Track * t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Wave) {
         if (proj->GetTracks()->GetLink(t)) {
            t->SetHeight(60);
         }
         else {
            t->SetHeight(120);
         }
      }
   }
   proj->RedrawProject();
}

void ScreenFrame::OnTallTracks(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackListIterator iter(proj->GetTracks());
   for (Track * t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Wave) {
         if (proj->GetTracks()->GetLink(t)) {
            t->SetHeight(85);
         }
         else {
            t->SetHeight(170);
         }
      }
   }
   proj->RedrawProject();
}

void ScreenFrame::OnCaptureTrackPanel(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   AdornedRulerPanel *ruler = panel->mRuler;
   int width, height, rulerheight;
   panel->GetClientSize(&width, &height);
   rulerheight = ruler->GetRulerHeight();

   wxClientDC dc(panel);
   Capture(wxT("trackpanel"), dc, proj, 0, -rulerheight, width, height);
}

void ScreenFrame::OnCaptureRuler(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   AdornedRulerPanel *ruler = panel->mRuler;
   int width, height;
   ruler->GetClientSize(&width, &height);
   height = ruler->GetRulerHeight();

   wxClientDC dc(ruler);
   Capture(wxT("ruler"), dc, proj, 0, 0, width, height);
}

void ScreenFrame::OnCaptureTracks(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   int width, height;
   panel->GetClientSize(&width, &height);

   wxClientDC dc(panel);
   Capture(wxT("tracks"), dc, proj, 0, 0, width, height);
}

void ScreenFrame::OnCaptureFirstTrack(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   TrackListIterator iter(proj->GetTracks());
   Track * t = iter.First();
   if (!t)
      return;
   wxRect r = panel->FindTrackRect(t, true);

   int width, height;
   panel->GetClientSize(&width, &height);

   wxClientDC dc(panel);
   Capture(wxT("firsttrack"), dc, proj, 0, r.y - 3, width, r.height + 6);
}

void ScreenFrame::OnCaptureSecondTrack(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   TrackListIterator iter(proj->GetTracks());
   Track * t = iter.First();
   if (!t)
      return;
   if (t->GetLinked())
      t = iter.Next();
   t = iter.Next();
   if (!t)
      return;
   wxRect r = panel->FindTrackRect(t, true);

   int width, height;
   panel->GetClientSize(&width, &height);

   wxClientDC dc(panel);
   Capture(wxT("secondtrack"), dc, proj, 0, r.y - 3, width, r.height + 6);
}

void ScreenFrame::OnDirChoose(wxCommandEvent& evt)
{
   wxString current = mDirectoryTextBox->GetValue();

   wxDirDialog dlog(this, 
                    wxT("Choose a location to save screenshot images"),
                    current);

   dlog.ShowModal();
   if (dlog.GetPath() != wxT("")) {
      wxFileName tmpDirPath;
      tmpDirPath.AssignDir(dlog.GetPath());
      mDirectoryTextBox->SetValue
         (tmpDirPath.GetPath(wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR));
      gPrefs->Write(wxT("/ScreenshotPath"),
         tmpDirPath.GetPath(wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR));
   }
}

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

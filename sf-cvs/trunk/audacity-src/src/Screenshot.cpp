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
#include <wx/tglbtn.h>
#include <wx/window.h>

#include "AudacityApp.h"
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

////////////////////////////////////////////////////////////////////////////////

class ScreenFrame:public wxFrame
{
 public:
   // constructors and destructors
   ScreenFrame(wxWindow *parent, wxWindowID id);
   virtual ~ScreenFrame();

   virtual bool ProcessEvent(wxEvent & e);

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui &S);

   wxTopLevelWindow *GetFrontWindow();

   wxRect GetBackgroundRect();

   void Capture(wxString basename,
                wxWindow *window,
                int x, int y, int width, int height,
                bool bg = false);
   void CaptureToolbar(int type, wxString name);

   void OnCloseWindow(wxCloseEvent & e);
   void OnUIUpdate(wxUpdateUIEvent & e);

   void OnDirChoose(wxCommandEvent & e);

   void OnMainWindowSmall(wxCommandEvent & e);
   void OnMainWindowLarge(wxCommandEvent & e);
   void OnToggleBackgroundBlue(wxCommandEvent & e);
   void OnToggleBackgroundWhite(wxCommandEvent & e);
   void OnCaptureWindowContents(wxCommandEvent & e);
   void OnCaptureFullWindow(wxCommandEvent & e);
   void OnCaptureWindowPlus(wxCommandEvent & e);
   void OnCaptureFullScreen(wxCommandEvent & e);

   void OnCaptureToolbars(wxCommandEvent & e);
   void OnCaptureSelectionBar(wxCommandEvent & e);
   void OnCaptureTools(wxCommandEvent & e);
   void OnCaptureControl(wxCommandEvent & e);
   void OnCaptureMixer(wxCommandEvent & e);
   void OnCaptureMeter(wxCommandEvent & e);
   void OnCaptureEdit(wxCommandEvent & e);
   void OnCaptureDevice(wxCommandEvent & e);
   void OnCaptureTranscription(wxCommandEvent & e);

   void OnOneSec(wxCommandEvent & e);
   void OnTenSec(wxCommandEvent & e);
   void OnOneMin(wxCommandEvent & e);
   void OnFiveMin(wxCommandEvent & e);
   void OnOneHour(wxCommandEvent & e);

   void OnShortTracks(wxCommandEvent & e);
   void OnMedTracks(wxCommandEvent & e);
   void OnTallTracks(wxCommandEvent & e);

   void OnCaptureTrackPanel(wxCommandEvent & e);
   void OnCaptureRuler(wxCommandEvent & e);
   void OnCaptureTracks(wxCommandEvent & e);
   void OnCaptureFirstTrack(wxCommandEvent & e);
   void OnCaptureSecondTrack(wxCommandEvent & e);

   wxCheckBox *mDelayCheckBox;
   wxTextCtrl *mDirectoryTextBox;
   wxToggleButton *mBlue;
   wxToggleButton *mWhite;
   wxStatusBar *mStatus;
   bool mBackground;
   wxColour mBackColor;

   DECLARE_EVENT_TABLE()
};

static ScreenFrame *mFrame = NULL;

////////////////////////////////////////////////////////////////////////////////

static void Yield()
{
   int cnt;
   for (cnt = 10; cnt && !wxGetApp().Yield(true); cnt--) {
      wxMilliSleep(10);
   }
   wxMilliSleep(200);
   for (cnt = 10; cnt && !wxGetApp().Yield(true); cnt--) {
      wxMilliSleep(10);
   }
}

void OpenScreenshotTools()
{
   if (!mFrame) {
      mFrame = new ScreenFrame(NULL, -1);
   }
   mFrame->Show();
   mFrame->Raise();
}

void CloseScreenshotTools()
{
   if (mFrame) {
      mFrame->Destroy();
      mFrame = NULL;
   }
}

////////////////////////////////////////////////////////////////////////////////

class ScreenFrameTimer:public wxTimer
{
 public:
   ScreenFrameTimer(ScreenFrame *frame,
                    wxEvent & e)
   {
      screenFrame = frame;
      evt = e.Clone();
   }

   virtual void Notify()
   {
      evt->SetEventObject(NULL);
      screenFrame->ProcessEvent(*evt);
      delete evt;
      delete this;
   }

 private:
   ScreenFrame *screenFrame;
   wxEvent *evt;
};

////////////////////////////////////////////////////////////////////////////////

enum
{
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
   IdCaptureFullWindow,
   IdCaptureWindowPlus,
   IdCaptureFullScreen,

   IdLastDelayedEvent,
};

BEGIN_EVENT_TABLE(ScreenFrame, wxFrame)
   EVT_CLOSE(ScreenFrame::OnCloseWindow)

   EVT_UPDATE_UI(IdCaptureFullScreen,   ScreenFrame::OnUIUpdate)

   EVT_BUTTON(IdMainWindowSmall,        ScreenFrame::OnMainWindowSmall)
   EVT_BUTTON(IdMainWindowLarge,        ScreenFrame::OnMainWindowLarge)
   EVT_TOGGLEBUTTON(IdToggleBackgroundBlue,   ScreenFrame::OnToggleBackgroundBlue)
   EVT_TOGGLEBUTTON(IdToggleBackgroundWhite,  ScreenFrame::OnToggleBackgroundWhite)
   EVT_BUTTON(IdCaptureWindowContents,  ScreenFrame::OnCaptureWindowContents)
   EVT_BUTTON(IdCaptureFullWindow,      ScreenFrame::OnCaptureFullWindow)
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

ScreenFrame::ScreenFrame(wxWindow * parent, wxWindowID id)
:  wxFrame(parent, id, _("Screen Capture Frame"),
           wxDefaultPosition, wxDefaultSize,
#if !defined(__WXMSW__)
           wxFRAME_TOOL_WINDOW|
#else
           wxSTAY_ON_TOP|
#endif
           wxSYSTEM_MENU|wxCAPTION|wxCLOSE_BOX)
{
   mDelayCheckBox = NULL;
   mDirectoryTextBox = NULL;
   mBackground = false;

   mStatus = CreateStatusBar();

   // Reset the toolbars to a known state
   GetActiveProject()->mToolManager->Reset();

   Populate();
}

ScreenFrame::~ScreenFrame()
{
}

void ScreenFrame::Populate()
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

void ScreenFrame::PopulateOrExchange(ShuttleGui & S)
{
   wxPanel *p = S.StartPanel();
   {
      S.SetBorder(3);

      S.StartStatic(_("Choose location to save files"));
      {
         S.StartMultiColumn(3, wxEXPAND);
         {
            S.SetStretchyCol(1);

            wxString dir =
               gPrefs->Read(wxT("/ScreenshotPath"),
                            wxFileName::GetHomeDir());
            mDirectoryTextBox =
               S.Id(IdDirectory).AddTextBox(_("Save images to:"),
                                            dir, 30);
            S.Id(IdDirChoose).AddButton(_("Choose..."));
         }
         S.EndMultiColumn();
      }
      S.EndStatic();

      S.StartStatic(_("Capture entire window or screen"));
      {
         S.StartHorizontalLay();
         {
            S.Id(IdMainWindowSmall).AddButton(_("Resize Small"));
            S.Id(IdMainWindowLarge).AddButton(_("Resize Large"));
            mBlue = new wxToggleButton(p,
                                       IdToggleBackgroundBlue,
                                       _("Blue Bkgnd"));
            S.AddWindow(mBlue);
            mWhite = new wxToggleButton(p,
                                        IdToggleBackgroundWhite,
                                        _("White Bkgnd"));
            S.AddWindow(mWhite);
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay();
         {
            S.Id(IdCaptureWindowContents).AddButton(_("Capture Window Only"));
            S.Id(IdCaptureFullWindow).AddButton(_("Capture Full Window"));
            S.Id(IdCaptureWindowPlus).AddButton(_("Capture Window Plus"));
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay();
         {
            S.Id(IdCaptureFullScreen).AddButton(_("Capture Full Screen"));
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay();
         {
            mDelayCheckBox = S.Id(IdDelayCheckBox).AddCheckBox
               (_("Wait 5 seconds and capture frontmost window/dialog"),
                _("false"));
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartStatic(_("Capture part of a project window"));
      {
         S.StartHorizontalLay();
         {
            S.Id(IdCaptureToolbars).AddButton(_("All Toolbars"));
            S.Id(IdCaptureSelectionBar).AddButton(_("SelectionBar"));
            S.Id(IdCaptureTools).AddButton(_("Tools"));
            S.Id(IdCaptureControl).AddButton(_("Control"));
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay();
         {
            S.Id(IdCaptureMixer).AddButton(_("Mixer"));
            S.Id(IdCaptureMeter).AddButton(_("Meter"));
            S.Id(IdCaptureEdit).AddButton(_("Edit"));
            S.Id(IdCaptureDevice).AddButton(_("Device"));
            S.Id(IdCaptureTranscription).AddButton(_("Transcription"));
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay();
         {
            S.Id(IdCaptureTrackPanel).AddButton(_("Track Panel"));
            S.Id(IdCaptureRuler).AddButton(_("Ruler"));
            S.Id(IdCaptureTracks).AddButton(_("Tracks"));
            S.Id(IdCaptureFirstTrack).AddButton(_("First Track"));
            S.Id(IdCaptureSecondTrack).AddButton(_("Second Track"));
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartStatic(_("Scale"));
      {
         S.StartHorizontalLay();
         {
            S.Id(IdOneSec).AddButton(_("One Sec"));
            S.Id(IdTenSec).AddButton(_("Ten Sec"));
            S.Id(IdOneMin).AddButton(_("One Min"));
            S.Id(IdFiveMin).AddButton(_("Five Min"));
            S.Id(IdOneHour).AddButton(_("One Hour"));
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay();
         {
            S.Id(IdShortTracks).AddButton(_("Short Tracks"));
            S.Id(IdMedTracks).AddButton(_("Medium Tracks"));
            S.Id(IdTallTracks).AddButton(_("Tall Tracks"));
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();
   }
   S.EndPanel();

   Layout();
   Fit();
   SetMinSize(GetSize());

   int top = 0;
#ifdef __WXMAC__
   // Allow for Mac menu bar
   top += 20;
#endif

   int width, height;
   GetSize(&width, &height);
   int displayWidth, displayHeight;
   wxDisplaySize(&displayWidth, &displayHeight);

   if (width > 100) {
      Move(displayWidth - width - 16, top + 16);
   }
   else {
      CentreOnParent(); 
   }

   SetIcon(GetActiveProject()->GetIcon());
}

bool ScreenFrame::ProcessEvent(wxEvent & e)
{
   int id = e.GetId();

   if (mDelayCheckBox &&
       mDelayCheckBox->GetValue() &&
       e.IsCommandEvent() &&
       e.GetEventType() == wxEVT_COMMAND_BUTTON_CLICKED &&
       id >= IdAllDelayedEvents && id <= IdLastDelayedEvent &&
       e.GetEventObject() != NULL) {
      ScreenFrameTimer *timer = new ScreenFrameTimer(this, e);
      timer->Start(5000, true);
      return true;
   }

   if (e.IsCommandEvent() && e.GetEventObject() == NULL) {
      e.SetEventObject(this);
   }
   return wxFrame::ProcessEvent(e);
}

wxTopLevelWindow *ScreenFrame::GetFrontWindow()
{
   wxWindow *front = NULL;
   wxWindow *proj = wxGetTopLevelParent(GetActiveProject());

   // This is kind of an odd hack.  There's no method to enumerate all
   // possible windows, so we search the whole screen for any windows
   // that are not this one and not the frontmost Audacity project and
   // if we find anything, we assume that's the dialog the user wants
   // to capture.

   int width, height, x, y;
   wxDisplaySize(&width, &height);
   for (x = 0; x < width; x += 50) {
      for (y = 0; y < height; y += 50) {
         wxWindow *win = wxFindWindowAtPoint(wxPoint(x, y));
         if (win) {
            win = wxGetTopLevelParent(win);
            if (win != this && win != proj) {
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

wxRect ScreenFrame::GetBackgroundRect()
{
   wxRect r;

   r.x = 16;
   r.y = 16;
   r.width = r.x * 2;
   r.height = r.y * 2;

   return r;
}

void ScreenFrame::Capture(wxString basename,
                          wxWindow *window,
                          int x, int y, int width, int height,
                          bool bg)
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

   Hide();
   Yield();

   int screenW, screenH;
   wxDisplaySize(&screenW, &screenH);
   wxBitmap full(screenW, screenH);

   wxScreenDC screenDC;
   wxMemoryDC fullDC;

   // We grab the whole screen image since there seems to be a problem with
   // using non-zero source coordinates on OSX.  (as of wx2.8.9)
   fullDC.SelectObject(full);
   fullDC.Blit(0, 0, screenW, screenH, &screenDC, 0, 0);
   fullDC.SelectObject(wxNullBitmap);

   wxRect r(x, y, width, height);

   // Ensure within bounds (x/y are negative on Windows when maximized)
   r.Intersect(wxRect(0, 0, screenW, screenH));

   // Convert to screen coordinates if needed
   if (window && window->GetParent() && !window->IsTopLevel()) {
      r.SetPosition(window->GetParent()->ClientToScreen(r.GetPosition()));
   }

   // Extract the actual image
   wxBitmap part = full.GetSubBitmap(r);

   // Add a background
   if (bg && mBackground) {
      wxRect b = GetBackgroundRect();

      wxBitmap back(width + b.width, height + b.height);
      fullDC.SelectObject(back);

      fullDC.SetBackground(wxBrush(mBackColor, wxSOLID));
      fullDC.Clear();

      fullDC.DrawBitmap(part, b.x, b.y);
      fullDC.SelectObject(wxNullBitmap);

      part = back;
   }

   // Save the final image
   wxImage image = part.ConvertToImage();
   if (image.SaveFile(filename)) {
      mStatus->SetStatusText(_("Saved ") + filename);
   }
   else {
      wxMessageBox(_("Error trying to save file: ") + filename);
   }

   ::wxBell();

   Show();
}

void ScreenFrame::CaptureToolbar(int type, wxString name)
{
   AudacityProject *proj = GetActiveProject();
   ToolManager *man = proj->mToolManager;

   bool visible = man->IsVisible(type);
   if (!visible) {
      man->ShowHide(type);
      Yield();
   }

   wxWindow *w = man->GetToolBar(type);
   int x = 0, y = 0;
   int width, height;

   w->ClientToScreen(&x, &y);
   w->GetParent()->ScreenToClient(&x, &y);
   w->GetClientSize(&width, &height);

   Capture(name, w, x, y, width, height);

   if (!visible) {
      man->ShowHide(type);
      Raise();
   }
}

void ScreenFrame::OnCloseWindow(wxCloseEvent & e)
{
   mFrame = NULL;
   Destroy();
}

void ScreenFrame::OnUIUpdate(wxUpdateUIEvent & e)
{
   wxTopLevelWindow *top = GetFrontWindow();
   bool needupdate = false;
   bool enable = false;

   if ((!top || top->IsIconized()) && mDirectoryTextBox->IsEnabled()) {
      needupdate = true;
      enable = false;
   }
   else if ((top && !top->IsIconized()) && !mDirectoryTextBox->IsEnabled()) {
      needupdate = true;
      enable = true;
   }

   if (needupdate) {
      for (int i = IdMainWindowSmall; i < IdLastDelayedEvent; i++) {
         wxWindow *w = wxWindow::FindWindowById(i, this);
         if (w) {
            w->Enable(enable);
         }
      }
   }
}

void ScreenFrame::OnDirChoose(wxCommandEvent & e)
{
   wxString current = mDirectoryTextBox->GetValue();

   wxDirDialog dlog(this, 
                    _("Choose a location to save screenshot images"),
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

void ScreenFrame::OnMainWindowSmall(wxCommandEvent & e)
{
   int top = 20;

   AudacityProject *proj = GetActiveProject();
   proj->Maximize(false);
   proj->SetSize(16, 16 + top, 680, 450);
   proj->mToolManager->Reset();
}

void ScreenFrame::OnMainWindowLarge(wxCommandEvent & e)
{
   int top = 20;

   AudacityProject *proj = GetActiveProject();
   proj->Maximize(false);
   proj->SetSize(16, 16 + top, 900, 600);
   proj->mToolManager->Reset();
}

void ScreenFrame::OnToggleBackgroundBlue(wxCommandEvent & e)
{
   mBackColor = wxColour(51, 102, 153);
   mBackground = mBlue->GetValue();
   mWhite->SetValue(false);
}

void ScreenFrame::OnToggleBackgroundWhite(wxCommandEvent & e)
{
   mBackColor = wxColour(255, 255, 255);
   mBackground = mWhite->GetValue();
   mBlue->SetValue(false);
}

void ScreenFrame::OnCaptureWindowContents(wxCommandEvent & e)
{
   wxTopLevelWindow *w = GetFrontWindow();
   if (!w) {
      return;
   }

   int x = 0, y = 0;
   int width, height;

   w->ClientToScreen(&x, &y);
   w->GetClientSize(&width, &height);

   wxString basename = wxT("window");
   if (w != GetActiveProject() && w->GetTitle() != wxT("")) {
      basename += (wxT("-") + w->GetTitle() + wxT("-"));
   }

   Capture(basename, w, x, y, width, height);
}

void ScreenFrame::OnCaptureFullWindow(wxCommandEvent & e)
{
   wxTopLevelWindow *w = GetFrontWindow();
   if (!w) {
      return;
   }

   wxRect r = w->GetRect();
   r.SetPosition(w->GetScreenPosition());
   r = w->GetScreenRect();

   wxString basename = wxT("fullwindow");
   if (w != GetActiveProject() && w->GetTitle() != wxT("")) {
      basename += (wxT("-") + w->GetTitle() + wxT("-"));
   }

#if defined(__WXGTK__)
   // In wxGTK, we need to include decoration sizes
   r.width += (wxSystemSettings::GetMetric(wxSYS_BORDER_X, w) * 2);
   r.height += wxSystemSettings::GetMetric(wxSYS_CAPTION_Y, w) +
               wxSystemSettings::GetMetric(wxSYS_BORDER_Y, w);
#endif

   Capture(basename, w, r.x, r.y, r.width, r.height, true);
}

void ScreenFrame::OnCaptureWindowPlus(wxCommandEvent & e)
{
   wxTopLevelWindow *w = GetFrontWindow();
   if (!w) {
      return;
   }

   wxRect r = w->GetRect();
   r.SetPosition(w->GetScreenPosition());
   r = w->GetScreenRect();

   wxString basename = wxT("windowplus");
   if (w != GetActiveProject() && w->GetTitle() != wxT("")) {
      basename += (wxT("-") + w->GetTitle() + wxT("-"));
   }

#if defined(__WXGTK__)
   // In wxGTK, we need to include decoration sizes
   r.width += (wxSystemSettings::GetMetric(wxSYS_BORDER_X, w) * 2);
   r.height += wxSystemSettings::GetMetric(wxSYS_CAPTION_Y, w) +
               wxSystemSettings::GetMetric(wxSYS_BORDER_Y, w);
#endif

   if (!mBackground) {  // background colour not selected but we want a background
      wxRect b = GetBackgroundRect();
      r.x = (r.x - b.x) >= 0 ? (r.x - b.x): 0;
      r.y = (r.y - b.y) >= 0 ? (r.y - b.y): 0;
      r.width += b.width;
      r.height += b.height;
   }

   Capture(basename, w, r.x, r.y, r.width, r.height, true);
}

void ScreenFrame::OnCaptureFullScreen(wxCommandEvent & e)
{
   int width, height;
   wxDisplaySize(&width, &height);

   Capture(wxT("fullscreen"), GetFrontWindow(), 0, 0, width, height);
}

void ScreenFrame::OnCaptureToolbars(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   wxWindow *w = proj->mToolManager->GetTopDock();

   int x = 0, y = 0;
   int width, height;

   w->ClientToScreen(&x, &y);
   w->GetParent()->ScreenToClient(&x, &y);
   w->GetClientSize(&width, &height);

   Capture(wxT("toolbars"), w, x, y, width, height);
}

void ScreenFrame::OnCaptureSelectionBar(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   wxWindow *w = proj->mToolManager->GetBotDock();

   int x = 0, y = 0;
   int width, height;

   w->ClientToScreen(&x, &y);
   w->GetParent()->ScreenToClient(&x, &y);
   w->GetClientSize(&width, &height);

   Capture(wxT("selectionbar"), w, x, y, width, height);
}

void ScreenFrame::OnCaptureTools(wxCommandEvent & e)
{
   CaptureToolbar(ToolsBarID, wxT("tools"));
}

void ScreenFrame::OnCaptureControl(wxCommandEvent & e)
{
   CaptureToolbar(ControlBarID, wxT("control"));
}

void ScreenFrame::OnCaptureMixer(wxCommandEvent & e)
{
   CaptureToolbar(MixerBarID, wxT("mixer"));
}

void ScreenFrame::OnCaptureMeter(wxCommandEvent & e)
{
   CaptureToolbar(MeterBarID, wxT("meter"));
}

void ScreenFrame::OnCaptureEdit(wxCommandEvent & e)
{
   CaptureToolbar(EditBarID, wxT("edit"));
}

void ScreenFrame::OnCaptureDevice(wxCommandEvent & e)
{
   CaptureToolbar(DeviceBarID, wxT("device"));
}

void ScreenFrame::OnCaptureTranscription(wxCommandEvent & e)
{
   CaptureToolbar(TranscriptionBarID, wxT("transcription"));
}

void ScreenFrame::OnCaptureTrackPanel(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   AdornedRulerPanel *ruler = panel->mRuler;

   int h = ruler->GetRulerHeight();
   int x = 0, y = -h;
   int width, height;

   panel->ClientToScreen(&x, &y);
   panel->GetParent()->ScreenToClient(&x, &y);
   panel->GetClientSize(&width, &height);

   Capture(wxT("trackpanel"), panel, x, y, width, height + h);
}

void ScreenFrame::OnCaptureRuler(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   AdornedRulerPanel *ruler = panel->mRuler;

   int x = 0, y = 0;
   int width, height;

   ruler->ClientToScreen(&x, &y);
   ruler->GetParent()->ScreenToClient(&x, &y);
   ruler->GetClientSize(&width, &height);
   height = ruler->GetRulerHeight();

   Capture(wxT("ruler"), ruler, x, y, width, height);
}

void ScreenFrame::OnCaptureTracks(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;

   int x = 0, y = 0;
   int width, height;

   panel->ClientToScreen(&x, &y);
   panel->GetParent()->ScreenToClient(&x, &y);
   panel->GetClientSize(&width, &height);

   Capture(wxT("tracks"), panel, x, y, width, height);
}

void ScreenFrame::OnCaptureFirstTrack(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   TrackListIterator iter(proj->GetTracks());
   Track * t = iter.First();
   if (!t) {
      return;
   }
   wxRect r = panel->FindTrackRect(t, true);

   int x = 0, y = r.y - 3;
   int width, height;

   panel->ClientToScreen(&x, &y);
   panel->GetParent()->ScreenToClient(&x, &y);
   panel->GetClientSize(&width, &height);

   Capture(wxT("firsttrack"), panel, x, y, width, r.height + 6);
}

void ScreenFrame::OnCaptureSecondTrack(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   TrackListIterator iter(proj->GetTracks());
   Track * t = iter.First();
   if (!t) {
      return;
   }
   if (t->GetLinked()) {
      t = iter.Next();
   }
   t = iter.Next();
   if (!t) {
      return;
   }
   wxRect r = panel->FindTrackRect(t, true);

   int x = 0, y = r.y - 3;
   int width, height;

   panel->ClientToScreen(&x, &y);
   panel->GetParent()->ScreenToClient(&x, &y);
   panel->GetClientSize(&width, &height);

   Capture(wxT("secondtrack"), panel, x, y, width, r.height + 6);
}

void ScreenFrame::OnOneSec(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 1.0;
   proj->RedrawProject();
}

void ScreenFrame::OnTenSec(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 10.0;
   proj->RedrawProject();
}

void ScreenFrame::OnOneMin(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 60.0;
   proj->RedrawProject();
}

void ScreenFrame::OnFiveMin(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 300.0;
   proj->RedrawProject();
}

void ScreenFrame::OnOneHour(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);
   proj->mViewInfo.zoom = (0.75 * width) / 3600.0;
   proj->RedrawProject();
}

void ScreenFrame::OnShortTracks(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   TrackListIterator iter(proj->GetTracks());
   for (Track * t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Wave) {
         t->SetHeight(t->GetMinimizedHeight());
      }
   }
   proj->RedrawProject();
}

void ScreenFrame::OnMedTracks(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   TrackListIterator iter(proj->GetTracks());
   for (Track * t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Wave) {
         if (t->GetLink()) {
            t->SetHeight(60);
         }
         else {
            t->SetHeight(120);
         }
      }
   }
   proj->RedrawProject();
}

void ScreenFrame::OnTallTracks(wxCommandEvent & e)
{
   AudacityProject *proj = GetActiveProject();
   TrackListIterator iter(proj->GetTracks());
   for (Track * t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Wave) {
         if (t->GetLink()) {
            t->SetHeight(85);
         }
         else {
            t->SetHeight(170);
         }
      }
   }
   proj->RedrawProject();
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

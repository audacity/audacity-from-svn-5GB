/**********************************************************************

  Audacity: A Digital Audio Editor

  Screenshot.cpp
  
  Dominic Mazzoni

*******************************************************************/

#include "Screenshot.h"

#include <wx/defs.h>
#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/dcmemory.h>
#include <wx/dcscreen.h>
#include <wx/dirdlg.h>
#include <wx/frame.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/panel.h>
#include <wx/statusbr.h>
#include <wx/textctrl.h>
#include <wx/window.h>

#include "Project.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "TrackPanel.h"
#include "toolbars/ToolManager.h"
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

   void OnPaint(wxPaintEvent& evt) {
      int width, height;
      GetClientSize(&width, &height);
      wxPaintDC dc(this);
      dc.SetBrush(wxBrush(wxColour(51, 102, 153), wxSOLID));
      dc.SetPen(*wxTRANSPARENT_PEN);
      dc.DrawRectangle(0, 0, width, height);
   }
   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(StaticColorPanel, wxWindow)
   EVT_PAINT(StaticColorPanel::OnPaint)
END_EVENT_TABLE();

class ScreenFrame : public wxFrame {
 public:
   // constructors and destructors
   ScreenFrame(wxWindow *parent, wxWindowID id);
   virtual ~ScreenFrame();

 private:
   void Capture(wxString basename, wxDC& src,
                int x, int y, int width, int height);
   void CaptureToolbar(int type, wxString name);

   void Populate();
   void PopulateOrExchange(ShuttleGui &S);

   void OnCloseWindow(wxCloseEvent& evt);

   void OnMainWindowSmall(wxCommandEvent& evt);
   void OnMainWindowLarge(wxCommandEvent& evt);
   void OnToggleBackground(wxCommandEvent& evt);
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
}

void CloseScreenshotTools() {

   if (mFrame) {
      mFrame->Destroy();
      mFrame = NULL;
   }
}

ScreenFrame::ScreenFrame(wxWindow * parent, wxWindowID id):
   wxFrame(parent, id, wxT("Screen Capture Frame"),
            wxDefaultPosition, wxDefaultSize,
            wxSYSTEM_MENU|wxCAPTION|wxCLOSE_BOX)
{
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

void ScreenFrame::Capture(wxString basename,
                           wxDC& src, int x, int y, int width, int height)
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

   proj->Raise();
   wxSafeYield();

   wxClientDC dc(w);
   Capture(name, dc, 0, 0, width, height);

   if (!visible) {
      man->ShowHide(type);
   }

   Raise();
}

void ScreenFrame::Populate()
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

enum {
   IdMainWindowSmall = 19200,
   IdMainWindowLarge,
   IdToggleBackground,
   IdCaptureWindowContents,
   IdCaptureWindowPlus,
   IdCaptureFullScreen,

   IdCaptureToolbars,
   IdCaptureSelectionBar,
   IdCaptureTools,
   IdCaptureControl,
   IdCaptureMixer,
   IdCaptureMeter,
   IdCaptureEdit,
   IdCaptureDevice,
   IdCaptureTranscription,

   IdOneSec,
   IdTenSec,
   IdOneMin,
   IdFiveMin,
   IdOneHour,

   IdShortTracks,
   IdMedTracks,
   IdTallTracks,

   IdCaptureTrackPanel,
   IdCaptureRuler,
   IdCaptureTracks,
   IdCaptureFirstTrack,
   IdCaptureSecondTrack,

   IdDirectory,
   IdDirChoose,
};

void ScreenFrame::PopulateOrExchange(ShuttleGui &S)
{
   S.StartPanel(0)->SetBackgroundColour(
      wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
   S.SetBorder(2);
   S.StartVerticalLay(0);

   S.StartStatic(wxT("Capture entire window or screen"), true);
   S.StartMultiColumn(2, wxEXPAND);
   S.SetStretchyCol( 1 );// Column 1 is stretchy...
   {
      wxString dir = gPrefs->Read(wxT("/ScreenshotPath"), ::wxGetCwd());
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
      S.Id(IdToggleBackground).AddButton(wxT("Toggle Background"));
   }
   S.EndHorizontalLay();
   S.StartHorizontalLay();
   {
      S.Id(IdCaptureWindowContents).AddButton(wxT("Capture Window Only"));
      S.Id(IdCaptureWindowPlus).AddButton(wxT("Capture Window Plus"));
      S.Id(IdCaptureFullScreen).AddButton(wxT("Capture Full Screen"));
   }
   S.EndHorizontalLay();
   S.EndStatic();

   S.StartStatic(wxT("Capture Toolbars"), true);
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
   S.EndStatic();

   S.StartStatic(wxT("Time Scale"), true);
   S.StartHorizontalLay();
   {
      S.Id(IdOneSec).AddButton(wxT("One Sec"));
      S.Id(IdTenSec).AddButton(wxT("Ten Sec"));
      S.Id(IdOneMin).AddButton(wxT("One Min"));
      S.Id(IdFiveMin).AddButton(wxT("Five Min"));
      S.Id(IdOneHour).AddButton(wxT("One Hour"));
   }
   S.EndHorizontalLay();
   S.EndStatic();

   S.StartStatic(wxT("Vertical Scale"), true);
   S.StartHorizontalLay();
   {
      S.Id(IdShortTracks).AddButton(wxT("Short Tracks"));
      S.Id(IdMedTracks).AddButton(wxT("Medium Tracks"));
      S.Id(IdTallTracks).AddButton(wxT("Tall Tracks"));
   }
   S.EndHorizontalLay();
   S.EndStatic();

   S.StartStatic(wxT("Capture Track Panel"), true);
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
   EVT_BUTTON(IdToggleBackground,       ScreenFrame::OnToggleBackground)
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

void ScreenFrame::OnMainWindowSmall(wxCommandEvent& evt)
{
   int top = 0;
   #ifdef __WXMAC__
   top += 20;
   #endif

   GetActiveProject()->SetSize(16, 16 + top, 620, 500);
   AdjustBackground();
}

void ScreenFrame::OnMainWindowLarge(wxCommandEvent& evt)
{
   int top = 0;
   #ifdef __WXMAC__
   top += 20;
   #endif

   GetActiveProject()->SetSize(16, 16 + top, 900, 700);
   AdjustBackground();
}

void ScreenFrame::AdjustBackground()
{
   int x, y;
   int width, height;
   GetActiveProject()->GetPosition(&x, &y);
   GetActiveProject()->GetSize(&width, &height);

   mBackground->SetSize(x - 20, y - 20, width + 40, height + 40);
   mStaticColorPanel->SetSize(0, 0, width + 40, height + 40);
   mStaticColorPanel->Show();
}

void ScreenFrame::OnToggleBackground(wxCommandEvent& evt)
{
   if (mBackground->IsShown()) {
      mBackground->Hide();
      return;
   }

   AdjustBackground();
   mBackground->Show();
   GetActiveProject()->Raise();
   Raise();
}

void ScreenFrame::OnCaptureWindowContents(wxCommandEvent& evt)
{
   wxWindow *proj = GetActiveProject();
   int width, height;
   proj->GetClientSize(&width, &height);

   proj->Raise();
   wxSafeYield();

   wxClientDC dc(proj);
   Capture(wxT("window"), dc, 0, 0, width, height);

   Raise();
}

void ScreenFrame::OnCaptureWindowPlus(wxCommandEvent& evt)
{
   wxWindow *proj = GetActiveProject();
   int x, y;
   int width, height;
   proj->GetPosition(&x, &y);
   proj->GetSize(&width, &height);

   proj->Raise();
   wxSafeYield();

   wxScreenDC screenDC;
   Capture(wxT("windowplus"), screenDC, 0, 0, width + x + 16, height + y + 16);

   Raise();
}

void ScreenFrame::OnCaptureFullScreen(wxCommandEvent& evt)
{
   int width, height;
   wxDisplaySize(&width, &height);

   GetActiveProject()->Raise();
   wxSafeYield();

   wxScreenDC screenDC;
   Capture(wxT("fullscreen"), screenDC, 0, 0, width, height);

   Raise();
}

void ScreenFrame::OnCaptureToolbars(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   wxRect r = proj->mToolManager->GetTopDock()->GetRect();

   proj->Raise();
   wxSafeYield();

   wxClientDC dc(proj);
   Capture(wxT("toolbars"), dc, r.x, r.y, r.width, r.height);

   Raise();
}

void ScreenFrame::OnCaptureSelectionBar(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   wxRect r = proj->mToolManager->GetBotDock()->GetRect();

   proj->Raise();
   wxSafeYield();

   wxClientDC dc(proj);
   Capture(wxT("selectionbar"), dc, r.x, r.y, r.width, r.height);

   Raise();
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
            if (t->GetLinked()) {
               t->SetHeight(150);
            }
         }
         else {
            t->SetHeight(70);
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
            if (t->GetLinked()) {
               t->SetHeight(180);
            }
         }
         else {
            t->SetHeight(130);
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
            if (t->GetLinked()) {
               t->SetHeight(240);
            }
         }
         else {
            t->SetHeight(160);
         }
      }
   }
   proj->RedrawProject();
}

void ScreenFrame::OnCaptureTrackPanel(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   int width, height;
   panel->GetClientSize(&width, &height);
   wxClientDC dc(panel);
   Capture(wxT("trackpanel"), dc, 0, 0, width, height);
}

void ScreenFrame::OnCaptureRuler(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   wxPanel *ruler = panel->mRuler;
   int width, height;
   panel->GetClientSize(&width, &height);
   wxClientDC dc(panel);
   Capture(wxT("ruler"), dc, 0, 0, width, height);
}

void ScreenFrame::OnCaptureTracks(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   int width, height;
   panel->GetClientSize(&width, &height);
   wxClientDC dc(panel);
   Capture(wxT("tracks"), dc, 0, 0, width, height);
}

void ScreenFrame::OnCaptureFirstTrack(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   TrackListIterator iter(proj->GetTracks());
   Track * t = iter.First();
   wxRect r = panel->FindTrackRect(t, true);

   int width, height;
   panel->GetClientSize(&width, &height);
   wxClientDC dc(panel);
   Capture(wxT("firsttrack"), dc, 0, r.y - 3, width, r.height + 6);
}

void ScreenFrame::OnCaptureSecondTrack(wxCommandEvent& evt)
{
   AudacityProject *proj = GetActiveProject();
   TrackPanel *panel = proj->mTrackPanel;
   TrackListIterator iter(proj->GetTracks());
   Track * t = iter.First();
   if (t->GetLinked())
      t = iter.Next();
   t = iter.Next();
   wxRect r = panel->FindTrackRect(t, true);

   int width, height;
   panel->GetClientSize(&width, &height);
   wxClientDC dc(panel);
   Capture(wxT("secondtrack"), dc, 0, r.y - 3, width, r.height + 6);
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

/**********************************************************************

  Audacity: A Digital Audio Editor

  ControlToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
 
  See ControlToolBar.h for details

**********************************************************************/

#include "ControlToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/brush.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/settings.h>
#endif

#include <wx/image.h>
#include <wx/tooltip.h>

// For pow() in GetSoundVol()
#include <math.h>

#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "AudioIO.h"
#include "Project.h"
#include "Track.h"

#include "../images/ControlButtons.h"

enum {
   ID_SELECT,
   ID_ENVELOPE,
   ID_SLIDE,
   ID_ZOOM,
   ID_DRAW,
   ID_PLAY_BUTTON,
   ID_STOP_BUTTON,
   ID_RECORD_BUTTON,
   ID_FF_BUTTON,
   ID_REW_BUTTON,

   ID_FIRST_TOOL = ID_SELECT,
   ID_LAST_TOOL = ID_DRAW
};

const int BUTTON_WIDTH = 50;

////////////////////////////////////////////////////////////
/// Methods for ControlToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ControlToolBar, wxWindow)
   EVT_PAINT(ControlToolBar::OnPaint)
   EVT_CHAR(ControlToolBar::OnKeyEvent)
   EVT_COMMAND_RANGE(ID_FIRST_TOOL, ID_LAST_TOOL,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnTool)
   EVT_COMMAND(ID_PLAY_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnPlay)
   EVT_COMMAND(ID_STOP_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnStop)
   EVT_COMMAND(ID_RECORD_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnRecord)
   EVT_COMMAND(ID_REW_BUTTON,
            wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnRewind)
   EVT_COMMAND(ID_FF_BUTTON,
            wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnFF)
END_EVENT_TABLE()

//Standard contructor
ControlToolBar::ControlToolBar(wxWindow * parent):
ToolBar(parent, -1, wxPoint(1, 1), wxSize(468, 55))
{
   InitializeControlToolBar();
}

//Another constructor
ControlToolBar::ControlToolBar(wxWindow * parent, wxWindowID id,
                               const wxPoint & pos,
                               const wxSize & size):ToolBar(parent, id,
                                                            pos, size)
{
   InitializeControlToolBar();
}


// This sets up the ControlToolBar, initializing all the important values
// and creating the buttons.
void ControlToolBar::InitializeControlToolBar()
{
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   int sliderX = 390;
#else
   int sliderX = 350;
#endif

   mIdealSize = wxSize(468, 55);
   mTitle = _("Audacity Control Toolbar");
   mType = ControlToolBarID;

   wxColour backgroundColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour origColour(204, 204, 204);

   MakeButtons();

#if wxVERSION_NUMBER < 2303
   wxImage *sliderOriginal = new wxImage(wxBitmap(Slider));
   wxImage *thumbOriginal = new wxImage(wxBitmap(SliderThumb));
#else
   wxImage *sliderOriginal = new wxImage(wxBitmap(Slider).ConvertToImage());
   wxImage *thumbOriginal = new wxImage(wxBitmap(SliderThumb).ConvertToImage());
#endif
   wxImage *sliderNew = ChangeImageColour(sliderOriginal,
                                          backgroundColour);
   wxImage *thumbNew = ChangeImageColour(thumbOriginal,
                                         backgroundColour);

   mVolume =
       new ASlider(this, 0, wxPoint(sliderX, 14), wxSize(100, 28),
                   sliderNew, thumbNew, 100);

   mVolume->SetToolTip(_("Master Gain Control"));

   delete sliderOriginal;
   delete thumbOriginal;
   delete sliderNew;
   delete thumbNew;

   mVolume->Set(80);

   mCurrentTool = 0;
   mTool[0]->PushDown();

   mBackgroundBrush.SetColour(backgroundColour);
   mBackgroundPen.SetColour(backgroundColour);

   mBackgroundBitmap = NULL;
   mBackgroundHeight = 0;
   mBackgroundWidth = 0;

#if 0
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   mDivBitmap = new wxBitmap((const char **) Div);
   mMuteBitmap = new wxBitmap((const char **) Mute);
   mLoudBitmap = new wxBitmap((const char **) Loud);
#endif
#endif
}


wxImage *ControlToolBar::MakeToolImage(wxImage * tool,
                                       wxImage * mask, int style)
{
   // This code takes the image of a tool, and its mask,
   // and creates one of four images of this tool inside
   // a little button, for the toolbar.  The tool
   // is alpha-blended onto the background.

   int width = tool->GetWidth();
   int height = tool->GetHeight();
   int i;

   wxImage *background = new wxImage(width, height);
   wxColour colour;
   unsigned char *bkgnd = background->GetData();
   unsigned char r, g, b;

   //
   // Background
   //

   colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   r = colour.Red();
   g = colour.Green();
   b = colour.Blue();
   if (style == 1) {            // hilite
      r += (255 - r) / 2;
      g += (255 - g) / 2;
      b += (255 - b) / 2;
   } else if (style == 2) {     // down
      if ((r + g + b) / 3 > 128) {
         r = r * 2 / 3;
         g = g * 2 / 3;
         b = b * 2 / 3;
      } else {
         r += (255 - r) / 3;
         g += (255 - g) / 3;
         b += (255 - b) / 3;
      }
   }

   unsigned char *p = bkgnd;
   for (i = 0; i < width * height; i++) {
      *p++ = r;
      *p++ = g;
      *p++ = b;
   }

   // 
   // Overlay the tool on top of it
   //

   wxImage *result;
   if (style == 2)              // down
      result = OverlayImage(background, tool, mask, 1, 1);
   else
      result = OverlayImage(background, tool, mask, 0, 0);
   delete background;

   //
   // Top hilite
   //

   if (style == 2)              // down
      colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW);
   else
      colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DHIGHLIGHT);
   r = colour.Red();
   g = colour.Green();
   b = colour.Blue();

   unsigned char *res = result->GetData();
   p = res;
   for (i = 0; i < width; i++) {
      *p++ = r;
      *p++ = g;
      *p++ = b;
   }

   p = res;
   for (i = 0; i < height; i++) {
      *p++ = r;
      *p++ = g;
      *p++ = b;
      p += 3 * (width - 1);
   }

   //
   // Bottom shadow
   //

   if (style == 2)              // down
      colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DHIGHLIGHT);
   else
      colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW);
   r = colour.Red();
   g = colour.Green();
   b = colour.Blue();

   p = res + 3 * (height - 1) * width;
   for (i = 0; i < width; i++) {
      *p++ = r;
      *p++ = g;
      *p++ = b;
   }

   p = res + 3 * (width - 1);
   for (i = 0; i < height; i++) {
      *p++ = r;
      *p++ = g;
      *p++ = b;
      p += 3 * (width - 1);
   }

   return result;
}

AButton *ControlToolBar::MakeTool(const char **tool, const char **alpha,
                                  wxWindowID id, int left, int top)
{
#if wxVERSION_NUMBER < 2303
   wxImage *ctr = new wxImage(wxBitmap(tool));
   wxImage *mask = new wxImage(wxBitmap(alpha));
#else
   wxImage *ctr = new wxImage(wxBitmap(tool).ConvertToImage());
   wxImage *mask = new wxImage(wxBitmap(alpha).ConvertToImage());
#endif
   wxImage *up = MakeToolImage(ctr, mask, 0);
   wxImage *hilite = MakeToolImage(ctr, mask, 1);
   wxImage *down = MakeToolImage(ctr, mask, 2);
   wxImage *dis = MakeToolImage(ctr, mask, 3);

   AButton *button =
       new AButton(this, id, wxPoint(left, top), wxSize(27, 27),
                   up, hilite, down, dis);

   delete ctr;
   delete mask;
   delete up;
   delete hilite;
   delete down;
   delete dis;

   return button;
}


// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments
AButton *ControlToolBar::MakeButton(char const **foreground,
                                    char const **disabled,
                                    char const **alpha, int id)
{

   // Windows (TM) has a little extra room for some reason, so the top of the
   // buttons should be a little lower.
   int buttonTop = 4;
#ifdef __WXMSW__
   buttonTop=4;
#endif


   AButton *r = ToolBar::MakeButton(upPattern, downPattern, hilitePattern,
                              foreground, disabled, alpha, wxWindowID(id),
                              wxPoint(mButtonPos,buttonTop), wxSize(48, 48), 16, 16);
   mButtonPos += BUTTON_WIDTH;
   return r;
}


void ControlToolBar::MakeButtons()
{
#if wxVERSION_NUMBER < 2303
   wxImage *upOriginal = new wxImage(wxBitmap(UpButton));
   wxImage *downOriginal = new wxImage(wxBitmap(DownButton));
   wxImage *hiliteOriginal = new wxImage(wxBitmap(HiliteButton));
#else
   wxImage *upOriginal = new wxImage(wxBitmap(UpButton).ConvertToImage());
   wxImage *downOriginal = new wxImage(wxBitmap(DownButton).ConvertToImage());
   wxImage *hiliteOriginal = new wxImage(wxBitmap(HiliteButton).ConvertToImage());
#endif

   wxColour newColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);

   upPattern = ChangeImageColour(upOriginal, newColour);
   downPattern = ChangeImageColour(downOriginal, newColour);
   hilitePattern = ChangeImageColour(hiliteOriginal, newColour);

   /* Buttons */

   mButtonPos = 92;

   mRewind = MakeButton((char const **) Rewind,
                        (char const **) RewindDisabled,
                        (char const **) RewindAlpha, ID_REW_BUTTON);
   mRewind->SetToolTip(_("Skip to Start"));

   mPlay = MakeButton((char const **) Play,
                      (char const **) PlayDisabled,
                      (char const **) PlayAlpha, ID_PLAY_BUTTON);
   mPlay->SetToolTip(_("Play"));

   mStop = MakeButton((char const **) Stop,
                      (char const **) StopDisabled,
                      (char const **) StopAlpha, ID_STOP_BUTTON);
   mStop->SetToolTip(_("Stop"));

   mRecord = MakeButton((char const **) Record,
                        (char const **) RecordDisabled,
                        (char const **) RecordAlpha, ID_RECORD_BUTTON);
   mRecord->SetToolTip(_("Record"));

   mFF = MakeButton((char const **) FFwd,
                    (char const **) FFwdDisabled,
                    (char const **) FFwdAlpha, ID_FF_BUTTON);
   mFF->SetToolTip(_("Skip to End"));

   delete upPattern;
   delete downPattern;
   delete hilitePattern;
   delete upOriginal;
   delete downOriginal;
   delete hiliteOriginal;

   /* Tools */

   mTool[0] = MakeTool(IBeam, IBeamAlpha, ID_SELECT, 0, 0);
   mTool[0]->SetToolTip(_("Selection Tool"));

   mTool[1] = MakeTool(Envelope, EnvelopeAlpha, ID_ENVELOPE, 28, 0);
   mTool[1]->SetToolTip(_("Envelope Tool"));

   mTool[2] = MakeTool(TimeShift, TimeShiftAlpha, ID_SLIDE, 0, 28);
   mTool[2]->SetToolTip(_("Time Shift Tool"));

   mTool[3] = MakeTool(Zoom, ZoomAlpha, ID_ZOOM, 28, 28);
   mTool[3]->SetToolTip(_("Zoom Tool"));

   mTool[4] = MakeTool(Draw, DrawAlpha, ID_DRAW, 56, 0);
   mTool[4]->SetToolTip(_("Draw Tool"));

   wxToolTip::Enable(true);     // MB: Should make this a pref
   wxToolTip::SetDelay(1000);
}

ControlToolBar::~ControlToolBar()
{
   for (int i = 0; i < 5; i++)
      delete mTool[i];

   delete mRewind;
   delete mPlay;
   delete mStop;
   delete mRecord;
   delete mFF;

   delete mVolume;

   if (mBackgroundBitmap)
      delete mBackgroundBitmap;

#if 0
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   delete mDivBitmap;
   delete mMuteBitmap;
   delete mLoudBitmap;
#endif
#endif
}

void ControlToolBar::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown() || event.AltDown()) {
      event.Skip();
      return;
   }

   if (event.KeyCode() == WXK_SPACE) {
      if (gAudioIO->IsBusy()) {
         SetPlay(false);
         SetStop(true);
         OnStop();
      } else {
         SetPlay(true);
         SetStop(false);
         OnPlay();
      }
      return;
   }
   event.Skip();
}


int ControlToolBar::GetCurrentTool()
{
   return mCurrentTool;
}

void ControlToolBar::SetPlay(bool down)
{
   if (down)
      mPlay->PushDown();
   else
      mPlay->PopUp();
}

void ControlToolBar::SetStop(bool down)
{
   if (down)
      mStop->PushDown();
   else {
      mStop->PopUp();
      mStop->Disable();
      mRecord->Enable();
      mPlay->Enable();
      mRewind->Enable();
      mFF->Enable();
   }
}

void ControlToolBar::SetRecord(bool down)
{
   if (down)
      mRecord->PushDown();
   else
      mRecord->PopUp();
}

void ControlToolBar::OnPlay()
{
   if (gAudioIO->IsBusy())
      return;

   mStop->Enable();
   mRewind->Disable();
   mRecord->Disable();
   mFF->Disable();

   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();
      double t0 = p->GetSel0();
      double t1 = p->GetSel1();

      if (t1 == t0 || t1 > t->GetEndTime())
         t1 = t->GetEndTime();
      if (t0 > t->GetEndTime())
         t0 = t->GetEndTime();

      bool success = (t1 > t0) && gAudioIO->StartPlay(p, t, t0, t1);

      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   }
}

void ControlToolBar::OnStop()
{
   gAudioIO->Stop();
   SetStop(false);
}

void ControlToolBar::OnRecord()
{
   if (gAudioIO->IsBusy())
      return;

   mPlay->Disable();
   mStop->Enable();
   mRewind->Disable();
   mFF->Disable();

   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();
      double t0 = p->GetSel0();
      double t1 = p->GetSel1();
      if (t1 == t0)
         t1 = 1000000000.0;     // record for a long, long time (tens of years)
      bool success = gAudioIO->StartRecord(p, t, t0, t1);
      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   }
}

void ControlToolBar::OnRewind()
{
   mRewind->PopUp();

   if (gAudioIO->IsBusy())
      OnStop();

   AudacityProject *p = GetActiveProject();
   if (p)
      p->Rewind(mRewind->WasShiftDown());
}

void ControlToolBar::OnFF()
{
   mFF->PopUp();

   if (gAudioIO->IsBusy())
      OnStop();

   AudacityProject *p = GetActiveProject();
   if (p)
      p->SkipEnd(mFF->WasShiftDown());
}

float ControlToolBar::GetSoundVol()
{
   int v = mVolume->Get();

   if (v == 0)
      return float(0.0);

   return (pow(2.0, (v / 10.0)) / 256.0);
}

void ControlToolBar::OnTool(wxCommandEvent & evt)
{
   int prev = mCurrentTool;
   mCurrentTool = evt.GetId() - ID_FIRST_TOOL;

   for (int i = 0; i < 5; i++)
      if (i == mCurrentTool)
         mTool[i]->PushDown();
      else
         mTool[i]->PopUp();

   if (mCurrentTool == envelopeTool || prev == envelopeTool)
      RedrawAllProjects();
}

void ControlToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

#if defined(__WXMAC__0)          // && defined(TARGET_CARBON)

   if (mBackgroundWidth < width) {
      if (mBackgroundBitmap)
         delete mBackgroundBitmap;

      mBackgroundBitmap = new wxBitmap(width, height);

      wxMemoryDC memDC;
      memDC.SelectObject(*mBackgroundBitmap);

      int y;
      memDC.SetPen(wxPen(wxColour(231, 231, 231), 1, wxSOLID));
      for (y = 0; y < height; y += 4)
         memDC.DrawLine(0, y, width, y);
      memDC.SetPen(wxPen(wxColour(239, 239, 239), 1, wxSOLID));
      for (y = 1; y < height; y += 2)
         memDC.DrawLine(0, y, width, y);
      memDC.SetPen(wxPen(wxColour(255, 255, 255), 1, wxSOLID));
      for (y = 2; y < height; y += 4)
         memDC.DrawLine(0, y, width, y);

      memDC.DrawBitmap(*mDivBitmap, 240, 4);
      memDC.DrawBitmap(*mMuteBitmap, 250, 4);
      memDC.DrawBitmap(*mLoudBitmap, 404, 4);
   }

   wxMemoryDC memDC;
   memDC.SelectObject(*mBackgroundBitmap);

   dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);

#else

   dc.SetBrush(mBackgroundBrush);
   dc.SetPen(mBackgroundPen);
   dc.DrawRectangle(0, 0, width, height);

   dc.SetPen(*wxBLACK_PEN);

   dc.DrawLine(27, 0, 27, height - 1);
   dc.DrawLine(55, 0, 55, height - 1);
   dc.DrawLine(83, 0, 83, height - 1);
   dc.DrawLine(0, 27, 83, 27);
#endif
}

void ControlToolBar::EnableDisableButtons()
{
   AudacityProject *p = GetActiveProject();

   bool tracks = (p && !p->GetTracks()->IsEmpty());
   bool busy = gAudioIO->IsBusy();

   if (tracks) {
      if (!busy)
         mPlay->Enable();
   } else mPlay->Disable();

   mStop->SetEnabled(busy);
   mRewind->SetEnabled(tracks && !busy);
   mFF->SetEnabled(tracks && !busy);
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  APalette.cpp

  Dominic Mazzoni

  This class manages the miniframe window (aka floating window)
  which contains the tool selection (ibeam, envelope, move, zoom),
  the play/stop/record buttons, and the volume control.  All of the
  controls in this window were custom-written for Audacity - they
  are not native controls on any platform - however, it is intended
  that the images could be easily replaced to allow "skinning" or
  just customization to match the look and feel of each platform.

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#include <wx/brush.h>
#include <wx/image.h>
#endif

#include <math.h>

#include "AButton.h"
#include "ASlider.h"
#include "APalette.h"
#include "AudioIO.h"
#include "Project.h"

APaletteFrame *gAPaletteFrame = NULL;

bool gWindowedPalette = false;

#ifdef __WXMAC__
#define APALETTE_HEIGHT_OFFSET 0
#endif

#ifdef __WXGTK__
#define APALETTE_HEIGHT_OFFSET 22
#endif

#ifdef __WXMSW__
#define APALETTE_HEIGHT_OFFSET 25
#endif

#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
#include "xpm/Aqua.xpm"
#else
#include "xpm/Palette.xpm"
#endif

void InitAPaletteFrame(wxWindow * parent)
{
   wxPoint where;

   where.x = 10;
   where.y = 10;

#ifdef __WXMAC__
   where.y += 20;
#endif

   gAPaletteFrame = new APaletteFrame(parent, -1, "Audacity Palette",
                                      where);

   if (gWindowedPalette) {
      gAPaletteFrame->Show(TRUE);
   }
}

int GetAPaletteHeight()
{
   return 55;
}

void ShowWindowedPalette(wxPoint * where /* = NULL */ )
{
   if (where)
      gAPaletteFrame->Move(*where);

   gAPaletteFrame->Show(true);
   gWindowedPalette = true;

   int len = gAudacityProjects.Count();
   for (int i = 0; i < len; i++)
      gAudacityProjects[i]->HidePalette();
}

void HideWindowedPalette()
{
   gAPaletteFrame->Show(false);
   gWindowedPalette = false;

   int len = gAudacityProjects.Count();
   for (int i = 0; i < len; i++)
      gAudacityProjects[i]->ShowPalette();
}

// APaletteFrame

BEGIN_EVENT_TABLE(APaletteFrame, wxMiniFrame)
    EVT_CLOSE(APaletteFrame::OnCloseWindow)
    END_EVENT_TABLE()

APaletteFrame::APaletteFrame(wxWindow * parent,
                                 wxWindowID id,
                                 const wxString & title,
                                 const wxPoint & pos):wxMiniFrame(parent,
                                                                  id,
                                                                  title,
                                                                  pos,
                                                                  wxSize
                                                                  (360,
                                                                   GetAPaletteHeight
                                                                   () +
                                                                   APALETTE_HEIGHT_OFFSET),
                                                                  wxTINY_CAPTION_HORIZ |
                                                                  wxSTAY_ON_TOP |
                                                                  wxMINIMIZE_BOX |
                                                                  wxFRAME_FLOAT_ON_PARENT),
mPalette(this, 0, wxPoint(0, 0), wxSize(300, GetAPaletteHeight()))
{

}

// APalette

BEGIN_EVENT_TABLE(APalette, wxWindow)
    EVT_PAINT(APalette::OnPaint)
    EVT_CHAR(APalette::OnKeyEvent)
    EVT_COMMAND_RANGE(ID_FIRST_TOOL, ID_LAST_TOOL,
                  wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnTool)
    EVT_COMMAND_RANGE(ID_PLAY_BUTTON, ID_PLAY_BUTTON,
                  wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnPlay)
    EVT_COMMAND_RANGE(ID_STOP_BUTTON, ID_STOP_BUTTON,
                  wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnStop)
    EVT_COMMAND_RANGE(ID_RECORD_BUTTON, ID_RECORD_BUTTON,
                  wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnRecord)
    END_EVENT_TABLE()

APalette::APalette(wxWindow * parent, wxWindowID id,
                       const wxPoint & pos,
                       const wxSize & size):wxWindow(parent, id, pos, size)
{
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   int off = 1;
#else
   int off = 0;
#endif

   mTool[0] =
       new AButton(this, ID_IBEAM, wxPoint(off, off), wxSize(27, 27),
                   (char **) IBeamUp, (char **) IBeamOver,
                   (char **) IBeamDown, (char **) IBeamUp);
   mTool[1] =
       new AButton(this, ID_SELECT, wxPoint(28, off), wxSize(27, 27),
                   (char **) SelectUp, (char **) SelectOver,
                   (char **) SelectDown, (char **) SelectUp);
   mTool[2] =
       new AButton(this, ID_MOVE, wxPoint(off, 28), wxSize(27, 27),
                   (char **) MoveUp, (char **) MoveOver,
                   (char **) MoveDown, (char **) MoveUp);
   mTool[3] =
       new AButton(this, ID_ZOOM, wxPoint(28, 28), wxSize(27, 27),
                   (char **) ZoomUp, (char **) ZoomOver,
                   (char **) ZoomDown, (char **) ZoomUp);

   mPlay =
       new AButton(this, ID_PLAY_BUTTON, wxPoint(64, 4), wxSize(48, 48),
                   (char **) PlayUp, (char **) PlayOver,
                   (char **) PlayDown, (char **) PlayDisabled);
   mStop =
       new AButton(this, ID_STOP_BUTTON, wxPoint(114, 4), wxSize(48, 48),
                   (char **) StopUp, (char **) StopOver,
                   (char **) StopDown, (char **) StopDisabled);

   mRecord =
       new AButton(this, ID_RECORD_BUTTON, wxPoint(164, 4), wxSize(48, 48),
                   (char **) RecordUp, (char **) RecordOver,
                   (char **) RecordDown, (char **) RecordDisabled);

#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   int sliderX = 262;
#else
   int sliderX = 222;
#endif

   mVolume =
       new ASlider(this, 0, wxPoint(sliderX, 14), wxSize(100, 28),
                   (char **) Slider, (char **) SliderThumb, 100);

   mVolume->Set(80);

   mCurrentTool = 0;
   mTool[0]->PushDown();

   mBackgroundBrush.SetColour(wxColour(204, 204, 204));
   mBackgroundPen.SetColour(wxColour(204, 204, 204));

   mBackgroundBitmap = NULL;
   mBackgroundHeight = 0;
   mBackgroundWidth = 0;

#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   mDivBitmap = new wxBitmap((const char **) Div);
   mMuteBitmap = new wxBitmap((const char **) Mute);
   mLoudBitmap = new wxBitmap((const char **) Loud);
#endif
}

APalette::~APalette()
{
   for (int i = 0; i < 4; i++)
      delete mTool[i];
   delete mPlay;
   delete mStop;
   delete mRecord;
   delete mVolume;

   if (mBackgroundBitmap)
      delete mBackgroundBitmap;

#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   delete mDivBitmap;
   delete mMuteBitmap;
   delete mLoudBitmap;
#endif
}

void APalette::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown()) {
      event.Skip();
      return;
   }

   long key = event.KeyCode();

   if (event.KeyCode() == WXK_SPACE) {
      if (gAudioIO->IsBusy()) {
         OnStop();
         SetPlay(false);
         SetStop(true);
      } else {
         OnPlay();
         SetPlay(true);
         SetStop(false);
      }
   }
}

int APalette::GetCurrentTool()
{
   return mCurrentTool;
}

void APalette::SetPlay(bool down)
{
   if (down)
      mPlay->PushDown();
   else
      mPlay->PopUp();
}

void APalette::SetStop(bool down)
{
   if (down)
      mStop->PushDown();
   else
      mStop->PopUp();
}

void APalette::SetRecord(bool down)
{
   if (down)
      mRecord->PushDown();
   else
      mRecord->PopUp();
}

void APalette::OnPlay()
{
   if (gAudioIO->IsBusy())
      return;

   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();
      double t0 = p->GetSel0();
      double t1 = p->GetSel1();
      if (t1 == t0)
         t1 = t->GetMaxLen();
      bool success = gAudioIO->StartPlay(p, t, t0, t1);

      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }

   }
}

void APalette::OnStop()
{
   gAudioIO->Stop();
   SetStop(false);
}

void APalette::OnRecord()
{
   if (gAudioIO->IsBusy())
      return;

   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();
      bool success = gAudioIO->StartRecord(p, t);
      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   }
}

float APalette::GetSoundVol()
{
   int v = mVolume->Get();
   float vol;

   if (v == 0)
      vol = 0.0;

   else
      vol = (pow(2.0, (v / 10.0)) / 256.0);

   return vol;
}

void APalette::OnTool(wxCommandEvent & evt)
{
   int prev = mCurrentTool;

   mCurrentTool = evt.GetId() - ID_FIRST_TOOL;

   for (int i = 0; i < 4; i++)
      if (i == mCurrentTool)
         mTool[i]->PushDown();
      else
         mTool[i]->PopUp();

   if (mCurrentTool == 1 || prev == 1)
      RedrawAllProjects();
}

void APalette::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);


#if defined(__WXMAC__)          // && defined(TARGET_CARBON)

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

      memDC.DrawBitmap(*mDivBitmap, 212, 4);
      memDC.DrawBitmap(*mMuteBitmap, 222, 4);
      memDC.DrawBitmap(*mLoudBitmap, 376, 4);
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
   dc.DrawLine(0, 27, 55, 27);
#endif
}

void APaletteFrame::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
   HideWindowedPalette();
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  APalette.cpp

  Dominic Mazzoni

  This class manages the miniframe window (aka floating window)
  which contains the tool selection (ibeam, envelope, move, zoom),
  the skip-start/play/stop/record/skip-end buttons, and the
  volume control.  All of the controls in this window were
  custom-written for Audacity - they are not native controls
  on any platform - however, it is intended that the images could
  be easily replaced to allow "skinning" or just customization
  to match the look and feel of each platform.

  Now pieces together the buttons by adjusting the background
  to match the user's system default background color, and then
  overlaying the button label on using alpha-blending.

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/brush.h>
#include <wx/dcclient.h>
#include <wx/intl.h>
#include <wx/settings.h>
#endif

#include <wx/image.h>
#include <wx/tooltip.h>

#include <math.h>

#include "widgets/AButton.h"
#include "widgets/ASlider.h"
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

#include "../images/Palette.xpm"

void InitAPaletteFrame(wxWindow * parent)
{
   wxPoint where;

   where.x = 10;
   where.y = 10;

#ifdef __WXMAC__
   where.y += 20;
#endif

   gAPaletteFrame = new APaletteFrame(parent, -1, _("Audacity Palette"),
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
                             const wxPoint & pos):
   wxMiniFrame(parent, id, title, pos,
               wxSize(460, GetAPaletteHeight() + APALETTE_HEIGHT_OFFSET),
               wxTINY_CAPTION_HORIZ | wxSTAY_ON_TOP |
               wxMINIMIZE_BOX | wxFRAME_FLOAT_ON_PARENT),
   mPalette(this, 0, wxPoint(0, 0), wxSize(400, GetAPaletteHeight()))
{

}

// APalette

BEGIN_EVENT_TABLE(APalette, wxWindow)
   EVT_PAINT(APalette::OnPaint)
   EVT_CHAR(APalette::OnKeyEvent)
   EVT_COMMAND_RANGE(ID_FIRST_TOOL, ID_LAST_TOOL,
                     wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnTool)
   EVT_COMMAND(ID_PLAY_BUTTON,
               wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnPlay)
   EVT_COMMAND(ID_STOP_BUTTON,
               wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnStop)
   EVT_COMMAND(ID_RECORD_BUTTON,
               wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnRecord)
   EVT_COMMAND(ID_REW_BUTTON,
               wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnRewind)
   EVT_COMMAND(ID_FF_BUTTON,
               wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnFF)
END_EVENT_TABLE()
   
wxImage *ChangeImageColour(wxImage *srcImage,
                           wxColour &dstColour)
{
   // This function takes a source image, which it assumes to
   // be grayscale, and smoothly changes the overall color
   // to the specified color, and returns the result as a
   // new image.  This works well for grayscale 3D images.
   // Audacity uses this routines to make the buttons
   // (skip-start, play, stop, record, skip-end) adapt to
   // the color scheme of the user.

   unsigned char *src = srcImage->GetData();
   int width = srcImage->GetWidth();
   int height = srcImage->GetHeight();

   wxImage *dstImage = new wxImage(width, height);
   unsigned char *dst = dstImage->GetData();

   int srcVal[3], srcOpp[3];
   srcVal[0] = src[0];
   srcVal[1] = src[1];
   srcVal[2] = src[2];

   int dstVal[3], dstOpp[3];
   dstVal[0] = dstColour.Red();
   dstVal[1] = dstColour.Green();
   dstVal[2] = dstColour.Blue();

   int i;
   for(i=0; i<3; i++) {
      srcOpp[i] = 255 - srcVal[i];
      dstOpp[i] = 255 - dstVal[i];
   }

   int c = 0;
   for(i=0; i<width*height*3; i++) {
      int s = (int)*src;

      if (s > srcVal[c])
         *dst++ = dstVal[c] + dstOpp[c] * (s - srcVal[c]) / srcOpp[c];
      else
         *dst++ = dstVal[c] * s / srcVal[c];
      src++;
      c = (c+1)%3;
   }

   return dstImage;
}

static wxImage *OverlayImage(wxImage *background,
                             wxImage *foreground,
                             wxImage *mask,
                             int xoff, int yoff)
{
   // Takes a background image, foreground image, and mask
   // (i.e. the alpha channel for the foreground), and
   // returns an new image where the foreground has been
   // overlaid onto the background using alpha-blending,
   // at location (xoff, yoff).

   unsigned char *bk = background->GetData();
   unsigned char *fg = foreground->GetData();
   unsigned char *m = mask->GetData();
   int width = background->GetWidth();
   int height = background->GetHeight();
   int w2 = foreground->GetWidth();
   int h2 = foreground->GetHeight();

   // If the foreground + offset is bigger than the background, masking
   // should only occur within these bounds of the foreground image

   int wcutoff =  (width > (w2 + xoff)) ? w2: width  - xoff;
   int hcutoff = (height > (h2 + yoff)) ? h2: height - yoff;
   
   wxImage *dstImage = new wxImage(width, height);
   unsigned char *dst = dstImage->GetData();

   int x, y;
   memcpy(dst, bk, width*height*3);

   // Go through the foreground image bit by bit and mask it on to the
   // background, at an offset of xoff,yoff.
   // BUT...Don't go beyond the size of the background image

   for(y=0; y<hcutoff; y++) {
   
      unsigned char *bkp = bk + 3*((y+yoff)*width+xoff);
      unsigned char *dstp = dst + 3*((y+yoff)*width+xoff);

      for(x=0; x < wcutoff; x++) {
         
         int value = m[3*(y*w2+x)];           
         int opp = 255 - value;
      
         for(int c=0; c<3; c++)
            dstp[x*3+c] =
               ((bkp[x*3+c] * opp) + (fg[3*(y*w2+x)+c] * value)) / 255;
      }
   }

   return dstImage;
}

wxImage *MakeToolImage(wxImage *tool,
                       wxImage *mask,
                       int style)
{
   // This code takes the image of a tool, and its mask,
   // and creates one of four images of this tool inside
   // a little button, for the tool palette.  The tool
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
   r = colour.Red(); g = colour.Green(); b = colour.Blue();
   if (style == 1) { // hilite
      r += (255-r)/2; g += (255-g)/2; b += (255-b)/2;
   }
   else if (style == 2) { // down
      if ((r + g + b) / 3 > 128) {
         r = r*2/3; g = g*2/3; b = b*2/3;
      }
      else {
         r += (255-r)/3; g += (255-g)/3; b += (255-b)/3;      
      }
   }

   unsigned char *p = bkgnd;
   for(i=0; i<width*height; i++) {
      *p++ = r; *p++ = g; *p++ = b;
   }

   // 
   // Overlay the tool on top of it
   //

   wxImage *result;
   if (style == 2) // down
      result = OverlayImage(background, tool, mask, 1, 1);
   else
      result = OverlayImage(background, tool, mask, 0, 0);
   delete background;

   //
   // Top hilite
   //

   if (style == 2) // down
      colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW);
   else
      colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DHIGHLIGHT);
   r = colour.Red(); g = colour.Green(); b = colour.Blue();

   unsigned char *res = result->GetData();
   p = res;
   for(i=0; i<width; i++) {
      *p++ = r; *p++ = g; *p++ = b;
   }

   p = res;
   for(i=0; i<height; i++) {
      *p++ = r; *p++ = g; *p++ = b;
      p += 3*(width-1);
   }

   //
   // Bottom shadow
   //

   if (style == 2) // down
      colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DHIGHLIGHT);
   else
      colour = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW);
   r = colour.Red(); g = colour.Green(); b = colour.Blue();

   p = res + 3*(height-1)*width;
   for(i=0; i<width; i++) {
      *p++ = r; *p++ = g; *p++ = b;
   }

   p = res + 3*(width-1);
   for(i=0; i<height; i++) {
      *p++ = r; *p++ = g; *p++ = b;
      p += 3*(width-1);
   }

   return result;
}

AButton *APalette::MakeButton(wxImage *up, wxImage *down, wxImage *hilite,
                              const char **foreground, const char **alpha,
                              wxWindowID id, int left)
{
   wxImage *ctr = new wxImage(foreground);                           
   wxImage *mask = new wxImage(alpha);
   wxImage *up2 = OverlayImage(up, ctr, mask, 16, 16);
   wxImage *hilite2 = OverlayImage(hilite, ctr, mask, 16, 16);
   wxImage *down2 = OverlayImage(down, ctr, mask, 17, 17);
   AButton *button =
      new AButton(this, id,
                  wxPoint(left, 4), wxSize(48, 48),
                  up2, hilite2, down2, up2);
   delete ctr;
   delete mask;
   delete up2;
   delete down2;
   delete hilite2;

   return button;
}

AButton *APalette::MakeTool(const char **tool, const char **alpha,
                            wxWindowID id, int left, int top)
{
   wxImage *ctr = new wxImage(tool);
   wxImage *mask = new wxImage(alpha);
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

void APalette::MakeButtons()
{
   wxImage *upOriginal = new wxImage(UpButton);
   wxImage *downOriginal = new wxImage(DownButton);
   wxImage *hiliteOriginal = new wxImage(HiliteButton);

   wxColour newColour = 
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);

   wxImage *upPattern = ChangeImageColour(upOriginal, newColour);
   wxImage *downPattern = ChangeImageColour(downOriginal, newColour);
   wxImage *hilitePattern = ChangeImageColour(hiliteOriginal, newColour);

   /* Buttons */

   mRewind = MakeButton(upPattern, downPattern, hilitePattern,
                        Rewind, RewindAlpha, ID_REW_BUTTON, 64);
   mRewind->SetToolTip(_("Skip to Start"));

   mPlay = MakeButton(upPattern, downPattern, hilitePattern,
                      Play, PlayAlpha, ID_PLAY_BUTTON, 114);
   mPlay->SetToolTip(_("Play"));

   mStop = MakeButton(upPattern, downPattern, hilitePattern,
                      Stop, StopAlpha, ID_STOP_BUTTON, 164);
   mStop->SetToolTip(_("Stop"));

   mRecord = MakeButton(upPattern, downPattern, hilitePattern,
                        Record, RecordAlpha, ID_RECORD_BUTTON, 214);
   mRecord->SetToolTip(_("Record"));

   mFF = MakeButton(upPattern, downPattern, hilitePattern,
                    FFwd, FFwdAlpha, ID_FF_BUTTON, 264);
   mFF->SetToolTip(_("Skip to End"));

   delete upPattern;
   delete downPattern;
   delete hilitePattern;
   delete upOriginal;
   delete downOriginal;
   delete hiliteOriginal;

   /* Tools */

   mTool[0] = MakeTool(IBeam, IBeamAlpha, ID_IBEAM, 0, 0);
   mTool[0]->SetToolTip(_("Selection Tool"));

   mTool[1] = MakeTool(Envelope, EnvelopeAlpha, ID_SELECT, 28, 0);
   mTool[1]->SetToolTip(_("Envelope Tool"));

   mTool[2] = MakeTool(TimeShift, TimeShiftAlpha, ID_MOVE, 0, 28);
   mTool[2]->SetToolTip(_("Time Shift Tool"));

   mTool[3] = MakeTool(Zoom, ZoomAlpha, ID_ZOOM, 28, 28);
   mTool[3]->SetToolTip(_("Zoom Tool"));

   wxToolTip::Enable(true);
   wxToolTip::SetDelay(1000);
}

APalette::APalette(wxWindow * parent, wxWindowID id,
                   const wxPoint & pos,
                   const wxSize & size):
   wxWindow(parent, id, pos, size)
{
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   int sliderX = 362;
#else
   int sliderX = 322;
#endif

   wxColour backgroundColour = 
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour origColour(204, 204, 204);

   MakeButtons();

   wxImage *sliderOriginal = new wxImage(Slider);
   wxImage *thumbOriginal = new wxImage(SliderThumb);
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

   delete mRewind;
   delete mPlay;
   delete mStop;
   delete mRecord;
   delete mFF;

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
   else event.Skip();
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

      if (t1 == t0 || t1 > t->GetMaxLen())
         t1 = t->GetMaxLen();
      if (t0 > t->GetMaxLen())
         t0 = t->GetMaxLen();

      bool success = (t1 > t0) && gAudioIO->StartPlay(p, t, t0, t1);

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
      double t0 = p->GetSel0();
      double t1 = p->GetSel1();
      if (t1 == t0)
         t1 = 1000000000.0;  // record for a long, long time (tens of years)
      bool success = gAudioIO->StartRecord(p, t, t0, t1);
      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
   }
}

void APalette::OnRewind()
{
   mRewind->PopUp();

   if (gAudioIO->IsBusy())
      OnStop();

   AudacityProject *p = GetActiveProject();
   if (p)
      p->Rewind(mRewind->WasShiftDown());
}

void APalette::OnFF()
{
   mFF->PopUp();   
   
   if (gAudioIO->IsBusy())
      OnStop();

   AudacityProject *p = GetActiveProject();
   if (p)
      p->SkipEnd(mFF->WasShiftDown());
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

/**********************************************************************

  Audacity: A Digital Audio Editor

  Meter.h

  Dominic Mazzoni

  VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

  2004.06.25 refresh rate limited to 30mS, by ChackoN

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/settings.h>

#include <math.h>

#include "Meter.h"

#include "../AudioIO.h"
#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../../images/MixerImages.h"
#include "../Project.h"
#include "../MeterToolBar.h"

enum {
   MeterEventID = 6000,
   OnHorizontalID,
   OnVerticalID,
   OnMultiID,
   OnEqualizerID,
   OnWaveformID,
   OnLinearID,
   OnDBID,
   OnClipID,
   OnMonitorID,
   OnFloatID
};

class MeterUpdateEvent: public wxCommandEvent
{
public:
   MeterUpdateEvent():
      wxCommandEvent(wxEVT_COMMAND_SLIDER_UPDATED, MeterEventID)
   {
   }

   virtual wxEvent *Clone() const 
   {
      MeterUpdateEvent *newEvent = new MeterUpdateEvent();
      memcpy(newEvent->peak, peak, 32*sizeof(float));
      memcpy(newEvent->rms, rms, 32*sizeof(float));
      memcpy(newEvent->clipping, clipping, 32*sizeof(bool));
      memcpy(newEvent->headPeakCount, headPeakCount, 32*sizeof(int));
      memcpy(newEvent->tailPeakCount, tailPeakCount, 32*sizeof(int));
      newEvent->numFrames = numFrames;
      return (wxEvent *)newEvent;
   }

   int numFrames;
   float peak[32];
   float rms[32];
   bool clipping[32];
   int headPeakCount[32];
   int tailPeakCount[32];
};

BEGIN_EVENT_TABLE(Meter, wxPanel)
   EVT_SLIDER(MeterEventID, Meter::OnMeterUpdate)
   EVT_MOUSE_EVENTS(Meter::OnMouse)
   EVT_PAINT(Meter::OnPaint)
   EVT_SIZE(Meter::OnSize)
   EVT_MENU(OnHorizontalID, Meter::OnHorizontal)
   EVT_MENU(OnVerticalID, Meter::OnVertical)
   EVT_MENU(OnMultiID, Meter::OnMulti)
   EVT_MENU(OnEqualizerID, Meter::OnEqualizer)
   EVT_MENU(OnWaveformID, Meter::OnWaveform)
   EVT_MENU(OnLinearID, Meter::OnLinear)
   EVT_MENU(OnDBID, Meter::OnDB)
   EVT_MENU(OnClipID, Meter::OnClip)
   EVT_MENU(OnMonitorID, Meter::OnMonitor)
   EVT_MENU(OnFloatID, Meter::OnFloat)
END_EVENT_TABLE()

IMPLEMENT_CLASS(Meter, wxPanel)

Meter::Meter(wxWindow* parent, wxWindowID id,
             bool isInput,
             const wxPoint& pos /*= wxDefaultPosition*/,
             const wxSize& size /*= wxDefaultSize*/):
   wxPanel(parent, id, pos, size),
   mWidth(size.x), mHeight(size.y),
   mIsInput(isInput),
   mStyle(HorizontalStereo),
   mDB(true),
   mDBRange(60),
   mDecay(true),
   mDecayRate(60),
   mClip(true),
   mNumPeakSamplesToClip(3),
   mPeakHoldDuration(3),
   mT(0),
   mRate(0),
   mNumBars(0),
   mLayoutValid(false),
   mBitmap(NULL),
   mIcon(NULL)
{
   int i;
   
   wxColour backgroundColour =
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   mBkgndBrush = wxBrush(backgroundColour, wxSOLID);
   wxColour origColour(204, 204, 204);
   wxImage *image;
   wxImage *alpha;

   /* i18n-hint: One-letter abbreviation for Left, in VU Meter */
   mLeftText = _("L");
   /* i18n-hint: One-letter abbreviation for Right, in VU Meter */
   mRightText = _("R");

   mLeftSize = wxSize(0, 0);
   mRightSize = wxSize(0, 0);

   if (mIsInput) {
      image = new wxImage(wxBitmap(Mic).ConvertToImage());
      alpha = new wxImage(wxBitmap(MicAlpha).ConvertToImage());
      mPen = wxPen(wxColour(204, 70, 70), 1, wxSOLID);
      mBrush = wxBrush(wxColour(204, 70, 70), wxSOLID);
      mRMSBrush = wxBrush(wxColour(255, 102, 102), wxSOLID);
      mClipBrush = wxBrush(wxColour(255, 53, 53), wxSOLID);
      mLightPen = wxPen(wxColour(255, 153, 153), 1, wxSOLID);
      mDarkPen = wxPen(wxColour(153, 61, 61), 1, wxSOLID);
   }
   else {
      image = new wxImage(wxBitmap(Speaker).ConvertToImage());
      alpha = new wxImage(wxBitmap(SpeakerAlpha).ConvertToImage());
      mPen = wxPen(wxColour(70, 204, 70), 1, wxSOLID);
      mBrush = wxBrush(wxColour(70, 204, 70), wxSOLID);
      mRMSBrush = wxBrush(wxColour(102, 255, 102), wxSOLID);
      mClipBrush = wxBrush(wxColour(255, 53, 53), wxSOLID);
      mLightPen = wxPen(wxColour(153, 255, 153), 1, wxSOLID);
      mDarkPen = wxPen(wxColour(61, 164, 61), 1, wxSOLID);
   }
   wxImage *bkgnd = CreateSysBackground(25, 25, 1,
                                        backgroundColour);
   wxImage *final = OverlayImage(bkgnd, image, alpha, 0, 0);
   mIcon = new wxBitmap(final);

   mRuler.SetFonts(GetFont(), GetFont());

   Reset(44100.0, true);
   for(i=0; i<32; i++)
      mBar[i].clipping = false;

   delete image;
   delete alpha;
   delete bkgnd;
   delete final;
}

Meter::~Meter()
{
   delete mIcon;
   if (mBitmap)
      delete mBitmap;
}

void Meter::OnPaint(wxPaintEvent &evt)
{
   wxPaintDC dc(this);
  #ifdef __WXMAC__
   // Mac OS X automatically double-buffers the screen for you,
   // so our bitmap is unneccessary
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBkgndBrush);
   dc.DrawRectangle(0, 0, mWidth, mHeight);
   HandlePaint(dc);
  #else
   if (!mBitmap)
      mBitmap = new wxBitmap(mWidth, mHeight);
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);
   memDC.SetPen(*wxTRANSPARENT_PEN);
   memDC.SetBrush(mBkgndBrush);
   memDC.DrawRectangle(0, 0, mWidth, mHeight);
   HandlePaint(memDC);
   dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
  #endif 
}

void Meter::OnSize(wxSizeEvent &evt)
{
   delete mBitmap;
   mBitmap = NULL;
   GetClientSize(&mWidth, &mHeight);
   mLayoutValid = false;
}

void Meter::OnMouse(wxMouseEvent &evt)
{
   if (evt.RightDown() ||
       (evt.ButtonDown() && mMenuRect.Inside(evt.m_x, evt.m_y))) {
      wxMenu *menu = new wxMenu();
      // Note: these should be kept in the same order as the enum
      menu->Append(OnHorizontalID, _("Horizontal Stereo"));
      menu->Append(OnVerticalID, _("Vertical Stereo"));
      //menu->Append(OnMultiID, _("Vertical Multichannel"));
      //menu->Append(OnEqualizerID, _("Equalizer"));
      //menu->Append(OnWaveformID, _("Waveform"));
      menu->Enable(OnHorizontalID + mStyle, false);
      menu->AppendSeparator();
      menu->Append(OnLinearID, _("Linear"));
      menu->Append(OnDBID, _("dB"));
      menu->Enable(mDB? OnDBID: OnLinearID, false);
      //menu->AppendSeparator();
      //menu->Append(OnClipID, _("Turn on clipping"));
      if (mIsInput) {
         menu->AppendSeparator();
         menu->Append(OnMonitorID, _("Monitor input"));
      }
      //menu->AppendSeparator();
      //menu->Append(OnFloatID, _("Float Window"));

      if (evt.RightDown())
         PopupMenu(menu, evt.m_x, evt.m_y);
      else
         PopupMenu(menu, mMenuRect.x + 1, mMenuRect.y + mMenuRect.height + 1);
      delete menu;
   }       
   else if (evt.ButtonDown()) {
      if (mIsInput)
         StartMonitoring();
   }
}

void Meter::SetStyle(Meter::Style newStyle)
{
   mStyle = newStyle;
   mLayoutValid = false;
   Refresh(true);
}

void Meter::Reset(double sampleRate, bool resetClipping)
{
   int j;

   mT = 0;
   mRate = sampleRate;
   for(j=0; j<mNumBars; j++)
      ResetBar(&mBar[j], resetClipping);

   mLayoutValid = false;
   Refresh(false);
}

static float floatMax(float a, float b)
{
   return a>b? a: b;
}

static int intmin(int a, int b)
{
   return a<b? a: b;
}

static int intmax(int a, int b)
{
   return a>b? a: b;
}

static float ClipZeroToOne(float z)
{
   if (z > 1.0)
      return 1.0;
   else if (z < 0.0)
      return 0.0;
   else
      return z;
}

static float ToDB(float v, float range)
{
   double db;
   if (v > 0)
      db = 20 * log10(fabs(v));
   else
      db = -999;
   return ClipZeroToOne((db + range) / range);
}

void Meter::UpdateDisplay(int numChannels, int numFrames, float *sampleData)
{
   int i, j;
   float *sptr = sampleData;
   int num = intmin(numChannels, mNumBars);

   MeterUpdateEvent *event = new MeterUpdateEvent();

   event->numFrames = numFrames;
   for(j=0; j<mNumBars; j++) {
      event->peak[j] = 0;
      event->rms[j] = 0;
      event->clipping[j] = false;
      event->headPeakCount[j] = 0;
      event->tailPeakCount[j] = 0;
   }

   for(i=0; i<numFrames; i++) {
      for(j=0; j<num; j++) {
         event->peak[j] = floatMax(event->peak[j], sptr[j]);
         event->rms[j] += sptr[j]*sptr[j];

         // In addition to looking for mNumPeakSamplesToClip peaked
         // samples in a row, also send the number of peaked samples
         // at the head and tail, in case there's a run of 
         // Send the number of peaked samples at the head and tail,
         // in case there's a run of peaked samples that crosses
         // block boundaries
         if (fabs(sptr[j])>=1.0) {
            if (event->headPeakCount[j]==i)
               event->headPeakCount[j]++;
            event->tailPeakCount[j]++;
            if (event->tailPeakCount[j] > mNumPeakSamplesToClip)
               event->clipping[j] = true;
         }
         else
            event->tailPeakCount[j] = 0;
      }
      sptr += numChannels;
   }
   for(j=0; j<mNumBars; j++)
      event->rms[j] = sqrt(event->rms[j]/numFrames);

   if (mDB) {
      for(j=0; j<mNumBars; j++) {
         event->peak[j] = ToDB(event->peak[j], mDBRange);
         event->rms[j] = ToDB(event->rms[j], mDBRange);
      }
   }

   AddPendingEvent(*event);
   delete event;
}

void Meter::OnMeterUpdate(MeterUpdateEvent &evt)
{
   double deltaT = evt.numFrames / mRate;
   int j;

   mT += deltaT;
   for(j=0; j<mNumBars; j++) {
      if (mDecay) {
         if (mDB) {
            float decayAmount = mDecayRate * deltaT / mDBRange;
            mBar[j].peak = floatMax(evt.peak[j],
                                    mBar[j].peak - decayAmount);
         }
         else {
            double decayAmount = mDecayRate * deltaT;
            double decayFactor = pow(10.0, -decayAmount/20);
            mBar[j].peak = floatMax(evt.peak[j],
                                    mBar[j].peak * decayFactor);
         }
      }
      else
         mBar[j].peak = evt.peak[j];

      // This smooths out the RMS signal
      mBar[j].rms = mBar[j].rms * 0.9 + evt.rms[j] * 0.1;

      if (mT - mBar[j].peakHoldTime > mPeakHoldDuration ||
          mBar[j].peak > mBar[j].peakHold) {
         mBar[j].peakHold = mBar[j].peak;
         mBar[j].peakHoldTime = mT;
      }

      if (evt.clipping[j] ||
          mBar[j].tailPeakCount+evt.headPeakCount[j] >= mNumPeakSamplesToClip)
         mBar[j].clipping = true;
      mBar[j].tailPeakCount = evt.tailPeakCount[j];
   }
   RepaintBarsNow();
}

wxFont Meter::GetFont()
{
   int fontSize = 10;
#if defined __WXMSW__
   fontSize = 8;
#endif

   return wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
}

void Meter::ResetBar(MeterBar *b, bool resetClipping)
{
   b->peak = 0.0;
   b->rms = 0.0;
   b->peakHold = 0.0;
   b->peakHoldTime = 0.0;
   if (resetClipping)
      b->clipping = false;
   b->tailPeakCount = 0;
}

void Meter::HandleLayout()
{
   int iconWidth = mIcon->GetWidth();
   int iconHeight = mIcon->GetHeight();
   int menuWidth = 17;
   int menuHeight = 11;
   int width = mWidth;
   int height = mHeight;
   int left = 0, top = 0;
   int right, bottom;
   int barw, barh;
   int i;

   mRuler.SetFlip(true);
   mRuler.SetLabelEdges(true);

   switch(mStyle) {
   default:
      printf("Style not handled yet!\n");
   case VerticalStereo:
      mMenuRect = wxRect(mWidth - menuWidth - 5, mHeight - menuHeight - 2,
                         menuWidth, menuHeight);
      if (mHeight < (menuHeight + iconHeight + 8))
         mIconPos = wxPoint(-999, -999); // Don't display
      else
         mIconPos = wxPoint(mWidth - iconWidth - 1, 1);
      width = intmin(mWidth-(iconWidth+2), mWidth-(menuWidth+3));
      if (width >= mLeftSize.x + mRightSize.x + 24) {
         mLeftTextPos = wxPoint(2, height-2-mLeftSize.y);
         mRightTextPos = wxPoint(width-mLeftSize.x, height-2-mLeftSize.y);
         left += mLeftSize.x+4;
         width -= mLeftSize.x + mRightSize.x + 8;
      }
      barw = (width-2)/2;
      barh = height - 4;
      mNumBars = 2;
      mBar[0].vert = true;
      ResetBar(&mBar[0], false);
      mBar[0].r = wxRect(left + width/2 - barw - 1, 2, barw, barh);
      if (mClip) {
         mBar[0].rClip = mBar[0].r;
         mBar[0].rClip.height = 3;
         mBar[0].r.y += 4;
         mBar[0].r.height -= 4;
      }
      mBar[1].vert = true;
      ResetBar(&mBar[1], false);
      mBar[1].r = wxRect(left + width/2 + 1, 2, barw, barh);
      if (mClip) {
         mBar[1].rClip = mBar[1].r;
         mBar[1].rClip.height = 3;
         mBar[1].r.y += 4;
         mBar[1].r.height -= 4;
      }
      mRuler.SetOrientation(wxVERTICAL);
      mRuler.SetBounds(mBar[1].r.x + mBar[1].r.width + 1,
                       mBar[1].r.y,
                       mBar[1].r.x + width,
                       mBar[1].r.height);
      if (mDB) {
         mRuler.SetRange(0, -mDBRange);
         mRuler.SetFormat(Ruler::LinearDBFormat);
      }
      else {
         mRuler.SetRange(1, 0);
         mRuler.SetFormat(Ruler::RealFormat);
      }
      mRuler.OfflimitsPixels(mMenuRect.y-mBar[1].r.y, mBar[1].r.height);
      break;
   case HorizontalStereo:
      if (mWidth < menuWidth + iconWidth + 8) {
         mIconPos = wxPoint(-999, -999); // Don't display icon
         mMenuRect = wxRect(2, mHeight - menuHeight - 2,
                            menuWidth, menuHeight);
      }         
      else {
         mIconPos = wxPoint(2, mHeight - iconHeight);
         mMenuRect = wxRect(iconWidth + 2, mHeight - menuHeight - 3,
                            menuWidth, menuHeight);
      }
      height = intmin(height-(menuHeight+3), height-iconHeight) - 2;
      left = 2 + intmax(mLeftSize.x, mRightSize.x);
      width -= left;
      mLeftTextPos = wxPoint(2, (height)/4 - mLeftSize.y/2);
      mRightTextPos = wxPoint(2, (height*3)/4 - mLeftSize.y/2);
      barw = width - 4;
      barh = (height-2)/2;
      mNumBars = 2;
      mBar[0].vert = false;
      ResetBar(&mBar[0], false);
      mBar[0].r = wxRect(left+2, height/2 - barh - 1, barw, barh);
      if (mClip) {
         mBar[0].rClip = mBar[0].r;
         mBar[0].rClip.x += mBar[0].rClip.width-3;
         mBar[0].rClip.width = 3;
         mBar[0].r.width -= 4;
      }
      mBar[1].vert = false;
      ResetBar(&mBar[1], false);
      mBar[1].r = wxRect(left+2, height/2 + 1, barw, barh);
      if (mClip) {
         mBar[1].rClip = mBar[1].r;
         mBar[1].rClip.x += mBar[1].rClip.width-3;
         mBar[1].rClip.width = 3;
         mBar[1].r.width -= 4;
      }
      mRuler.SetOrientation(wxHORIZONTAL);
      mRuler.SetBounds(mBar[1].r.x,
                       mBar[1].r.y + mBar[1].r.height + 1,
                       mBar[1].r.x + mBar[1].r.width,
                       mWidth);
      if (mDB) {
         mRuler.SetRange(-mDBRange, 0);
         mRuler.SetFormat(Ruler::LinearDBFormat);
      }
      else {
         mRuler.SetRange(0, 1);
         mRuler.SetFormat(Ruler::RealFormat);
      }
      mRuler.OfflimitsPixels(0, mMenuRect.x+mMenuRect.width-4);
      break;
   case Waveform:
      mNumBars = 0;
      break;
   }

   if (mNumBars > 0) {
      // Compute bounding rectangle of all bars (to save time when
      // blitting just the bars to the screen)
      left = mBar[0].r.x;
      top = mBar[0].r.y;
      right = mBar[0].r.x + mBar[0].r.width;
      bottom = mBar[0].r.y + mBar[0].r.height;
      for(i=1; i<mNumBars; i++) {
         left = intmin(left, mBar[i].r.x);
         top = intmin(top, mBar[i].r.y);
         right = intmax(right, mBar[i].r.x + mBar[i].r.width);
         bottom = intmax(bottom, mBar[i].r.y + mBar[i].r.height);
         left = intmin(left, mBar[i].rClip.x);
         top = intmin(top, mBar[i].rClip.y);
         right = intmax(right, mBar[i].rClip.x + mBar[i].rClip.width);
         bottom = intmax(bottom, mBar[i].rClip.y + mBar[i].rClip.height);

      }
      mAllBarsRect = wxRect(left, top, right-left+1, bottom-top+1);
   }

   mLayoutValid = true;
}

void Meter::HandlePaint(wxDC &dc)
{
   int i;

   dc.SetFont(GetFont());
   if (mLeftSize.x == 0) {
      dc.GetTextExtent(mLeftText, &mLeftSize.x, &mLeftSize.y);
      dc.GetTextExtent(mRightText, &mRightSize.x, &mRightSize.y);
   }

   if (!mLayoutValid)
      HandleLayout();

   dc.DrawBitmap(*mIcon, mIconPos.x, mIconPos.y);

   dc.SetPen(*wxBLACK_PEN);
   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   wxRect r = mMenuRect;
   dc.DrawRectangle(r);
   for(i=2; i<r.height-2; i++)   // Menu triangle
      dc.DrawLine(r.x + i, r.y + i,
                  r.x + r.width - i, r.y + i);
   dc.DrawLine(r.x + r.width, r.y + 1,
               r.x + r.width, r.y + r.height);
   dc.DrawLine(r.x + 1, r.y + r.height,
               r.x + r.width, r.y + r.height);

   if (mNumBars>0)
      mRuler.Draw(dc);
   
   dc.SetFont(GetFont());
   dc.DrawText(mLeftText, mLeftTextPos.x, mLeftTextPos.y);
   dc.DrawText(mRightText, mRightTextPos.x, mRightTextPos.y);
   
   for(i=0; i<mNumBars; i++)
      DrawMeterBar(dc, &mBar[i]);
}

void Meter::RepaintBarsNow()
{
   if (!mLayoutValid)
      return;

   wxClientDC dc(this);
   int i;

   // to limit repaint at <=30 mS intervals, by ChackoN
   static wxLongLong lastTime = ::wxGetLocalTimeMillis();
   wxLongLong now = ::wxGetLocalTimeMillis();
   long delta = (now - lastTime).ToLong();
   if (delta < 30)
       return;
   lastTime = now;

  #ifdef __WXMAC__
   // Mac OS X automatically double-buffers the screen for you,
   // so our bitmap is unneccessary
   for(i=0; i<mNumBars; i++)
      DrawMeterBar(dc, &mBar[i]);
  #else
   if (!mBitmap)
      mBitmap = new wxBitmap(mWidth, mHeight);
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);
   for(i=0; i<mNumBars; i++)
      DrawMeterBar(memDC, &mBar[i]);
   dc.Blit(mAllBarsRect.x, mAllBarsRect.y,
           mAllBarsRect.width, mAllBarsRect.height,
           &memDC, mAllBarsRect.x, mAllBarsRect.y,
           wxCOPY, false);
  #endif 
}

void Meter::DrawMeterBar(wxDC &dc, MeterBar *meterBar)
{
   wxRect r = meterBar->r;
   wxRect rRMS;
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBkgndBrush);
   dc.DrawRectangle(r);
   AColor::Bevel(dc, false, r);

   /*
   AColor::Dark(&dc, false);
   for(i=0; i<mNumTicks; i++)
      if (meterBar->vert)
         dc.DrawLine(r.x+r.width/2-1, mTick[i], r.x+r.width/2+1, mTick[i]);
      else
         dc.DrawLine(mTick[i], r.y+r.height/2-1, mTick[i], r.y+r.height/2+1);
   */

   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.SetPen(mPen);

   if (meterBar->vert) {
      int ht = (int)(meterBar->peakHold * r.height + 0.5);
      dc.DrawLine(r.x+1, r.y + r.height - ht,
                  r.x+r.width, r.y + r.height - ht);
      if (ht > 1)
         dc.DrawLine(r.x+1, r.y + r.height - ht + 1,
                     r.x+r.width, r.y + r.height - ht + 1);
      ht = (int)(meterBar->peak * r.height + 0.5);
      r = wxRect(r.x, r.y + r.height - ht,
                 r.width, ht);
      ht = (int)(meterBar->rms * r.height + 0.5);
      rRMS = wxRect(r.x, r.y + r.height - ht,
                    r.width, ht);
   }
   else {
      int wd = (int)(meterBar->peakHold * r.width + 0.5);
      dc.DrawLine(r.x + wd, r.y + 1, r.x + wd, r.y + r.height);
      if (wd > 1)
         dc.DrawLine(r.x + wd - 1, r.y + 1, r.x + wd - 1, r.y + r.height);
      wd = (int)(meterBar->peak * r.width + 0.5);
      r = wxRect(r.x, r.y,
                 wd, r.height);
      wd = (int)(meterBar->rms * r.width + 0.5);
      rRMS = wxRect(r.x, r.y,
                    wd, r.height);
   }
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBrush);
   dc.DrawRectangle(r);
   dc.SetBrush(mRMSBrush);
   dc.DrawRectangle(rRMS);

   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.SetPen(mLightPen);
   dc.DrawLine(r.x, r.y, r.x + r.width, r.y);
   dc.DrawLine(r.x, r.y, r.x, r.y + r.height);
   dc.SetPen(mDarkPen);
   dc.DrawLine(r.x + r.width, r.y, r.x + r.width, r.y + r.height);
   dc.DrawLine(r.x, r.y + r.height, r.x + r.width + 1, r.y + r.height);

   if (mClip) {
      if (meterBar->clipping)
         dc.SetBrush(mClipBrush);
      else
         dc.SetBrush(mBkgndBrush);
      dc.SetPen(*wxTRANSPARENT_PEN);
      dc.DrawRectangle(meterBar->rClip);
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      AColor::Bevel(dc, false, meterBar->rClip);
   }
}

void Meter::StartMonitoring()
{
   if (gAudioIO->IsMonitoring())
      gAudioIO->StopStream();
   else {
      gAudioIO->StartMonitoring(44100.0);
      AudacityProject *p = GetActiveProject();
      if (p) {
         MeterToolBar *bar;
         bar = p->GetMeterToolBar();
         if (bar) {
            Meter *play, *record;
            bar->GetMeters(&play, &record);
            gAudioIO->SetMeters(record, play);
         }
      }
   }
}

//
// Pop-up menu handlers
//

void Meter::OnHorizontal(wxCommandEvent &evt)
{
   SetStyle(HorizontalStereo);
}

void Meter::OnVertical(wxCommandEvent &evt)
{
   SetStyle(VerticalStereo);
}

void Meter::OnMulti(wxCommandEvent &evt)
{
   SetStyle(VerticalMulti);
}

void Meter::OnEqualizer(wxCommandEvent &evt)
{
   SetStyle(Equalizer);
}

void Meter::OnWaveform(wxCommandEvent &evt)
{
   SetStyle(Waveform);
}

void Meter::OnLinear(wxCommandEvent &evt)
{
   mDB = false;
   mLayoutValid = false;
   Refresh(false);
}

void Meter::OnDB(wxCommandEvent &evt)
{
   mDB = true;
   mLayoutValid = false;
   Refresh(false);
}

void Meter::OnClip(wxCommandEvent &evt)
{
}

void Meter::OnMonitor(wxCommandEvent &evt)
{
   StartMonitoring();
}

void Meter::OnFloat(wxCommandEvent &evt)
{
}


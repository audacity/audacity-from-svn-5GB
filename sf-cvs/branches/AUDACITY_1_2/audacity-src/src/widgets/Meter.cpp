/**********************************************************************

  Audacity: A Digital Audio Editor

  Meter.h

  Dominic Mazzoni

  VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/settings.h>

#include "Meter.h"

#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../../images/MixerImages.h"

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
      memcpy(newEvent->value, value, 32*sizeof(float));
      newEvent->numFrames = numFrames;
      return (wxEvent *)newEvent;
   }

   int numFrames;
   float value[32];
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
   mT(0),
   mRate(0),
   mLayoutValid(false),
   mBitmap(NULL),
   mIcon(NULL)
{
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
      mBrush = wxBrush(wxColour(255, 102, 102), wxSOLID);
      mLightPen = wxPen(wxColour(255, 153, 153), 1, wxSOLID);
      mDarkPen = wxPen(wxColour(153, 61, 61), 1, wxSOLID);
   }
   else {
      image = new wxImage(wxBitmap(Speaker).ConvertToImage());
      alpha = new wxImage(wxBitmap(SpeakerAlpha).ConvertToImage());
      mBrush = wxBrush(wxColour(102, 255, 102), wxSOLID);
      mLightPen = wxPen(wxColour(153, 255, 153), 1, wxSOLID);
      mDarkPen = wxPen(wxColour(61, 164, 61), 1, wxSOLID);
   }
   wxImage *bkgnd = CreateSysBackground(25, 25, 1,
                                        backgroundColour);
   wxImage *final = OverlayImage(bkgnd, image, alpha, 0, 0);
   mIcon = new wxBitmap(final);

   mRuler.SetFonts(GetFont(), GetFont());

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
      menu->Append(OnMultiID, _("Vertical Multichannel"));
      menu->Append(OnEqualizerID, _("Equalizer"));
      menu->Append(OnWaveformID, _("Waveform"));
      menu->Enable(OnHorizontalID + mStyle, false);
      menu->AppendSeparator();
      menu->Append(OnLinearID, _("Linear"));
      menu->Append(OnDBID, _("dB"));
      menu->Enable(mDB? OnDBID: OnLinearID, false);
      menu->AppendSeparator();
      menu->Append(OnClipID, _("Turn on clipping"));
      if (mIsInput) {
         menu->AppendSeparator();
         menu->Append(OnMonitorID, _("Monitor input"));
      }
      menu->AppendSeparator();
      menu->Append(OnFloatID, _("Float Window"));

      if (evt.RightDown())
         PopupMenu(menu, evt.m_x, evt.m_y);
      else
         PopupMenu(menu, mMenuRect.x + 1, mMenuRect.y + mMenuRect.height + 1);
      delete menu;
   }       
}

void Meter::SetStyle(Meter::Style newStyle)
{
   mStyle = newStyle;
   mLayoutValid = false;
   Refresh(true);
}

void Meter::Reset(double sampleRate)
{
   int j;

   mT = 0;
   mRate = sampleRate;
   for(j=0; j<mNumBars; j++)
      mBar[j].value = 0;

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

void Meter::UpdateDisplay(int numChannels, int numFrames, float *sampleData)
{
   int i, j;
   float *sptr = sampleData;
   int num = intmin(numChannels, mNumBars);
   MeterUpdateEvent *event = new MeterUpdateEvent();

   event->numFrames = numFrames;
   for(j=0; j<mNumBars; j++)
      event->value[j] = 0;
   for(i=0; i<numFrames; i++) {
      for(j=0; j<num; j++) {
         event->value[j] = floatMax(event->value[j], sptr[j]);
      }
      sptr += numChannels;
   }
   if (mDB) {
      for(j=0; j<mNumBars; j++) {
         double db;
         if (event->value[j] > 0)
            db = 20 * log10(fabs(event->value[j]));
         else
            db = -999;
         event->value[j] = ClipZeroToOne((db + mDBRange) / mDBRange);
      }
   }
   AddPendingEvent(*event);
   delete event;
}

void Meter::OnMeterUpdate(MeterUpdateEvent &evt)
{
   double deltaT = evt.numFrames / mRate;
   int j;

   for(j=0; j<mNumBars; j++) {
      if (mDecay) {
         if (mDB) {
            float decayAmount = mDecayRate * deltaT / mDBRange;
            mBar[j].value = floatMax(evt.value[j],
                                     mBar[j].value - decayAmount);
         }
         else {
            double decayAmount = mDecayRate * deltaT;
            double decayFactor = pow(10.0, decayAmount/20);
            mBar[j].value = floatMax(evt.value[j],
                                     mBar[j].value * decayFactor);
         }
      }
      else
         mBar[j].value = evt.value[j];
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
      mBar[0].value = 0.0;
      mBar[0].r = wxRect(left + width/2 - barw - 1, 2, barw, barh);
      mBar[1].vert = true;
      mBar[1].value = 0.0;
      mBar[1].r = wxRect(left + width/2 + 1, 2, barw, barh);
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
      mBar[0].value = 0.0;
      mBar[0].r = wxRect(left+2, height/2 - barh - 1, barw, barh);
      mBar[1].vert = false;
      mBar[1].value = 0.0;
      mBar[1].r = wxRect(left+2, height/2 + 1, barw, barh);
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

   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBrush);
   if (meterBar->vert) {
      int ht = (int)(meterBar->value * r.height + 0.5);
      r = wxRect(r.x, r.y + r.height - ht,
                 r.width, ht);
   }
   else {
      int wd = (int)(meterBar->value * r.width + 0.5);
      r = wxRect(r.x, r.y,
                 wd, r.height);
   }
   dc.DrawRectangle(r);
   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.SetPen(mLightPen);
   dc.DrawLine(r.x, r.y, r.x + r.width, r.y);
   dc.DrawLine(r.x, r.y, r.x, r.y + r.height);
   dc.SetPen(mDarkPen);
   dc.DrawLine(r.x + r.width, r.y, r.x + r.width, r.y + r.height);
   dc.DrawLine(r.x, r.y + r.height, r.x + r.width + 1, r.y + r.height);
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
}

void Meter::OnFloat(wxCommandEvent &evt)
{
}


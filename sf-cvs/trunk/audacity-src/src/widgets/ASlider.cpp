/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.cpp

  Dominic Mazzoni

  This class is a custom slider (currently used just for the volume
  control).  It is not very customizable in the sense that the
  background image must be exactly the size you want it, but it does
  allow for a slicker look and feel by allowing you to use images
  for the slider background and the thumb.

**********************************************************************/

#include <math.h>

#include <wx/defs.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/tooltip.h>

#ifdef __WXMSW__
#include <wx/popupwin.h>
#endif

#include "ASlider.h"


#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../Project.h"

#include "../../images/SliderThumb.xpm"

#if defined __WXMSW__
const int sliderFontSize = 10;
#else
const int sliderFontSize = 12;
#endif

//
// TipPanel
//

#ifdef __WXMSW__
class TipPanel : public wxPopupWindow
#else
class TipPanel : public wxPanel
#endif
{
 public:
   TipPanel(wxWindow * parent, wxWindowID id,
            wxString label,
            const wxPoint &pos);
   
   void SetPos(const wxPoint &pos);

   void OnPaint(wxPaintEvent & event);

   wxString label;
   wxString origLabel;

   wxWindow *mParent;

   DECLARE_EVENT_TABLE()
};

#ifdef __WXMSW__

BEGIN_EVENT_TABLE(TipPanel, wxPopupWindow)
   EVT_PAINT(TipPanel::OnPaint)
END_EVENT_TABLE()

TipPanel::TipPanel(wxWindow *parent, wxWindowID id,
                   wxString label, const wxPoint &pos):
   wxPopupWindow(parent)
{
   this->label = label;
   this->origLabel = label;
   mParent = parent;
   SetPos(pos);
}

void TipPanel::SetPos(const wxPoint& pos)
{
   int x = pos.x;
   int y = pos.y;

   if (mParent)
      mParent->ClientToScreen(&x,&y);

   wxClientDC dc(this);
   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   int width, height;
   dc.GetTextExtent(origLabel, &width, &height);
   height += 4;
   SetSize(x - width/2, y, width, height);
}

#else

BEGIN_EVENT_TABLE(TipPanel, wxPanel)
   EVT_PAINT(TipPanel::OnPaint)
END_EVENT_TABLE()

TipPanel::TipPanel(wxWindow *parent, wxWindowID id,
                   wxString label,
                   const wxPoint &pos):
   wxPanel(parent, id)
{
   this->label = label;
   this->origLabel = label;
   SetPos(pos);
}

void TipPanel::SetPos(const wxPoint& pos)
{
   wxClientDC dc(this);
   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   int width, height;
   dc.GetTextExtent(origLabel, &width, &height);
   width += 4;
   height += 4;
   SetSize(pos.x - width/2, pos.y, width, height);   
}

#endif

void TipPanel::OnPaint(wxPaintEvent& event)
{
   wxPaintDC dc(this);
   int width, height, textWidth, textHeight;

   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   GetClientSize(&width, &height);
   dc.SetPen(*wxBLACK_PEN);
   dc.SetBrush(*wxWHITE_BRUSH);
   dc.DrawRectangle(0, 0, width, height);
   dc.GetTextExtent(label, &textWidth, &textHeight);
   dc.DrawText(label, (width-textWidth)/2, (height-textHeight)/2);
}

//
// LWSlider
//

LWSlider::LWSlider(wxWindow *parent,
                   wxString name,
                   const wxPoint &pos,
                   const wxSize &size,
                   int style)
{
   mName = name;
   mValue = 0;
   mIsDragging = false;
   mWidth = size.x;
   mHeight = size.y;
   mParent = parent;
   mStyle = style;
   mHW = false;
   mID = -1;

   if (!(mWidth & 0))
      mWidth--;

   mCenterY = mHeight - 9;

   wxMemoryDC *dc = new wxMemoryDC();
   wxBitmap *bitmap = new wxBitmap(8, 8);
   dc->SelectObject(*bitmap);

   AColor::Medium(dc, false);
   wxColour bkgnd = dc->GetPen().GetColour();
   AColor::Medium(dc, true);   
   wxColour selBkgnd = dc->GetPen().GetColour();

   wxImage *thumbImage = new wxImage(wxBitmap(SliderThumb).ConvertToImage());
   wxImage *thumb1 = ChangeImageColour(thumbImage, bkgnd);
   wxImage *thumb2 = ChangeImageColour(thumbImage, selBkgnd);
   mThumbBitmap = new wxBitmap(thumb1);
   mSelThumbBitmap = new wxBitmap(thumb2);
   delete thumb1;
   delete thumb2;
   delete thumbImage;

   delete dc;
   delete bitmap;

   mThumbWidth = mThumbBitmap->GetWidth();
   mThumbHeight = mThumbBitmap->GetHeight();

   mLeftX = mThumbWidth/2;
   mRightX = mWidth - mThumbWidth/2 - 1;
   mWidthX = mRightX - mLeftX;

   int i;
   for(i=0; i<2; i++) {
      wxBitmap *bitmap;

      bitmap = new wxBitmap(mWidth, mHeight);
      wxMemoryDC *dc = new wxMemoryDC();
      dc->SelectObject(*bitmap);

      AColor::Medium(dc, i==1);
      dc->DrawRectangle(0, 0, mWidth, mHeight);
      AColor::Light(dc, i==1);
      dc->DrawLine(mLeftX, mCenterY, mRightX+1, mCenterY);
      AColor::Dark(dc, i==1);
      dc->DrawLine(mLeftX, mCenterY+1, mRightX+1, mCenterY+1);

      int divs = 10;
      double upp = divs / (double)(mWidthX-1);
      double d = 0;
      int int_d = -1;
      for(int p=0; p<=mWidthX; p++) {
         if (((int)d) > int_d) {
            int_d = (int)d;
            int ht = (int_d==0 || int_d==divs? 5: 3);
            AColor::Light(dc, false);
            dc->DrawLine(mLeftX+p, mCenterY-ht, mLeftX+p, mCenterY);
            AColor::Dark(dc, false);
            dc->DrawLine(mLeftX+p+1, mCenterY-ht+1, mLeftX+p+1, mCenterY);
         }
         d += upp;
      }

      switch(mStyle) {
      case FRAC_SLIDER:
      case DB_SLIDER:
         dc->SetPen(*wxBLACK_PEN);
         dc->DrawLine(mLeftX, mCenterY-10, mLeftX+5, mCenterY-10);
         dc->DrawLine(mRightX-7, mCenterY-10, mRightX-2, mCenterY-10);
         dc->DrawLine(mRightX-5, mCenterY-12, mRightX-5, mCenterY-7);
         break;
      case PAN_SLIDER:
         wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
         dc->SetFont(labelFont);
         dc->DrawText(_("L"), mLeftX, 1);
         dc->DrawText(_("R"), mRightX-7, 1);
      }
      
      delete dc;

      if (i==0)
         mBitmap = bitmap;
      else
         mSelBitmap = bitmap;
   }

   int x=mWidth/2, y=mHeight, wx, wy;
   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top->GetPosition(&wx, &wy);
      x += wx;
      y += wy;
      top = top->GetParent();
   }
   
   mPopWin = NULL;
   Move(pos);
   CreatePopWin();
}

LWSlider::~LWSlider()
{
   delete mBitmap;
   delete mSelBitmap;
   delete mThumbBitmap;
   delete mSelThumbBitmap;
   delete mPopWin;
}

void LWSlider::SetId(wxWindowID id)
{
   mID = id;
}

void LWSlider::CreatePopWin()
{
   if (mPopWin)
      return;

   wxString maxStr = mName + ": 000000";

   if (mStyle == PAN_SLIDER || mStyle == DB_SLIDER)
      maxStr += "000";

   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top = top->GetParent();
   }

   mPopWin = new TipPanel(top, -1, maxStr, wxDefaultPosition);
   mPopWin->Hide();
}

void LWSlider::SetPopWinPosition()
{
   int x, y, wx, wy;

   x=mWidth/2 + mLeft;
   y=mHeight + mTop + 1;
   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top->GetPosition(&wx, &wy);
      x += wx;
      y += wy;
      top = top->GetParent();
   }

   if (mPopWin)
      ((TipPanel *)mPopWin)->SetPos(wxPoint(x, y));
}

void LWSlider::Move(const wxPoint &newpos)
{
   mLeft = newpos.x;
   mTop = newpos.y;
}

void LWSlider::RecreateTipWin()
{
   delete mPopWin;
   mPopWin = NULL;
   CreatePopWin();
}

void LWSlider::OnPaint(wxDC &dc, bool selected)
{
   //thumbPos should be in pixels
   int thumbPos = mValue;
   int thumbY = mCenterY - (mThumbHeight/2);
   wxBitmap *bitmap;
   wxBitmap *thumbBitmap;

   if (selected) {
      bitmap = mSelBitmap;
      thumbBitmap = mSelThumbBitmap;
   }
   else {
      bitmap = mBitmap;
      thumbBitmap = mThumbBitmap;
   }

#if defined(__WXMAC__) || defined(__WXMSW__)
   dc.DrawBitmap(*bitmap, mLeft, mTop);
   dc.DrawBitmap(*thumbBitmap, mLeft+thumbPos, mTop+thumbY);
#else
   wxMemoryDC memDC;
   memDC.SelectObject(*bitmap);
   dc.Blit(mLeft, mTop, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
   memDC.SelectObject(*thumbBitmap);
   dc.Blit(mLeft+thumbPos, mTop+thumbY, mThumbWidth, mThumbHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
#endif

   if (mPopWin)
      mPopWin->Refresh();
}

void LWSlider::FormatPopWin()
{
   wxString label;
   float val = (mValue / (float)mWidthX);

   switch(mStyle) {
   case FRAC_SLIDER:
      label.Printf("%s: %.1f", (const char *)mName, val);
      break;
   case DB_SLIDER:
      val = (((int)(val*24.0))-12)*3.0;
      if (val == 0.0)
         label.Printf("%s: 0 dB", (const char *)mName);
      else if (val < 0)
         label.Printf("%s: %.0f dB", (const char *)mName, val);
      else if (val > 0)
         label.Printf("%s: +%.0f dB", (const char *)mName, val);
      break;
   case PAN_SLIDER:
      val = (val * 2.0) - 1.0;
      if (val >= -0.05 && val <= 0.05)
         label.Printf("%s: %s", (const char *)mName,
                      _("Center"));
      else if (val < 0.0)
         label.Printf("%s: %.1f %s", (const char *)mName,
                      -val, _("Left"));
      else if (val > 0.0)
         label.Printf("%s: %.1f %s", (const char *)mName,
                      val, _("Right"));
         
      break;
   }

   ((TipPanel *)mPopWin)->label = label;
}

void LWSlider::OnMouseEvent(wxMouseEvent & event)
{
   if (event.Entering()) {
      // Display the tooltip in the status bar
      if (mParent->GetToolTip()) {
         wxString tip = mParent->GetToolTip()->GetTip();
         GetActiveProject()->TP_DisplayStatusMessage(tip, 0);
         Refresh();
      }
   }
   else if (event.Leaving()) {
      GetActiveProject()->TP_DisplayStatusMessage("",0);
      Refresh();
   }
   
   int oldValue = mValue;

   if (event.ButtonDown()) {

      //This jumps the thumb to clicked position
      if (!mIsDragging) {

         //First, figure out where the thumb should go:
         //The thumb should go at event.m_x - thumbwidth/2,
         //BUT, shouldn't be less than 0 or greater than width - thumbwidth

         int newValue = (event.m_x - mLeft) - mLeftX;
         if (newValue < 0)
            newValue = 0;
         if (newValue > mWidthX)
            newValue = mWidthX;

         if (abs(newValue - mValue) > mThumbWidth/2) {
            mValue = newValue;
         }

         mClickValue = mValue;
         mClickX = (event.m_x - mLeft);

         mIsDragging = true;
         mParent->CaptureMouse();

         FormatPopWin();
         SetPopWinPosition();
         mPopWin->Show();
      }

      return;

   } else if (event.ButtonUp() && mIsDragging) {
      mIsDragging = false;
      mParent->ReleaseMouse();
      mPopWin->Hide();
   } else if (event.Dragging() && mIsDragging) {
      //If we're dragging, figure out where the thumb should go
      int delta = (event.m_x - mLeft) - mClickX;
      mValue = mClickValue + delta;

      if (mValue < 0)
         mValue = 0;
      if (mValue > mWidthX)
         mValue = mWidthX;
   }

   if (oldValue != mValue) {
      FormatPopWin();
      mPopWin->Refresh();
      Refresh();

      wxCommandEvent *e =
         new wxCommandEvent(wxEVT_COMMAND_SLIDER_UPDATED, mID);
      e->SetInt( mValue * 1000 / mWidthX );
      mParent->ProcessEvent(*e);
      delete e;
   }
}

float LWSlider::Get()
{
   float val = (mValue / (float)mWidthX);
   float rval;

   switch(mStyle) {
   default:
   case FRAC_SLIDER:
      rval = val;
      break;
   case PAN_SLIDER:
      rval = (val*2.0)-1.0;
      break;
   case DB_SLIDER:
      val = (val*72.0)-36.0;
      rval = pow(10, val/20);
      break;
   }

   return rval;
}

void LWSlider::Set(float value)
{
   switch(mStyle) {
   default:
   case FRAC_SLIDER:
      break;
   case PAN_SLIDER:
      value = (value + 1.0) / 2.0;
      break;
   case DB_SLIDER:
      value = 20*log10(value);
      value = (value + 36.0) / 72.0;
      break;
   }

   mValue = (int)(value * mWidthX + 0.5);
   if (mValue < 0)
      mValue = 0;
   if (mValue > mWidthX)
      mValue = mWidthX;

   Refresh();
}

void LWSlider::Refresh()
{
   if (mHW)
      mParent->Refresh(false);
}

//
// ASlider
//

BEGIN_EVENT_TABLE(ASlider, wxWindow)
   EVT_MOUSE_EVENTS(ASlider::OnMouseEvent)
   EVT_PAINT(ASlider::OnPaint)
END_EVENT_TABLE()

ASlider::ASlider(wxWindow * parent, wxWindowID id,
                 wxString name,
                 const wxPoint & pos,
                 const wxSize & size):
   wxWindow(parent, id, pos, size)
{
   mLWSlider = new LWSlider(this, name, wxPoint(0, 0), size, FRAC_SLIDER);
   mLWSlider->SetId(id);
   mLWSlider->mHW = true;
}

ASlider::~ASlider()
{
   delete mLWSlider;
}

void ASlider::OnPaint(wxPaintEvent &event)
{
   wxPaintDC dc(this);

   mLWSlider->OnPaint(dc, false);
}

void ASlider::OnMouseEvent(wxMouseEvent &event)
{
   mLWSlider->OnMouseEvent(event);
}

void ASlider::RecreateTipWin()
{
   mLWSlider->RecreateTipWin();
}

float ASlider::Get()
{
   return mLWSlider->Get();
}

void ASlider::Set(float value)
{
   mLWSlider->Set(value);
}

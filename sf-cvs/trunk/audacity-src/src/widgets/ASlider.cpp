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

#include <wx/defs.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/tooltip.h>

#include "ASlider.h"

#include "../AColor.h"
#include "../Project.h"

#include "../../images/SliderThumb.xpm"

//
// TipPanel
//

class TipPanel : public wxPanel
{
 public:
   TipPanel(wxWindow * parent, wxWindowID id,
            wxString label,
            const wxPoint & pos);
   
   void OnPaint(wxPaintEvent & event);

   wxString label;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(TipPanel, wxPanel)
   EVT_PAINT(TipPanel::OnPaint)
END_EVENT_TABLE()

TipPanel::TipPanel(wxWindow * parent, wxWindowID id,
                   wxString label,
                   const wxPoint & pos):
   wxPanel(parent, id)
{
   wxClientDC dc(this);
   int width, height;
   dc.GetTextExtent(label, &width, &height);
   width += 4;
   height += 4;
   SetSize(pos.x - width/2, pos.y, width, height);
   this->label = label;
}

void TipPanel::OnPaint(wxPaintEvent & event)
{
   wxPaintDC dc(this);
   int width, height, textWidth, textHeight;

   GetClientSize(&width, &height);
   dc.SetPen(*wxBLACK_PEN);
   dc.SetBrush(*wxWHITE_BRUSH);
   dc.DrawRectangle(0, 0, width, height);

   dc.GetTextExtent(label, &textWidth, &textHeight);
   dc.DrawText(label, (width-textWidth)/2, (height-textHeight)/2);
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
   mName = name;
   mValue = 0;
   mIsDragging = false;
   GetSize(&mWidth, &mHeight);
   
   mCenterY = mHeight - 9;

   mThumbBitmap = new wxBitmap(SliderThumb);

   mThumbWidth = mThumbBitmap->GetWidth();
   mThumbHeight = mThumbBitmap->GetHeight();

   mLeftX = mThumbWidth/2;
   mRightX = mWidth - mThumbWidth/2 - 1;
   mWidthX = mRightX - mLeftX;

   mBitmap = new wxBitmap(mWidth, mHeight);
   wxMemoryDC *dc = new wxMemoryDC();
   dc->SelectObject(*mBitmap);
   AColor::Medium(dc, false);
   dc->DrawRectangle(0, 0, mWidth, mHeight);
   AColor::Light(dc, false);
   dc->DrawLine(mLeftX, mCenterY, mRightX+1, mCenterY);
   AColor::Dark(dc, false);   
   dc->DrawLine(mLeftX, mCenterY+1, mRightX+1, mCenterY+1);

   int divs = 10;
   double upp = divs / (double)(mWidthX-1);
   double d = 0;
   int id = -1;
   for(int p=0; p<=mWidthX; p++) {
      if (((int)d) > id) {
         id = (int)d;
         int ht = (id==0 || id==divs? 5: 3);
         AColor::Light(dc, false);
         dc->DrawLine(mLeftX+p, mCenterY-ht, mLeftX+p, mCenterY);
         AColor::Dark(dc, false);
         dc->DrawLine(mLeftX+p+1, mCenterY-ht+1, mLeftX+p+1, mCenterY);
      }
      d += upp;
   }

   dc->SetPen(*wxBLACK_PEN);
   dc->DrawLine(mLeftX, mCenterY-10, mLeftX+5, mCenterY-10);
   dc->DrawLine(mRightX-7, mCenterY-10, mRightX-2, mCenterY-10);
   dc->DrawLine(mRightX-5, mCenterY-12, mRightX-5, mCenterY-7);

   delete dc;

   //
   // Create pop-up window to show while dragging
   //

   int x=mWidth/2, y=mHeight, wx, wy;
   wxWindow *top = this;
   while(top && !top->IsTopLevel()) {
      top->GetPosition(&wx, &wy);
      x += wx;
      y += wy;
      top = top->GetParent();
   }
   
   mPopWin = new TipPanel(top, -1,
                          mName + ": 000000",
                          wxPoint(x, y));
   mPopWin->Hide();
}

ASlider::~ASlider()
{
   delete mBitmap;
   delete mThumbBitmap;
   delete mPopWin;
}

void ASlider::OnPaint(wxPaintEvent & event)
{
   wxPaintDC dc(this);

   //thumbPos should be in pixels
   int thumbPos = mValue;
   int thumbY = mCenterY - (mThumbHeight/2);

#if defined(__WXMAC__) || defined(__WXMSW__)
   dc.DrawBitmap(*mBitmap, 0, 0);
   dc.DrawBitmap(*mThumbBitmap, thumbPos, thumbY);
#else
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);
   dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
   memDC.SelectObject(*mThumbBitmap);
   dc.Blit(thumbPos, thumbY, mThumbWidth, mThumbHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
#endif
}

void ASlider::FormatPopWin()
{
   ((TipPanel *)mPopWin)->label =
      wxString::Format("%s: %.1f", (const char *)mName, Get());
}

void ASlider::OnMouseEvent(wxMouseEvent & event)
{
   if (event.Entering()) {
      // Display the tooltip in the status bar
      if (GetToolTip()) {
         wxString tip = GetToolTip()->GetTip();
         GetActiveProject()->TP_DisplayStatusMessage(tip, 0);
         this->Refresh(false);
      }
   }
   else if (event.Leaving()) {
      GetActiveProject()->TP_DisplayStatusMessage("",0);
      this->Refresh(false);
   }
   
   int oldValue = mValue;

   if (event.ButtonDown()) {

      //This jumps the thumb to clicked position
      if (!mIsDragging) {

         //First, figure out where the thumb should go:
         //The thumb should go at event.m_x - thumbwidth/2,
         //BUT, shouldn't be less than 0 or greater than width - thumbwidth

         int newValue = event.m_x - mLeftX;
         if (newValue < 0)
            newValue = 0;
         if (newValue > mWidthX)
            newValue = mWidthX;

         if (abs(newValue - mValue) > mThumbWidth/2) {
            mValue = newValue;
            this->Refresh(false);
         }

         mClickValue = mValue;
         mClickX = event.m_x;

         mIsDragging = true;
         CaptureMouse();

         FormatPopWin();
         mPopWin->Show();
      }

   } else if (event.ButtonUp() && mIsDragging) {
      mIsDragging = false;
      ReleaseMouse();
      mPopWin->Hide();
   } else if (event.Dragging() && mIsDragging) {
      //If we're dragging, figure out where the thumb should go
      int delta = event.m_x - mClickX;
      mValue = mClickValue + delta;

      if (mValue < 0)
         mValue = 0;
      if (mValue > mWidthX)
         mValue = mWidthX;
   }

   if (oldValue != mValue || event.ButtonDown()) {
      this->Refresh(false);
      FormatPopWin();
      mPopWin->Refresh();
   }
}

float ASlider::Get()
{
   return (mValue / (float)mWidthX);
}

void ASlider::Set(float value)
{
   mValue = (int)(value * mWidthX + 0.5);
   if (mValue < 0)
      mValue = 0;
   if (mValue > mWidthX)
      mValue = mWidthX;

   this->Refresh(false);
}

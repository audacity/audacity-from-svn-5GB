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

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/image.h>

#include "ASlider.h"

BEGIN_EVENT_TABLE(ASlider, wxWindow)
   EVT_MOUSE_EVENTS(ASlider::OnMouseEvent)
   EVT_PAINT(ASlider::OnPaint)
END_EVENT_TABLE()

ASlider::ASlider(wxWindow * parent, wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 wxImage * slider,
                 wxImage * thumb,
                 int max):wxWindow(parent, id, pos, size)
{
   mMax = max;
   mValue = 0;
   mIsDragging = false;
   mBitmap = new wxBitmap(slider->ConvertToBitmap());
   mThumbBitmap = new wxBitmap(thumb->ConvertToBitmap());
   mThumbWidth = mThumbBitmap->GetWidth();
   mThumbHeight = mThumbBitmap->GetHeight();
   GetSize(&mWidth, &mHeight);
}

ASlider::~ASlider()
{
   delete mBitmap;
   delete mThumbBitmap;
}

void ASlider::OnPaint(wxPaintEvent & event)
{
   wxPaintDC dc(this);

   //thumbPos should be in pixels
   int thumbPos = mValue * (mWidth - 1 - mThumbWidth) / (mMax + 1);
   int thumbY = (mHeight - mThumbHeight) / 2;

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

void ASlider::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown()) {

      //This jumps the thumb to clicked position
      if (!mIsDragging) {

         //First, figure out where the thumb should go:
         //The thumb should go at event.m_x - thumbwidth/2,
         //BUT, shouldn't be less than 0 or greater than width - thumbwidth

         int newthumbPos = event.m_x - mThumbWidth / 2;
         if (newthumbPos < 0)
            newthumbPos = 0;
         if (newthumbPos > (mWidth - 1 - mThumbWidth))
            newthumbPos = mWidth - 1 - mThumbWidth;

         //calculate where (in pixels) the thumb currently is
         int thumbPos = mValue * (mWidth - 1 - mThumbWidth) / (mMax + 1);

         //Only move the thumb if we aren't clicking on it already
         if ((newthumbPos < thumbPos)
             || (newthumbPos > (thumbPos + mThumbWidth))) {
            thumbPos = newthumbPos;

            //Calculate the new value, based on thumbPos
            mValue = thumbPos * (mMax + 1) / (mWidth - 1 - mThumbWidth);
            this->Refresh(false);
         }

         mIsDragging = true;
         CaptureMouse();
      }

   } else if (event.ButtonUp() && mIsDragging) {
      mIsDragging = false;
      ReleaseMouse();
   } else if (mIsDragging) {
      //If we're dragging, figure out where the thumb should go
      int newthumbPos = event.m_x - mThumbWidth / 2;
      if (newthumbPos < 0)
         newthumbPos = 0;
      if (newthumbPos > (mWidth - 1 - mThumbWidth))
         newthumbPos = mWidth - 1 - mThumbWidth;

      //Calculate a new mValue based on new thumb position
      int newValue = newthumbPos * (mMax + 1) / (mWidth - 1 - mThumbWidth);

      //Redraw if necessary
      if (newValue != mValue) {
         mValue = newValue;
         this->Refresh(false);
      }
   }
}

int ASlider::Get()
{
   return mValue;
}

void ASlider::Set(int value)
{
   mValue = value;
   if (mValue < 0)
      mValue = 0;
   if (mValue > mMax)
      mValue = mMax;
   this->Refresh(false);
}

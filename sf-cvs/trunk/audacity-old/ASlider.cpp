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

ASlider::ASlider(wxWindow *parent, wxWindowID id,
				 const wxPoint& pos,
				 const wxSize& size,
				 wxString sliderImage,
				 wxString thumbImage,
				 int max) :
  wxWindow(parent, id, pos, size)
{
  mMax = max;
  mValue = 0;

  mIsDragging = false;

  mImage = new wxImage(sliderImage);
  mThumbImage = new wxImage(thumbImage);

  mThumbWidth = mThumbImage->GetWidth();
  mThumbHeight = mThumbImage->GetHeight();

  GetSize(&mWidth, &mHeight);
}

ASlider::~ASlider()
{
  delete mImage;
  delete mThumbImage;
}

void ASlider::OnPaint(wxPaintEvent& event)
{
  wxPaintDC dc(this);
  wxMemoryDC memDC;
  memDC.SelectObject(mImage->ConvertToBitmap());  
  dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
  int thumbPos = mValue * (mWidth - mThumbWidth) / mMax;
  int thumbY = (mHeight - mThumbHeight) / 2;
  memDC.SelectObject(mThumbImage->ConvertToBitmap());  
  dc.Blit(thumbPos, thumbY, mThumbWidth, mThumbHeight,
		  &memDC, 0, 0, wxCOPY, FALSE);
}

void ASlider::OnMouseEvent(wxMouseEvent& event)
{
  if (event.ButtonDown()) {
    mIsDragging = true;
    CaptureMouse();
	mInitialX = event.m_x;
	mInitialY = event.m_y;
	mInitialPos = mValue * (mWidth - mThumbWidth) / mMax;
  }

  if (event.ButtonUp()) {
    mIsDragging = false;
    ReleaseMouse();
  }

  if (mIsDragging) {
	int newPos = mInitialPos + (event.m_x - mInitialX);
	int newValue = newPos * mMax / (mWidth - mThumbWidth);
	if (newValue < 0)
	  newValue = 0;
	if (newValue > mMax)
	  newValue = mMax;
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
  this->Refresh(false);
}

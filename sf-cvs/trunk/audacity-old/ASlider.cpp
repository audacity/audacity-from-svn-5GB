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

#include "AButton.h"
#include "ASlider.h"

BEGIN_EVENT_TABLE(ASlider, wxWindow)
  EVT_MOUSE_EVENTS(ASlider::OnMouseEvent)
  EVT_PAINT(ASlider::OnPaint)
END_EVENT_TABLE()

ASlider::ASlider(wxWindow *parent, wxWindowID id,
				 const wxPoint& pos,
				 const wxSize& size,
				 char **sliderXPM,
				 char **thumbXPM,
				 int max) :
  wxWindow(parent, id, pos, size)
{
  mMax = max;
  mValue = 0;

  mIsDragging = false;

  mBitmap = new wxBitmap((const char **)sliderXPM);

  mThumbBitmap = new wxBitmap((const char **)thumbXPM);

  mThumbWidth = mThumbBitmap->GetWidth();
  mThumbHeight = mThumbBitmap->GetHeight();

  GetSize(&mWidth, &mHeight);
}

ASlider::~ASlider()
{
  delete mBitmap;
  delete mThumbBitmap;
}

void ASlider::OnPaint(wxPaintEvent& event)
{
  wxPaintDC dc(this);
  int thumbPos = mValue * (mWidth - mThumbWidth) / mMax;
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

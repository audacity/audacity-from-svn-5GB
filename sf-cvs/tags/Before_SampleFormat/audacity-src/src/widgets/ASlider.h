/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.h

  Dominic Mazzoni

  This class is a custom slider (currently used just for the volume
  control).  It is not very customizable in the sense that the
  background image must be exactly the size you want it, but it does
  allow for a slicker look and feel by allowing you to use images
  for the slider background and the thumb.

**********************************************************************/

#ifndef __AUDACITY_SLIDER__
#define __AUDACITY_SLIDER__

#include <wx/window.h>

class wxBitmap;

class ASlider:public wxWindow {
 public:

   ASlider(wxWindow * parent, wxWindowID id,
           const wxPoint & pos,
           const wxSize & size,
           wxImage * slider, wxImage * thumb, int max);

    virtual ~ ASlider();

   virtual int Get();
   virtual void Set(int value);

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnMouseEvent(wxMouseEvent & event);

 private:

   int mWidth;                  //In pixels
   int mHeight;                 //In pixels

   int mThumbWidth;             //In pixels
   int mThumbHeight;            //In pixels

   int mValue;                  //slider value units
   int mMax;                    //max slider value units

   bool mIsDragging;

   wxBitmap *mBitmap;
   wxBitmap *mThumbBitmap;

 public:

    DECLARE_EVENT_TABLE()
};

#endif

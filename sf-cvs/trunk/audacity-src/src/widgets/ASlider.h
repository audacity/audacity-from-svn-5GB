/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.h

  Dominic Mazzoni

  This class is a custom slider.  Values are always returned
  between 0.0 and 1.0, inclusive.

**********************************************************************/

#ifndef __AUDACITY_SLIDER__
#define __AUDACITY_SLIDER__

#include <wx/defs.h>
#include <wx/window.h>

class wxBitmap;
class wxImage;
class wxSize;
class wxPoint;

class ASlider:public wxWindow {
 public:

   ASlider(wxWindow * parent, wxWindowID id,
           wxString name,
           const wxPoint & pos,
           const wxSize & size);

    virtual ~ ASlider();

   virtual float Get();
   virtual void Set(float value);

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnMouseEvent(wxMouseEvent & event);

 private:

   void FormatPopWin();

   int mWidth;                  //In pixels
   int mHeight;                 //In pixels

   int mCenterY;

   int mLeftX;
   int mRightX;
   int mWidthX;

   int mThumbWidth;             //In pixels
   int mThumbHeight;            //In pixels

   int mValue;                  //slider position, 0...mWidthX

   int mClickValue;
   int mClickX;

   wxWindow *mPopWin;

   bool mIsDragging;

   wxBitmap *mBitmap;
   wxBitmap *mThumbBitmap;

   wxString mName;

 public:

    DECLARE_EVENT_TABLE()
};

#endif

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

#define FRAC_SLIDER 1    // 0.0...1.0
#define DB_SLIDER 2      // -36...36 dB
#define PAN_SLIDER 3     // -1.0...1.0

//
// Lightweight slider - i.e. a slider that doesn't appear in
// its own window, but rather draws itself inside an existing
// window (used inside Track Labels).  The ASlider class,
// which uses this class, is below.
//

class LWSlider
{
   friend class ASlider;

 public:

   LWSlider(wxWindow * parent,
            wxString name,
            const wxPoint &pos,
            const wxSize &size,
            int style);

   virtual ~LWSlider();

   float Get();
   void Set(float value);

   void Move(const wxPoint &newpos);

   void OnPaint(wxDC &dc, bool selected);
   void OnMouseEvent(wxMouseEvent &event);
   void Refresh();

 private:

   void FormatPopWin();

   wxWindow *mParent;

   bool mHW; // is it really heavyweight (in a window)

   int mStyle;                  // FRAC, PAN, or DB

   int mLeft;
   int mTop;

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
   wxBitmap *mSelBitmap;
   wxBitmap *mThumbBitmap;
   wxBitmap *mSelThumbBitmap;

   wxString mName;

};

class ASlider :public wxWindow
{
 public:
   ASlider(wxWindow * parent, wxWindowID id,
           wxString name,
           const wxPoint & pos,
           const wxSize & size);

   virtual ~ASlider();
   
   float Get();
   void Set(float value);

   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);

 private:
   LWSlider *mLWSlider;

 public:
    DECLARE_EVENT_TABLE()
};

#endif

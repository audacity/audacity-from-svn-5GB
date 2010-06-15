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

//
// Predefined slider types (mStyle)
//
#define FRAC_SLIDER 1    // 0.0...1.0
#define DB_SLIDER 2      // -36...36 dB
#define PAN_SLIDER 4     // -1.0...1.0

#define NO_AQUA 0x100

// Customizable slider only: If stepValue is STEP_CONTINUOUS,
// every value on the slider between minValue and maxValue
// will be possible
//
#define STEP_CONTINUOUS 0.0f

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

   // MM: Construct customizable slider
   LWSlider(wxWindow * parent,
       wxString name,
       const wxPoint &pos,
       const wxSize &size,
       float minValue,
       float maxValue,
       float stepValue,
       bool canUseShift,
       int style,
       int orientation = wxHORIZONTAL, // wxHORIZONTAL or wxVERTICAL 
       bool heavyweight=false
       );
    
   // Construct predefined slider
   LWSlider(wxWindow * parent,
            wxString name,
            const wxPoint &pos,
            const wxSize &size,
            int style,
            int orientation = wxHORIZONTAL, // wxHORIZONTAL or wxVERTICAL 
            bool heavyweight=false);

   void Init(wxWindow * parent,
      wxString name,
      const wxPoint &pos,
      const wxSize &size,
      float minValue,
      float maxValue,
      float stepValue,
      bool canUseShift,
      int style,
      bool aquaOk,
      int orientation = wxHORIZONTAL, // wxHORIZONTAL or wxVERTICAL 
      bool heavyweight=false
   );

   virtual ~LWSlider();

   void SetId(wxWindowID id);

   float Get();
   void Set(float value);

   void Move(const wxPoint &newpos);

   void OnPaint(wxDC &dc, bool selected);
   void OnMouseEvent(wxMouseEvent &event);
   void OnSize(const wxSize &size);

   void Refresh();

   void RecreateTipWin();

 private:

   void FormatPopWin();
   void SetPopWinPosition();
   void CreatePopWin();

   int ValueToPosition(float val);
   float PositionToValue(int fromPos, bool shiftDown);
      
   wxWindow *mParent;

   int mStyle;
   bool mAquaOk;
   int mOrientation; // wxHORIZONTAL or wxVERTICAL 

   bool mHW; // is it really heavyweight (in a window)

   int mLeft;
   int mTop;

   int mWidth;                  //In pixels
   int mHeight;                 //In pixels

   // for (mOrientation == wxHORIZONTAL)
   int mCenterY;

   int mLeftX;
   int mRightX;
   int mWidthX;

   // for (mOrientation == wxVERTICAL) //v Vertical PAN_SLIDER currently not handled, forced to horizontal.
   int mCenterX;

   int mTopY;
   int mBottomY; // low values at bottom 
   int mHeightY;


   int mThumbWidth;             //In pixels
   int mThumbHeight;            //In pixels

   int mClickValue;
   int mClickX;

   float mMinValue;
   float mMaxValue;
   float mStepValue;
   
   float mCurrentValue;

   bool mCanUseShift;

   wxWindowID mID;

   wxWindow *mPopWin;

   bool mIsDragging;

   wxBitmap *mBitmap;
   wxBitmap *mSelBitmap;
   wxBitmap *mThumbBitmap;
   wxBitmap *mSelThumbBitmap;
   wxColour mBkgndColor;
   wxColour mSelBkgndColor;

   wxString mName;

};

class ASlider :public wxWindow
{
 public:
   ASlider(wxWindow * parent, wxWindowID id,
           wxString name,
           const wxPoint & pos,
           const wxSize & size, 
           int style = FRAC_SLIDER, 
           int orientation = wxHORIZONTAL);

   virtual ~ASlider();
   
   float Get();
   void Set(float value);

   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void OnSize(wxSizeEvent &event);

   void RecreateTipWin();

 private:
   LWSlider *mLWSlider;

 public:
    DECLARE_EVENT_TABLE()
};

#endif

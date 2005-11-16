/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.h

  Dominic Mazzoni

  This class is a custom slider.  

**********************************************************************/

#ifndef __AUDACITY_SLIDER__
#define __AUDACITY_SLIDER__

#include <wx/defs.h>
#include <wx/window.h>
#include <wx/dialog.h>

class wxBitmap;
class wxImage;
class wxSize;
class wxPoint;
class wxTextCtrl;
class wxButton;

//
// Predefined slider types
//
#define FRAC_SLIDER 1    // 0.0...1.0
#define DB_SLIDER 2      // -36...36 dB
#define PAN_SLIDER 3     // -1.0...1.0
#define SPEED_SLIDER 4  // 0.0 ..3.0 

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
       bool heavyweight=false
       );
    
   // Construct predefined slider
   LWSlider(wxWindow * parent,
            wxString name,
            const wxPoint &pos,
            const wxSize &size,
            int style,
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
      bool heavyweight=false
   );

   virtual ~LWSlider();

   void SetId(wxWindowID id);

   float Get();
   void Set(float value);

   void Increase(int steps);
   void Decrease(int steps);

   void Move(const wxPoint &newpos);

   void OnPaint(wxDC &dc, bool selected);
   void OnMouseEvent(wxMouseEvent &event);
   void Refresh();

   void RecreateTipWin();

 private:

   void FormatPopWin();
   void SetPopWinPosition();
   void CreatePopWin();

   int ValueToPosition(float val);
   float PositionToValue(int xPos, bool shiftDown);
      
   wxWindow *mParent;

   int mStyle;

   bool mHW; // is it really heavyweight (in a window)

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

   wxString mName;

};

class ASlider :public wxWindow
{
 public:
   ASlider(wxWindow * parent, wxWindowID id,
           wxString name,
           const wxPoint & pos,
           const wxSize & size);
   ASlider(wxWindow * parent, wxWindowID id,
           wxString name, const wxPoint & pos, 
           const wxSize & size,
           int style);
   virtual ~ASlider();
   
   float Get();
   void Set(float value);

   void Increase(int steps);
   void Decrease(int steps);

   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);

   void RecreateTipWin();

 private:
   LWSlider *mLWSlider;

 public:
    DECLARE_EVENT_TABLE()
};



#define SLIDER_DIALOG_TEXTCTRL 100


// This is a modal dialog that contains an ASlider
// and a text-entry box which can be used to set the
// value of a slider.
class SliderDialog: public wxDialog
{
 public:
   SliderDialog(wxWindow * parent, wxWindowID id,
                const wxString & title, 
                wxPoint position,
                wxSize size, 
                int style,
                float value);
   ~SliderDialog();
   
   void OnSlider(wxCommandEvent &event);
   void OnKeyEvent(wxCommandEvent &event);
   void OnEnter(wxCommandEvent & event);
   float Get();
   
 private:
   
   ASlider * mSlider;
   wxTextCtrl * mTextCtrl;
     
 public:
   DECLARE_EVENT_TABLE()
};


#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: ecca7118-dcc2-453b-a58d-6914d1daeeea


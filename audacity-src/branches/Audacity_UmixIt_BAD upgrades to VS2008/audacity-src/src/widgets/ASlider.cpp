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
#include <wx/debug.h>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#define USE_POPUPWIN 1
#endif

#if USE_POPUPWIN
#include <wx/popupwin.h>
#endif

#include "ASlider.h"


#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../Project.h"

#include "../../images/SliderThumb.xpm"
#include "../../images/VerticalSliderThumb.xpm"

#if defined __WXMSW__
const int sliderFontSize = 10;
#else
const int sliderFontSize = 12;
#endif

//
// TipPanel
//

#if USE_POPUPWIN
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

#if USE_POPUPWIN

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
   int left = pos.x - width/2;
   if (left < 0)
      left = 0;
   SetSize(left, pos.y, width, height);   
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
   dc.SetBrush(AColor::tooltipBrush);
   dc.DrawRectangle(0, 0, width, height);
   dc.GetTextExtent(label, &textWidth, &textHeight);
   dc.DrawText(label, (width-textWidth)/2, (height-textHeight)/2);
}

//
// LWSlider
//

// Construct customizable slider
LWSlider::LWSlider(wxWindow * parent,
         wxString name,
         const wxPoint &pos,
         const wxSize &size,
         float minValue,
         float maxValue,
         float stepValue,
         bool canUseShift,
         int style,
         int orientation /*= wxHORIZONTAL*/, // wxHORIZONTAL or wxVERTICAL 
         bool heavyweight /* = false */
         )
{
   bool aquaOk = true;
   if (style & NO_AQUA) {
      style -= NO_AQUA;
      aquaOk = false;
   }

   Init(parent, name, pos, size, minValue, maxValue,
      stepValue, canUseShift, style, aquaOk, orientation, heavyweight);
}

// Construct predefined slider
LWSlider::LWSlider(wxWindow *parent,
                   wxString name,
                   const wxPoint &pos,
                   const wxSize &size,
                   int style,
                   int orientation /*= wxHORIZONTAL*/, // wxHORIZONTAL or wxVERTICAL 
                   bool heavyweight /* = false */)
{
   wxString leftLabel, rightLabel;
   float minValue, maxValue, stepValue;
   bool aquaOk = true;
   if (style & NO_AQUA) {
      style -= NO_AQUA;
      aquaOk = false;
   }

   switch(style)
   {
   case PAN_SLIDER:
      minValue = -1.0f;
      maxValue = +1.0f;
      stepValue = 0.1f;
      orientation = wxHORIZONTAL; //v Vertical PAN_SLIDER currently not handled, forced to horizontal.
      break;
   case DB_SLIDER:
      minValue = -36.0f;
      if (orientation == wxHORIZONTAL)
      {
         maxValue = 36.0f;
         stepValue = 3.0f;
      }
      else
      {
         maxValue = 6.0f;
         stepValue = 1.0f;
      }
      break;
   case FRAC_SLIDER:
      minValue = 0.0f;
      maxValue = 1.0f;
      stepValue = STEP_CONTINUOUS;
      break;
   default:
      minValue = 0.0f;
      maxValue = 1.0f;
      stepValue = 0.0f;
      wxASSERT(false); // undefined style
   }

   Init(parent, name, pos, size, minValue, maxValue, stepValue,
        true, style, aquaOk, orientation, heavyweight);
}

void LWSlider::Init(wxWindow * parent,
                    wxString name,
                    const wxPoint &pos,
                    const wxSize &size,
                    float minValue,
                    float maxValue,
                    float stepValue,
                    bool canUseShift,
                    int style,
                    bool aquaOk,
                    int orientation /*= wxHORIZONTAL*/, // wxHORIZONTAL or wxVERTICAL 
                    bool heavyweight /* = false */
                    )
{
   mName = name;
   mStyle = style;
   mAquaOk = aquaOk;
   mOrientation = orientation;
   mIsDragging = false;
   mWidth = size.x;
   mHeight = size.y;
   mParent = parent;
   mHW = heavyweight;
   mID = -1;
   mMinValue = minValue;
   mMaxValue = maxValue;
   mStepValue = stepValue;
   mCanUseShift = canUseShift;
   mCurrentValue = 0.0f;

   // Set the background colors and thumb image.
   wxMemoryDC *dc = new wxMemoryDC();
   wxBitmap *bitmap = new wxBitmap(8, 8);
   dc->SelectObject(*bitmap);

   AColor::Medium(dc, false);
   mBkgndColor = dc->GetBrush().GetColour();
   AColor::Medium(dc, true);   
   mSelBkgndColor = dc->GetBrush().GetColour();

   wxImage *thumbImage;
   if (mOrientation == wxHORIZONTAL)
      thumbImage = new wxImage(wxBitmap(SliderThumb).ConvertToImage());
   else
      thumbImage = new wxImage(wxBitmap(VerticalSliderThumb).ConvertToImage());
   wxImage *thumb1 = ChangeImageColour(thumbImage, mBkgndColor);
   wxImage *thumb2 = ChangeImageColour(thumbImage, mSelBkgndColor);
   mThumbBitmap = new wxBitmap(thumb1);
   mSelThumbBitmap = new wxBitmap(thumb2);
   delete thumb1;
   delete thumb2;
   delete thumbImage;

   delete dc;
   delete bitmap;

   mThumbWidth = mThumbBitmap->GetWidth();
   mThumbHeight = mThumbBitmap->GetHeight();


   this->OnSize(size); // Adjusts mWidth & mHeight. Creates new background images.

   //int x=mWidth/2, y=mHeight, wx, wy;
   //wxWindow *top = mParent;
   //while(top && !top->IsTopLevel()) {
   //   top->GetPosition(&wx, &wy);
   //   x += wx;
   //   y += wy;
   //   top = top->GetParent();
   //}
   
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
   if (mPopWin == NULL)
      return;

   int x, y, wx, wy;

   if (mOrientation == wxHORIZONTAL)
   {
      x=mWidth/2 + mLeft;
      y=mHeight + mTop + 1;
   }
   else
   {
      x = mWidth + mLeft + 1;
      y = mHeight/2 + mTop;
   }
   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top->GetPosition(&wx, &wy);
      x += wx;
      y += wy;
      top = top->GetParent();
   }

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
   int thumbPos = ValueToPosition(mCurrentValue);
   int thumbOrtho; // position in axis orthogonal to mOrientation
   if (mOrientation == wxHORIZONTAL)
      thumbOrtho = mCenterY - (mThumbHeight/2);
   else
      thumbOrtho = mCenterX - (mThumbWidth/2);
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
   if (mOrientation == wxHORIZONTAL)
      dc.DrawBitmap(*thumbBitmap, mLeft+thumbPos, mTop+thumbOrtho);
   else
      dc.DrawBitmap(*thumbBitmap, mLeft+thumbOrtho, mTop+thumbPos);
#else
   wxMemoryDC memDC;
   memDC.SelectObject(*bitmap);
   dc.Blit(mLeft, mTop, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
   memDC.SelectObject(*thumbBitmap);
   if (mOrientation == wxHORIZONTAL)
      dc.Blit(mLeft+thumbPos, mTop+thumbOrtho, mThumbWidth, mThumbHeight,
            &memDC, 0, 0, wxCOPY, FALSE);
   else
      dc.Blit(mLeft+thumbOrtho, mTop+thumbPos, mThumbWidth, mThumbHeight,
            &memDC, 0, 0, wxCOPY, FALSE);
#endif

   if (mPopWin)
      mPopWin->Refresh();
}

void LWSlider::OnSize(const wxSize &size)
{
   mWidth = size.GetWidth();
   mHeight = size.GetHeight();

   if (!(mWidth & 1))
      mWidth--;

   if (mOrientation == wxHORIZONTAL)
      mCenterY = mHeight - 9;
   else
      mCenterX = mWidth - 9; 

   if (mOrientation == wxHORIZONTAL)
   {
      mLeftX = mThumbWidth/2;
      mRightX = mWidth - mThumbWidth/2 - 1;
      mWidthX = mRightX - mLeftX;
   }
   else
   {
      mTopY = mThumbWidth/2;
      mBottomY = mHeight - mThumbWidth/2 - 1; 
      mHeightY = mBottomY - mTopY;
   }

   wxImage *backgroundImage;
   if (mAquaOk)
      backgroundImage = CreateSysBackground(mWidth, mHeight, 0, mBkgndColor);
   else
      backgroundImage = CreateBackground(mWidth, mHeight, mBkgndColor);
#if wxCHECK_VERSION(2, 5, 0)
   wxBitmap backgroundBitmap(backgroundImage);
#else
   wxBitmap backgroundBitmap = 
      backgroundImage->ConvertToBitmap();
#endif

   int i;
   for(i=0; i<2; i++) {
      wxBitmap *bitmap;

      bitmap = new wxBitmap(mWidth, mHeight);
      wxMemoryDC *dc = new wxMemoryDC();
      dc->SelectObject(*bitmap);

      // background
      if (mHW) {
         dc->DrawBitmap(backgroundBitmap, 0, 0);
      }
      else {
         AColor::Medium(dc, i==1);
         dc->DrawRectangle(-1, -1, mWidth+2, mHeight+2);
      }

      // tick marks at extremes
      AColor::Medium(dc, i==1);
      if (mOrientation == wxHORIZONTAL)
         dc->DrawLine(mLeftX, mCenterY, mRightX+2, mCenterY);
      else
         dc->DrawLine(mCenterX, mTopY, mCenterX, mBottomY+2);

      AColor::Dark(dc, false);
      if (mOrientation == wxHORIZONTAL)
         dc->DrawLine(mLeftX, mCenterY+1, mRightX+2, mCenterY+1);
      else
         dc->DrawLine(mCenterX+1, mTopY, mCenterX+1, mBottomY+2);

      // internal tick marks
      int divs = 10;
      double upp;
      if (mOrientation == wxHORIZONTAL) 
         upp = divs / (double)(mWidthX-1);
      else 
      {
         if (mStyle == DB_SLIDER)
            // 3dB increments for vertical DB_SLIDER
            divs = (int)((mMaxValue - mMinValue) / 3.0);
         upp = divs / (double)(mHeightY-1);
      }
      double d = 0;
      int int_d = -1;
      const int kMax = (mOrientation == wxHORIZONTAL) ? mWidthX : mHeightY;
      for(int p=0; p<= kMax; p++) {
         if (((int)d) > int_d) {
            int_d = (int)d;
            int ht = (int_d==0 || int_d==divs? 5: 3);

            AColor::Light(dc, false);
            if (mOrientation == wxHORIZONTAL)
               dc->DrawLine(mLeftX+p, mCenterY-ht, mLeftX+p, mCenterY); // ticks above
            else
               dc->DrawLine(mCenterX-ht, mTopY+p, mCenterX, mTopY+p); // ticks at left

            AColor::Dark(dc, false);
            if (mOrientation == wxHORIZONTAL)
               dc->DrawLine(mLeftX+p+1, mCenterY-ht+1, mLeftX+p+1, mCenterY); // ticks above
            else
               dc->DrawLine(mCenterX-ht+1, mTopY+p+1, mCenterX, mTopY+p+1); // ticks at left
         }
         d += upp;
      }

      if (mStyle == PAN_SLIDER)
      {
         //v Vertical PAN_SLIDER currently not handled, forced to horizontal.

         AColor::SetLabelFont(*dc);

         /* i18n-hint: One-letter abbreviation for Left, in the Pan slider */
         dc->DrawText(_("L"), mLeftX, 0);

         int width, height;
         /* i18n-hint: One-letter abbreviation for Right, in the Pan slider */
         dc->GetTextExtent(_("R"), &width, &height);
         dc->DrawText(_("R"), mRightX-width+1, 0);
      } else
      {
         // draw the '-' and the '+'
         dc->SetPen(*wxBLACK_PEN);
         if (mOrientation == wxHORIZONTAL)
         {
            dc->DrawLine(mLeftX, mCenterY-10, mLeftX+5, mCenterY-10);
            dc->DrawLine(mRightX-5, mCenterY-10, mRightX+0, mCenterY-10);
            dc->DrawLine(mRightX-3, mCenterY-12, mRightX-3, mCenterY-7);
         }
         else
         {
            dc->DrawLine(mCenterX-12, mBottomY-3,  mCenterX-7, mBottomY-3);   // '-'
            dc->DrawLine(mCenterX-12, mTopY+2,     mCenterX-7, mTopY+2);      // '+' horizontal
            dc->DrawLine(mCenterX-10, mTopY,       mCenterX-10, mTopY+5);     // '+' vertical
         }
      }
      
      delete dc;

      if (i==0)
         mBitmap = bitmap;
      else
         mSelBitmap = bitmap;
   }

   delete backgroundImage;
}


void LWSlider::FormatPopWin()
{
   wxString label;
   wxString valstr;

   switch(mStyle) {
   case FRAC_SLIDER:
      label.Printf("%s: %.1f", (const char *)mName, mCurrentValue);
      break;
   case DB_SLIDER:
      valstr.Printf("%.1f", mCurrentValue);
      if (valstr.Right(1) == "0")
         valstr = valstr.Left(valstr.Length() - 2);
      if (mCurrentValue > 0)
         valstr = "+" + valstr;
      
      label.Printf("%s: %s dB", (const char*)mName, (const char *)valstr);
      break;
   case PAN_SLIDER:
      if (mCurrentValue == 0.0)
         label.Printf("%s: %s", (const char *)mName,
                      _("Center"));
      else {
         if (mCurrentValue < 0.0)
            label.Printf("%s: %.0f%% %s", (const char *)mName,
                      -mCurrentValue * 100.0f, _("Left"));
         else /* if (val > 0.0) */
            label.Printf("%s: %.0f%% %s", (const char *)mName,
                      mCurrentValue * 100.0f, _("Right"));
      }
         
      break;
   }

   ((TipPanel *)mPopWin)->label = label;
}

void LWSlider::OnMouseEvent(wxMouseEvent & event)
{
   if (event.Entering()) {
      #if wxUSE_TOOLTIPS // Not available in wxX11
      // Display the tooltip in the status bar
      if (mParent->GetToolTip()) {
         wxString tip = mParent->GetToolTip()->GetTip();
         GetActiveProject()->TP_DisplayStatusMessage(tip, 0);
         Refresh();
      }
      #endif
   }
   else if (event.Leaving()) {
      GetActiveProject()->TP_DisplayStatusMessage("",0);
      Refresh();
   }
   
   float prevValue = mCurrentValue;

   if (event.ButtonDown()) {

      //This jumps the thumb to clicked position
      if (!mIsDragging) {
         if (mOrientation == wxHORIZONTAL)
            mCurrentValue = PositionToValue(event.m_x, event.ShiftDown());
         else 
            mCurrentValue = PositionToValue(event.m_y, event.ShiftDown());

         mIsDragging = true;
         mParent->CaptureMouse();

         FormatPopWin();
         SetPopWinPosition();
         mPopWin->Show();
      }

      // Don't generate notification yet
      return;

   } else if (event.ButtonUp() && mIsDragging) {
      mIsDragging = false;
      mParent->ReleaseMouse();
      mPopWin->Hide();
      ((TipPanel *)mPopWin)->SetPos(wxPoint(-1000, -1000));
   } else if (event.Dragging() && mIsDragging) {
      if (mOrientation == wxHORIZONTAL)
         mCurrentValue = PositionToValue(event.m_x, event.ShiftDown());
      else 
         mCurrentValue = PositionToValue(event.m_y, event.ShiftDown());
   }

   if (prevValue != mCurrentValue) {
      FormatPopWin();
      mPopWin->Refresh();
      Refresh();

      wxCommandEvent *e =
         new wxCommandEvent(wxEVT_COMMAND_SLIDER_UPDATED, mID);
      int intValue =
         (int)((mCurrentValue - mMinValue) * 1000.0f
         / (mMaxValue - mMinValue));
      e->SetInt( intValue );
      mParent->ProcessEvent(*e);
      delete e;
   }
}

int LWSlider::ValueToPosition(float val)
{
   float fRange = mMaxValue - mMinValue;
   if (mOrientation == wxHORIZONTAL)
      return (int)rint((val - mMinValue) * mWidthX / fRange);
   else
      // low values at bottom 
      return (int)rint((mMaxValue - val) * mHeightY / fRange);
}

float LWSlider::PositionToValue(int fromPos, bool shiftDown)
{
   int nSpan;
   int pos;
   if (mOrientation == wxHORIZONTAL)
   {
      pos = (fromPos - mLeft) - mLeftX;
      nSpan = mWidthX;
   }
   else
   {
      // wxVERTICAL => Low values at bottom.
      pos = mBottomY - fromPos;
      nSpan = mHeightY;
   }

   // MM: Special cases: If position is at the very left or the
   // very right (or top/bottom for wxVERTICAL), set minimum/maximum value without other checks
   if (pos <= 0)
      return mMinValue;
   if (pos >= nSpan)
      return mMaxValue;
   
   float val = ((float)pos/(float)nSpan) 
      * (mMaxValue - mMinValue) + mMinValue;

   if (!(mCanUseShift && shiftDown) && mStepValue != STEP_CONTINUOUS)
   {
      // MM: If shift is not down, or we don't allow usage
      // of shift key at all, trim value to steps of
      // provided size.
      val = (int)(val / mStepValue + 0.5 * (val>0?1.0f:-1.0f)) * mStepValue;
   }

   return val;
}

float LWSlider::Get()
{
   if (mStyle == DB_SLIDER)
      return pow(10.0f, mCurrentValue / 20.0f);
   else
      return mCurrentValue;
}

void LWSlider::Set(float value)
{
   if(mIsDragging)
      return;

   if (mStyle == DB_SLIDER)
      mCurrentValue = 20.0f*log10(value);
   else
      mCurrentValue = value;

   if (mCurrentValue < mMinValue)
      mCurrentValue = mMinValue;
   if (mCurrentValue > mMaxValue)
      mCurrentValue = mMaxValue;

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
   EVT_SIZE(ASlider::OnSize)
END_EVENT_TABLE()

ASlider::ASlider(wxWindow * parent, wxWindowID id,
                 wxString name,
                 const wxPoint & pos,
                 const wxSize & size, 
                 int style /*= FRAC_SLIDER*/, 
                 int orientation /*= wxHORIZONTAL*/): // wxHORIZONTAL or wxVERTICAL 
   wxWindow(parent, id, pos, size)
{
   mLWSlider = new LWSlider(this, name, wxPoint(0, 0), size,
                            style, orientation, true);
   mLWSlider->SetId(id);
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

void ASlider::OnSize(wxSizeEvent &event)
{
   mLWSlider->OnSize(event.GetSize());
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

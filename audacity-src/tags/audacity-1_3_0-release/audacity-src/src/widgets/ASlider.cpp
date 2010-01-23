/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.cpp

  Dominic Mazzoni

  This class is a custom slider, allowing for a 
  slicker look and feel by allowing you to use images
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
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/statline.h>
#include <wx/sizer.h>


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

#include <iostream>

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




///
///SliderDialog
///
#define ID_SLIDER 10000
BEGIN_EVENT_TABLE(SliderDialog, wxDialog)
   EVT_COMMAND(ID_SLIDER,wxEVT_COMMAND_SLIDER_UPDATED,SliderDialog::OnSlider)
   EVT_TEXT_ENTER(SLIDER_DIALOG_TEXTCTRL,SliderDialog::OnEnter)
   EVT_TEXT(SLIDER_DIALOG_TEXTCTRL,SliderDialog::OnKeyEvent)
END_EVENT_TABLE()
   ;

SliderDialog::SliderDialog(wxWindow * parent, wxWindowID id,
                           const wxString & title, 
                           wxPoint position,
                           wxSize size,
                           int style, 
                           float value):
   wxDialog(NULL,id,title,position)
{
   wxBeginBusyCursor();

   //Use a vertical sizer
   wxBoxSizer * vs = new wxBoxSizer(wxVERTICAL);
   
   //Add the text
   wxString * dummy = new wxString(wxString::Format(wxT("%2.2f"),value));
   mTextCtrl = new wxTextCtrl(this,SLIDER_DIALOG_TEXTCTRL,*dummy,
                              wxDefaultPosition,
                              wxDefaultSize,
                              wxTE_PROCESS_ENTER, 
                              wxTextValidator(wxFILTER_NUMERIC,dummy));
   vs->Add(mTextCtrl,0,wxEXPAND|wxALL,5);

   //Add a slider 
   mSlider = new ASlider(this,ID_SLIDER,title,wxDefaultPosition,size,style);
   vs->Add(mSlider,wxEXPAND|wxLEFT|wxRIGHT, 5 );
   
   //Create buttons 
   vs->Add(CreateStdDialogButtonSizer(wxOK|wxCANCEL),0,wxEXPAND|wxALL,5);
      
   //lay it out
   SetAutoLayout(true);
   SetSizer(vs);
   vs->SetSizeHints(this);
   vs->Fit(this);

   mTextCtrl->SetSelection(-1,-1);
   mTextCtrl->SetFocus();
   mSlider->Set(value);

   wxEndBusyCursor();
}

SliderDialog::~SliderDialog()
{
   delete mSlider;
   delete mTextCtrl;
}


void SliderDialog::OnSlider(wxCommandEvent & event)
{
   mTextCtrl->SetValue(wxString::Format(wxT("%2.2f"),mSlider->Get()));

   event.Skip(false);
}


void SliderDialog::OnKeyEvent(wxCommandEvent & event)
{
   //Do not do anything here right now.  Possibly, better rounding
   //and validation could go here.
}


void SliderDialog::OnEnter(wxCommandEvent & event)
{
   wxString text = mTextCtrl->GetValue();
   //Convert to a double
   double val=0;
   text.ToDouble(&val);

   //Set the slider to the value.
   mSlider->Set(val);

   //Set the text value to the slider's
   mTextCtrl->SetValue(wxString::Format(wxT("%2.2f"),mSlider->Get()));

   OnOK(event);
}


float SliderDialog:: Get()
{
   return mSlider->Get();
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
         bool heavyweight /* = false */
         )
{
   Init(parent, name, pos, size, minValue, maxValue,
      stepValue, canUseShift, style, heavyweight);
}

// Construct predefined slider
LWSlider::LWSlider(wxWindow *parent,
                   wxString name,
                   const wxPoint &pos,
                   const wxSize &size,
                   int style,
                   bool heavyweight /* = false */)
{
   wxString leftLabel, rightLabel;
   float minValue, maxValue, stepValue;

   switch(style)
   {
   case PAN_SLIDER:
      minValue = -1.0f;
      maxValue = +1.0f;
      stepValue = 0.1f;
      break;
   case DB_SLIDER:
      minValue = -36.0f;
      maxValue = 36.0f;
      stepValue = 3.0f;
      break;
   case FRAC_SLIDER:
      minValue = 0.0f;
      maxValue = 1.0f;
      stepValue = STEP_CONTINUOUS;
      break;
   case SPEED_SLIDER:
      minValue = 0.0f;
      maxValue = 3.0f;
      stepValue = STEP_CONTINUOUS;
      break;

   default:
      minValue = 0.0f;
      maxValue = 1.0f;
      stepValue = 0.0f;
      wxASSERT(false); // undefined style
   }

   Init(parent, name, pos, size, minValue, maxValue, stepValue,
        true, style, heavyweight);
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
     bool heavyweight /* = false */
     )
{
   mName = name;
   mStyle = style;
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

   if (!(mWidth & 0))
      mWidth--;

   mCenterY = mHeight - 9;

   wxMemoryDC *dc = new wxMemoryDC();
   wxBitmap *bitmap = new wxBitmap(8, 8);
   dc->SelectObject(*bitmap);

   AColor::Medium(dc, false);
   wxColour bkgnd = dc->GetPen().GetColour();
   AColor::Medium(dc, true);   
   wxColour selBkgnd = dc->GetPen().GetColour();

   wxImage *backgroundImage =
      CreateSysBackground(mWidth, mHeight, 0, bkgnd);
#if wxCHECK_VERSION(2, 5, 0)
   wxBitmap backgroundBitmap(backgroundImage);
#else
   wxBitmap backgroundBitmap =
      backgroundImage->ConvertToBitmap();
#endif
   wxImage *thumbImage = new wxImage(wxBitmap(SliderThumb).ConvertToImage());
   wxImage *thumb1 = ChangeImageColour(thumbImage, bkgnd);
   wxImage *thumb2 = ChangeImageColour(thumbImage, selBkgnd);
   mThumbBitmap = new wxBitmap(thumb1);
   mSelThumbBitmap = new wxBitmap(thumb2);
   delete thumb1;
   delete thumb2;
   delete thumbImage;

   delete dc;
   delete bitmap;

   mThumbWidth = mThumbBitmap->GetWidth();
   mThumbHeight = mThumbBitmap->GetHeight();

   mLeftX = mThumbWidth/2;
   mRightX = mWidth - mThumbWidth/2 - 1;
   mWidthX = mRightX - mLeftX;

   int i;
   for(i=0; i<2; i++) {
      wxBitmap *bitmap;

      bitmap = new wxBitmap(mWidth, mHeight);
      wxMemoryDC *dc = new wxMemoryDC();
      dc->SelectObject(*bitmap);

      if (mHW) {
         dc->DrawBitmap(backgroundBitmap, 0, 0);
      }
      else {
         AColor::Medium(dc, i==1);
         dc->DrawRectangle(0, 0, mWidth, mHeight);
      }

      AColor::Medium(dc, i==1);
      dc->DrawLine(mLeftX, mCenterY, mRightX+2, mCenterY);
      AColor::Dark(dc, false);
      dc->DrawLine(mLeftX, mCenterY+1, mRightX+2, mCenterY+1);

      int divs = 10;
      double upp = divs / (double)(mWidthX-1);
      double d = 0;
      int int_d = -1;
      for(int p=0; p<=mWidthX; p++) {
         if (((int)d) > int_d) {
            int_d = (int)d;
            int ht = (int_d==0 || int_d==divs? 5: 3);
            AColor::Light(dc, false);
            dc->DrawLine(mLeftX+p, mCenterY-ht, mLeftX+p, mCenterY);
            AColor::Dark(dc, false);
            dc->DrawLine(mLeftX+p+1, mCenterY-ht+1, mLeftX+p+1, mCenterY);
         }
         d += upp;
      }

      if (style == PAN_SLIDER)
      {
         wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
         dc->SetFont(labelFont);

         /* i18n-hint: One-letter abbreviation for Left, in the Pan slider */
         dc->DrawText(_("L"), mLeftX, 1);

         /* i18n-hint: One-letter abbreviation for Right, in the Pan slider */
         dc->DrawText(_("R"), mRightX-7, 1);
      } else
      {
         // draw the '-' and the '+'
         dc->SetPen(*wxBLACK_PEN);
         dc->DrawLine(mLeftX, mCenterY-10, mLeftX+5, mCenterY-10);
         dc->DrawLine(mRightX-7, mCenterY-10, mRightX-2, mCenterY-10);
         dc->DrawLine(mRightX-5, mCenterY-12, mRightX-5, mCenterY-7);
      }
      
      delete dc;

      if (i==0)
         mBitmap = bitmap;
      else
         mSelBitmap = bitmap;
   }

   delete backgroundImage;

   int x=mWidth/2, y=mHeight, wx, wy;
   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top->GetPosition(&wx, &wy);
      x += wx;
      y += wy;
      top = top->GetParent();
   }
   
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

   wxString maxStr = mName + wxT(": 000000");

   if (mStyle == PAN_SLIDER || mStyle == DB_SLIDER || mStyle == SPEED_SLIDER)
      maxStr += wxT("000");

   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top = top->GetParent();
   }

   mPopWin = new TipPanel(top, -1, maxStr, wxDefaultPosition);
   mPopWin->Hide();
}

void LWSlider::SetPopWinPosition()
{
   int x, y, wx, wy;

   x=mWidth/2 + mLeft;
   y=mHeight + mTop + 1;
   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top->GetPosition(&wx, &wy);
      x += wx;
      y += wy;
      top = top->GetParent();
   }

   if (mPopWin)
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
   int thumbY = mCenterY - (mThumbHeight/2);
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
   dc.DrawBitmap(*thumbBitmap, mLeft+thumbPos, mTop+thumbY);
#else
   wxMemoryDC memDC;
   memDC.SelectObject(*bitmap);
   dc.Blit(mLeft, mTop, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
   memDC.SelectObject(*thumbBitmap);
   dc.Blit(mLeft+thumbPos, mTop+thumbY, mThumbWidth, mThumbHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
#endif

   if (mPopWin)
      mPopWin->Refresh();
}

void LWSlider::FormatPopWin()
{
   wxString label;
   wxString valstr;

   switch(mStyle) {
   case FRAC_SLIDER:
      label.Printf(wxT("%s: %.1f"), mName.c_str(), mCurrentValue);
      break;
     
   case DB_SLIDER:
      valstr.Printf(wxT("%.1f"), mCurrentValue);
      if (valstr.Right(1) == wxT("0"))
         valstr = valstr.Left(valstr.Length() - 2);
      if (mCurrentValue > 0)
         valstr = wxT("+") + valstr;
      
      label.Printf(wxT("%s: %s dB"), mName.c_str(), valstr.c_str());
      break;
   case PAN_SLIDER:
      if (mCurrentValue == 0.0)
         label.Printf(wxT("%s: %s"), mName.c_str(),
                      _("Center"));
      else {
         if (mCurrentValue < 0.0)
            label.Printf(wxT("%s: %.0f%% %s"), mName.c_str(),
                         -mCurrentValue * 100.0f, _("Left"));
         else /* if (val > 0.0) */
            label.Printf(wxT("%s: %.0f%% %s"), mName.c_str(),
                         mCurrentValue * 100.0f, _("Right"));
      }
        
      break;
   case SPEED_SLIDER:
      label.Printf(wxT("%s: %.2fx"), mName.c_str(), mCurrentValue);
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
         GetActiveProject()->TP_DisplayStatusMessage(tip);
         Refresh();
      }
      #endif
   }
   else if (event.Leaving()) {
      GetActiveProject()->TP_DisplayStatusMessage(wxT(""));
      Refresh();
   }
   
   float prevValue = mCurrentValue;
   if (event.ButtonDClick())
      {
         //On a double-click, we should pop up a dialog.
         SliderDialog * dialog = 
            new SliderDialog(mParent, -1,
                                      mName,
                                      mParent->ClientToScreen(wxPoint(event.m_x,event.m_y)),
                                      wxSize(mWidth,mHeight),
                                      mStyle,
                                      Get());


         if(dialog->ShowModal() == wxID_OK)
               Set(dialog->Get());

         dialog->Destroy();
         Refresh();

      } else  if (event.ButtonDown()) {

      //This jumps the thumb to clicked position
      if (!mIsDragging) {
         mCurrentValue = PositionToValue(event.m_x, event.ShiftDown());

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
      mCurrentValue = PositionToValue(event.m_x, event.ShiftDown());
   } else if (event.m_wheelRotation != 0)
      {
         
         //Calculate the number of steps in a given direction this event
         //represents (allows for two or more clicks on a single event.)
         int steps =  event.m_wheelRotation /
            (event.m_wheelDelta > 0 ? event.m_wheelDelta : 120);
                  
         float stepSize  = mStepValue;
         if(stepSize == 0)
            {
               stepSize = (mMaxValue - mMinValue)/10;
            }
         Set(Get()+ steps * stepSize);
         Refresh();
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
   return (int)rint((val - mMinValue) * mWidthX / (mMaxValue - mMinValue));
}

float LWSlider::PositionToValue(int xPos, bool shiftDown)
{
   int pos = (xPos - mLeft) - mLeftX;

   // MM: Special cases: If position is at the very left or the
   // very right, set minimum/maximum value without other checks
   if (pos <= 0)
      return mMinValue;
   if (pos >= mWidthX)
      return mMaxValue;
   
   float val = ((float)pos/(float)mWidthX) 
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

void LWSlider::Increase(int steps)
{
   if(mIsDragging)
      return;

   mCurrentValue += (steps * mStepValue);

   if (mCurrentValue < mMinValue)
      mCurrentValue = mMinValue;
   if (mCurrentValue > mMaxValue)
      mCurrentValue = mMaxValue;

   Refresh();
}

void LWSlider::Decrease(int steps)
{
   if(mIsDragging)
      return;

   mCurrentValue -= (steps * mStepValue);

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
END_EVENT_TABLE()

ASlider::ASlider(wxWindow * parent, wxWindowID id,
                 wxString name,
                 const wxPoint & pos,
                 const wxSize & size):
   wxWindow(parent, id, pos, size)
{
   mLWSlider = new LWSlider(this, name, wxPoint(0, 0), size,
                            FRAC_SLIDER, true);
   mLWSlider->SetId(id);
}


ASlider::ASlider(wxWindow * parent, wxWindowID id,
        wxString name, const wxPoint & pos, 
        const wxSize & size,
        int style):
   wxWindow(parent,id,pos,size)
{
   mLWSlider = new LWSlider(this, name, wxPoint(0,0),size,
                            style, true);
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

void ASlider::Increase(int steps)
{
   mLWSlider->Increase(steps);
}

void ASlider::Decrease(int steps)
{
   mLWSlider->Decrease(steps);
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0030c06f-7a62-40eb-b833-0a9fb96d1f55


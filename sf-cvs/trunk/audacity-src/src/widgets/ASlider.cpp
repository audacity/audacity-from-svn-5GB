/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.cpp

  Dominic Mazzoni

*******************************************************************//**

\class ASlider
\brief ASlider is a custom slider, allowing for a slicker look and 
feel.

It allows you to use images for the slider background and 
the thumb.

*//****************************************************************//**

\class LWSlider
\brief Lightweight version of ASlider.  In other words it does not 
have a window permanaently associated with it.

*//****************************************************************//**

\class SliderDialog
\brief Pop up dialog used with an LWSlider.

*//****************************************************************//**

\class TipPanel
\brief A wxPanel or a wxPopupWindow used to give the numerical value
of an LWSlider or ASlider.

*//*******************************************************************/


#include "../Audacity.h"

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

#include "../Theme.h"
#include "../AllThemeResources.h"

//#include "../../images/SliderThumb.xpm"
//#include "../../images/SliderThumbAlpha.xpm"

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
   Raise();
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
// SliderDialog
//

BEGIN_EVENT_TABLE(SliderDialog, wxDialog)
   EVT_SLIDER(wxID_ANY,SliderDialog::OnSlider)
END_EVENT_TABLE();

SliderDialog::SliderDialog(wxWindow * parent, wxWindowID id,
                           const wxString & title, 
                           wxPoint position,
                           wxSize size,
                           int style, 
                           float value):
   wxDialog(parent,id,title,position),
   mStyle(style)
{
   //Use a vertical sizer
   wxBoxSizer * vs = new wxBoxSizer(wxVERTICAL);

   //Add the text
   mTextCtrl = new wxTextCtrl(this,
                              SLIDER_DIALOG_TEXTCTRL,
                              wxT(""),
                              wxDefaultPosition,
                              wxDefaultSize,
                              0,
                              wxTextValidator(wxFILTER_NUMERIC));
   vs->Add(mTextCtrl,0,wxEXPAND|wxALL,5);

   //Add a slider 
   mSlider = new ASlider(this,wxID_ANY,title,wxDefaultPosition,size,style,false);
   vs->Add(mSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );
   
   //Create buttons 
   vs->Add(CreateStdDialogButtonSizer(wxOK|wxCANCEL),0,wxEXPAND|wxALL,5);
      
   //lay it out
   SetSizerAndFit(vs);

   mTextCtrl->SetSelection(-1,-1);
   mTextCtrl->SetFocus();

   mSlider->Set(value);
}

SliderDialog::~SliderDialog()
{
}

bool SliderDialog::TransferDataToWindow()
{
   mTextCtrl->SetValue(wxString::Format(wxT("%g"), mSlider->Get(false)));

   return true;
}

bool SliderDialog::TransferDataFromWindow()
{
   double value;

   mTextCtrl->GetValue().ToDouble(&value);
   if (mStyle == DB_SLIDER)
      value = pow(10.0, value / 20.0);
   mSlider->Set(value);

   return true;
}

void SliderDialog::OnSlider(wxCommandEvent & event)
{
   TransferDataToWindow();

   event.Skip(false);
}

float SliderDialog::Get()
{
   return mSlider->Get(false);
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
         bool heavyweight /* = false */,
         bool popup /* = true */
         )
{
   Init(parent, name, pos, size, minValue, maxValue,
      stepValue, canUseShift, style, heavyweight, popup);
}

// Construct predefined slider
LWSlider::LWSlider(wxWindow *parent,
                   wxString name,
                   const wxPoint &pos,
                   const wxSize &size,
                   int style,
                   bool heavyweight /* = false */,
                   bool popup /* = true */)
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
      minValue = 0.01f;
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
        true, style, heavyweight, popup);
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
     bool heavyweight /* = false */,
     bool popup /* = true */
     )
{
   mName = name;
   mStyle = style;
   mIsDragging = false;
   mWidth = size.x;
   mHeight = size.y;
   mParent = parent;
   mHW = heavyweight;
   mPopup = popup;
   mID = wxID_ANY;
   mMinValue = minValue;
   mMaxValue = maxValue;
   mStepValue = stepValue;
   mCanUseShift = canUseShift;
   mCurrentValue = 0.0f;
   mBitmap = NULL;

   // Get the Thumb bitmap.  Generic version fo rnow...
//#ifdef USE_AQUA
//   mThumbBitmap = &theTheme.Bitmap( bmpMacSliderThumb );
//#else
   mThumbBitmap = &theTheme.Bitmap( bmpSliderThumb );
//#endif

//   mThumbBitmap = new wxBitmap( SliderThumb );
//   mThumbBitmap->SetMask( new wxMask( wxBitmap( SliderThumbAlpha ), *wxBLACK ) );

   Draw();

   mPopWin = NULL;
   Move(pos);
   CreatePopWin();
}

LWSlider::~LWSlider()
{
   delete mBitmap;
//   delete mThumbBitmap;
   delete mPopWin;
}

wxWindowID LWSlider::GetId()
{
   return mID;
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

#if defined(__WXMSW__)
   if( mHW )
   {
      dc.Clear();
   }
#endif

   dc.DrawBitmap(*mBitmap, mLeft, mTop, true);
   dc.DrawBitmap(*mThumbBitmap, mLeft+thumbPos, mTop+thumbY, true);

   if (mPopWin)
      mPopWin->Refresh();
}

void LWSlider::OnSize( wxSizeEvent & event )
{
   mWidth = event.GetSize().GetX();
   mHeight = event.GetSize().GetY();

   Draw();

   Refresh();
}

void LWSlider::Draw()
{
   if( mBitmap )
   {
      delete mBitmap;
      mBitmap = NULL;
   }

   mCenterY = mHeight - 9;
   mThumbWidth = mThumbBitmap->GetWidth();
   mThumbHeight = mThumbBitmap->GetHeight();

   mLeftX = mThumbWidth/2;
   mRightX = mWidth - mThumbWidth/2 - 1;
   mWidthX = mRightX - mLeftX;

   wxMemoryDC *dc = new wxMemoryDC();
   mBitmap = new wxBitmap(mWidth, mHeight);

   // Set background to an unused color.  This allows for easy
   // mask creation.  And garbage can be displayed if it isn't
   // cleared.
   dc->SelectObject(*mBitmap);
   dc->SetBackground( wxBrush( wxColour( 255, 254, 255 ) ) );// DONT-THEME Mask colour.
   dc->Clear();

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

   if (mStyle == PAN_SLIDER)
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
   
   // Must preceed creating the mask as that will attempt to
   // select the bitmap into another DC.
   delete dc;

   mBitmap->SetMask( new wxMask( *mBitmap, wxColour( 255, 254, 255 ) ) );
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

bool LWSlider::ShowDialog()
{
   return DoShowDialog( mParent->ClientToScreen(wxPoint( mLeft, mTop ) ) );
}

bool LWSlider::DoShowDialog(wxPoint pos)
{
   float value;
   bool changed = false;

   SliderDialog * dialog =
      new SliderDialog( mParent,
                        wxID_ANY,
                        mName,
                        pos,
                        wxSize( mWidth, mHeight ),
                        mStyle,
                        Get());

   if( dialog->ShowModal() == wxID_OK )
   {
      value = dialog->Get();
      if( value != mCurrentValue )
      {
         mCurrentValue = value;
         changed = true;
      }
   }

   dialog->Destroy();

   return changed;
}

void LWSlider::OnMouseEvent(wxMouseEvent & event)
{
   if( event.Entering() )
   {
#if wxUSE_TOOLTIPS // Not available in wxX11
      // Display the tooltip in the status bar
      if( mParent->GetToolTip() )
      {
         wxString tip = mParent->GetToolTip()->GetTip();
         GetActiveProject()->TP_DisplayStatusMessage(tip);
         Refresh();
      }
#endif
   }
   else if( event.Leaving() )
   {
      GetActiveProject()->TP_DisplayStatusMessage(wxT(""));
      Refresh();
   }

   float prevValue = mCurrentValue;

   if( event.ButtonDClick() && mPopup ) // Should probably use a right click instead
   {
      //On a double-click, we should pop up a dialog.
      DoShowDialog(mParent->ClientToScreen(wxPoint(event.m_x,event.m_y)));
   }
   else if( event.ButtonDown() )
   {
      mParent->SetFocus();

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
   }
   else if( event.ButtonUp() && mIsDragging )
   {
      mIsDragging = false;
      mParent->ReleaseMouse();
      mPopWin->Hide();
      ((TipPanel *)mPopWin)->SetPos(wxPoint(-1000, -1000));
   }
   else if( event.Dragging() && mIsDragging )
   {
      mCurrentValue = PositionToValue(event.m_x, event.ShiftDown());
   } 
   else if( event.m_wheelRotation != 0 )
   {

      //Calculate the number of steps in a given direction this event
      //represents (allows for two or more clicks on a single event.)
      int steps =  event.m_wheelRotation /
         (event.m_wheelDelta > 0 ? event.m_wheelDelta : 120);

      if( steps < 0 )
      {
         Decrease( -steps );
      }
      else
      {
         Increase( steps );
      }
      SendUpdate( mCurrentValue );
   }

   if( prevValue != mCurrentValue )
      SendUpdate( mCurrentValue );
}

void LWSlider::OnKeyEvent(wxKeyEvent & event)
{
   switch( event.GetKeyCode() )
   {
      case WXK_RIGHT:
      case WXK_UP:
         Increase( 1 );
         SendUpdate( mCurrentValue );
      break;

      case WXK_LEFT:
      case WXK_DOWN:
         Decrease( 1 );
         SendUpdate( mCurrentValue );
      break;

      case WXK_PAGEUP:
      case WXK_PRIOR:
         Increase( 5 );
         SendUpdate( mCurrentValue );
      break;

      case WXK_PAGEDOWN:
      case WXK_NEXT:
         Decrease( 5 );
         SendUpdate( mCurrentValue );
      break;

      case WXK_HOME:
         SendUpdate( mMinValue );
      break;

      case WXK_END:
         SendUpdate( mMaxValue );
      break;

      case WXK_TAB:
      {
        wxNavigationKeyEvent nevent;
        nevent.SetWindowChange( event.ControlDown() );
        nevent.SetDirection( !event.ShiftDown() );
        nevent.SetEventObject( mParent );
        nevent.SetCurrentFocus( mParent );
        mParent->GetParent()->ProcessEvent( nevent );
      }
      break;

      case WXK_RETURN:
      case WXK_NUMPAD_ENTER:
      {
         wxWindow *def = mParent->GetParent()->GetDefaultItem();
         if (def) {
            wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED,
                                  def->GetId());
            mParent->ProcessEvent( cevent );
         }
      }

      default:
         // Allow it to propagate
         event.Skip();
      break;
   }

   event.Skip();
}

void LWSlider::SendUpdate( float newValue )
{
   mCurrentValue = newValue;
   FormatPopWin();
   mPopWin->Refresh();
   Refresh();

   wxCommandEvent e( wxEVT_COMMAND_SLIDER_UPDATED, mID );
   int intValue = (int)( ( mCurrentValue - mMinValue ) * 1000.0f /
                         ( mMaxValue - mMinValue ) );
   e.SetInt( intValue );
   mParent->ProcessEvent( e );
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

float LWSlider::Get( bool convert )
{
   if (mStyle == DB_SLIDER)
      return ( convert ? pow(10.0f, mCurrentValue / 20.0f) : mCurrentValue );
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
   float stepValue = mStepValue;

   if( stepValue == 0.0 )
   {
      stepValue = ( mMaxValue - mMinValue ) / 10.0;
   }

   mCurrentValue += ( steps * stepValue );

   if( mCurrentValue < mMinValue )
   {
      mCurrentValue = mMinValue;
   }
   else if( mCurrentValue > mMaxValue )
   {
      mCurrentValue = mMaxValue;
   }

   Refresh();
}

void LWSlider::Decrease(int steps)
{
   float stepValue  = mStepValue;

   if( stepValue == 0.0 )
   {
      stepValue = ( mMaxValue - mMinValue ) / 10.0;
   }

   mCurrentValue -= ( steps * stepValue );

   if( mCurrentValue < mMinValue )
   {
      mCurrentValue = mMinValue;
   }
   else if( mCurrentValue > mMaxValue )
   {
      mCurrentValue = mMaxValue;
   }

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
   EVT_CHAR(ASlider::OnKeyEvent)
   EVT_MOUSE_EVENTS(ASlider::OnMouseEvent)
   EVT_PAINT(ASlider::OnPaint)
   EVT_SIZE(ASlider::OnSize)
   EVT_ERASE_BACKGROUND(ASlider::OnErase)
   EVT_SLIDER(wxID_ANY, ASlider::OnSlider)
   EVT_SET_FOCUS(ASlider::OnSetFocus)
   EVT_KILL_FOCUS(ASlider::OnKillFocus)
END_EVENT_TABLE()

ASlider::ASlider( wxWindow * parent,
                  wxWindowID id,
                  wxString name,
                  const wxPoint & pos, 
                  const wxSize & size,
                  int style,
                  bool popup ):
   wxPanel( parent, id, pos, size, wxWANTS_CHARS )
{
   mLWSlider = new LWSlider( this,
                             name,
                             wxPoint(0,0),
                             size,
                             style,
                             true,
                             popup );
   mLWSlider->SetId( id );
   SetName( name );

   mSliderIsFocused = false;

#if wxUSE_ACCESSIBILITY
   SetAccessible( new ASliderAx( this ) );
#endif
}


ASlider::~ASlider()
{
   delete mLWSlider;
}

void ASlider::OnSlider(wxCommandEvent &event)
{

   if( event.GetId() == mLWSlider->GetId() )
   {
#if wxUSE_ACCESSIBILITY
      GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_VALUECHANGE,
                                    this,
                                    wxOBJID_CLIENT,
                                    wxACC_SELF );
#endif
   }

   event.Skip();
}

void ASlider::OnSize(wxSizeEvent &event)
{
   mLWSlider->OnSize( event );
}

void ASlider::OnErase(wxEraseEvent &event)
{
   // Ignore it to prevent flashing
}

void ASlider::OnPaint(wxPaintEvent &event)
{
   wxPaintDC dc(this);

   mLWSlider->OnPaint(dc, false);

   if( mSliderIsFocused )
   {
      wxRect r( 0, 0, mLWSlider->mWidth, mLWSlider->mHeight );

      r.Deflate( 1, 1 );

      AColor::DrawFocus( dc, r );
   }
}

void ASlider::OnMouseEvent(wxMouseEvent &event)
{
   mLWSlider->OnMouseEvent(event);
}

void ASlider::OnKeyEvent(wxKeyEvent &event)
{
   mLWSlider->OnKeyEvent(event);
}

void ASlider::OnSetFocus(wxFocusEvent & event)
{
   mSliderIsFocused = true;
   Refresh();
}

void ASlider::OnKillFocus(wxFocusEvent & event)
{
   mSliderIsFocused = false;
   Refresh();
}

void ASlider::RecreateTipWin()
{
   mLWSlider->RecreateTipWin();
}

float ASlider::Get( bool convert )
{
   return mLWSlider->Get( convert );
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

#if wxUSE_ACCESSIBILITY

ASliderAx::ASliderAx( wxWindow * window ):
   wxWindowAccessible( window )
{
}

ASliderAx::~ASliderAx()
{
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus ASliderAx::GetChild( int childId, wxAccessible** child )
{
   if( childId == wxACC_SELF )
   {
      *child = this;
   }
   else
   {
      *child = NULL;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus ASliderAx::GetChildCount(int* childCount)
{
   *childCount = 3;

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus ASliderAx::GetDefaultAction( int childId, wxString *actionName )
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus ASliderAx::GetDescription( int childId, wxString *description )
{
   description->Clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus ASliderAx::GetFocus(int* childId, wxAccessible** child)
{
   *childId = 0;
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus ASliderAx::GetHelpText( int childId, wxString *helpText )
{
   helpText->Clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus ASliderAx::GetKeyboardShortcut( int childId, wxString *shortcut )
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus ASliderAx::GetLocation( wxRect& rect, int elementId )
{
   ASlider *as = wxDynamicCast( GetWindow(), ASlider );

   rect = as->GetRect();
   rect.SetPosition( as->GetParent()->ClientToScreen( rect.GetPosition() ) );

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus ASliderAx::GetName(int childId, wxString* name)
{
   ASlider *as = wxDynamicCast( GetWindow(), ASlider );

   *name = as->GetName();

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus ASliderAx::GetRole(int childId, wxAccRole* role)
{
   switch( childId )
   {
      case 0:
         *role = wxROLE_SYSTEM_SLIDER;
      break;

      case 1:
      case 3:
         *role = wxROLE_SYSTEM_PUSHBUTTON;
      break;

      case 2:
         *role = wxROLE_SYSTEM_INDICATOR;
      break;
   }

   return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus ASliderAx::GetSelections( wxVariant *selections )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus ASliderAx::GetState(int childId, long* state)
{
   ASlider *as = wxDynamicCast( GetWindow(), ASlider );

   switch( childId )
   {
      case 0:
         *state = wxACC_STATE_SYSTEM_FOCUSABLE;
      break;

      case 1:
         if( as->mLWSlider->mCurrentValue == as->mLWSlider->mMinValue )
         {
            *state = wxACC_STATE_SYSTEM_INVISIBLE;
         }
      break;

      case 3:
         if( as->mLWSlider->mCurrentValue == as->mLWSlider->mMaxValue )
         {
            *state = wxACC_STATE_SYSTEM_INVISIBLE;
         }
      break;
   }

   // Do not use mSliderIsFocused is not set until after this method
   // is called.
   *state |= ( as == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0 );

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus ASliderAx::GetValue(int childId, wxString* strValue)
{
   ASlider *as = wxDynamicCast( GetWindow(), ASlider );

   if( childId == 0 )
   {
      switch( as->mLWSlider->mStyle )
      {
         case FRAC_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue * 100 );
            break;
           
         case DB_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue );
            break;

         case PAN_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue * 100 );
            break;

         case SPEED_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue * 100 );
            break;
      }

      return wxACC_OK;
   }

   return wxACC_NOT_SUPPORTED;
}

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
// arch-tag: 0030c06f-7a62-40eb-b833-0a9fb96d1f55


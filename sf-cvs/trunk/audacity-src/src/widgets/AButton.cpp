/**********************************************************************

  Audacity: A Digital Audio Editor

  AButton.cpp

  Dominic Mazzoni

  This is a custom button class for Audacity.  The main feature it
  supports that a wxButton does not is mouseovers.  It uses an image
  for all of its states: up, over, down, and disabled, allowing any
  sort of customization you want.  Currently it does not support
  transparency effects, so the images must be rectangular and
  opaque.

**********************************************************************/

#include "../Audacity.h"

#include "AButton.h"
#include "../AColor.h"

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/dcbuffer.h>
#include <wx/image.h>

//This is needed for tooltips
#include "../Project.h"
#include <wx/tooltip.h>

BEGIN_EVENT_TABLE(AButton, wxWindow)
   EVT_MOUSE_EVENTS(AButton::OnMouseEvent)
   EVT_KEY_DOWN(AButton::OnKeyDown)
   EVT_KEY_UP(AButton::OnKeyUp)
   EVT_SET_FOCUS(AButton::OnSetFocus)
   EVT_KILL_FOCUS(AButton::OnKillFocus)
   EVT_PAINT(AButton::OnPaint)
   EVT_SIZE(AButton::OnSize)
   EVT_ERASE_BACKGROUND(AButton::OnErase)
END_EVENT_TABLE()

AButton::AButton(wxWindow * parent,
                 wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 char **upXPM,
                 char **overXPM,
                 char **downXPM,
                 char **disXPM,
                 bool processdownevents):
   wxWindow()
{
   Init(parent,
        id,
        pos,
        size,
        new wxBitmap((const char **) upXPM),
        new wxBitmap((const char **) overXPM),
        new wxBitmap((const char **) downXPM),
        new wxBitmap((const char **) disXPM),
        processdownevents);
}

AButton::AButton(wxWindow * parent,
                 wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 wxImage *up,
                 wxImage *over,
                 wxImage *down,
                 wxImage *dis,
                 bool processdownevents):
   wxWindow()
{
   Init(parent,
        id,
        pos,
        size,
        new wxBitmap(up),
        new wxBitmap(over),
        new wxBitmap(down),
        new wxBitmap(dis),
        processdownevents);
}

AButton::~AButton()
{
   delete mBitmap[0];
   delete mBitmap[1];
   delete mBitmap[2];
   delete mBitmap[3];

   if (mAltBitmap[0]) {
      delete mAltBitmap[0];
      delete mAltBitmap[1];
      delete mAltBitmap[2];
      delete mAltBitmap[3];
   }
}

void AButton::Init(wxWindow * parent,
                   wxWindowID id,
                   const wxPoint & pos,
                   const wxSize & size,
                   wxBitmap *up,
                   wxBitmap *over,
                   wxBitmap *down,
                   wxBitmap *dis,
                   bool processdownevents)
{
   Create(parent, id, pos, size);

   mParent = parent;
   mWasShiftDown = false;
   mButtonIsDown = false;
   mButtonState = AButtonUp;
   mIsClicking = false;
   mEnabled = true;
   mProcessDownEvents = processdownevents;

   mBitmap[0] = up;
   mBitmap[1] = over;
   mBitmap[2] = down;
   mBitmap[3] = dis;

   mAltBitmap[0] = NULL;
   mAltBitmap[1] = NULL;
   mAltBitmap[2] = NULL;
   mAltBitmap[3] = NULL;

   mAlternate = false;

   GetSize(&mWidth, &mHeight);

   mButtonIsFocused = false;
   mFocusRect = GetRect().Deflate( 3, 3 );

#if wxUSE_ACCESSIBILITY
   SetName( wxT("") );
   SetAccessible(new AButtonAx(this));
#endif
}

void AButton::SetAlternateImages(wxImage *up,
                                 wxImage *over,
                                 wxImage *down,
                                 wxImage *dis)
{
   mAltBitmap[0] = new wxBitmap(up);
   mAltBitmap[1] = new wxBitmap(over);
   mAltBitmap[2] = new wxBitmap(down);
   mAltBitmap[3] = new wxBitmap(dis);
}

void AButton::SetAlternate(bool useAlternateImages)
{
   mAlternate = useAlternateImages;
   Refresh(false);
}

void AButton::SetFocusRect(wxRect & r)
{
   mFocusRect = r;
}

void AButton::OnPaint(wxPaintEvent & event)
{
   wxBufferedPaintDC dc(this);

   if (mAlternate)
      dc.DrawBitmap(*mAltBitmap[mButtonState], 0, 0);
   else
      dc.DrawBitmap(*mBitmap[mButtonState], 0, 0);

#if defined(__WXMSW__)
   if( mButtonIsFocused )
   {
      AColor::DrawFocus( dc, mFocusRect );
   }
#endif
}

void AButton::OnErase(wxEraseEvent & event)
{
   // Ignore it to prevent flashing
}

void AButton::OnSize(wxSizeEvent & event)
{
   Refresh(false);
}

void AButton::OnMouseEvent(wxMouseEvent & event)
{
   if (mAltBitmap[0] && mButtonState != AButtonDown) {
      if (mAlternate != event.ShiftDown()) {
         mAlternate = event.ShiftDown();
         Refresh(false);
      }
   }

   if (event.Leaving()){
      GetActiveProject()->TP_DisplayStatusMessage(wxT(""));
   }
   

// In windows, Leave/Enter events appear to clobber each other,
// so the new enter event doesn't get processed.  If we change to a newer
// version of WXWINDOWS, (Post version 2), this may be fixed

#if defined __WXMSW__
   else {
#else
   else if (event.Entering()) {
#endif

      #if wxUSE_TOOLTIPS // Not available in wxX11
      // Display the tooltip in the status bar
      wxToolTip * pTip = this->GetToolTip();
      if( pTip ) {
         wxString tipText = pTip->GetTip();
         if (!mEnabled)
            tipText += _(" (disabled)");
         GetActiveProject()->TP_DisplayStatusMessage(tipText);
      }
      #endif
   }

   //If the graphical button is disabled, or the button is down
   // the button can't process down events, get out of here.
   if (!mEnabled || (mButtonIsDown && !mProcessDownEvents) ) {
      this->Refresh(false);
      return;
   }

   //If the mouse button is released, the following stuff happens
   if (event.ButtonUp() ) {
      SetFocus();
      mIsClicking = false;
      if (HasCapture())
         ReleaseMouse();


      //Only process the event if you are releasing on the button--if you moved
      //off the button, dump the event.
      if (event.m_x >= 0 && event.m_y >= 0 &&
          event.m_x < mWidth && event.m_y < mHeight) {
      
         // Although this may be a little redundant, I'm segregating
         // two types of buttons for clarity
         // mProcessDownEvents=false buttons can only by pushed down-their pushing up must
         // be handled immediately, by the action of another button, or by
         // some other event (e.g., the file ending stops playing and pops up
         // the play button.
         // mProcessDownEvents=true buttons can be pushed down and then pushed up again
         // by clicking on them again once in the down state.
         if(mProcessDownEvents)
            {
               if(mButtonIsDown)
                  {
                     
                     //If the button is down, set the button state to up 
                     // and 'over'--highlighted.
                     mButtonState = AButtonOver;
                     mButtonIsDown = false;          
                  }
               else
                  {
                     //If the button is up, set the button state to down
                     mButtonState=AButtonDown;
                     mButtonIsDown=true;
                  }
            }
         else
            {
               //If it is a one-state button,
               //Set the button state to down undconditionally
               mButtonState = AButtonDown;
               mButtonIsDown = true;
            }

         mWasShiftDown = event.ShiftDown();

         //Create an event for the parent window to process.
         wxCommandEvent *e =
             new wxCommandEvent(wxEVT_COMMAND_BUTTON_CLICKED, GetId());
         GetParent()->AddPendingEvent(*e);
         delete e;
      }

      this->Refresh(false);
      return;
   }
      //This handles the mouse down event.
   else if (event.ButtonDown()) {
      SetFocus();
      mIsClicking = true;
      CaptureMouse();
   }


   //This following logic handles button in situations other than the 
   //up-click
   if (mProcessDownEvents)
      {
         if (mIsClicking) {
            if (event.m_x >= 0 && event.m_y >= 0 &&
                event.m_x < mWidth && event.m_y < mHeight) {
               mButtonState =  AButtonDown;
            } else
               mButtonState = mButtonIsDown ? AButtonDown: AButtonUp;
         }
         else {
            if (event.Entering())
               mButtonState = mButtonIsDown ? AButtonDown: AButtonOver;
            
            //If mouse leaves the button, put it in its previous state.
            else if (event.Leaving())
               mButtonState = mButtonIsDown ? AButtonDown: AButtonUp;
            
         }

      }
   else  //This is a push-down-only traditional button
      {
         if (mIsClicking) {
            if (event.m_x >= 0 && event.m_y >= 0 &&
                event.m_x < mWidth && event.m_y < mHeight) {
               mButtonState = AButtonDown;
            } else
               mButtonState = AButtonUp;
         }
         else {
            if (event.Entering())
               mButtonState = AButtonOver;
            else if (event.Leaving())
               mButtonState = AButtonUp;
         }
         
         
      }

   // If the button is disabled, make sure it doesn't accidentally get
   // set to the "up" state by the above logic:
   if (mButtonState == AButtonUp && !mEnabled)
      mButtonState = AButtonDis;

   //Do a final Refresh() event
   this->Refresh(false);
}

void AButton::OnKeyDown(wxKeyEvent & event)
{
   wxMouseEvent evt;

   switch( event.GetKeyCode() )
   {
      case WXK_RETURN:
         mWasShiftDown = event.ShiftDown();
         Click();
      break;

      case WXK_SPACE:
         if( !mIsClicking )
         {
            mIsClicking = true;
            Toggle();
         }
      break;

      default:
         if( mIsClicking )
         {
            Toggle();
         }
      break;
   }

   event.Skip();
}

void AButton::OnKeyUp(wxKeyEvent & event)
{
   switch( event.GetKeyCode() )
   {
      case WXK_SPACE:
         if( IsDown() && !mProcessDownEvents )
         {
            PopUp();
         }
         mWasShiftDown = event.ShiftDown();
         Click();
         mIsClicking = false;
      break;
   }

   event.Skip();
}

void AButton::OnSetFocus(wxFocusEvent & event)
{
   mButtonIsFocused = true;
   Refresh( false );
}

void AButton::OnKillFocus(wxFocusEvent & event)
{
   mButtonIsFocused = false;
   Refresh( false );
}

bool AButton::WasShiftDown()
{
   return mWasShiftDown;
}

void AButton::Enable()
{
   mEnabled = true;
   if (mButtonIsDown)
      mButtonState = AButtonDown;
   else
      mButtonState = AButtonUp;
   this->Refresh(false);
}

void AButton::Disable()
{
   mEnabled = false;
   mButtonState = AButtonDis;
   if (GetCapture()==this)
      ReleaseMouse();
   this->Refresh(false);
}

void AButton::PushDown()
{
   mButtonIsDown = true;
   mButtonState = AButtonDown;
   this->Refresh(false);
}

void AButton::PopUp()
{

   mButtonIsDown = false;
   if (mEnabled)
      mButtonState = AButtonUp;
   else
      mButtonState = AButtonDis;

   if (GetCapture()==this)
      ReleaseMouse();

   this->Refresh(false);
}

void AButton::Click()
{
   //Create an event for the parent window to process.
   wxCommandEvent *e =
         new wxCommandEvent(wxEVT_COMMAND_BUTTON_CLICKED, GetId());
   GetParent()->AddPendingEvent(*e);
   delete e;
}

#if wxUSE_ACCESSIBILITY

AButtonAx::AButtonAx( wxWindow *window ):
   wxWindowAccessible( window )
{
}

AButtonAx::~AButtonAx()
{
}

// Performs the default action. childId is 0 (the action for this object)
// or > 0 (the action for a child).
// Return wxACC_NOT_SUPPORTED if there is no default action for this
// window (e.g. an edit control).
wxAccStatus AButtonAx::DoDefaultAction(int childId)
{
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   if( ab->GetState() != AButton::AButtonDis )
   {
      ab->Click();
   }

   return wxACC_OK;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus AButtonAx::GetChild( int childId, wxAccessible** child )
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
wxAccStatus AButtonAx::GetChildCount(int* childCount)
{
   *childCount = 0;

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus AButtonAx::GetDefaultAction(int childId, wxString* actionName)
{
   *actionName = _( "Press" );

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus AButtonAx::GetDescription( int childId, wxString *description )
{
   description->Clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus AButtonAx::GetFocus(int* childId, wxAccessible** child)
{
   *childId = 0;
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus AButtonAx::GetHelpText( int childId, wxString *helpText )
{
#if wxUSE_TOOLTIPS // Not available in wxX11
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   wxToolTip *pTip = ab->GetToolTip();
   if( pTip )
   {
      *helpText = pTip->GetTip();
   }

   return wxACC_OK;
#else
   helpText->Clear();

   return wxACC_NOT_SUPPORTED;
#endif
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus AButtonAx::GetKeyboardShortcut( int childId, wxString *shortcut )
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus AButtonAx::GetLocation( wxRect& rect, int elementId )
{
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   rect = ab->GetRect();
   rect.SetPosition( ab->GetParent()->ClientToScreen( rect.GetPosition() ) );

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus AButtonAx::GetName(int childId, wxString* name)
{
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   *name = ab->GetName();
   if( name->IsEmpty() )
   {
      *name = ab->GetLabel();
   }

   if( name->IsEmpty() )
   {
      *name = _("Button");
   }

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus AButtonAx::GetRole(int childId, wxAccRole* role)
{
   *role = wxROLE_SYSTEM_PUSHBUTTON;

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
wxAccStatus AButtonAx::GetSelections( wxVariant *selections )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus AButtonAx::GetState(int childId, long* state)
{
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   switch( ab->GetState() )
   {
      case AButton::AButtonDown:
         *state = wxACC_STATE_SYSTEM_PRESSED | wxACC_STATE_SYSTEM_FOCUSABLE;
      break;

      case AButton::AButtonOver:
         *state = wxACC_STATE_SYSTEM_HOTTRACKED | wxACC_STATE_SYSTEM_FOCUSABLE;
      break;

      case AButton::AButtonDis:
         *state = wxACC_STATE_SYSTEM_UNAVAILABLE;
      break;

      default:
         *state = wxACC_STATE_SYSTEM_FOCUSABLE;
      break;
   }

   // Do not use mButtonIsFocused is not set until after this method
   // is called.
   *state |= ( ab == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0 );

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus AButtonAx::GetValue(int childId, wxString* strValue)
{
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
// arch-tag: c0be128d-3a41-4ff8-9dc5-2264a0439761


/**********************************************************************

  Audacity: A Digital Audio Editor

  AButton.h

  Dominic Mazzoni

  This is a custom button class for Audacity.  The main feature it
  supports that a wxButton does not is mouseovers.  It uses an image
  for all of its states: up, over, down, and disabled, allowing any
  sort of customization you want.  Currently it does not support
  transparency effects, so the image musts be rectangular and
  opaque.

**********************************************************************/

#ifndef __AUDACITY_BUTTON__
#define __AUDACITY_BUTTON__

#include <wx/window.h>

class wxBitmap;
class wxImage;

class AButton:public wxWindow {
 public:

   AButton(wxWindow * parent, wxWindowID id,
           const wxPoint & pos,
           const wxSize & size,
           wxImage *up,
           wxImage *over,
           wxImage *down,
           wxImage *dis);

   AButton(wxWindow * parent, wxWindowID id,
           const wxPoint & pos,
           const wxSize & size,
           char **upXPM, char **overXPM, char **downXPM, char **disXPM);

   virtual ~ AButton();

   virtual void Disable();
   virtual void Enable();

   virtual void PushDown();
   virtual void PopUp();

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnMouseEvent(wxMouseEvent & event);

   virtual bool WasShiftDown(); // returns true if shift was held down
                                // the last time the button was clicked

 private:

   enum AButtonState {
      AButtonUp,
      AButtonOver,
      AButtonDown,
      AButtonDis
   };

   int mWidth;
   int mHeight;

   bool mWasShiftDown;

   bool mButtonIsDown;
   bool mIsClicking;
   bool mEnabled;
   AButtonState mButtonState;

   wxBitmap *mBitmap[4];

 public:

    DECLARE_EVENT_TABLE()
};

#endif

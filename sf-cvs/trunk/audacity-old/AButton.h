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

class wxImage;

class AButton: public wxWindow
{
public:

  AButton(wxWindow *parent, wxWindowID id,
	  const wxPoint& pos,
	  const wxSize& size,
	  wxString upImage,
	  wxString overImage,
	  wxString downImage,
	  wxString disImage);

  virtual ~AButton();

  virtual void Disable();
  virtual void Enable();

  virtual void PushDown();
  virtual void PopUp();

  virtual void OnPaint(wxPaintEvent& event);
  virtual void OnMouseEvent(wxMouseEvent& event);  

private:

  enum AButtonState {
    AButtonUp,
    AButtonOver,
    AButtonDown,
	AButtonDis
  };

  int             mWidth;
  int             mHeight;

  bool            mButtonIsDown;
  bool            mIsClicking;
  bool            mEnabled;
  AButtonState    mButtonState;

  wxImage         *mImage[4];

public:

  DECLARE_EVENT_TABLE()
};

#endif

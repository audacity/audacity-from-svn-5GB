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

  wxBitmap        *mBitmap[4];

public:

  DECLARE_EVENT_TABLE()
};

#ifdef __WXMSW__
  #define AUDACITY_BITMAP_TYPE wxBITMAP_TYPE_BMP_RESOURCE
  #define BITMAP_PRE ""
  #define BITMAP_SUF ""
#endif

#ifdef __WXMAC__
  #define AUDACITY_BITMAP_TYPE wxBITMAP_TYPE_PICT_RESOURCE
  #define BITMAP_PRE ""
  #define BITMAP_SUF ""
#endif

#ifdef __WXGTK__
  #define AUDACITY_BITMAP_TYPE wxBITMAP_TYPE_PNG
  #define BITMAP_PRE "icons/"
  #define BITMAP_SUF ".png"
#endif

#endif

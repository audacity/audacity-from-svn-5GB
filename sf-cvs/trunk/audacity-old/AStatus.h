/**********************************************************************

  Audacity: A Digital Audio Editor

  AStatus.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_STATUS__
#define __AUDACITY_STATUS__

#include <wx/gdicmn.h>
#include <wx/window.h>

class wxBitmap;
class wxMenu;

int GetStatusHeight();

class AStatusListener {
public:
  virtual void AS_SetRate(double rate) = 0;
};

class AStatus: public wxWindow
{
public:

  AStatus(wxWindow *parent, wxWindowID id,
		  const wxPoint& pos,
		  const wxSize& size,
		  double rate,
		  AStatusListener *listener);
  
  virtual ~AStatus();

  virtual void OnPaint(wxPaintEvent& event);
  virtual void OnMouseEvent(wxMouseEvent& event);  

  void SetField(const char *msg, int fieldNum);

  void SetRate(double rate);

  void OnRate8();
  void OnRate11();
  void OnRate22();
  void OnRate44();
  void OnRate48();
  void OnRateOther();

private:

  AStatusListener *mListener;
  wxBitmap        *mBitmap;
  int              mWidth;
  int              mHeight;
  wxRect           mRateField;
  wxMenu          *mRateMenu;
  double           mRate;
  wxString         mField[10];

public:

  DECLARE_EVENT_TABLE()
};

#endif

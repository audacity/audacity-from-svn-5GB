/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.cpp

  Dominic Mazzoni

  Manages color brushes and pens

**********************************************************************/

#include <wx/dc.h>
#include <wx/settings.h>

#include "AColor.h"

bool AColor::inited = false;
wxBrush AColor::lightBrush[2];
wxBrush AColor::mediumBrush[2];
wxBrush AColor::darkBrush[2];
wxPen AColor::lightPen[2];
wxPen AColor::mediumPen[2];
wxPen AColor::darkPen[2];

void AColor::Bevel(wxDC& dc, bool up, wxRect& r)
{
  if (up)
	AColor::Light(&dc, false);
  else
	AColor::Dark(&dc, false);

  dc.DrawLine(r.x, r.y, r.x + r.width, r.y);
  dc.DrawLine(r.x, r.y, r.x, r.y + r.height);

  if (!up)
	AColor::Light(&dc, false);
  else
	AColor::Dark(&dc, false);

  dc.DrawLine(r.x + r.width, r.y, r.x + r.width, r.y + r.height);
  dc.DrawLine(r.x, r.y + r.height, r.x + r.width + 1, r.y + r.height);
}

void AColor::Light(wxDC *dc, bool selected)
{
  if (!inited) Init();
  int index = (int)selected;
  dc->SetBrush(lightBrush[index]);
  dc->SetPen(lightPen[index]);
}

void AColor::Medium(wxDC *dc, bool selected)
{
  if (!inited) Init();
  int index = (int)selected;
  dc->SetBrush(mediumBrush[index]);
  dc->SetPen(mediumPen[index]);
}

void AColor::Dark(wxDC *dc, bool selected)
{
  if (!inited) Init();
  int index = (int)selected;
  dc->SetBrush(darkBrush[index]);
  dc->SetPen(darkPen[index]);
}

void AColor::Init()
{
  wxColour light =
	wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DHIGHLIGHT);
  wxColour med =
	wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
  wxColour dark =
	wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW);

  #if defined(__WXMSW__) || defined(__WXGTK__)
  // unselected
  lightBrush[0].SetColour(light);
  mediumBrush[0].SetColour(med);
  darkBrush[0].SetColour(dark);
  lightPen[0].SetColour(light);
  mediumPen[0].SetColour(med);
  darkPen[0].SetColour(dark);

  // selected
  lightBrush[1].SetColour(204,204,255);
  mediumBrush[1].SetColour(200,200,214);
  darkBrush[1].SetColour(148,148,170);
  lightPen[1].SetColour(204,204,255);
  mediumPen[1].SetColour(200,200,214);
  darkPen[1].SetColour(148,148,170);

  #else

  // unselected
  lightBrush[0].SetColour(255,255,255);
  mediumBrush[0].SetColour(204,204,204);
  darkBrush[0].SetColour(130,130,130);
  lightPen[0].SetColour(255,255,255);
  mediumPen[0].SetColour(204,204,204);
  darkPen[0].SetColour(130,130,130);

  // selected
  lightBrush[1].SetColour(204,204,255);
  mediumBrush[1].SetColour(180,180,192);
  darkBrush[1].SetColour(148,148,170);
  lightPen[1].SetColour(204,204,255);
  mediumPen[1].SetColour(180,180,192);
  darkPen[1].SetColour(148,148,170);

  #endif

  inited = true;
}

void GetColorGradient(float value,
                      unsigned char *red,
                      unsigned char *green,
                      unsigned char *blue)
{
  /* grayscale
  *red = unsigned char (255*(1.0-value));
  *green = *red;
  *blue = *red;
  */
  
  float r, g, b;

  if (value < 0.25) {
    r = 1.0;
    g = 1.0;
    b = (0.25 - value)/0.25;
    // --> (1.0, 1.0, 0.0)
  }
  else if (value < 0.5) {
    r = (0.5 - value)/0.25;
    g = 1.0 - (value - 0.25);
    b = 0.0;
    // --> (0.0, 0.75, 0.0)
  }
  else {
    r = (value - 0.5)/0.5;
    g = 0.75 - (value-0.5)*1.5;
    b = 0.0;
  }
  
  *red = (unsigned char) (255*r);
  *green = (unsigned char) (255*g);
  *blue = (unsigned char) (255*b);
}


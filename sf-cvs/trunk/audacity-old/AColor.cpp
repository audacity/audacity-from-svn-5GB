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
					  bool selected,
                      unsigned char *red,
                      unsigned char *green,
                      unsigned char *blue)
{
  /* grayscale
  *red = unsigned char (255*(1.0-value));
  *green = *red;
  *blue = *red;
  */

  const int gsteps = 4;
  float gradient[gsteps+1][3] = {
	{0.75, 0.75, 0.75},  // lt gray
	{0.30, 0.60, 1.00},  // lt blue
	{0.90, 0.10, 0.90},  // violet
	{1.00, 0.00, 0.00},  // red
	{1.00, 1.00, 1.00}}; // white

  
  float r, g, b;

  int left = int(value * gsteps);
  int right = (left==gsteps? gsteps: left+1);

  float rweight = (value * gsteps) - left;
  float lweight = 1.0 - rweight;

  r = (gradient[left][0] * lweight) + (gradient[right][0] * rweight);
  g = (gradient[left][1] * lweight) + (gradient[right][1] * rweight);
  b = (gradient[left][2] * lweight) + (gradient[right][2] * rweight);
  
  if (selected) {
	r *= 0.77;
	g *= 0.77;
	b *= 0.885;
  }

  *red = (unsigned char) (255*r);
  *green = (unsigned char) (255*g);
  *blue = (unsigned char) (255*b);
}


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
  mediumBrush[1].SetColour(130,130,204);
  darkBrush[1].SetColour(130,130,170);
  lightPen[1].SetColour(204,204,255);
  mediumPen[1].SetColour(130,130,204);
  darkPen[1].SetColour(130,130,170);

  #endif

  inited = true;
}



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

  #if defined(__WXMAC__) // && defined(TARGET_CARBON)

  // unselected
  lightBrush[0].SetColour(246,246,255);
  mediumBrush[0].SetColour(232,232,232);
  darkBrush[0].SetColour(140,140,160);
  lightPen[0].SetColour(246,246,255);
  mediumPen[0].SetColour(232,232,232);
  darkPen[0].SetColour(140,140,160);

  // selected
  lightBrush[1].SetColour(204,204,255);
  mediumBrush[1].SetColour(180,180,192);
  darkBrush[1].SetColour(148,148,170);
  lightPen[1].SetColour(204,204,255);
  mediumPen[1].SetColour(180,180,192);
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

  #endif

  inited = true;
}

const int AColor_midicolors[16][3] = {
    {255, 102, 102}, // 1=salmon
    {204, 0, 0},     // 2=red
    {255, 117, 23},  // 3=orange
    {255, 255, 0},   // 4=yellow    
    {0, 204, 0},     // 5=green
    {0, 204, 204},   // 6=turquoise
    {0, 0, 204},     // 7=blue
    {153, 0, 255},   // 8=blue-violet

    {140, 97, 54},   // 9=brown
    {120, 120, 120},    // 10=gray (drums)
    {255, 175, 40},  // 11=lt orange
    {102, 255, 102}, // 12=lt green
    {153, 255, 255}, // 13=lt turquoise
    {153, 153, 255}, // 14=lt blue
    {204, 102, 255}, // 15=lt blue-violet
    {255, 51, 204}}; // 16=lt red-violet

void AColor::MIDIChannel(wxDC *dc, int channel /* 1 - 16 */)
{
  if (channel >= 1 && channel <= 16) {
    const int *colors = AColor_midicolors[channel-1];
  
    dc->SetPen(wxPen(wxColour(colors[0],
                              colors[1],
                              colors[2]), 1, wxSOLID));
    dc->SetBrush(wxBrush(wxColour(colors[0],
                                  colors[1],
                                  colors[2]), wxSOLID));
  }
  else {
    dc->SetPen(wxPen(wxColour(153, 153, 153), 1, wxSOLID));
    dc->SetBrush(wxBrush(wxColour(153, 153, 153), wxSOLID));
  }

}

void AColor::LightMIDIChannel(wxDC *dc, int channel /* 1 - 16 */)
{
  if (channel >= 1 && channel <= 16) {
    const int *colors = AColor_midicolors[channel-1];
  
    dc->SetPen(wxPen(wxColour(127+colors[0]/2,
                              127+colors[1]/2,
                              127+colors[2]/2), 1, wxSOLID));
    dc->SetBrush(wxBrush(wxColour(127+colors[0]/2,
                                  127+colors[1]/2,
                                  127+colors[2]/2), wxSOLID));
  }
  else {
    dc->SetPen(wxPen(wxColour(204, 204, 204), 1, wxSOLID));
    dc->SetBrush(wxBrush(wxColour(204, 204, 204), wxSOLID));
  }

}

void AColor::DarkMIDIChannel(wxDC *dc, int channel /* 1 - 16 */)
{
  if (channel >= 1 && channel <= 16) {
    const int *colors = AColor_midicolors[channel-1];
  
    dc->SetPen(wxPen(wxColour(colors[0]/2,
                              colors[1]/2,
                              colors[2]/2), 1, wxSOLID));
    dc->SetBrush(wxBrush(wxColour(colors[0]/2,
                                  colors[1]/2,
                                  colors[2]/2), wxSOLID));
  }
  else {
    dc->SetPen(wxPen(wxColour(102, 102, 102), 1, wxSOLID));
    dc->SetBrush(wxBrush(wxColour(102, 102, 102), wxSOLID));
  }

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


/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.h

  Dominic Mazzoni

  Manages color brushes and pens and provides utility
  drawing functions

**********************************************************************/

#ifndef __AUDACITY_COLOR__
#define __AUDACITY_COLOR__

#include <wx/brush.h>
#include <wx/pen.h>

class wxDC;
class wxRect;

class AColor
{
public:
  static void Bevel(wxDC& dc, bool up, wxRect& r);

  static void Light(wxDC *dc, bool selected);
  static void Medium(wxDC *dc, bool selected);
  static void Dark(wxDC *dc, bool selected);
  
  static void MIDIChannel(wxDC *dc, int channel /* 1 - 16 */);

  static wxBrush lightBrush[2];
  static wxBrush mediumBrush[2];
  static wxBrush darkBrush[2];
  static wxPen lightPen[2];
  static wxPen mediumPen[2];
  static wxPen darkPen[2];

private:
  static bool inited;
  static void Init();

};

void GetColorGradient(float value,
					  bool selected,
                      unsigned char *red,
                      unsigned char *green,
                      unsigned char *blue);

#endif

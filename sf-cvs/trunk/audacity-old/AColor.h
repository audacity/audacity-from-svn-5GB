/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.h

  Dominic Mazzoni

  Manages color brushes and pens

**********************************************************************/

#ifndef __AUDACITY_COLOR__
#define __AUDACITY_COLOR__

#include <wx/brush.h>
#include <wx/pen.h>

class wxDC;

class AColor
{
public:
  static void Light(wxDC *dc, bool selected);
  static void Medium(wxDC *dc, bool selected);
  static void Dark(wxDC *dc, bool selected);

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

#endif

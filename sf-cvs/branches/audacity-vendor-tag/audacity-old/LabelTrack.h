/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _LABELTRACK_
#define _LABELTRACK_

#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/dynarray.h>
#include <wx/string.h>

#include "Track.h"

class DirManager;

struct LabelStruct {
	double start;
	double len;
	wxString title;
};

WX_DEFINE_ARRAY(LabelStruct *, LabelArray);

class LabelTrack: public VTrack
{
public:
  LabelArray labels;

  LabelTrack(DirManager *projDirManager);

  virtual void Draw(wxDC &dc, wxRect &r, double h, double pps,
					double sel0, double sel1);

  virtual int GetKind() {return Label;}
  virtual double GetMaxLen();
  
private:

  wxBrush labelBrush;
  wxBrush backgroundBrush;
  wxPen   labelPen;
  wxPen   backgroundPen;

};

#endif




/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/brush.h>
#include <wx/dc.h>
#include <wx/pen.h>

#include "LabelTrack.h"
#include "DirManager.h"

LabelTrack::LabelTrack(DirManager *projDirManager):
  VTrack(projDirManager)
{
  labelBrush.SetColour(204,51,255);
  backgroundBrush.SetColour(192,192,192);  
  labelPen.SetColour(204,51,255);
  backgroundPen.SetColour(192,192,192);  

  expandedHeight = 42; // Label tracks are narrow

  LabelStruct *l = new LabelStruct();
  l->start = 1.0;
  l->len = 3.0;
  l->title = "Test Label";
  
  labels.Insert(l, 0);
}

void LabelTrack::Draw(wxDC &dc, wxRect &r, double h, double pps,
					 double sel0, double sel1)
{
  dc.SetBrush(backgroundBrush);
  dc.SetPen(backgroundPen);
  dc.DrawRectangle(r);

  dc.SetBrush(labelBrush);
  dc.SetPen(labelPen);

  for(int i=0; i<labels.Count(); i++) {  
    wxRect lr;
    lr.y = r.y;
    lr.height = 20;
    lr.x = (int)(labels[i]->start - h)*pps;
    lr.width = (int)labels[i]->len*pps;
    
    if (lr.x < r.x + r.width && lr.x + lr.width > r.x) {
  	  dc.DrawRectangle(lr);
 			dc.SetTextForeground(wxColour(255,255,255));

			#ifdef __WXMAC__
			long textWidth, textHeight;
			#else
 			int textWidth, textHeight;
            #endif

 			dc.GetTextExtent(labels[i]->title, &textWidth, &textHeight);
 			if (textWidth + 8 < lr.width)
			  dc.DrawText(labels[i]->title, lr.x+4, lr.y+4);
  	  
  	}
  }
}

double LabelTrack::GetMaxLen()
{
  return 0.0;
}



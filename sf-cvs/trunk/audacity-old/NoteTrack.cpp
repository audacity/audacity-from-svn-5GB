/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/pen.h>

#include "NoteTrack.h"
#include "DirManager.h"

NoteTrack::NoteTrack(DirManager *projDirManager):
  VTrack(projDirManager)
{
  numEvents = 0;
  maxEvents = 1024;
  event = new NoteEvent[maxEvents];
  wxASSERT(event);
}

void NoteTrack::Draw(wxDC &dc, wxRect &r, double h, double pps,
					 double sel0, double sel1, bool drawEnvelope)
{
  int note0 = 50;
  int noteht = 4;
  int n;

  bool sel = false; // TODO

  wxBrush backBrush;
  wxPen backPen;

  backBrush.SetColour(214,214,214);
  backPen.SetColour(214,214,214);

  dc.SetBrush(backBrush);
  dc.SetPen(backPen);

  dc.DrawRectangle(r);
  
  dc.SetPen(wxPen(wxColour(151,0,255),1,wxSOLID));
  
  for(n=noteht; n<r.height; n+=noteht)
	dc.DrawLine(r.x, r.y+r.height-n,
				 r.x + r.width, r.y+r.height-n);
  
  int index;
  
  dc.SetBrush(wxBrush(wxColour(0,0,204),wxSOLID));
  dc.SetPen(wxPen(wxColour(0,0,204),1,wxSOLID));

  for(index=0; index < numEvents; index++) {
	
	int ypos = noteht * (event[index].note - note0);

	if (ypos >= 0 && ypos < r.height) {
	  int ht = noteht;
	  if (ypos + noteht >= r.height)
		ht = r.height - ypos;
	  
	  wxRect nr;

	  nr.x = (int)(((event[index].when / 44100.0) - h) * pps);
	  nr.width = (int)(event[index].len / 44100.0 * pps) + 1;
	  nr.y = r.y + r.height - ypos;
	  nr.height = ht;
	  
	  dc.DrawRectangle(nr);
	}

  }
  
}

double NoteTrack::GetMaxLen()
{
  if (numEvents == 0)
	return 0.0;
  else
	return ((event[numEvents-1].when + event[numEvents-1].len) /
			44100.0);
}

void NoteTrack::Add(int when, int len, int note)
{
  if (numEvents == maxEvents) {
	NoteEvent *ne = new NoteEvent[maxEvents*2];
	wxASSERT(ne);
	for(int i=0; i<maxEvents; i++)
	  ne[i] = event[i];
	delete[] event;
	event = ne;
	maxEvents*=2;
  }
  int pos = Find(when);
  for(int i=numEvents; i>pos; i--)
	event[i] = event[i-1];
  event[pos].when = when;
  event[pos].len = len;
  event[pos].note = note;
  numEvents++;
}

int NoteTrack::Find(int when)
{
  // TODO: This should use binary search

  int i=0;
  while(i<numEvents && event[i].when>=when)
	i++;

  return i;
}



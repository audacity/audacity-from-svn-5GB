/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _NOTETRACK_
#define _NOTETRACK_

#include <wx/string.h>

#include "Track.h"

class DirManager;

struct NoteEvent {
  int when;
  int len;
  int note;
};

class NoteTrack: public VTrack
{
public:
  int numEvents;
  int maxEvents;
  NoteEvent *event;

  NoteTrack(DirManager *projDirManager);

  virtual void Draw(wxDC &dc, wxRect &r, double h, double pps,
					double sel0, double sel1);

  virtual int GetKind() {return Note;}
  virtual double GetMaxLen();

  virtual void Add(int when, int len, int note);

  virtual int Find(int when);
};

#endif




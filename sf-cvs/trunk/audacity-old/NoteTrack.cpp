/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/pen.h>

#include "AColor.h"
#include "NoteTrack.h"
#include "DirManager.h"

NoteTrack::NoteTrack(DirManager *projDirManager):
  VTrack(projDirManager)
{
  mSeq = NULL;
  mLen = 0.0;
  
  mVisibleChannels = 0xFFFF;
}

void NoteTrack::Draw(wxDC &dc, wxRect &r, double h, double pps,
					 double sel0, double sel1, bool drawEnvelope)
{
  int ctrpitch = 60;
  int pitch0;
  int pitchht = 4;
  int n;

  int numPitches = r.height / pitchht;
  pitch0 = (ctrpitch - numPitches/2);

  wxBrush backBrush;
  wxPen backPen;

  backBrush.SetColour(214,214,214);
  backPen.SetColour(214,214,214);

  dc.SetBrush(backBrush);
  dc.SetPen(backPen);

  dc.DrawRectangle(r);
  
  dc.SetPen(wxPen(wxColour(151,0,255),1,wxSOLID));
  
  for(n=pitchht; n<r.height; n+=pitchht)
    dc.DrawLine(r.x, r.y+r.height-n,
                r.x + r.width, r.y+r.height-n);

  int numEvents = mSeq->notes.len;
  int index;

  for(index=0; index<numEvents; index++) {
  
    if (mSeq->notes[index]->type == 'n') {
    
      Note_ptr note = (Note_ptr)(mSeq->notes[index]);
      
      if (mVisibleChannels & (1 << (mSeq->notes[index]->chan & 15))) {
      
      	int ypos = int(pitchht * (note->pitch - pitch0) + 0.5);

      	if (ypos >= 0 && ypos < r.height) {
      	  int ht = pitchht;
      	  if (ypos + pitchht >= r.height)
            ht = r.height - ypos;
      	  
      	  wxRect nr;

      	  nr.x = r.x + (int)((note->time - h) * pps);
      	  nr.width = (int)(note->dur * pps) + 1;
      	  nr.y = r.y + r.height - ypos - pitchht;
      	  nr.height = ht;
      	  
      	  if (nr.x + nr.width >= r.x && nr.x<= r.x + r.width) {
      	    if (nr.x < r.x) {
      	      nr.width -= (r.x - nr.x);
      	      nr.x = r.x;
      	    }
      	    if (nr.x + nr.width > r.x + r.width)
      	      nr.width = r.x + r.width - nr.x;
      	    
      	    AColor::MIDIChannel(&dc, note->chan+1);
      	    
      	    if (note->time + note->dur >= sel0 &&
      	        note->time <= sel1)
      	      dc.SetBrush(*wxWHITE_BRUSH);

      	    dc.DrawRectangle(nr);
      	  }
      	}
      }

    }

  }
  
}

void NoteTrack::DrawLabelControls(wxDC &dc, wxRect &r)
{
  int wid = 23;
  int ht = 16;

  if (r.height < ht*4)
    return;
  
  int x = r.x + r.width / 2 - wid*2;
  int y = r.y + 4;
  
  for(int row=0; row<4; row++)
    for(int col=0; col<4; col++) {
      int channel = row*4 + col + 1;
      
      wxRect box;
      box.x = x + col*wid;
      box.y = y + row*ht;
      box.width = wid;
      box.height = ht;
      
      if (mVisibleChannels & (1 << (channel-1))) {      
        AColor::MIDIChannel(&dc, channel);
        dc.DrawRectangle(box);

        AColor::LightMIDIChannel(&dc, channel);
        dc.DrawLine(box.x, box.y, box.x+box.width-1, box.y);
        dc.DrawLine(box.x, box.y, box.x, box.y+box.height-1);
        
        AColor::DarkMIDIChannel(&dc, channel);
        dc.DrawLine(box.x+box.width-1, box.y, box.x+box.width-1, box.y+box.height);
        dc.DrawLine(box.x, box.y+box.height-1, box.x+box.width, box.y+box.height-1);
      }
      else {
        AColor::MIDIChannel(&dc, 0);
        dc.DrawRectangle(box);
      }
      
      dc.DrawText(wxString::Format("%d", channel), box.x+5, box.y+3);
    }
      
}

bool NoteTrack::LabelClick(wxRect &r, int mx, int my, bool right)
{
  int wid = 23;
  int ht = 16;

  if (r.height < ht*4)
    return false;
  
  int x = r.x + r.width / 2 - wid*2;
  int y = r.y + 4;
  
  int col = (mx - x) / wid;
  int row = (my - y) / ht;
  
  if (row < 0 || row >= 4 || col < 0 || col >= 4)
    return false;
  
  int channel = row*4 + col;
  
  if (right) {
    if (mVisibleChannels == (1 << channel))
      mVisibleChannels = 0xFFFF;
    else
      mVisibleChannels = (1 << channel);
  }
  else
    mVisibleChannels ^= (1 << channel);
  
  return true;
}

double NoteTrack::GetMaxLen()
{
  return mLen;
}

void NoteTrack::CalcLen()
{
  int numEvents = mSeq->notes.len;
  
  if (numEvents <= 0)
    mLen = 0.0;
  else {
    mLen = 0.0;
    for(int i=0; i<numEvents; i++) {
      if (mSeq->notes[numEvents-1]->time > mLen)
        mLen = mSeq->notes[numEvents-1]->time;
      if (mSeq->notes[numEvents-1]->type == 'n') {
        double endtime = mSeq->notes[numEvents-1]->time +
               ((Note_ptr)mSeq->notes[numEvents-1])->dur;
        if (endtime > mLen)
          mLen = endtime;
      }
    }
  }
}

void NoteTrack::SetSequence(Seq_ptr seq)
{
  if (mSeq)
    delete mSeq;
  
  mSeq = seq;
  
  CalcLen();
}

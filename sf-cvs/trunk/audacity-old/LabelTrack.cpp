/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/brush.h>
#include <wx/dc.h>
#include <wx/event.h>
#include <wx/pen.h>
#include <wx/string.h>
#include <wx/textfile.h>

#include "LabelTrack.h"
#include "DirManager.h"

LabelTrack::LabelTrack(DirManager *projDirManager):
  VTrack(projDirManager)
{
  mFlagBrush.SetColour(204, 0, 0);
  mUnselectedBrush.SetColour(192,192,192);
  mSelectedBrush.SetColour(148,148,170);

  mFlagPen.SetColour(204, 0, 0);
  mUnselectedPen.SetColour(192,192,192);
  mSelectedPen.SetColour(148,148,170);

  expandedHeight = 30; // Label tracks are narrow

  mSelIndex = -1;
}

LabelTrack::~LabelTrack()
{
  int len = mLabels.Count();

  for(int i=0; i<len; i++)
	delete mLabels[i];
}

void LabelTrack::Draw(wxDC &dc, wxRect &r, double h, double pps,
					  double sel0, double sel1)
{
  double right = h + r.width / pps;
  double dsel0 = sel0;
  if (dsel0 < h)
	dsel0 = h;
  if (dsel0 > right)
	dsel0 = right;
  double dsel1 = sel1;
  if (dsel1 < h)
	dsel1 = h;
  if (dsel1 > right)
	dsel1 = right;

  wxRect before = r;
  before.width = int((dsel0 - h) * pps);
  dc.SetBrush(mUnselectedBrush);
  dc.SetPen(mUnselectedPen);
  dc.DrawRectangle(before);

  wxRect selr = r;
  selr.x += before.width;
  selr.width = int((dsel1-dsel0)*pps);
  dc.SetBrush(mSelectedBrush);
  dc.SetPen(mSelectedPen);
  dc.DrawRectangle(selr);

  wxRect after = r;
  after.x += (before.width + selr.width);
  after.width -= (before.width + selr.width);
  if (after.x < r.x) {
	after.width -= (r.x - after.x);
	after.x = r.x;
  }
  dc.SetBrush(mUnselectedBrush);
  dc.SetPen(mUnselectedPen);
  dc.DrawRectangle(after);

  dc.SetBrush(mFlagBrush);
  dc.SetPen(mFlagPen);

  int nextx=0;

  for(int i=0; i<mLabels.Count(); i++) {  

	int x = r.x + (int)((mLabels[i]->t - h)*pps);
	int y = r.y;
	int height = r.height;

    if (x >= r.x && x < r.x + r.width) {

	  if (x < nextx && i != mSelIndex) {
		// Draw flag obscured by the previous label

		dc.DrawLine(x, y, x, y+2);
		dc.DrawLine(x, y+height-2, x, y+height);

		mLabels[i]->width = 0;
	  }
	  else {
		// Draw the flag and label

		wxPoint tri[3];
		tri[0].x = x;
		tri[0].y = y;
		tri[1].x = x-8;
		tri[1].y = y+4;
		tri[2].x = x;
		tri[2].y = y+8;

		if (mSelIndex == i)
		  dc.SetBrush(*wxWHITE_BRUSH);
		dc.DrawPolygon(3, tri);
		if (mSelIndex == i)
		  dc.SetBrush(mFlagBrush);
		
		dc.DrawLine(x, y, x, y+height);	  

		dc.SetTextForeground(wxColour(204, 0, 0));
		
        #ifdef __WXMAC__
		long textWidth, textHeight;
        #else
		int textWidth, textHeight;
        #endif
		
		dc.GetTextExtent(mLabels[i]->title, &textWidth, &textHeight);
		dc.DrawText(mLabels[i]->title, x+4, y+4);

		if (i == mSelIndex) {
		  wxRect outline;
		  
		  outline.x = x+2;
		  outline.y = y+2;
		  outline.width = textWidth+4;
		  outline.height = height-4;

		  dc.SetBrush(*wxTRANSPARENT_BRUSH);
		  dc.DrawRectangle(outline);
		  dc.SetBrush(mFlagBrush);
		}

		mLabels[i]->width = textWidth + 8;
		
		nextx = x+textWidth+8;
	  }
  	  
  	}
  }
}

double LabelTrack::GetMaxLen()
{
  int len = mLabels.Count();

  if (len == 0)
	return 0.0;
  else
	return mLabels[len-1]->t;
}

void LabelTrack::MouseDown(int x, int y, wxRect &r,
						   double h, double pps)
{
  double mouseH = h + (x - r.x) / pps;

  for(int i=0; i<mLabels.Count(); i++) {
	if (mLabels[i]->t - (8/pps) < mouseH &&
		mouseH < mLabels[i]->t + (mLabels[i]->width/pps)) {
	  mSelIndex = i;
	  return;
	}
  }

  mSelIndex = -1;
}

void LabelTrack::KeyEvent(double sel0, double sel1, wxKeyEvent &event)
{
  long keyCode = event.KeyCode();
  
  if (mSelIndex >= 0) {
	switch(keyCode) {
	case WXK_BACK: {
	  int len = mLabels[mSelIndex]->title.Length();
	  if (len > 0)
		mLabels[mSelIndex]->title = mLabels[mSelIndex]->title.Left(len-1);
	}
	break;

	case WXK_RETURN:
	  if (mLabels[mSelIndex]->title == "") {
		delete mLabels[mSelIndex];
		mLabels.RemoveAt(mSelIndex);
	  }
	  mSelIndex = -1;
	  break;

	case WXK_TAB:
	  if (event.ShiftDown()) {
		if (mSelIndex > 0)
		  mSelIndex--;
	  }
	  else {
		if (mSelIndex < mLabels.Count()-1)
		  mSelIndex++;
	  }
	  break;

	default:
	  mLabels[mSelIndex]->title += keyCode;
	  break;
	}
  }
  else {
	// Create new label

	LabelStruct *l = new LabelStruct();
	l->t = sel0;
	l->title += wxChar(keyCode);
	
	int len = mLabels.Count();
	int pos = 0;
	
	while(pos < len && l->t > mLabels[pos]->t)
	  pos++;
	
	mLabels.Insert(l, pos);
	
	mSelIndex = pos;
  }
}

void LabelTrack::Unselect()
{
  mSelIndex = -1;
}

void LabelTrack::Export(wxTextFile& f)
{
  for(int i=0; i<mLabels.Count(); i++) {
	f.AddLine(wxString::Format("%lf\t%s",
							   mLabels[i]->t,
							   (const char *)(mLabels[i]->title)));
  }
}

void LabelTrack::Import(wxTextFile& in)
{
  wxString currentLine;
  int i, len;
  int index, lines;
  wxString s;
  wxString title;
  double t;

  lines = in.GetLineCount();

  mLabels.Clear();
  mLabels.Alloc(lines);

  for(index=0; index<lines; index++) {
	currentLine = in.GetLine(index);

	len = currentLine.Length();
	if (len==0)
	  return;

	i = 0;
	while(i<len && currentLine.GetChar(i)!=' ' && currentLine.GetChar(i)!='\t')
	  i++;
	
	s = currentLine.Left(i);
	if (!s.ToDouble(&t))
	  return;
	
	while(i<len && (currentLine.GetChar(i)==' ' || currentLine.GetChar(i)=='\t'))
	  i++;
	
	title = currentLine.Right(len-i);

	LabelStruct *l = new LabelStruct();
	l->t = t;
	l->title = title;
	mLabels.Add(l);
  }
}

VTrack *LabelTrack::Duplicate()
{
  LabelTrack *copy = new LabelTrack(dirManager);
  int len = mLabels.Count();
  
  for(int i=0; i<len; i++) {
	LabelStruct *l = new LabelStruct();
	l->t = mLabels[i]->t;
	l->title = mLabels[i]->title;
	copy->mLabels.Add(l);
  }
  
  return copy;
}

bool LabelTrack::Load(wxTextFile *in, DirManager *dirManager)
{
  if (in->GetNextLine() != "NumMLabels") return false;

  long len;
  if (!(in->GetNextLine().ToLong(&len))) return false;

  int i;
  for(i=0; i<mLabels.Count(); i++)
	delete mLabels[i];
  mLabels.Clear();
  mLabels.Alloc(len);

  for(i=0; i<len; i++) {
	LabelStruct *l = new LabelStruct();
	if (!(in->GetNextLine().ToDouble(&l->t))) return false;
	l->title = in->GetNextLine();
	mLabels.Add(l);
  }

  if (in->GetNextLine() != "MLabelsEnd") return false;

  return true;
}

bool LabelTrack::Save(wxTextFile *out, bool overwrite)
{
  out->AddLine("NumMLabels");
  int len = mLabels.Count();
  out->AddLine(wxString::Format("%d", len));

  for(int i=0; i<len; i++) {
	out->AddLine(wxString::Format("%lf", mLabels[i]->t));
	out->AddLine(mLabels[i]->title);
  }
  out->AddLine("MLabelsEnd");

  return true;
}

void LabelTrack::Cut(double t0, double t1, VTrack **dest)
{
  *dest = new LabelTrack(dirManager);
  int len = mLabels.Count();
  
  for(int i=0; i<len; i++) {
	if (t0 <= mLabels[i]->t &&
		mLabels[i]->t <= t1) {
	  mLabels[i]->t -= t0;
	  ((LabelTrack *)(*dest))->mLabels.Add(mLabels[i]);
	  mLabels.RemoveAt(i);
	  len--;
	  i--;
	}
	if (mLabels[i]->t > t1)
	  mLabels[i]->t -= (t1 - t0);
  }
  ((LabelTrack *)(*dest))->mClipLen = (t1 - t0);
}

void LabelTrack::Copy(double t0, double t1, VTrack **dest)
{
  *dest = new LabelTrack(dirManager);
  int len = mLabels.Count();
  
  for(int i=0; i<len; i++) {
	if (t0 <= mLabels[i]->t &&
		mLabels[i]->t <= t1) {
	  LabelStruct *l = new LabelStruct();
	  l->t = mLabels[i]->t - t0;
	  l->title = mLabels[i]->title;
	  ((LabelTrack *)(*dest))->mLabels.Add(l);
	}
  }
  ((LabelTrack *)(*dest))->mClipLen = (t1 - t0);
}

void LabelTrack::Paste(double t, VTrack *src)
{
  if (src->GetKind() != VTrack::Label)
	return;

  int len = mLabels.Count();
  int pos = 0;

  while(pos < len && mLabels[pos]->t < t)
	pos++;

  LabelTrack *sl = (LabelTrack *)src;
  for(int j=0; j<sl->mLabels.Count(); j++) {
	LabelStruct *l = new LabelStruct();
	l->t = sl->mLabels[j]->t + t;
	l->title = sl->mLabels[j]->title;
	mLabels.Insert(l, pos++);
	len++;
  }

  while(pos<len) {
	mLabels[pos]->t += sl->mClipLen;
	pos++;
  }
}

void LabelTrack::Clear(double t0, double t1)
{
  int len = mLabels.Count();
  
  for(int i=0; i<len; i++) {
  	if (t0 <= mLabels[i]->t &&
  		mLabels[i]->t <= t1) {
  	  mLabels.RemoveAt(i);
  	  len--;
  	  i--;
  	}
  	if (mLabels[i]->t > t1)
  	  mLabels[i]->t -= (t1 - t0);
  }
}

void LabelTrack::Silence(double t0, double t1)
{
  int len = mLabels.Count();
  
  for(int i=0; i<len; i++) {
  	if (t0 <= mLabels[i]->t &&
  		mLabels[i]->t <= t1) {
  	  mLabels.RemoveAt(i);
  	  len--;
  	  i--;
  	}
  }  
}

void LabelTrack::InsertSilence(double t, double len)
{
  int numLabels = mLabels.Count();
  
  for(int i=0; i<numLabels; i++)
  	if (mLabels[i]->t >= t)
  	  mLabels[i]->t += len;
}



/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/pen.h>

#include "Envelope.h"
#include "DirManager.h"

Envelope::Envelope()
{
  mOffset = 0.0;
  mTrackLen = 1000000000.0;

  Insert(0.0, 1.0);
  Insert(1000000000.0, 1.0);

  mDragPoint = -1;
  mDirty = false;

  mIsDeleting = false;
}

Envelope::~Envelope()
{
  int len = mEnv.Count();
  for(int i=0; i<len; i++)
	delete mEnv[i];
}

void Envelope::CopyFrom(Envelope *e)
{
  mOffset = e->mOffset;
  int len = e->mEnv.Count();
  mTrackLen = e->mEnv[len-1]->t;
  for(int i=len-2; i>=1; i--)
	Insert(e->mEnv[i]->t, e->mEnv[i]->val);
}

void Envelope::Draw(wxDC &dc, wxRect &r, double h, double pps)
{
  h -= mOffset;

  double tright = h + (r.width / pps);

  dc.SetPen(*wxGREEN_PEN);
  dc.SetBrush(*wxWHITE_BRUSH);

  int ctr = r.y + (r.height/2);

  int len = mEnv.Count();
  for(int i=0; i<len; i++) {
	if (mEnv[i]->t >= h && mEnv[i]->t <= tright) {
	  if (i == mDragPoint) {
		dc.SetPen(*wxGREEN_PEN);
		dc.SetBrush(*wxGREEN_BRUSH);
	  }

	  double v = mEnv[i]->val;
	  int x = int((mEnv[i]->t - h) * pps);
	  int y = int(ctr - v*(r.height/2));
	  wxRect circle(r.x+x-1, y-1, 4, 4);
	  dc.DrawEllipse(circle);
	  circle.y = int(ctr + v*(r.height/2))-2;
	  dc.DrawEllipse(circle);

	  if (i == mDragPoint) {
		dc.SetPen(*wxGREEN_PEN);
		dc.SetBrush(*wxWHITE_BRUSH);
	  }
	}
  }
}

bool Envelope::Load(wxTextFile *in, DirManager *dirManager)
{
  if (in->GetNextLine() != "EnvNumPoints") return false;

  long len;
  if (!(in->GetNextLine().ToLong(&len))) return false;

  mEnv.Clear();
  mEnv.Alloc(len);

  for(int i=0; i<len; i++) {
	EnvPoint *e = new EnvPoint();
	if (!(in->GetNextLine().ToDouble(&e->t))) return false;
	if (!(in->GetNextLine().ToDouble(&e->val))) return false;
	if (i>0 && mEnv[i-1]->t > e->t) return false;
	mEnv.Add(e);
  }

  if (in->GetNextLine() != "EnvEnd") return false;

  return true;
}

bool Envelope::Save(wxTextFile *out, bool overwrite)
{
  out->AddLine("EnvNumPoints");
  int len = mEnv.Count();
  out->AddLine(wxString::Format("%d", len));
  for(int i=0; i<len; i++) {
	out->AddLine(wxString::Format("%lf", mEnv[i]->t));
	out->AddLine(wxString::Format("%lf", mEnv[i]->val));
  }
  out->AddLine("EnvEnd");

  return true;
}

// Returns true if parent needs to be redrawn
bool Envelope::MouseEvent(wxMouseEvent &event, wxRect &r, double h, double pps)
{
  //h -= mOffset;

  int ctr = r.y + r.height/2;
  bool upper = (event.m_y < ctr);

  if (event.ButtonDown()) {
	mIsDeleting = false;
	double tright = h + (r.width / pps);
	int bestNum = -1;
	int bestDist = 8;

	int len = mEnv.Count();
	for(int i=0; i<len; i++) {
	  if (mEnv[i]->t >= h && mEnv[i]->t <= tright) {
		double v = mEnv[i]->val;
		int x = int((mEnv[i]->t - h) * pps) + r.x;
		int y;
		if (upper)
		  y = int(ctr - v*(r.height/2));
		else
		  y = int(ctr + v*(r.height/2));
		
        #ifndef SQR
        #define SQR(X) ((X)*(X))
        #endif
		
		int d = int(sqrt(SQR(x-event.m_x)+SQR(y-event.m_y))+0.5);
		if (d < bestDist) {
		  bestNum = i;
		  bestDist = d;
		}
	  }
	}

	if (bestNum >= 0) {
	  mDragPoint = bestNum;
	}
	else {
	  // Create new point
	  double when = h + (event.m_x - r.x)/pps - mOffset;

	  int y;
	  if (upper)
		y = ctr - event.m_y;
	  else
		y = event.m_y - ctr;
	  
	  double newVal = y / double(r.height/2);
	  if (newVal < 0.0)
		newVal = 0.0;
	  if (newVal > 1.0)
		newVal = 1.0;
	
	  mDragPoint = Insert(when, newVal);
	  mDirty = true;
	}

	mUpper = upper;

	mInitialWhen = mEnv[mDragPoint]->t;
	mInitialVal = mEnv[mDragPoint]->val;
	
	mInitialX = event.m_x;
	mInitialY = event.m_y;

	return true;
  }
  
  if (event.Dragging() && mDragPoint >= 0) {
	mDirty = true;

	wxRect larger = r;
	larger.Inflate(5, 5);
	
	if (!mIsDeleting &&
		mDragPoint > 0 && mDragPoint < mEnv.Count()-1 &&
		!larger.Inside(event.m_x, event.m_y)) {
	  
	  mEnv[mDragPoint]->t = mEnv[mDragPoint-1]->t;
	  mEnv[mDragPoint]->val = mEnv[mDragPoint-1]->val;

	  mIsDeleting = true;

	  return true;
	}

	if (larger.Inside(event.m_x, event.m_y))
	  mIsDeleting = false;

	if (mIsDeleting)
	  return false;

	int y;
	if (mUpper)
	  y = ctr - event.m_y;
	else
	  y = event.m_y - ctr;

	double newVal = y / double(r.height/2);
	if (newVal < 0.0)
	  newVal = 0.0;
	if (newVal > 1.0)
	  newVal = 1.0;

	double newWhen = mInitialWhen + (event.m_x - mInitialX)/pps;

	if (mDragPoint > 0 && newWhen < mEnv[mDragPoint-1]->t)
	  newWhen = mEnv[mDragPoint-1]->t;

	if (mDragPoint < mEnv.Count()-1 && newWhen > mEnv[mDragPoint+1]->t)
	  newWhen = mEnv[mDragPoint+1]->t;

	if (mDragPoint == 0)
	  newWhen = 0;

	if (mDragPoint == mEnv.Count()-1)
	  newWhen = mTrackLen;

	mEnv[mDragPoint]->t = newWhen;
	mEnv[mDragPoint]->val = newVal;
	
	return true;
  }
  
  if (event.ButtonUp()) {
	if (mIsDeleting) {
	  delete mEnv[mDragPoint];
	  mEnv.RemoveAt(mDragPoint);
	}
	mDragPoint = -1;
	return true;
  }
  
  return false;
}

void Envelope::CollapseRegion(double t0, double t1)
{
  // This gets called when somebody clears samples.  All of the
  // control points within the region disappear and the points
  // to the right get shifted over.

  t0 -= mOffset;
  t1 -= mOffset;

  int len = mEnv.Count();
  int i;

  for(i=1; i<len-1; i++)
	if (mEnv[i]->t >= t0 && mEnv[i]->t < t1) {
	  delete mEnv[i];
	  mEnv.RemoveAt(i);
	  len--;
	}

  for(i=0; i<len; i++)
	if (mEnv[i]->t >= t1)
	  mEnv[i]->t -= (t1 - t0);
}

void Envelope::ExpandRegion(double t0, double deltat)
{
  t0 -= mOffset;

  // This gets called when somebody pastes samples.  All of the
  // control points to the right of the paste get shifted to the
  // right.  It's not always exactly what you wanted, but it's
  // at least intuitive how to fix it.  If you think about it, you'll
  // realize there's no logical way to preserve envelope information
  // across a cut/paste sequence.

  int len = mEnv.Count();

  for(int i=0; i<len; i++)
	if (mEnv[i]->t > t0)
	  mEnv[i]->t += deltat;
}

// Private methods

int Envelope::Insert(double when, double value)
{
  EnvPoint *e = new EnvPoint();
  e->t = when;
  e->val = value;

  int len = mEnv.Count();
  if (len==0) {
	mEnv.Add(e);
	return 0;
  }
  else {
	int i=0;
	while(i<len && when>mEnv[i]->t)
	  i++;
	
	if (i<len) {
	  mEnv.Insert(e, i);
	  return i;
	}
	else {
	  mEnv.Add(e);
	  return len;
	}
  }
}

// Control

void Envelope::SetOffset(double newOffset)
{
  mOffset = newOffset;
}

void Envelope::SetTrackLen(double trackLen)
{
  mTrackLen = trackLen;

  int len = mEnv.Count();
  for(int i=0; i<len-1; i++)
	if (mEnv[i]->t > mTrackLen) {
	  delete mEnv[i];
	  mEnv.RemoveAt(i);
	  len--;
	}
  mEnv[len-1]->t = mTrackLen;
}

// Accessors

bool Envelope::IsLinearInRegion(double t0, double t1)
{
  // i.e. is this region totally free from control
  // points.  This means that external functions can
  // interpolate instead of calling GetValue() at every
  // sample...

  t0 -= mOffset;
  t1 -= mOffset;

  int len = mEnv.Count();
  for(int i=0; i<len; i++) {
	if (mEnv[i]->t > t0 && mEnv[i]->t < t1)
	  return false;
	if (mEnv[i]->t > t1)
	  return true;
  }

  return true;
}

double Envelope::GetValue(double t)
{
  t -= mOffset;

  // TODO: This should use a binary search
  int len = mEnv.Count();
  int i=0;
  while(i<len && t>mEnv[i]->t)
	i++;

  if (i>0 && i<len) {
	double t0 = mEnv[i-1]->t;
	double t1 = mEnv[i]->t;
	double v0 = mEnv[i-1]->val;
	double v1 = mEnv[i]->val;

	// Interpolate

	double dt = (t1 - t0);

	// This should never happen, but we certainly
	// don't want to divide by zero...
	if (dt <= 0.0)
	  return 1.0;

	double to = t - t0;
	return (v0*(dt-to) + v1*to) / dt;
  }
  
  return 1.0;
}

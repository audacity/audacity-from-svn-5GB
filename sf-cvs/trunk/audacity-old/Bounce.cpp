/**********************************************************************

  Audacity: A Digital Audio Editor

  Bounce.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/msgdlg.h>
#include <wx/file.h>
#include <wx/filedlg.h>
#endif

#include <wx/textfile.h>

#include <math.h>

#include "AColor.h"
#include "Bounce.h"
#include "LabelTrack.h"
#include "Project.h"
#include "Track.h"

enum {
  FirstID = 7900,
  
  BounceCloseButtonID,
};

#define BOUNCE_WINDOW_WIDTH 300
#define BOUNCE_WINDOW_HEIGHT 100

// Bounce

BEGIN_EVENT_TABLE(Bounce, wxFrame)
  EVT_CLOSE  (                      Bounce::OnCloseWindow)
  EVT_SIZE   (                      Bounce::OnSize)
  EVT_PAINT  (                      Bounce::OnPaint)
END_EVENT_TABLE()

Bounce::Bounce(wxWindow* parent, wxWindowID id, const wxString& title,
			   const wxPoint& pos) :
  wxFrame(parent, id, title, pos,
	      wxSize(BOUNCE_WINDOW_WIDTH, BOUNCE_WINDOW_HEIGHT))
{
  mBouncePane = new BouncePane(this, 0,
						   wxPoint(0, 0),
						   wxSize(BOUNCE_WINDOW_WIDTH, 330));
  
  // Min size, max size
  SetSizeHints(100, 100,
			   20000, 20000);
}

Bounce::~Bounce()
{
  delete mBouncePane;
}

void Bounce::OnSize(wxSizeEvent &event)
{
  mBouncePane->Resize();
}

void Bounce::OnPaint(wxPaintEvent &evt)
{
}

void Bounce::SetProject(AudacityProject *project)
{
  mBouncePane->SetProject(project);
}

void Bounce::SetTime(double t)
{
  mBouncePane->SetTime(t);
}

void Bounce::OnCloseWindow(wxCloseEvent& WXUNUSED(event))
{
  this->Show(FALSE);
}

BEGIN_EVENT_TABLE(BouncePane, wxWindow)
  EVT_PAINT  (                      BouncePane::OnPaint)
  EVT_MOUSE_EVENTS(                 BouncePane::OnMouseEvent)
END_EVENT_TABLE()

BouncePane::BouncePane(wxWindow *parent, wxWindowID id,
				   const wxPoint& pos,
				   const wxSize& size):
  wxWindow(parent, id, pos, size),
  mBitmap(NULL)
{
  mBackgroundBrush.SetColour(wxColour(204, 204, 204));
  mBackgroundPen.SetColour(wxColour(204, 204, 204));

  project = NULL;
  labels = NULL;
  t = 0.0;
}

BouncePane::~BouncePane()
{
  if (mBitmap)
	delete mBitmap;

  DestroyLabels();
}

void BouncePane::Resize()
{
  int width, height;
  GetParent()->GetClientSize(&width, &height);

  SetSize(0, 0, width, height);

  if (mBitmap)
	delete mBitmap;
  mBitmap = NULL;
  
  #ifdef __WXMAC__
  Refresh(true);
  #endif
}

void BouncePane::SetTime(double t)
{
  this->t = t;
  Refresh(false);
}

void BouncePane::CreateLabels(LabelArray *l)
{
  DestroyLabels();

  labels = new BounceLabelArray();
  int len = 0;

  for(int i=0; i<l->Count(); i++) {
	wxString s = l->Item(i)->title;
	if (s != "" && s[0] != '$' && s[1] != '*') {
	  BounceLabel *b = new BounceLabel();
	  b->t = l->Item(i)->t;
	  b->title = l->Item(i)->title;
	  if (b->title.Find('('))
		b->title = b->title.Left(b->title.Find('('));
	  b->spacing = 10;
	  labels->Add(b);
	  len++;
	}
	else if (len>0 && i>0 && (l->Item(i)->t - l->Item(i-1)->t) >= 0.5)
	  labels->Item(len-1)->spacing = 30;
  }
  
  if (len == 0)
	return;

  BounceLabel *b = new BounceLabel();
  b->t = labels->Item(len-1)->t + 10;
  b->title = "";
  b->spacing = 0;
  labels->Add(b);
}

void BouncePane::DestroyLabels()
{
  if (labels) {
	for(int i=0; i<labels->Count(); i++)
	  delete labels->Item(i);
	delete labels;
	labels = NULL;
  }
}

void BouncePane::SetProject(AudacityProject *project)
{
  if (project == NULL) {	
	GetParent()->Show(false);
	return;
  }

  TrackListIterator iter(project->GetTracks());
  VTrack *t = iter.First();
  while(t) {
	if (t->GetKind() == VTrack::Label) {
	  this->project = project;
	  CreateLabels(&((LabelTrack *)t)->mLabels);

	  GetParent()->Show(true);

	  return;
	}
	t = iter.Next();
  }
 
  this->project = NULL;
  this->labels = NULL;
}

void BouncePane::OnMouseEvent(wxMouseEvent& event)
{
}

void BouncePane::OnPaint(wxPaintEvent &evt)
{
  if (project == NULL || labels==NULL)
	return;

  wxPaintDC dc(this);

  int width, height;
  GetSize(&width, &height);
  
  if (!mBitmap)
	mBitmap = new wxBitmap(width, height);

  wxMemoryDC memDC;
  
  memDC.SelectObject(*mBitmap);

  memDC.SetBrush(mBackgroundBrush);
  memDC.SetPen(mBackgroundPen);
  memDC.DrawRectangle(0, 0, width, height);

  wxFont font(18, wxROMAN, wxBOLD, wxNORMAL);
  memDC.SetFont(font);

  long preWidth = 0;
  long curWidth = 0;

  double deltat = 0.0;

  int bx = width/2;
  int by = height-22;

  long sw, sh;
  int len = labels->Count();

  int i=0;

  while(i < len-1 && labels->Item(i+1)->t < t) {
	memDC.GetTextExtent(labels->Item(i)->title, &sw, &sh);
	preWidth += sw + labels->Item(i)->spacing;
	i++;
  }

  if (i < len-1) {
	curWidth = 0;
	memDC.GetTextExtent(labels->Item(i)->title, &sw, &sh);
	preWidth += sw/2;
	curWidth += sw/2 + labels->Item(i)->spacing;
	memDC.GetTextExtent(labels->Item(i+1)->title, &sw, &sh);
	curWidth += sw/2;

	deltat = labels->Item(i+1)->t - labels->Item(i)->t;
	if (deltat > 0.0) {
	  double frac = (t - labels->Item(i)->t) / deltat;
	  preWidth += long(curWidth * frac);
	  double ht = deltat * height/2;
	  if (ht > (height-25))
		ht = height-25;
	  by -= int(ht * 4.0 * (-frac*frac + frac));
	}
  }

  int pos = width/2 - preWidth;

  memDC.SetTextForeground(*wxBLACK);

  for(int j=0; j<i; j++) {
	memDC.GetTextExtent(labels->Item(j)->title, &sw, &sh);
	if (pos + sw >= 0 && pos < width &&
		labels->Item(j)->title != "*") {
	  memDC.DrawText(labels->Item(j)->title,
					 pos,
					 height-20);
	}
	pos += sw + labels->Item(j)->spacing;
  }

  if (i>=0 && i<len) {

	memDC.SetTextForeground(*wxRED);
	memDC.GetTextExtent(labels->Item(i)->title, &sw, &sh);

	if (labels->Item(i)->title != "*")
	  memDC.DrawText(labels->Item(i)->title,
					 pos,
					 height-20);

	pos += sw + labels->Item(i)->spacing;

	memDC.SetTextForeground(*wxBLACK);
  }

  for(int j=i+1; j<len; j++) {
	memDC.GetTextExtent(labels->Item(j)->title, &sw, &sh);
	if (pos + sw >= 0 && pos < width &&
		labels->Item(j)->title != "*") {
	  memDC.DrawText(labels->Item(j)->title,
					 pos,
					 height-20);
	}
	pos += sw + labels->Item(j)->spacing;
  }

  memDC.SetPen(*wxRED_PEN);
  memDC.SetBrush(*wxRED_BRUSH);
  memDC.DrawEllipse(bx-3, by-3, 6, 6);

  dc.Blit(0, 0, width, height,
		  &memDC, 0, 0, wxCOPY, FALSE);
}




/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/image.h>

#include "TrackPanel.h"
#include "APalette.h"
#include "AColor.h"
#include "Track.h"
#include "WaveTrack.h"

BEGIN_EVENT_TABLE(TrackPanel, wxWindow)
  EVT_MOUSE_EVENTS(TrackPanel::OnMouseEvent)
  EVT_PAINT(TrackPanel::OnPaint)
END_EVENT_TABLE()

TrackPanel::TrackPanel(wxWindow *parent, wxWindowID id,
					   const wxPoint& pos,
					   const wxSize& size,
					   TrackList *tracks,
					   ViewInfo *viewInfo) :
  wxWindow(parent, id, pos, size),
  mTracks(tracks),
  mViewInfo(viewInfo),
  mBitmap(NULL)
{
  mIsSelecting = false;
  mIsResizing = false;
  mIsSliding = false;

  mArrowCursor = new wxCursor(wxCURSOR_ARROW);
  mSelectCursor = new wxCursor(wxCURSOR_IBEAM);
  mSlideCursor = new wxCursor(wxCURSOR_SIZEWE);
  mResizeCursor = new wxCursor(wxCURSOR_SIZENS);
  mZoomInCursor = new wxCursor(wxCURSOR_MAGNIFIER);
  mZoomOutCursor = new wxCursor(wxCURSOR_MAGNIFIER);

  mTimeCount = 0;
  mTimer.parent = this;
  mTimer.Start(50, FALSE);
}

TrackPanel::~TrackPanel()
{
  if (mBitmap)
	delete mBitmap;

  delete mArrowCursor;
  delete mSelectCursor;
  delete mSlideCursor;
  delete mResizeCursor;
  delete mZoomInCursor;
  delete mZoomOutCursor;
}

void TrackPanel::SelectNone()
{
  VTrack *t = mTracks->First();
  while(t) {
	t->selected = false;
	t = mTracks->Next();
  }
}

void TrackPanel::GetTracksUsableArea(int *width, int *height)
{
  int w, h;
  GetSize(&w, &h);

  w -= GetLabelWidth();
  w -= 10; // (4 pixel inset + 1 pixel bevel) * 2

  *width = w;
  *height = h;
}

void TrackPanel::OnTimer()
{
  // Handle auto-scroll
  
  mTimeCount = (mTimeCount+1)%10;
  if (mTimeCount == 0) {
	if (mTracks->First() != NULL &&
		mViewInfo->sel0 == mViewInfo->sel1) {
	  // The difference between a wxClientDC and a wxPaintDC
	  // is that the wxClientDC is used outside of a paint loop,
	  // whereas the wxPaintDC is used inside a paint loop
	  wxClientDC dc(this);
	  dc.SetLogicalFunction(wxINVERT);
	  dc.SetPen(*wxBLACK_PEN);
	  dc.SetBrush(*wxBLACK_BRUSH);
	  
	  int x = GetLabelOffset() +
		int((mViewInfo->sel0 - mViewInfo->h) * mViewInfo->zoom);

	  int y = -mViewInfo->vpos + GetRulerHeight();
	  
	  // Draw cursor in ruler
	  dc.DrawLine(x,1,x,GetRulerHeight()-2);

	  if (x >= GetLabelOffset()) {
		// Draw cursor in all selected tracks
		VTrack *t;
		t = mTracks->First();
		while(t) {
		  int height = t->GetHeight();
		  if (t->selected)
			dc.DrawLine(x,y+5,x,y+height-5);
		  t = mTracks->Next();
		  y += height;
		}
	  }
	}
  }
}

void TrackPanel::OnPaint(wxPaintEvent& event)
{
  wxPaintDC dc(this);
  int width, height;
  GetSize(&width, &height);
  if (width != mPrevWidth || height != mPrevHeight || !mBitmap) {
	mPrevWidth = width;
	mPrevHeight = height;
	
	if (mBitmap)
	  delete mBitmap;
	
	mBitmap = new wxBitmap(width, height);
  }

  wxMemoryDC memDC;

  memDC.SelectObject(*mBitmap);

  /*
  memDC.SetBrush(*wxWHITE_BRUSH);
  memDC.SetPen(*wxWHITE_PEN);
  memDC.DrawRectangle(0, 0, width, height);
  */

  DrawRuler(memDC);
  DrawTracks(memDC);

  dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);
}

void TrackPanel::MakeParentRedrawScrollbars()
{
  // An AudacityProject is programmed to redraw its scrollbars
  // whenever it gets a thumb release message from its main pane.

  wxScrollEvent *e =
	new wxScrollEvent(wxEVT_SCROLL_THUMBTRACK, GetId());      
  GetParent()->ProcessEvent(*e);      
  delete e;
}

void TrackPanel::HandleCursor(wxMouseEvent& event)
{
  if (mIsSelecting) {
	SetCursor(*mSelectCursor);	
	return;
  }

  if (mIsSliding) {
	SetCursor(*mSlideCursor);	
	return;
  }

  wxRect r;
  int num;
  
  VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);  

  if (t) {

	// First test to see if we're over the area that
	// resizes a track

	if (event.m_y >= (r.y + r.height - 10) &&
		event.m_y <  (r.y + r.height)) {	  
	  SetCursor(*mResizeCursor);
	  return;
	}

	// Otherwise set the cursor based on the current tool

	switch(gAPalette->GetCurrentTool()) {
	case 0: // select
	  SetCursor(*mSelectCursor);
	  break;
	case 1: // envelope
	  SetCursor(*mArrowCursor);
	  break;
	case 2: // move/slide
	  SetCursor(*mSlideCursor);
	  break;
	case 3: // zoom
	  if (event.ShiftDown())
		SetCursor(*mZoomInCursor);
	  else
		SetCursor(*mZoomOutCursor);
	  break;
	}  
  }
  else {
	// Not over a track
	SetCursor(*mArrowCursor);
  }
}

void TrackPanel::HandleSelect(wxMouseEvent& event)
{
  if (event.ButtonDown()) {
	wxRect r;
	int num;
	
	VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);
	
	if (t) {
	  mCapturedTrack = t;
	  mCapturedRect = r;
	  mCapturedNum = num;
	  
	  mMouseClickX = event.m_x;
	  mMouseClickY = event.m_y;
	  
	  mMouseMostRecentX = event.m_x;
	  mMouseMostRecentY = event.m_y;

	  if (event.ShiftDown()) { // Extend selection
		double selend = mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);
		
		if (selend > mViewInfo->sel1) {
		  mViewInfo->sel1 = selend;
		  mSelStart = mViewInfo->sel0;
		}
		else if (selend < mViewInfo->sel0) {
		  mViewInfo->sel0 = selend;
		  mSelStart = mViewInfo->sel1;
		} else {
		  mViewInfo->sel1 = selend;
		  mSelStart = mViewInfo->sel0;
		}
	  }
	  else { // Selecting
		mSelStart = mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);
		
		SelectNone();
		t->selected = true;
		
		mViewInfo->sel0 = mSelStart;
		mViewInfo->sel1 = mSelStart;

		mIsSelecting = true;
	  }

	  Refresh(false);
	
	}
	else {
	  // No track was selected
	  
	  SelectNone();
	  Refresh(false);
	}
  }

  if (!mIsSelecting)
	return;

  if (event.Dragging()) {
	VTrack *t;
	wxRect r;
	int num;
	
	if (mCapturedTrack) {
	  t = mCapturedTrack;
	  r = mCapturedRect;
	  num = mCapturedNum;
	}
	else
	  t = FindTrack(event.m_x, event.m_y, false, &r, &num);
	
	if (t) {
	  mMouseMostRecentX = event.m_x;
	  mMouseMostRecentY = event.m_y;
	  
	  // Selecting
	  
      double selend = mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);
      
	  if (selend < 0.0)
		selend = 0.0;

      if (selend >= mSelStart) {
		mViewInfo->sel0 = mSelStart;
		mViewInfo->sel1 = selend;
	  }
	  else {
		mViewInfo->sel0 = selend;
		mViewInfo->sel1 = mSelStart;
	  }

	  // Handle which tracks are selected

	  int num2;
	  if (0 != FindTrack(event.m_x, event.m_y, false, NULL, &num2)) {
		// The tracks numbered num...num2 should be selected
		
		VTrack *t = mTracks->First();
		int i=1;
		while(t) {
		  t->selected = (i>=num && i<=num2) || (i>=num2 && i<=num);
		  t = mTracks->Next();
		  i++;
		}
	  }

	  //wxString str;
	  //str.Printf("Selection: %lf - %lf seconds",sel0,sel1);
	  //status->SetLabel(str);

	  Refresh(false);
    }
  }

  if (event.ButtonUp()) {
	mCapturedTrack = NULL;
	mIsSelecting = false;
  }
}

void TrackPanel::HandleEnvelope(wxMouseEvent& event)
{
}

void TrackPanel::HandleSlide(wxMouseEvent& event)
{
  if (event.ButtonDown()) {
	wxRect r;
	int num;
	
	VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);
	
	if (t) {
	  mCapturedTrack = t;
	  mCapturedRect = r;
	  mCapturedNum = num;
	  
	  mMouseClickX = event.m_x;
	  mMouseClickY = event.m_y;
	  
	  mMouseMostRecentX = event.m_x;
	  mMouseMostRecentY = event.m_y;
	  
	  mSelStart = mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);
	  mIsSliding = true;
	}
  }

  if (!mIsSliding)
	return;
  
  if (event.Dragging()) {
	

	mMouseMostRecentX = event.m_x;
	mMouseMostRecentY = event.m_y;

	double selend = mViewInfo->h +
	  ((event.m_x - mCapturedRect.x) / mViewInfo->zoom);
	
	if (selend < 0.0)
	  selend = 0.0;
	
	if (selend != mSelStart) {
	  mCapturedTrack->Offset(selend - mSelStart);
	  Refresh(false);
	}
    
	mSelStart = selend;
  }

  if (event.ButtonUp()) {
	mCapturedTrack = NULL;
	mIsSliding = false;
	MakeParentRedrawScrollbars();
  }
  
}

void TrackPanel::HandleZoom(wxMouseEvent& event)
{
  if (event.ButtonDown() || event.ButtonDClick()) {
	double center_h = mViewInfo->h + (event.m_x - 115) / mViewInfo->zoom;
	
	if (event.RightDown() || event.RightDClick() || event.ShiftDown())
	  mViewInfo->zoom /= 2.0;
	else
	  mViewInfo->zoom *= 2.0;
	
	if (event.MiddleDown() || event.MiddleDClick())
	  mViewInfo->zoom = 44100.0 / 512.0;
	
	double new_center_h = mViewInfo->h + (event.m_x - 115) / mViewInfo->zoom;
	
	mViewInfo->h += (center_h - new_center_h);
	
	if (mViewInfo->h < 0)
	  mViewInfo->h = 0;
	
	MakeParentRedrawScrollbars();
	Refresh(false);
  }
}

void TrackPanel::HandleLabelClick(wxMouseEvent& event)
{
  if (event.ButtonDown()) {
	wxRect r;
	int num;
	
	VTrack *t = FindTrack(event.m_x, event.m_y, true, &r, &num);

	if (t) {
	  if (event.ShiftDown()) {
		t->selected = !t->selected;
	  }
	  else {
		SelectNone();
		t->selected = true;
		mViewInfo->sel0 = 0.0;
		mViewInfo->sel1 = mTracks->GetMaxLen();
	  }

	  Refresh(false);
	}
  }
}

void TrackPanel::HandleResize(wxMouseEvent& event)
{
  if (event.ButtonDown()) {
	wxRect r;
	int num;
	
	VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);
	
	if (t) {
	  mCapturedTrack = t;
	  mCapturedRect = r;
	  mCapturedNum = num;
	  
	  mMouseClickX = event.m_x;
	  mMouseClickY = event.m_y;
	  
	  mMouseMostRecentX = event.m_x;
	  mMouseMostRecentY = event.m_y;

	  mIsResizing = true;
	  mInitialTrackHeight = t->GetHeight();
	}
  }

  if (!mIsResizing)
	return;

  if (event.Dragging()) {
	int delta = (event.m_y - mMouseClickY);
	int newTrackHeight = mInitialTrackHeight + delta;
	if (newTrackHeight < 20)
	  newTrackHeight = 20;
	mCapturedTrack->SetHeight(newTrackHeight);
	Refresh(false);
  }
  
  if (event.ButtonUp()) {
	mCapturedTrack = NULL;
	mIsResizing = false;
	MakeParentRedrawScrollbars();
  }
}

void TrackPanel::OnMouseEvent(wxMouseEvent& event)
{
  if (event.ButtonDown()) {
	wxActivateEvent *e = new wxActivateEvent();
	GetParent()->ProcessEvent(*e);      
	delete e;
  }

  if (mIsResizing) {
	HandleResize(event);
	HandleCursor(event);
	return;
  }

  wxRect r;
  int num;
  
  VTrack *t = FindTrack(event.m_x, event.m_y, false, &r, &num);

  if (event.ButtonDown() &&
	  event.m_y >= (r.y + r.height - 10) &&
	  event.m_y <  (r.y + r.height)) {
	HandleResize(event);
	HandleCursor(event);
	return;
  }
  
  if (!mCapturedTrack && event.m_x < GetLabelWidth()) {
	HandleLabelClick(event);
	HandleCursor(event);
	return;
  }

  switch(gAPalette->GetCurrentTool()) {

  case 0: // select
	HandleSelect(event);
	break;
  case 1: // envelope
	HandleEnvelope(event);
	break;
  case 2: // move/slide
	HandleSlide(event);
	break;
  case 3: // zoom
	HandleZoom(event);
	break;
  }

  if (event.Moving() || event.ButtonUp())
	HandleCursor(event);
}

void TrackPanel::DrawRuler(wxDC& dc)
{
  wxRect r;
  
  GetSize(&r.width, &r.height);
  r.x = 0;
  r.y = 0;
  r.height = GetRulerHeight()-1;

  //
  // Draw ruler border
  //
  
  AColor::Medium(&dc, false);  
  dc.DrawRectangle(r);

  r.width--;
  r.height--;
  Bevel(dc, true, r);  

  dc.SetPen(*wxBLACK_PEN);
  dc.DrawLine(r.x, r.y+r.height+1, r.x+r.width+1, r.y+r.height+1);

  //
  // Draw selection
  //

  if (mViewInfo->sel0 < mViewInfo->sel1) {
	double sel0 = mViewInfo->sel0 - mViewInfo->h +
	  GetLabelOffset() / mViewInfo->zoom;
	double sel1 = mViewInfo->sel1 - mViewInfo->h +
	  GetLabelOffset() / mViewInfo->zoom;

	if (sel0 < 0.0)
	  sel0 = 0.0;
	if (sel1 > (r.width / mViewInfo->zoom))
	  sel1 = r.width / mViewInfo->zoom;

	int p0 = int(sel0 * mViewInfo->zoom + 0.5);
	int p1 = int(sel1 * mViewInfo->zoom + 0.5);

	wxBrush selectedBrush;
	selectedBrush.SetColour(148,148,170);
	wxPen selectedPen;
	selectedPen.SetColour(148,148,170);
	dc.SetBrush(selectedBrush);
	dc.SetPen(selectedPen);
	
	wxRect sr;
	sr.x = p0;
	sr.y = 1;
	sr.width = p1-p0-1;
	sr.height = GetRulerHeight()-3;
	dc.DrawRectangle(sr);
  }

  //
  // Draw marks on ruler
  //

  dc.SetPen(*wxBLACK_PEN);

  int fontSize = 10;
  #ifdef __WXMSW__
  fontSize = 8;
  #endif

  wxFont rulerFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
  dc.SetFont(rulerFont);

  int minSpace = 60;  // min pixels between labels

  wxString unitStr;
  double unit = 1.0;
  double base;
  
  while(unit*mViewInfo->zoom < minSpace)
    unit *= 2.0;
  while(unit*mViewInfo->zoom > minSpace*2)
    unit /= 2.0;
	
  if (unit < 0.0005) {
    unitStr = "us"; // microseconds
    base = 0.000001;
  }
  else if (unit < 0.5) {
    unitStr = "ms"; // milliseconds
    base = 0.001;
  }
  else if (unit < 30.0) {
    unitStr = "s"; // seconds
    base = 1.0;  
  }
  else if (unit < 1800.0) {
    unitStr = "m"; // minutes
    base = 60.0;  
  }
  else {
    unitStr = "h"; // hours
    base = 3600.0;
  }
  
  unit = base;
  
  bool hand=true;
	
  while(unit*mViewInfo->zoom < minSpace) {
    unit *= (hand? 5.0 : 2.0);
    hand = !hand;
  }
  while(unit*mViewInfo->zoom > minSpace*(hand? 2.0 : 5.0)) {
    unit /= (hand? 2.0 : 5.0);
    hand = !hand;
  }
  
  unit /= 4;
  
  double pos = mViewInfo->h - GetLabelOffset() / mViewInfo->zoom;
  int unitcount = (int)(pos / unit);

  dc.SetTextForeground(wxColour(0,0,204));
  
  int nextxpos = 0;

  for(int pixel=0; pixel<r.width; pixel++) {

	if (((int)(floor(pos / unit))) > unitcount) {
	  unitcount = (int)(floor(pos/unit));

	  switch((unitcount)%4) {
	  case 0:
		dc.DrawLine(r.x+pixel,r.y+8,r.x+pixel,r.y+r.height);
				
		char str[100];
		sprintf(str,"%.1f%s",unitcount*unit/base, (const char *)unitStr);
		long textWidth, textHeight;
		dc.GetTextExtent(str, &textWidth, &textHeight);
		if (pixel >= nextxpos && pixel+2+textWidth < r.width) {
		  dc.DrawText(str, r.x+pixel+3, r.y+2);
		  
		  nextxpos = pixel + textWidth + 12;
		}
		break;

	  case 1:
	  case 3:
		dc.DrawLine(r.x+pixel,r.y+r.height-4,r.x+pixel,r.y+r.height);
		break;

	  case 2:
		dc.DrawLine(r.x+pixel,r.y+r.height-6,r.x+pixel,r.y+r.height);
		break;
	  }
	}
	pos += 1.0 / mViewInfo->zoom;
  }
}

void TrackPanel::DrawTracks(wxDC& dc)
{
  wxRect r;
  
  GetSize(&r.width, &r.height);
  r.x = 0;
  r.y = -mViewInfo->vpos;

  // Make room for ruler
  r.y += GetRulerHeight();
  r.height -= GetRulerHeight();
  
  VTrack *t;
  int num=0;
  
  t = mTracks->First();
  while(t) {
	r.height = t->GetHeight();

	// Draw label area

    int fontSize = 10;
    #ifdef __WXMSW__
    fontSize = 8;
    #endif

	wxFont labelFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
	dc.SetFont(labelFont);

	wxRect labelRect = r;
	labelRect.width = GetLabelWidth();

	AColor::Medium(&dc, false);
	dc.DrawRectangle(labelRect);

	labelRect.Inflate(-4, -4);
	AColor::Medium(&dc, t->selected);
	dc.DrawRectangle(labelRect);
	Bevel(dc, false, labelRect);

	int top = labelRect.y;
	int height = labelRect.height;

	wxRect titleRect = labelRect;
	if (height > 23) {
	  titleRect.height = 23;
	  titleRect.Inflate(-4, -4);
	  Bevel(dc, false, titleRect);
	  char str[100];
	  sprintf(str,"Track %d", num+1);
	  dc.DrawText(str, titleRect.x + 3, titleRect.y + 2);
	  top += 27;
	  height -= 27;
	}

	if (t->GetKind()==VTrack::Wave) {
	  wxRect rateRect = labelRect;
	  if (height > 23) {
		rateRect.y = top;
		rateRect.height = 23;
		rateRect.Inflate(-4, -4);
		dc.DrawText("Rate:", rateRect.x + 2, rateRect.y + 2);
		rateRect.x += rateRect.width / 2;
		rateRect.width /= 2;
		Bevel(dc, true, rateRect);
		char str[100];
		sprintf(str,"%d",(int)(((WaveTrack *)t)->rate + 0.5));
		dc.DrawText(str, rateRect.x + 3, rateRect.y + 2);
		top += 23;
		height -= 23;
	  }
	}

	wxRect channelRect = labelRect;
	if (channelRect.height > 23) {
	  channelRect.y = top;
	  channelRect.height = 23;
	  channelRect.Inflate(-4, -4);
	  dc.DrawText("Channel:", channelRect.x + 2, channelRect.y + 2);
	  channelRect.x += channelRect.width / 2;
	  channelRect.width /= 2;
	  Bevel(dc, true, channelRect);
	  wxString str;
	  switch(t->channel)
		{
		case VTrack::MonoChannel: str = "Mono"; break;
		case VTrack::LeftChannel: str = "Left"; break;
		case VTrack::RightChannel: str = "Right"; break;
		default: str = "Other"; break;
		}
	  dc.DrawText(str, channelRect.x + 3, channelRect.y + 2);
	  top += 23;
	  height -= 23;
	}

	// Draw track area

	wxRect trackRect = r;
	trackRect.x += GetLabelWidth();
	trackRect.width -= GetLabelWidth();

	AColor::Medium(&dc, false);
	dc.DrawRectangle(trackRect);

	trackRect.Inflate(-4, -4);
	Bevel(dc, false, trackRect);	

	// Don't draw if it's not visible at all (vertically)
	// if (r.y < (visible->y + visible->height)
    // && (r.y + r.height) > visible->y)

	double h = mViewInfo->h;

	bool sel = t->selected;
	
	// Tell VTrack to draw itself
	
	wxRect innerRect = trackRect;
	innerRect.Inflate(-1, -1);

	if (!t->IsCollapsed()) {
	  if (sel)
		t->Draw(dc, innerRect, h,
				mViewInfo->zoom, mViewInfo->sel0, mViewInfo->sel1);
	  else
		t->Draw(dc, innerRect, h,
				mViewInfo->zoom, 0.0, 0.0);
	}
	else {
	  /*
	  dc->SetBrush(backgroundBrush);
	  dc->SetPen(backgroundPen);
	  
	  dc->DrawRectangle(innerRect);
	  */
	}
		
    r.y += r.height;
	num++;
    t = mTracks->Next();
  }

  GetSize(&r.width, &r.height);
  AColor::Medium(&dc, false);
  dc.DrawRectangle(r);
}

VTrack *TrackPanel::FindTrack(int mouseX, int mouseY, bool label,
							 wxRect *trackRect, int *trackNum)
{
  wxRect r;
  if (label) {
	r.x = 0;
	r.width = GetLabelWidth() - 1;
  }
  else {
	int wid, ht;
	r.x = GetLabelWidth() + 5;
	GetSize(&wid, &ht);
	r.width = wid - GetLabelWidth() - 10;
  }
  r.y = -mViewInfo->vpos;
  r.y += GetRulerHeight();

  VTrack *t = mTracks->First();

  int n=1;

  r.y += 5; // Offset top border
  
  while(t) {
	r.height = t->GetHeight();
	
  	if (r.Inside(mouseX, mouseY)) {
      if (trackRect)
        *trackRect = r;
      if (trackNum)
        *trackNum = n;
      return t;
    }

	r.y += r.height;
	n++;
	t = mTracks->Next();
  }

  if (mouseY >= r.y && trackNum)
	*trackNum = n-1;
  
  return NULL;
}

int TrackPanel::GetLabelWidth()
{
  return 110;
}

int TrackPanel::GetLabelOffset()
{
  return 115;
}

int TrackPanel::GetRulerHeight()
{
  return 20;
}

// inline
void TrackPanel::Bevel(wxDC& dc, bool up, wxRect& r)
{
  if (up)
	AColor::Light(&dc, false);
  else
	AColor::Dark(&dc, false);

  dc.DrawLine(r.x, r.y, r.x + r.width, r.y);
  dc.DrawLine(r.x, r.y, r.x, r.y + r.height);

  if (!up)
	AColor::Light(&dc, false);
  else
	AColor::Dark(&dc, false);

  dc.DrawLine(r.x + r.width, r.y, r.x + r.width, r.y + r.height);
  dc.DrawLine(r.x, r.y + r.height, r.x + r.width + 1, r.y + r.height);
}

//
// AudacityTimer method
//

void AudacityTimer::Notify()
{
  parent->OnTimer();
}

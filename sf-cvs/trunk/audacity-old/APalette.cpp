/**********************************************************************

  Audacity: A Digital Audio Editor

  APalette.cpp

  Dominic Mazzoni

  This class manages the miniframe window (aka floating window)
  which contains the tool selection (ibeam, envelope, move, zoom),
  the play/stop buttons, and the volume control.  All of the
  controls in this window were custom-written for Audacity - they
  are not native controls on any platform - however, it is intended
  that the images could be easily replaced to allow "skinning" or
  just customization to match the look and feel of each platform.

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#include <wx/brush.h>
#include <wx/image.h>
#endif

#include "AButton.h"
#include "ASlider.h"
#include "APalette.h"
#include "Project.h"
#include "Play.h"

APalette *gAPalette = NULL;

#ifdef __WXMAC__
#define APALETTE_HEIGHT 55
#endif

#ifdef __WXGTK__
#define APALETTE_HEIGHT 75
#endif

#ifdef __WXMSW__
#define APALETTE_HEIGHT 80
#endif

void InitAPalette(wxFrame *parent)
{
  wxPoint where;

  where.x = 10;
  where.y = 10;

  #ifdef __WXMAC__
  where.y += 20;
  #endif

  gAPalette = new APalette(parent, -1, "Audacity Palette",
						   where);
  gAPalette->Show(TRUE);
}

// APalette

BEGIN_EVENT_TABLE(APalette, wxMiniFrame)
  EVT_CLOSE  (              APalette::OnCloseWindow)
  EVT_PAINT  (              APalette::OnPaint)
  EVT_COMMAND_RANGE (ID_FIRST_TOOL, ID_LAST_TOOL,
                     wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnTool)
  EVT_COMMAND_RANGE (ID_PLAY_BUTTON, ID_PLAY_BUTTON,
                     wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnPlay)
  EVT_COMMAND_RANGE (ID_STOP_BUTTON, ID_STOP_BUTTON,
                     wxEVT_COMMAND_BUTTON_CLICKED, APalette::OnStop)
END_EVENT_TABLE()

APalette::APalette(wxFrame* parent, wxWindowID id, const wxString& title,
				   const wxPoint& pos) :
  wxMiniFrame(parent, id, title, pos, wxSize(300, APALETTE_HEIGHT) )
{
  mTool[0] =
    new AButton(this, ID_IBEAM, wxPoint(0, 0), wxSize(27, 27),
				"IBeamUp", "IBeamOver",
				"IBeamDown", "IBeamUp");
  mTool[1] = 
    new AButton(this, ID_SELECT, wxPoint(28, 0), wxSize(27, 27),
				"SelectUp", "SelectOver",
				"SelectDown", "SelectUp");
  mTool[2] = 
    new AButton(this, ID_MOVE, wxPoint(0, 28), wxSize(27, 27),
				"MoveUp", "MoveOver",
				"MoveDown", "MoveUp");
  mTool[3] = 
    new AButton(this, ID_ZOOM, wxPoint(28, 28), wxSize(27, 27),
				"ZoomUp", "ZoomOver",
				"ZoomDown", "ZoomUp");

  mPlay =
	new AButton(this, ID_PLAY_BUTTON, wxPoint(64, 4), wxSize(48, 48),
				"PlayUp", "PlayOver",
				"PlayDown", "PlayDisabled");
  mStop =
	new AButton(this, ID_STOP_BUTTON, wxPoint(114, 4), wxSize(48, 48),
				"StopUp", "StopOver",
				"StopDown", "StopDisabled");
  
  mVolume =
	new ASlider(this, 0, wxPoint(172, 14), wxSize(100, 28),
				"Slider", "SliderThumb", 100);

  mVolume->Set(80);

  mCurrentTool = 0;
  mTool[0]->PushDown();

  mBackgroundBrush.SetColour(wxColour(204, 204, 204));
  mBackgroundPen.SetColour(wxColour(204, 204, 204));
}

APalette::~APalette()
{
  for(int i=0; i<4; i++)
	delete mTool[i];
  delete mPlay;
  delete mStop;
  delete mVolume;
}

int APalette::GetCurrentTool()
{
  return mCurrentTool;
}

void APalette::SetPlay(bool down)
{
  if (down)
	mPlay->PushDown();
  else
	mPlay->PopUp();
}

void APalette::SetStop(bool down)
{
  if (down)
	mStop->PushDown();
  else
	mStop->PopUp();
}

void APalette::OnPlay()
{
  AudacityProject *p = GetActiveProject();
  if (p) {
	TrackList *t = p->GetTracks();
	double t0 = p->GetSel0();
	double t1 = p->GetSel1();
	if (t1 == t0)
	  t1 = t->GetMaxLen();
	gSoundPlayer->Begin(p, t, t0, t1);
  }
}

void APalette::OnStop()
{
  gSoundPlayer->Stop();
  SetStop(false);
}

float APalette::GetSoundVol()
{
  return mVolume->Get() / 100.0;
}

void APalette::OnTool(wxCommandEvent& evt)
{
  int prev = mCurrentTool;

  mCurrentTool = evt.GetId() - ID_FIRST_TOOL;

  for(int i=0; i<4; i++)
    if (i == mCurrentTool)
      mTool[i]->PushDown();
    else
      mTool[i]->PopUp();

  if (mCurrentTool==1 || prev==1)
	RedrawAllProjects();
}

void APalette::OnPaint(wxPaintEvent &evt)
{
  wxPaintDC dc(this);

  int width, height;
  GetSize(&width, &height);
  
  dc.SetBrush(mBackgroundBrush);
  dc.SetPen(mBackgroundPen);
  dc.DrawRectangle(0, 0, width, height);

  dc.SetPen(*wxBLACK_PEN);

  dc.DrawLine(27, 0, 27, 55);
  dc.DrawLine(55, 0, 55, 55);
  dc.DrawLine(0, 27, 55, 27);
}

void APalette::OnCloseWindow(wxCloseEvent& WXUNUSED(event))
{
  this->Show(FALSE);
}

/**********************************************************************

  Audacity: A Digital Audio Editor

  APalette.h

  Dominic Mazzoni

  This class manages the miniframe window (aka floating window)
  which contains the tool selection (ibeam, envelope, move, zoom),
  the play/stop buttons, and the volume control.  All of the
  controls in this window were custom-written for Audacity - they
  are not native controls on any platform - however, it is intended
  that the images could be easily replaced to allow "skinning" or
  just customization to match the look and feel of each platform.

**********************************************************************/

#ifndef __AUDACITY_PALETTE__
#define __AUDACITY_PALETTE__

#include <wx/brush.h>
#include <wx/gdicmn.h>
#include <wx/pen.h>
#include <wx/minifram.h>

class AButton;
class ASlider;
class APalette;

extern APalette *gAPalette;

void InitAPalette(wxFrame *parent);

// Define a new mini frame
class APalette: public wxMiniFrame
{
public:
  APalette(wxFrame *parent, wxWindowID id,
		   const wxString& title,
		   const wxPoint& pos);

  virtual ~APalette();

  int GetCurrentTool();
  
  void OnCloseWindow(wxCloseEvent& event);
  void OnPaint(wxPaintEvent& event);
  void OnTool(wxCommandEvent& evt);

  void OnPlay();
  void OnStop();

  void SetPlay(bool down);
  void SetStop(bool down);

  float GetSoundVol();
  
private:
  AButton *mTool[4];
  
  AButton *mPlay;
  AButton *mStop;

  ASlider *mVolume;

  int mCurrentTool;
  
  wxBrush mBackgroundBrush;
  wxPen mBackgroundPen;
    
DECLARE_EVENT_TABLE()
};

#define ID_FIRST_TOOL      500
#define ID_IBEAM           500
#define ID_SELECT          501
#define ID_MOVE            502
#define ID_ZOOM            503
#define ID_LAST_TOOL       503
#define ID_PLAY_BUTTON     504
#define ID_STOP_BUTTON     505

#endif

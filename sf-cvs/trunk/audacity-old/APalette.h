/**********************************************************************

  Audacity: A Digital Audio Editor

  APalette.h

  Dominic Mazzoni

  This class manages the miniframe window (aka floating window)
  which contains the tool selection (ibeam, envelope, move, zoom),
  the play/stop/record buttons, and the volume control.  All of the
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

#include "Project.h"

class AButton;
class ASlider;
class APalette;
class APaletteFrame;

extern APaletteFrame *gAPaletteFrame;
extern bool gWindowedPalette;

void InitAPaletteFrame(wxWindow *parent);
int GetAPaletteHeight();

void ShowWindowedPalette(wxPoint *where = NULL);
void HideWindowedPalette();

class APalette: public wxWindow
{
public:
  APalette(wxWindow *parent, wxWindowID id,
		   const wxPoint& pos,
		   const wxSize& size);

  virtual ~APalette();

  int GetCurrentTool();
  
  void OnPaint(wxPaintEvent& event);
  void OnTool(wxCommandEvent& evt);

  void OnPlay();
  void OnStop();
  void OnRecord();

  void SetPlay(bool down);
  void SetStop(bool down);
  void SetRecord(bool down);

  float GetSoundVol();
  
private:
  AButton *mTool[4];
  
  AButton *mPlay;
  AButton *mStop;
  AButton *mRecord;

  ASlider *mVolume;

  int mCurrentTool;
  
  wxBrush mBackgroundBrush;
  wxPen mBackgroundPen;
  
  wxBitmap *mBackgroundBitmap;
  int mBackgroundWidth;
  int mBackgroundHeight;
  wxBitmap *mDivBitmap;
  wxBitmap *mMuteBitmap;
  wxBitmap *mLoudBitmap;
    
DECLARE_EVENT_TABLE()
};

class APaletteFrame: public wxMiniFrame
{
public:
  APaletteFrame(wxWindow *parent, wxWindowID id,
				const wxString& title,
				const wxPoint& pos);

  void OnCloseWindow(wxCloseEvent& event);

  APalette     mPalette;

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
#define ID_RECORD_BUTTON   506

#endif

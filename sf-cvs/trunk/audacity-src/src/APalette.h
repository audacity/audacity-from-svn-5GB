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
#include <wx/pen.h>
#include <wx/minifram.h>

class AButton;
class ASlider;
class APalette;
class APaletteFrame;

class wxImage;
class wxSize;
class wxPoint;

extern APaletteFrame *gAPaletteFrame;
extern bool gWindowedPalette;

void InitAPaletteFrame(wxWindow * parent);
int GetAPaletteHeight();

void ShowWindowedPalette(wxPoint * where = NULL);
void HideWindowedPalette();

enum {
   selectTool,
   envelopeTool,
   slideTool,
   zoomTool,
   numTools
};

class APalette:public wxWindow {
 public:
   APalette(wxWindow * parent, wxWindowID id,
            const wxPoint & pos, const wxSize & size);

   virtual ~ APalette();

   int GetCurrentTool();

   void OnPaint(wxPaintEvent & event);
   void OnKeyEvent(wxKeyEvent & event);
   void OnTool(wxCommandEvent & evt);

   void OnRewind();
   void OnPlay();
   void OnStop();
   void OnRecord();
   void OnFF();

   void SetPlay(bool down);
   void SetStop(bool down);
   void SetRecord(bool down);

   float GetSoundVol();

 private:
   AButton *MakeTool(const char **tool, const char **alpha,
                     wxWindowID id, int left, int top);
   AButton *MakeButton(wxImage *up, wxImage *down, wxImage *hilite,
                       const char **foreground, const char **alpha,
                       wxWindowID id, int left);
   void MakeButtons();
   
 private:

   AButton * mTool[4];

   AButton *mRewind;
   AButton *mPlay;
   AButton *mStop;
   AButton *mRecord;
   AButton *mFF;

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

class APaletteFrame:public wxMiniFrame {
 public:
   APaletteFrame(wxWindow * parent, wxWindowID id,
                 const wxString & title, const wxPoint & pos);

   void OnCloseWindow(wxCloseEvent & event);

   APalette mPalette;

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
#define ID_FF_BUTTON       507
#define ID_REW_BUTTON      508

#endif

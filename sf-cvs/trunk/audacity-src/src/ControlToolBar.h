/**********************************************************************

  Audacity: A Digital Audio Editor

  
  ControlToolbar.h
 
  Dominic Mazzoni
  Shane T. Mueller
 
  This class, which is a child of Toolbar, creates the
  window containing the tool selection (ibeam, envelope,
  move, zoom), the rewind/play/stop/record/ff buttons, and
  the volume control. The window can be embedded within a
  normal project window, or within a ToolbarFrame that is
  managed by a global ToolBarStub called
  gControlToolBarStub.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

**********************************************************************/

#ifndef __AUDACITY_CONTROL_TOOLBAR__
#define __AUDACITY_CONTROL_TOOLBAR__

#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/minifram.h>
#include <wx/object.h>

#include "ToolBar.h"


class AButton;
class ASlider;
class ControlToolBar;
class ToolBar;
class ToolBarFrame;


class wxImage;
class wxSize;
class wxPoint;

enum {
   selectTool,
   envelopeTool,
   slideTool,
   zoomTool,
   numTools
};



class ControlToolBar:public ToolBar {

   DECLARE_DYNAMIC_CLASS(ControlToolBar)

 public:
   ControlToolBar() {
   };
   ControlToolBar(wxWindow * parent, wxWindowID id,
                  const wxPoint & pos, const wxSize & size);
   ControlToolBar(wxWindow * parent);
   virtual ~ ControlToolBar();

   int GetCurrentTool();

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnKeyEvent(wxKeyEvent & event);
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

   virtual void EnableDisableButtons(int sumOfFlags);

 private:

   void InitializeControlToolBar();

   wxImage *MakeToolImage(wxImage * tool, wxImage * mask, int style);
   AButton *MakeTool(const char **tool, const char **alpha,
                     wxWindowID id, int left, int top);
   AButton *MakeButton(wxImage * up, wxImage * down, wxImage * hilite,
                       char const **foreground,
                       char const **disabled,
                       char const **alpha, int ID, int left);
   void MakeButtons();

   AButton *mTool[4];

   AButton *mRewind;
   AButton *mPlay;
   AButton *mStop;
   AButton *mRecord;
   AButton *mFF;

   ASlider *mVolume;
   int mCurrentTool;


   wxBitmap *mBackgroundBitmap;
   int mBackgroundWidth;
   int mBackgroundHeight;
   wxBitmap *mDivBitmap;
   wxBitmap *mMuteBitmap;
   wxBitmap *mLoudBitmap;

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

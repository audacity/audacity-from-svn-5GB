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
   drawTool,
   numTools
};


class ControlToolBar:public ToolBar {
 public:
   ControlToolBar() {};
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
   void OnPause();

   //These allow buttons to be controlled externally:
   void SetPlay(bool down);
   void SetStop(bool down);
   void SetRecord(bool down);

   float GetSoundVol();

   virtual void EnableDisableButtons();

 private:

   void InitializeControlToolBar();

   wxImage *MakeToolImage(wxImage * tool, wxImage * mask, int style);
   AButton *MakeTool(const char **tool, const char **alpha,
                     wxWindowID id, int left, int top);
   AButton *MakeButton(char const **foreground, char const **disabled,
                       char const **alpha, int id, bool processdownevents);
   void MakeButtons();
   int mButtonPos;

   AButton *mTool[5];

   AButton *mRewind;
   AButton *mPlay;
   AButton *mRecord;
   AButton *mPause;
   AButton *mStop;
   AButton *mFF;
   

   ASlider *mVolume;
   int mCurrentTool;

   wxBitmap *mDivBitmap;
   wxBitmap *mMuteBitmap;
   wxBitmap *mLoudBitmap;

   wxImage *upPattern;
   wxImage *downPattern;
   wxImage *hilitePattern;

   bool mPaused;         //Determines which state we are in (paused or not paused)
   //                      This maybe doesn't belong in the toolbar.

   DECLARE_EVENT_TABLE()
};

#endif

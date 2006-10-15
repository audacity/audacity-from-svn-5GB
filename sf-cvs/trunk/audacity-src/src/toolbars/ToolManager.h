/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolManager.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLMANAGER__
#define __AUDACITY_TOOLMANAGER__

#include <wx/defs.h>
#include <wx/frame.h>
#include <wx/timer.h>

#include "ToolDock.h"
#include "ToolBar.h"

class wxArrayPtrVoid;
class wxBitmap;
class wxCommandEvent;
class wxMouseEvent;
class wxPaintEvent;
class wxPoint;
class wxRect;
class wxRegion;
class wxSize;
class wxTimer;
class wxTimerEvent;
class wxWindow;

////////////////////////////////////////////////////////////
/// class ToolManager
////////////////////////////////////////////////////////////

class ToolManager:public wxEvtHandler
{

 public:

   ToolManager( wxWindow *parent );
   ~ToolManager();

   void LayoutToolBars();

   bool IsDocked( int type );

   bool IsVisible( int type );

   void ShowHide( int type );

   ToolBar *GetToolBar( int type ) const;

   ToolDock *GetTopDock();
   ToolDock *GetBotDock();

   void UnDock( ToolBar *bar );

 private:

   wxWindow *Float( ToolBar *t, wxPoint & pos );

   void OnTimer( wxTimerEvent & event );
   void OnMouse( wxMouseEvent & event );
   void OnGrabber( GrabberEvent & event );
   void OnPaint( wxPaintEvent & event );
   void OnCreate( wxWindowCreateEvent & event );
  
   void ReadConfig();
   void WriteConfig();
   void StartDrag( const wxPoint & pos );
   void Updated();

   wxWindow *mParent;

   ToolDock *mDragDock;
   ToolBar *mDragBar;
   int mDragBefore;

   wxPoint mLastPos;
   wxRect mBarPos;
   wxFrame *mIndicator;
   wxRegion *mLeft;
   wxRegion *mDown;
   wxRegion *mCurrent;

   wxTimer mWatchdog;

#if defined(__WXMAC__)
   bool mTransition;
#endif

   wxArrayPtrVoid mDockedBars;
   ToolDock *mTopDock;
   ToolDock *mBotDock;

   ToolBar *mBars[ ToolBarCount ];

   wxWindow *mDragWindow;

 public:

   DECLARE_CLASS( ToolManager );
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 5a2a21f8-6c9e-45a4-8718-c26cad5cfe65


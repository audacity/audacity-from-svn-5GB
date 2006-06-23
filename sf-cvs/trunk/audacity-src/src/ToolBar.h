/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.h

  Dominic Mazzoni
  Shane T. Mueller

**********************************************************************/

#ifndef __AUDACITY_TOOLBAR__
#define __AUDACITY_TOOLBAR__

#include <wx/defs.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/minifram.h>
#include <wx/object.h>
#include <wx/sizer.h>
#include <wx/dcclient.h>
#include <wx/window.h>
#include <wx/panel.h>
#include <wx/dynarray.h>
#include <wx/timer.h>

class wxImage;
class wxSize;
class wxPoint;

class ControlToolBar;
class EditToolBar;
class MeterToolBar;
class MixerToolBar;
class ToolsToolBar;
class TranscriptionToolBar;

class AButton;
class Grabber;

enum
{
   NoBarID = -1,
   ControlBarID,
   ToolsBarID,
   MeterBarID,
   MixerBarID,
   EditBarID,
   TranscriptionBarID,
   ToolBarCount
};
  
////////////////////////////////////////////////////////////
/// Custom events
////////////////////////////////////////////////////////////

DECLARE_EVENT_TYPE(EVT_TOOLBAR_UPDATED, -1);
DECLARE_EVENT_TYPE(EVT_TOOLBAR_BEGINDRAG, -1);

////////////////////////////////////////////////////////////
/// class ToolBar
////////////////////////////////////////////////////////////

#include "Theme.h"  // to get the teBmps enum.

class ToolBar:
   public wxPanel
{
public:
   ToolBar();
   virtual ~ToolBar();
   virtual void ReCreateButtons();
   wxString GetTitle();
   wxString GetLabel();
   int GetType();
   bool IsResizeable();
   bool IsVisible();
   bool IsDocked();
   void SetDocked( bool dock );
   void ShowMarker( bool state );
   virtual void EnableDisableButtons() = 0;

protected:
   void InitToolBar( wxWindow *parent,
                     int type,
                     const wxString &title,
                     const wxString &label,
                     bool resizable = false );
#if 0
   virtual AButton * MakeButton(wxImage * up,
                                wxImage * down,
                                wxImage * hilite,
                                const char **foreground,
                                const char **disabledfg,
                                const char **alpha,
                                wxWindowID id,
                                wxPoint placement, 
                                bool processdownevents,
                                wxSize size,
                                int xoff,
                                int yoff);
#endif

   AButton * MakeButton(teBmps eUp,
                              teBmps eDown,
                              teBmps eHilite,
                              teBmps eStandard,
                              teBmps eDisabled,
                              wxWindowID id,
                              wxPoint placement,
                              bool processdownevents, 
                              wxSize size);

   void SetButton(bool down, AButton* button);

   void MakeMacRecoloredImage(teBmps eBmpOut, teBmps eBmpIn );
   void MakeRecoloredImage( teBmps eBmpOut, teBmps eBmpIn );
   void MakeButtonBackgroundsLarge();
   void MakeButtonBackgroundsSmall();

   wxBoxSizer *GetSizer();

   void Add( wxWindow *window,
             int proportion = 0,
             int flag = wxALIGN_TOP, //wxALIGN_CENTER_VERTICAL,
             int border = 0,
             wxObject* userData = NULL );

   void Add( wxSizer *sizer,
             int proportion = 0,
             int flag = 0,
             int border = 0,
             wxObject* userData = NULL );

   void Add( int width,
             int height,
             int proportion = 0,
             int flag = 0,
             int border = 0,
             wxObject* userData = NULL );

   void AddSpacer( int size = 14 );
   void AddStretchSpacer( int prop = 1 );

   void Detach( wxWindow *window );
   void Detach( wxSizer *sizer );

   void Updated();
   void Capture();

   virtual void Populate() = 0;
   virtual void Repaint( wxDC *dc ) = 0;

   void OnMouseEvent( wxMouseEvent & event );
   void OnSize( wxSizeEvent & event );
   void OnErase( wxEraseEvent & event );
   void OnPaint( wxPaintEvent & event );

   wxBitmap *mBackgroundBitmap;
   int mBackgroundWidth;
   int mBackgroundHeight;

   bool mDocked;
   bool mVisible;
   bool mResizeable;

   DECLARE_EVENT_TABLE()

private:
   void Init( wxWindow *parent, int type, const wxString &title, const wxString &label );

   wxWindow *mParent;
   Grabber *mGrabber;
   wxBoxSizer *mVSizer;
   wxBoxSizer *mHSizer;

   wxString mTitle;
   wxString mLabel;
   int mType;
};

////////////////////////////////////////////////////////////
/// class ToolBarDock
////////////////////////////////////////////////////////////

class ToolBarDock:
   public wxPanel
{

public:
   ToolBarDock( wxWindow * Parent );
   ~ToolBarDock();

   void LayoutToolBars();

   bool IsDocked( int type );

   bool IsVisible( int type );

   void ShowHide( int type );

   ToolBar *GetToolBar( int type );

   ControlToolBar *GetControlToolBar();
   EditToolBar *GetEditToolBar();
   MeterToolBar *GetMeterToolBar();
   MixerToolBar *GetMixerToolBar();
   ToolsToolBar *GetToolsToolBar();
   TranscriptionToolBar *GetTranscriptionToolBar();

public:
   DECLARE_EVENT_TABLE()

protected:
   void OnErase( wxEraseEvent & event );
   void OnSize( wxSizeEvent & event );
   void OnTimer( wxTimerEvent & event );
   void OnMouse( wxMouseEvent & event );
   void OnBeginDrag( wxCommandEvent & event );
   void OnToolBarUpdate( wxCommandEvent & event );
   void OnPaint( wxPaintEvent & event );

private:
   void ReadConfig();
   void WriteConfig();

   int FlowLayout( int cnt,
                   wxRect boxen[],
                   wxRect ideal[],
                   int i,
                   int x,
                   int y,
                   int width,
                   int height );
   void SetDocked( int id, wxPoint & position );
   void Dock( ToolBar *t, int before );
   wxWindow *Float( ToolBar *t, wxPoint & pos );
   void Updated();

   int mTotalToolBarHeight;
   wxWindow *mParent;
   wxString mTitle;

   ToolBar *mOver;
   wxWindow *mDragParent;
   wxWindow *mDrag;
   ToolBar *mDragBar;
   wxPoint mLastPos;
   wxTimer mWatchdog;

#if defined(__WXMAC__)
   bool mTransition;
#endif

   wxArrayPtrVoid mDockedBars;
   ToolBar *mBars[ ToolBarCount ];
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


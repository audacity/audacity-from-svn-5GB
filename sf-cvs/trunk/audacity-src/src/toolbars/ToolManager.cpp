/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolManager.cpp
  
  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

  See ToolManager.h for details.

*******************************************************************//**

\file ToolManager.cpp

  Implements ToolManager

*//*******************************************************************//**

\class ToolManager
\brief Manages the ToolDocks and handles the dragging, floating, and
  docking of ToolBars.

*//**********************************************************************/

#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/frame.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/region.h>
#include <wx/settings.h>
#include <wx/sysopt.h>
#include <wx/timer.h>
#include <wx/window.h>
#endif  /*  */

#include <wx/minifram.h>

#include "ToolManager.h"
#include "ControlToolBar.h"
#include "DeviceToolBar.h"
#include "EditToolBar.h"
#include "MeterToolBar.h"
#include "MixerToolBar.h"
#include "SelectionBar.h"
#include "ToolsToolBar.h"
#include "TranscriptionToolBar.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../widgets/AButton.h"
#include "../widgets/Grabber.h"

IMPLEMENT_CLASS( ToolManager, wxFrame );

////////////////////////////////////////////////////////////
/// Methods for ToolManager
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( ToolManager, wxFrame )
   EVT_TIMER( wxID_ANY, ToolManager::OnTimer )
   EVT_MOUSE_EVENTS( ToolManager::OnMouse )
   EVT_GRABBER( wxID_ANY, ToolManager::OnGrabber )
END_EVENT_TABLE()  

//
// Constructor
//
ToolManager::ToolManager( wxWindow *parent )
: wxFrame( parent,
           wxID_ANY,
           wxEmptyString,
           wxDefaultPosition,
           wxDefaultSize,
           wxFRAME_NO_TASKBAR |
           wxCLIP_CHILDREN | 
           wxSIMPLE_BORDER |
           wxFRAME_FLOAT_ON_PARENT )
{
   wxPoint pt[ 3 ];

#if defined(__WXMAC__)
   // Save original transition
   mTransition = wxSystemOptions::GetOptionInt(wxMAC_WINDOW_PLAIN_TRANSITION);
#endif

   // Initialize everything
   mParent = parent;
   mLastPos.x = mBarPos.x = -1;
   mLastPos.y = mBarPos.y = -1;
   mDragDock = NULL;
   mDragBar = NULL;
   mWatchdog.SetOwner( this, wxID_ANY );

#if !defined(__WXMSW__)
   // Mac and GTK need a little border
   SetBackgroundColour( *wxBLACK );
   ClearBackground();
#endif

   // We're a little bashful
   Hide();

   // Create the down arrow
   pt[ 0 ].x = 0;
   pt[ 0 ].y = 0;
   pt[ 1 ].x = 9;
   pt[ 1 ].y = 9;
   pt[ 2 ].x = 18;
   pt[ 2 ].y = 0;

   // Create the shaped region
   mDown = new wxRegion( 3, &pt[0] );

   // Create the down arrow
   pt[ 0 ].x = 9;
   pt[ 0 ].y = 0;
   pt[ 1 ].x = 0;
   pt[ 1 ].y = 9;
   pt[ 2 ].x = 9;
   pt[ 2 ].y = 18;

   // Create the shaped region
   mLeft = new wxRegion( 3, &pt[0] );

   // Create the indicator frame
   mIndicator = new wxFrame( NULL,
                             wxID_ANY,
                             wxEmptyString,
                             wxDefaultPosition,
                             wxSize( 32, 32 ),
                             wxFRAME_SHAPED |
                             wxSIMPLE_BORDER | 
                             wxFRAME_NO_TASKBAR |
                             wxSTAY_ON_TOP );

   // It's a little shy too
   mIndicator->Hide();

   // We handle the painting for it
   mIndicator->Connect(wxEVT_PAINT, wxPaintEventHandler(ToolManager::OnPaint), NULL, this);

#if defined(__WXGTK__)
   // Under GTK we must set the shape at creation since it's not fully defined yet
   mIndicator->Connect(wxEVT_CREATE, wxWindowCreateEventHandler(ToolManager::OnCreate), NULL, this);
#endif

   // Create the top and bottom docks
   mTopDock = new ToolDock( this, parent, TopDockID );
   mBotDock = new ToolDock( this, parent, BotDockID );

   // Create all of the toolbars
   mBars[ ToolsBarID ]         = new ToolsToolBar();
   mBars[ ControlBarID ]       = new ControlToolBar();
   mBars[ MeterBarID ]         = new MeterToolBar();
   mBars[ EditBarID ]          = new EditToolBar();
   mBars[ MixerBarID ]         = new MixerToolBar();
   mBars[ TranscriptionBarID ] = new TranscriptionToolBar();
   mBars[ SelectionBarID ]     = new SelectionBar();
   mBars[ DeviceBarID ]        = new DeviceToolBar();

   // Process the toolbar config settings
   ReadConfig();
} 

//
// Destructer
//
ToolManager::~ToolManager()
{
   // Delete the indicator frame
#if defined(__WXGTK__)
   // Under GTK we must set the shape at creation since it's not fully defined yet
   mIndicator->Disconnect(wxEVT_CREATE, wxWindowCreateEventHandler(ToolManager::OnCreate), NULL, this);
#endif

   mIndicator->Disconnect(wxEVT_PAINT, wxPaintEventHandler(ToolManager::OnPaint), NULL, this);
   delete mIndicator;

   // Delete the indicator regions
   delete mLeft;
   delete mDown;

   // Save the toolbar states
   WriteConfig();
}

//
// Read the toolbar states
//
void ToolManager::ReadConfig()
{
   wxString oldpath = gPrefs->GetPath();
   wxArrayInt unordered[ DockCount ];
   int order[ DockCount ][ ToolBarCount ];
   bool show[ ToolBarCount ];
   int x, y;
   int dock, ord, ndx;

   // Invalidate all order entries
   for( dock = 0; dock < DockCount; dock++ )
   {
      for( ord = 0; ord < ToolBarCount; ord++ )
      {
         order[ dock ][ ord ] = NoBarID;
      }
   }

   // Change to the bar root
   gPrefs->SetPath( wxT("/GUI/ToolBars") );

   // Load and apply settings for each bar
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      // Change to the bar subkey
      gPrefs->SetPath( mBars[ ndx ]->GetLabel() );

      // Read in all the settings
      gPrefs->Read( wxT("Dock"), &dock, ndx == SelectionBarID ? BotDockID : TopDockID );
      gPrefs->Read( wxT("Order"), &ord, NoBarID );
      gPrefs->Read( wxT("Show"), &show[ ndx ], ndx == DeviceBarID ? false : true );
      gPrefs->Read( wxT("X"), &x, -1 );
      gPrefs->Read( wxT("Y"), &y, -1 );

      // Docked or floating?
      if( dock )
      {
         // Default to top dock if the ID isn't valid
         if( dock < NoDockID || dock > DockCount ) {
            dock = TopDockID;
         }

         // Create the bar with the correct parent
         if( dock == TopDockID )
         {
            mBars[ ndx ]->Create( mTopDock );
         }
         else
         {
            mBars[ ndx ]->Create( mBotDock );
         }

         // Is order within range and unoccupied?
         if( ( ord >= 0 ) &&
             ( ord < ToolBarCount ) &&
             ( order[ dock - 1 ][ ord ] == NoBarID ) )
         {
            // Insert at ordered location
            order[ dock - 1 ][ ord ] = ndx;
         }
         else
         {
            // These must go at the end
            unordered[ dock - 1 ].Add( ndx );
         }
      }
      else
      {
         // Create the bar (with the top dock being temporary parent)
         mBars[ ndx ]->Create( mTopDock );

         // Set window position (validate these somehow????)
         wxPoint pos( x, y );

         // Set the bar afloat and show/hide it
         Float( mBars[ ndx ], pos )->Show( show[ ndx ] );
      }

      // Change back to the bar root
      gPrefs->SetPath( wxT("..") );
   }

   // Add all toolbars to their target dock
   for( dock = 0; dock < DockCount; dock++ )
   {
      ToolDock *d = ( dock + 1 == TopDockID ? mTopDock : mBotDock );

      // Add all ordered toolbars
      for( ord = 0; ord < ToolBarCount; ord++ )
      {
         ndx = order[ dock ][ ord ];

         // Bypass empty slots
         if( ndx != NoBarID )
         {
            ToolBar *t = mBars[ ndx ];

            // Dock it
            d->Dock( t );

            // Hide the bar 
            if( !show[ t->GetId() ] )
            {
               d->ShowHide( t->GetId() );
            }
         }
      }

      // Add all unordered toolbars
      for( ord = 0; ord < (int) unordered[ dock ].GetCount(); ord++ )
      {
         ToolBar *t = mBars[ unordered[ dock ][ ord ] ];

         // Dock it
         d->Dock( t );

         // Hide the bar 
         if( !show[ t->GetId() ] )
         {
            d->ShowHide( t->GetId() );
         }
      }
   }

   // Restore original config path
   gPrefs->SetPath( oldpath );
}

//
// Save the toolbar states
//
void ToolManager::WriteConfig()
{
   if( !gPrefs )
   {
      return;
   }

   wxString oldpath = gPrefs->GetPath();
   int ndx;

   // Change to the bar root
   gPrefs->SetPath( wxT("/GUI/ToolBars") );

   // Save state of each bar
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      // Change to the bar subkey
      gPrefs->SetPath( mBars[ ndx ]->GetLabel() );

      // Search both docks for toolbar order
      int to = mTopDock->GetOrder( mBars[ ndx ] );
      int bo = mBotDock->GetOrder( mBars[ ndx ] );

      // Save
      gPrefs->Write( wxT("Dock"), to ? TopDockID : bo ? BotDockID : NoDockID );
      gPrefs->Write( wxT("Order"), to + bo );
      gPrefs->Write( wxT("Show"), IsVisible( ndx ) );
      if( mBars[ ndx ]->IsDocked() )
      {
         gPrefs->Write( wxT("X"), -1 );
         gPrefs->Write( wxT("Y"), -1 );
      }
      else
      {
         wxPoint p = mBars[ ndx ]->GetParent()->GetPosition();
         gPrefs->Write( wxT("X"), p.x );
         gPrefs->Write( wxT("Y"), p.y );
      }

      // Change back to the bar root
      gPrefs->SetPath( wxT("..") );
   }

   // Restore original config path
   gPrefs->SetPath( oldpath );
}

//
// Return a pointer to the specified toolbar
//
ToolBar *ToolManager::GetToolBar( int type ) const
{
   return mBars[ type ];
}

//
// Return a pointer to the top dock
//
ToolDock *ToolManager::GetTopDock()
{
   return mTopDock;
}

//
// Return a pointer to the bottom dock
//
ToolDock *ToolManager::GetBotDock()
{
   return mBotDock;
}

//
// Toggle the docked/floating state of the specified toolbar
//
wxWindow *ToolManager::Float( ToolBar *t, wxPoint & pos )
{
   wxWindow *parent;
   wxSize sz;

#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

   // Create the floater window
   int flags = wxCAPTION | wxFRAME_TOOL_WINDOW | wxFRAME_FLOAT_ON_PARENT;
   if( t->IsResizeable() )
   {
      sz = t->GetSize();
      parent = new wxFrame( this,
                            wxID_ANY,
                            t->GetTitle(),
                            pos,
                            wxDefaultSize,
                            flags | wxRESIZE_BORDER,
                            t->GetLabel() );
   }
   else
   {
      sz = t->GetSize();
      parent = new wxMiniFrame( this,
                                wxID_ANY,
                                t->GetTitle(),
                                pos,
                                wxDefaultSize,
                                flags,
                                t->GetLabel() );
   }

   // Move the toolbar from the toolbar dock to the floater window
   t->Reparent( parent );

   // Tell the toolbar about the change
   t->SetDocked( false );

   // Resize the floater client size to the toolbars minimum size
   parent->SetClientSize( sz );

   // Make sure resizable floaters don't get any smaller than initial size
   if( t->IsResizeable() )
   {
      // Adjust bar minimum size to account for frame decorations
      wxSize msz = t->GetMinSize();
      wxSize psz = parent->GetSize();
      msz.x += ( psz.x - sz.x );
      msz.y += ( psz.y - sz.y );

      // Set the minimum size
      parent->SetSizeHints( msz );
   }

   // Show it to the world
   parent->Show();

   // Hide the ferry
   Hide();

#if defined(__WXMAC__)
   // Reinstate original transition
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, mTransition );
#endif

   return parent;
}

//
// Queues an EVT_TOOLBAR_UPDATED command event to notify any
// interest parties of an updated toolbar or dock layout
//
void ToolManager::Updated()
{
   // Queue an update event
   wxCommandEvent e( EVT_TOOLBAR_UPDATED, GetId() );
   mParent->GetEventHandler()->AddPendingEvent( e );
}

//
// Return docked state of specified toolbar
//
bool ToolManager::IsDocked( int type )
{
   return mBars[ type ]->IsDocked();
}

//
// Returns the visibility of the specified toolbar
//
bool ToolManager::IsVisible( int type )
{
   ToolBar *t = mBars[ type ];

   // If toolbar is floating
   if( !t->IsDocked() )
   {
      // Must return state of floater window
      return t->GetParent()->IsShown();
   }

   // Return state of docked toolbar
   return t->IsShown();
}

//
// Toggles the visible/hidden state of a toolbar
//
void ToolManager::ShowHide( int type )
{
   ToolBar *t = mBars[ type ];

   // Handle docked and floaters differently
   if( t->IsDocked() )
   {
      t->GetDock()->ShowHide( type );
   }
   else
   {
      t->GetParent()->Show( !t->GetParent()->IsShown() );
   }
}

//
// Transition a toolbar from docked to dragging
//
void ToolManager::UnDock( ToolBar *bar )
{
#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

   // Remember which bar we're dragging
   mDragBar = bar;

   // Inform toolbar of change
   mDragBar->SetDocked( false );

   // Continue the drag 
   StartDrag( wxGetMousePosition() );
}

//
// Common handler for transitioning a bar to dragging
//
void ToolManager::StartDrag( const wxPoint & pos )
{
   wxSize sz;

   // Get the current size and force the height to the minimum
   sz = mDragBar->GetSize();
   sz.y = mDragBar->GetMinSize().y;

   // Mac and GTK require a border around the dragging bar
#if defined(__WXMSW__)
   int extra = 0;
#else
   int extra = 1;
#endif

   // Make the ferry the right size for the bar
   SetSize( pos.x,
            pos.y,
            sz.GetWidth() + ( extra * 2 ),
            sz.GetHeight() + ( extra * 2 ) );

   // Transfer the bar to the ferry
   mDragBar->Reparent( this );

   // And anchor it down
   mDragBar->SetSize( extra,
                      extra,
                      sz.GetWidth(),
                      sz.GetHeight() );

#if defined(__WXGTK__)
//   // Move the ferry to the right spot (under the mouse)
//   Move( mDragBar->ScreenToClient( pos ) );
#endif

   // Make sure the ferry is visible
   Show();

   // wxGTK requires a yield before proper dragging can occur
   wxYieldIfNeeded();

   // Notify parent of change
   Updated();

   // We want all mouse events from this point on
   CaptureMouse();

   // Setup a watchdog in case we lose the capture
   mWatchdog.Start( 50, wxTIMER_CONTINUOUS );
}

//
// Ask both docks to (re)layout their bars
//
void ToolManager::LayoutToolBars()
{
   // Update the layout
   mTopDock->LayoutToolBars();
   mBotDock->LayoutToolBars();
}

//
// Handle toolbar dragging
//
void ToolManager::OnMouse( wxMouseEvent & event )
{
   // Go ahead and set the event to propagate
   event.Skip();

   // Can't do anything if we're not dragging
   if( !mDragBar )
   {
      return;
   }

   // Retrieve the mouse position
   wxPoint pos = ClientToScreen( event.GetPosition() );

   // Button was released...finish the drag
   if( !event.LeftIsDown())
   {
      // Stop the watchdog
      mWatchdog.Stop();

      // Release the mouse if we have it (we should, but sometimes...)
      if( HasCapture() )
      {
         ReleaseMouse();
      }

      // Hide the indicator
      mIndicator->Hide();

      // Transition the bar to a dock or a floater
      if( mDragDock )
      {
         // Trip over...everyone ashore that's going ashore...
         mDragDock->Dock( mDragBar, mDragBefore );
      }
      else
      {
         // Set the bar afloat
         Float( mDragBar, pos );
      }

      // Hide the ferry
      Hide();
   
      // Done dragging
      mDragDock = NULL;
      mDragBar = NULL;
      mLastPos.x = mBarPos.x = -1;
      mLastPos.y = mBarPos.y = -1;

#if defined(__WXMAC__)
      // Reinstate original transition
      wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, mTransition );
#endif
   }
   else if( event.Dragging() && pos != mLastPos )
   {
      // Make toolbar follow the mouse
      Move(pos);

      // Remember to prevent excessive movement
      mLastPos = pos;

      // Calc the top dock hittest rectangle
      wxRect tr = mTopDock->GetRect();
      tr.SetBottom( tr.GetBottom() + 10 );
      tr.SetPosition( mTopDock->GetParent()->ClientToScreen( tr.GetPosition() ) );

      // Calc the bottom dock hittest rectangle
      wxRect br = mBotDock->GetRect();
      br.SetTop( br.GetTop() - 10 );
      br.SetBottom( br.GetBottom() + 20 );
      br.SetPosition( mBotDock->GetParent()->ClientToScreen( br.GetPosition() ) );

      // Is mouse pointer within either dock?
      ToolDock *dock = NULL;
      if( tr.Inside( pos ) )
      {
         dock = mTopDock;
      }
      else if( br.Inside( pos ) )
      {
         dock = mBotDock;
      }

      // Looks like we have a winner...
      if( dock )
      {
         wxPoint p;
         wxRect r;

         // Calculate where the bar would be placed
         mDragBefore = dock->PositionBar( mDragBar, pos, r );

         // If different than the last time, the indicator must be moved
         if( r != mBarPos )
         {
            wxRect dr = dock->GetRect();

            // Hide the indicator before changing the shape
            mIndicator->Hide();

            // Decide which direction the arrow should point
            if( r.GetBottom() >= dr.GetHeight() )
            {
               p.x = dr.GetLeft() + ( dr.GetWidth() / 2 );
               p.y = dr.GetBottom() - mDown->GetBox().GetHeight();
               mCurrent = mDown;
            }
            else
            {
               p.x = dr.GetLeft() + r.GetLeft();
               p.y = dr.GetTop() + r.GetTop() +
                     ( ( r.GetHeight() - mLeft->GetBox().GetHeight() ) / 2 );
               mCurrent = mLeft;
            }

            // Move it into position and put it on stage
            mIndicator->SetShape( *mCurrent );
            mIndicator->Move( dock->GetParent()->ClientToScreen( p ) );
            mIndicator->Show();

            // Remember for next go round
            mBarPos = r;
         }
      }
      else
      {
         // Hide the indicator if it's still shown
         if( mBarPos.x != -1 )
         {
            // Hide any 
            mIndicator->Hide();
            mBarPos.x = -1;
            mBarPos.y = -1;
         }
      }

      // Remember to which dock the drag bar belongs.
      mDragDock = dock;
   }
}

//
// Protect against losing mouse capture
//
void ToolManager::OnTimer( wxTimerEvent & event )
{
   // If we've lost capture, simulate a button up event
   if( !HasCapture() )
   {
      wxMouseEvent e( wxEVT_LEFT_UP );
      wxPoint p( wxGetMousePosition() );
      e.m_x = p.x;
      e.m_y = p.y;
      OnMouse( e );
   }
}

//
// Handle Indicator paint events
//
// Really only needed for the Mac since SetBackgroundColour()
// doesn't seem to work with shaped frames.
//
void ToolManager::OnPaint( wxPaintEvent & event )
{
   wxPaintDC dc( mIndicator );

   dc.BeginDrawing();
   dc.SetBackground( *wxBLUE_BRUSH );
   dc.Clear();
   dc.EndDrawing();
}

#if defined(__WXGTK__)
//
// Handle Indicator creation event
//
// Without this, the initial Indicator window will be a solid blue square
// until the next time it changes.
//
void ToolManager::OnCreate( wxWindowCreateEvent & event )
{
   mIndicator->SetShape( *mCurrent );
}
#endif

//
// Transition a toolbar from float to dragging
//
void ToolManager::OnGrabber( GrabberEvent & event )
{
   wxWindow *parent;

#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

   // No need to propagate any further
   event.Skip( true );

   // Remember which bar we're dragging
   mDragBar = mBars[ event.GetId() ];

   // Get the parent
   parent = mDragBar->GetParent();

   // And make it disappear
   parent->Hide();

   // Start the dragging
   StartDrag( event.GetPosition() );

   // Kill the parent
   parent->Destroy();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 2f4ec75c-bdb7-4889-96d1-5d00abc41027


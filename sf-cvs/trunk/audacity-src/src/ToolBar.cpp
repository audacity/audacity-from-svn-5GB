/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.cpp
  
  Dominic Mazzoni
  Shane T. Mueller

  See ToolBar.h for details.

**********************************************************************/  

#include "ToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif  /*  */

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/brush.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/button.h>
#include <wx/window.h>
#include <wx/list.h>
#endif  /*  */

#include <wx/image.h>

#include "Audacity.h"
#include "AColor.h"
#include "Prefs.h"
#include "Project.h"
#include "ImageManipulation.h"
#include "widgets/AButton.h"

#include "ControlToolBar.h"
#include "ToolsToolBar.h"
#include "EditToolBar.h"
#include "MeterToolBar.h"
#include "MixerToolBar.h"
#include "TranscriptionToolBar.h"

// Custom events

DEFINE_EVENT_TYPE(EVT_TOOLBAR_UPDATED)
DEFINE_EVENT_TYPE(EVT_TOOLBAR_BEGINDRAG)
DEFINE_EVENT_TYPE(EVT_TOOLBAR_ENDDRAG)

////////////////////////////////////////////////////////////
/// Grabber Class
////////////////////////////////////////////////////////////

class GrabberPosition
{
public:
   GrabberPosition() {}
   virtual ~GrabberPosition() {};

   wxPoint position;
   wxPoint offset;
};

class Grabber:
   public wxWindow
{

public:
   Grabber( wxWindow *parent, wxWindowID id );
   virtual ~Grabber();
   
protected:
   void OnLeftDown( wxMouseEvent & event );
   void OnLeftUp( wxMouseEvent & event );
   void OnMotion( wxMouseEvent & event );
   void OnEnter( wxMouseEvent & event );
   void OnLeave( wxMouseEvent & event );
   void OnPaint( wxPaintEvent & event );

   DECLARE_EVENT_TABLE();

private:
   void Grabber::DrawGrabber( wxDC & dc );
   void SendEvent( wxEventType type, int x, int y );

   bool mUp;
   bool mOver;
   wxPoint mHotspot;
   wxGenericDragImage *mDrag;
};

////////////////////////////////////////////////////////////
/// Methods for Grabber
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( Grabber, wxWindow )
   EVT_ENTER_WINDOW( Grabber::OnEnter )
   EVT_LEAVE_WINDOW( Grabber::OnLeave )
   EVT_LEFT_DOWN( Grabber::OnLeftDown )
   EVT_LEFT_UP( Grabber::OnLeftUp )
   EVT_MOTION( Grabber::OnMotion )
   EVT_PAINT( Grabber::OnPaint )
END_EVENT_TABLE()  

// Specifies how wide the grabber will be
#define grabberWidth 10

//
// Contructor
//
Grabber::Grabber( wxWindow * parent, wxWindowID id ):
   wxWindow( parent,
             id,
             wxDefaultPosition,
             wxSize( grabberWidth, 27 ),
             wxFULL_REPAINT_ON_RESIZE )
{
   mUp = true;
   mOver = false;
   mDrag = NULL;
}

//
// Destructor
//
Grabber::~Grabber()
{
}

//
// Queue a drag event
//
void Grabber::SendEvent( wxEventType type, int x, int y )
{
   wxCommandEvent e( type, GetId() );

   // Convert coordinates to screen space
   ClientToScreen( mHotspot );
   ClientToScreen( &x, &y );

   // Create and populate the position info
   GrabberPosition *p = new GrabberPosition();
   p->position.x = x;
   p->position.y = y;
   p->offset     = mHotspot;
   e.SetClientData( (void *) p );

   // Queue the event
   GetParent()->GetEventHandler()->AddPendingEvent( e );
}

//
// Draw the grabber
//
void Grabber::DrawGrabber( wxDC & dc )
{
   wxRect r = GetRect();
   int y, left, right, top, bottom;

   // Paint the background
   AColor::Medium( &dc, mOver );
   dc.DrawRectangle( r );

#ifndef __WXMAC__

   // Add a box
   r.width -= 1;
   r.height -= 1;
   AColor::Bevel( dc, mUp, r );
   r.width += 1;
   r.height += 1;

#endif

   // Calculate the bump rectangle
   r.Deflate( 3, 3 );
   if( ( r.GetHeight() % 4 ) < 2 )
   {
      r.Offset( 0, 1 );
   }

   // Cache
   left = r.GetLeft();
   right = r.GetRight() + 1;  //+1 for DrawLine()'s lack of not plotting last pixel
   top = r.GetTop();
   bottom = r.GetBottom();

   // Draw the raised bumps
   if( mUp )
      AColor::Light( &dc, false );
   else
      AColor::Dark( &dc, false );

   for( y = top; y < bottom; y += 4 )
   {
      dc.DrawLine( left, y, right, y );
   }

   // Draw the pushed bumps
   if( mUp )
      AColor::Dark( &dc, false );
   else
      AColor::Light( &dc, false );

   for( y = top + 1; y <= bottom; y += 4 )
   {
      dc.DrawLine( left, y, right, y );
   }
}

//
// Handle left button down events
//
void Grabber::OnLeftDown( wxMouseEvent & event )
{
   // Button should be drawn pushed
   mUp = false;

   // Only do something if not already dragging
   // (Protects against missed mouse events)
   if( !mDrag )
   {
      // Redraw the button
      wxClientDC cdc( this );
      DrawGrabber( cdc );

      // Hotspot is relative to our top/left corner
      mHotspot = wxPoint( event.GetX(), event.GetY() ) - GetPosition();
      
      // Get the containing toolbar dimensions
      wxSize sz = GetParent()->GetMinSize();
      int width = sz.GetWidth();
      int height = sz.GetHeight();

      // Make the new bitmap a bit bigger
      wxBitmap bitmap( width + 2, height + 2 );

      // Create the DC
      wxMemoryDC memDC;
      memDC.SelectObject( bitmap );

      // Draw a black box on perimeter
      memDC.SetPen( *wxBLACK_PEN );
      memDC.DrawRectangle( 0, 0, width + 2, height + 2 );

      // Copy an image of the toolbar into the box
      wxClientDC pdc( GetParent() );
      memDC.Blit( 1, 1, width, height, &pdc, 1, 0 );

      // Start dragging
      mDrag = new wxGenericDragImage( bitmap );
      mDrag->BeginDrag( mHotspot, this , true );
      mDrag->Move( mHotspot );
      mDrag->Show();

      // Notify parent
      SendEvent( EVT_TOOLBAR_BEGINDRAG, event.GetX(), event.GetY() );
   }
}

//
// Handle left button down events
//
void Grabber::OnLeftUp( wxMouseEvent & event )
{
   mUp = true;

   // Only process if currently dragging
   // (Protects against missed mouse events)
   if( mDrag )
   {
      // Stop dragging and cleanup
      mDrag->Hide();
      mDrag->EndDrag();
      delete mDrag;
      mDrag = NULL;

      // Notify parent
      SendEvent( EVT_TOOLBAR_ENDDRAG, event.GetX(), event.GetY() );
   }
}

//
// Handle mouse movement events
//
void Grabber::OnMotion( wxMouseEvent & event )
{
   // Only use it if we're dragging
   if( event.Dragging() && mDrag )
   {
      // Move the image to the new location
      mDrag->Move( wxPoint( event.GetX() - 1, event.GetY() - 1 ) );
   }
}

//
// Handle mouse enter events
//
void Grabber::OnEnter( wxMouseEvent & event )
{
   mOver = true;
   Refresh( false );
}

//
// Handle mouse leave events
//
void Grabber::OnLeave( wxMouseEvent & event )
{
   mOver = false;
   Refresh( false );
}

//
// Handle the paint events
//
void Grabber::OnPaint( wxPaintEvent & event )
{
   wxPaintDC dc( this );

   // Redraw the grabber
   DrawGrabber( dc );
}

////////////////////////////////////////////////////////////
/// Methods for ToolBarDock
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( ToolBarDock, wxPanel )
   EVT_PAINT( ToolBarDock::OnPaint )
   EVT_COMMAND( wxID_ANY, EVT_TOOLBAR_UPDATED,   ToolBarDock::OnToolBarUpdate )
   EVT_COMMAND( wxID_ANY, EVT_TOOLBAR_BEGINDRAG, ToolBarDock::OnBeginDrag )
   EVT_COMMAND( wxID_ANY, EVT_TOOLBAR_ENDDRAG,   ToolBarDock::OnEndDrag )
END_EVENT_TABLE()  

//
// Amount of space between toolbars
//
#define toolbarGap 1

//
// Contructor
//
ToolBarDock::ToolBarDock( wxWindow *parent ):
   wxPanel( parent, wxID_ANY, wxPoint( 0, 0 ), parent->GetSize() )
{
   SetName( wxT( "ToolBarDock" ) );

   // Use for testing spacing
   // SetOwnBackgroundColour( wxColour( 255, 0, 0 ) );

   // Create all of the toolbars
   mBars[ ToolsBarID ]         = new ToolsToolBar( this );
   mBars[ ControlBarID ]       = new ControlToolBar( this );
   mBars[ MeterBarID ]         = new MeterToolBar( this );
   mBars[ EditBarID ]          = new EditToolBar( this );
   mBars[ MixerBarID ]         = new MixerToolBar( this );
   mBars[ TranscriptionBarID ] = new TranscriptionToolBar( this );

   // Process the toolbar config settings
   ReadConfig();
} 

//
// Read the toolbar states
//
void ToolBarDock::ReadConfig()
{
   wxString oldpath = gPrefs->GetPath();
   bool dock, show;
   int order[ ToolBarCount ];
   int x, y;
   int ord, ndx;

   // Invalidate all order entries
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      order[ ndx ] = NoBarID;
   }

   // Change to the bar root
   gPrefs->SetPath( wxT("/GUI/ToolBars") );

   // Load and apply settings for each bar
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      // Change to the bar subkey
      gPrefs->SetPath( mBars[ ndx ]->GetLabel() );

      // Read in all the settings
      gPrefs->Read( wxT("Dock"), &dock, true );
      gPrefs->Read( wxT("Show"), &show, true );
      gPrefs->Read( wxT("X"), &x, -1 );
      gPrefs->Read( wxT("Y"), &y, -1 );
      gPrefs->Read( wxT("Order"), &ord, NoBarID );

      // Docked or floating?
      if( dock )
      {
         // Only want bars that will be shown
         if( show )
         {
            // Is order within range?
            if( ( ord >= 0 ) && ( ord < ToolBarCount ) )
            {
               // Remember it
               order[ ord ] = ndx;
            }
            else
            {
               // Add it (will wind after the rest)
               mDockedBars.Add( mBars[ ndx ] );
            }
         }

         // Show/hide the bar
         mBars[ ndx ]->Show( show );
      }
      else
      {
         // Set window position (validate these somehow????)
         wxPoint pos( x, y );

         // Must temporarily add the bar as Float() will blindly
         // remove it and an assertion would be triggered.
         //
         // LLL: I'd rather do it this way than add a check in Float()
         //      since that could hide processing errors by skipping the
         //      removal when it should have been able to remove it.
         mDockedBars.Add( mBars[ ndx ] );

         // Set the bar afloat and show/hide it
         Float( mBars[ ndx ], pos )->Show( show );
      }

      // Change back to the bar root
      gPrefs->SetPath( wxT("..") );
   }

   // Add all of the docked and visible bars in sorted order
   for( ndx = ToolBarCount - 1; ndx >= 0; ndx-- )
   {
      // Only process valid entries
      if( order[ ndx ] != NoBarID )
      {
         // Add the bar to the end of the array
         mDockedBars.Insert( mBars[ order[ ndx ] ], 0 );
      }
   }

   // Restore original config path
   gPrefs->SetPath( oldpath );
}

//
// Save the toolbar states
//
void ToolBarDock::WriteConfig()
{
   wxString oldpath = gPrefs->GetPath();
   int ndx;

   // Change to the bar root
   gPrefs->SetPath( wxT("/GUI/ToolBars") );

   // Save state of each bar
   for( ndx = 0; ndx < ToolBarCount; ndx++ )
   {
      // Change to the bar subkey
      gPrefs->SetPath( mBars[ ndx ]->GetLabel() );

      // Save
      gPrefs->Write( wxT("Dock"), mBars[ ndx ]->IsDocked() );
      gPrefs->Write( wxT("Order"), mDockedBars.Index( mBars[ ndx ] ) );
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
      gPrefs->SetPath( wxT(".." ) );
   }

   // Restore original config path
   gPrefs->SetPath( oldpath );
}

//
// Destructer
//
ToolBarDock::~ToolBarDock()
{
   // Save the toolbar states
   WriteConfig();
}

//
// Return a pointer to the specified toolbar
//
ToolBar *ToolBarDock::GetToolBar( int type )
{
   return mBars[ type ];
}

//
// Return a pointer to the Control toolbar
//
ControlToolBar *ToolBarDock::GetControlToolBar()
{
   return (ControlToolBar *) mBars[ ControlBarID ];
}

//
// Return a pointer to the Edit toolbar
//
EditToolBar *ToolBarDock::GetEditToolBar()
{
   return (EditToolBar *) mBars[ EditBarID ];
}

//
// Return a pointer to the Meter toolbar
//
MeterToolBar *ToolBarDock::GetMeterToolBar()
{
   return (MeterToolBar *) mBars[ MeterBarID ];
}

//
// Return a pointer to the Mixer toolbar
//
MixerToolBar *ToolBarDock::GetMixerToolBar()
{
   return (MixerToolBar *) mBars[ MixerBarID ];
}

//
// Return a pointer to the Tools toolbar
//
ToolsToolBar *ToolBarDock::GetToolsToolBar()
{
   return (ToolsToolBar *) mBars[ ToolsBarID ];
}

//
// Return a pointer to the Transcription toolbar
//
TranscriptionToolBar *ToolBarDock::GetTranscriptionToolBar()
{
   return (TranscriptionToolBar *) mBars[ TranscriptionBarID ];
}

//
// Layout the toolbars
//
void ToolBarDock::LayoutToolBars()
{
   // Get the number of docked toolbars and take a quick exit
   // if we don't have any
   int cnt = mDockedBars.GetCount();
   if( cnt == 0 )
   {
      // Set the size of the dock window
      SetMinSize( wxSize( -1, toolbarGap ) );
      return;
   }

   wxRect stack[ ToolBarCount + 1 ];
   wxPoint cpos, lpos;
   ToolBar *lt = NULL;
   int ndx, stkcnt = 0;

   // Get size of our parent since we haven't been sized yet
   int width, height;
   GetParent()->GetClientSize( &width, &height );
   width -= toolbarGap;
   height -= toolbarGap;

   // Set initial stack entry to maximum size
   stack[ 0 ].SetX( toolbarGap );
   stack[ 0 ].SetY( toolbarGap );
   stack[ 0 ].SetWidth( width );
   stack[ 0 ].SetHeight( height );

   // Process all docked and visible toolbars
   for( ndx = 0; ndx < cnt; ndx++ )
   {
      // Cache toolbar pointer
      ToolBar *ct = (ToolBar *)mDockedBars[ ndx ];

      // Get and cache the toolbar sizes
      wxSize sz = ct->GetMinSize();
      int tw = sz.GetWidth() + toolbarGap;
      int th = sz.GetHeight() + toolbarGap;

      // Will this one fit in remaining horizontal space?
      if( ( tw > stack[ stkcnt ].GetWidth() ) ||
          ( th > stack[ stkcnt ].GetHeight() ) ) 
      {
         // Destack entries until one is found in which this bar
         // will fit or until we run out of stacked entries
         while( stkcnt > 0 )
         {
            stkcnt--;

            // Get out if it will fit
            if( ( tw <= stack[ stkcnt ].GetWidth() ) &&
                ( th <= stack[ stkcnt ].GetHeight() ) )
            {
               break;
            }
         }
      }

      // The current stack entry position is where the bar
      // will be placed.
      cpos = stack[ stkcnt ].GetPosition();

      // We'll be using at least a portion of this stack entry, so
      // adjust the location and size.  It is possible that these
      // will become zero if this entry and the toolbar have the
      // same height.  This is what we want as it will be destacked
      // in the next iteration.
      stack[ stkcnt ].SetY(      stack[ stkcnt ].GetY()      + th );
      stack[ stkcnt ].SetHeight( stack[ stkcnt ].GetHeight() - th );

      // Calc the next possible horizontal location.
		int x = cpos.x + tw;

      // Add a new stack entry
      stkcnt++;
      stack[ stkcnt ].SetX( x );
		stack[ stkcnt ].SetY( cpos.y );
		stack[ stkcnt ].SetWidth( width - x );
		stack[ stkcnt ].SetHeight( th );

      // Position the previous toolbar
      if( ndx > 0 )
      {
         if( lpos.y != cpos.y )
         {
            // Place/stretch the last toolbar in the row
            lt->SetSize( lpos.x, lpos.y, width - lpos.x, lt->GetMinSize().GetHeight() );
         }
         else
         {
            // Place the unstretched toolbar
            lt->SetSize( lpos.x, lpos.y, cpos.x - lpos.x - toolbarGap, lt->GetMinSize().GetHeight() );
         }
      }

      // Place and stretch the final toolbar
      if( ndx == cnt - 1 )
      {
         ct->SetSize( cpos.x, cpos.y, width - cpos.x, sz.GetHeight() );
      }

      // Remember for next iteration
      lt = ct;
      lpos = cpos;
   }

   // Set the final size of the dock window
   SetMinSize( wxSize( -1, stack[ 0 ].GetY() ) );

   // Clean things up
   Refresh( false );
}

//
// Toggle the docked/floating state of the specified toolbar
//
void ToolBarDock::SetDocked( wxWindowID id, wxPoint & pos, wxPoint & off )
{
   ToolBar *t = mBars[ id ];

   // Handle a floating bar
   if( !t->IsDocked() )
   {
      // Convert location to client space
      wxPoint p = ScreenToClient( pos );

      // Search for a docked bar over which this bar was dropped
      int ndx, cnt = mDockedBars.GetCount();
      for( ndx = 0; ndx < cnt; ndx++ )
      {
         // Does the location fall within this bar?
         wxRect r = ( (ToolBar *) mDockedBars[ ndx ] )->GetRect();
         if( r.Inside( p ) )
         {
            break;
         }
      }

      // Move it to the dock
      Dock( t, ndx );
   }
   else
   {
      wxWindow *parent;

      parent = Float( t, pos );

      // LLL:  If this hackery is not desired, just remove it and
      //       the top/left corner of the floater window will be
      //       positioned at the location where the mouse button
      //       was released.

      // We need to get the difference between the screen position
      // of the toolbar and the screen position of the floater window.
      // This will provide us with the amount of space used by any
      // decorations the floater window is using.
      //
      // We use that info and the hotspot info passed to us to move
      // the floater window so that the mouse pointer will be
      // directly over the spot it was on in the drag image.

      // Show and refresh the window before calculating the offsets
      // otherwise GTK will return zeros for the coordinates.
      parent->Show();
      parent->Refresh();
#if 0
      // LLL: Since this didn't work under wxGTK 2.6.2, I've just
      //      disabled it.

      // Refresh the positions of both of the windows and convert
      // the toolbar coordinates to screen space.
      wxPoint ppos = parent->GetPosition();
      wxPoint tpos = parent->ClientToScreen( t->GetPosition() );

      // GTK does not return consistent window positions.  (At least
      // I couldn't get it to.)  Sometimes it would return the actual
      // screen position of the toolbar and other times it would only
      // return the offset of the toolbar within the parent window
      // in screen space.  We can use either, but we have to include
      // this check to deal with both cases.
      //
      // Unfortunately, this doesn't work for resizeable windows. :-(
      if( tpos.x > ppos.x && tpos.y > ppos.y )
      {
         tpos.x -= ppos.x;
         tpos.y -= ppos.y;
      }

      // Finally, after all the hackage, move the floater to
      // it's final location.
      parent->Move( ppos.x - tpos.x - off.x, ppos.y - tpos.y - off.y );
#endif
   }

   // Notify parent of changes
   Updated();
}

//
// Toggle the docked/floating state of the specified toolbar
//
void ToolBarDock::Dock( ToolBar *t, int before )
{
   wxWindow *parent;

   // Insert bar into the docked bars array
   mDockedBars.Insert( t, before );

   // Get the floater window
   parent = t->GetParent();

   // Move the toolbar from the floater window to the toolbar dock
   t->Reparent( this );

   // Tell the toolbar about the change
   t->SetDocked( true );

   // Close/destroy the floater window
   parent->Close( true );
}

//
// Toggle the docked/floating state of the specified toolbar
//
wxWindow *ToolBarDock::Float( ToolBar *t, wxPoint & pos )
{
   wxWindow *parent;

   // Remove the bar from the docked bars array
   mDockedBars.Remove( t );

   // Create the floater window
   int flags = wxCAPTION | wxFRAME_TOOL_WINDOW | wxFRAME_FLOAT_ON_PARENT;
   if( t->IsResizeable() )
   {
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
   parent->SetClientSize( t->GetMinSize() );

   // Make sure resizable floaters don't get any smaller than initial size
   if( t->IsResizeable() )
   {
      parent->SetSizeHints( parent->GetSize() );
   }

   return parent;
}

//
// Return docked state of specified toolbar
//
bool ToolBarDock::IsDocked( int type )
{
   return mBars[ type ]->IsDocked();
}

//
// Returns the visibility of the specified toolbar
//
bool ToolBarDock::IsVisible( int type )
{
   ToolBar *t = mBars[ type ];

   // Is toolbar floating?
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
void ToolBarDock::ShowHide( int type )
{
   ToolBar *t = mBars[ type ];

   // Handle a docked toolbar
   if( t->IsDocked() )
   {
      // Maintain the docked array
      if( t->IsShown() )
      {
         mDockedBars.Remove( t );
      }
      else
      {
         mDockedBars.Add( t );
      }

      // Make it (dis)appear
      t->Show( !t->IsShown() );

      // Update the layout
      LayoutToolBars();

      // Notify parent of change
      Updated();
   }
   else
   {
      // Make a floater (dis)appear
      t->GetParent()->Show( !t->GetParent()->IsShown() );
   }
}

//
// Queues an EVT_TOOLBAR_UPDATED command event to notify any
// interest parties of an updated toolbar or dock layout
//
void ToolBarDock::Updated()
{
   // Queue an update event
   wxCommandEvent e( EVT_TOOLBAR_UPDATED, GetId() );
   GetParent()->GetEventHandler()->AddPendingEvent( e );
}

//
// Handle EVT_TOOLBAR_BEGINDRAG events
//
void ToolBarDock::OnBeginDrag( wxCommandEvent &event )
{
   // The idea here is to prevent other drawing while dragging
   // since it "corrupts" our drag image.
   //
   // Under Windows and OSX Freeze()/Thaw() be used, but
   // GTK doesn't appear to support them so don't do anything
   // for now.
   //
   // Freeze();

   // We don't use the position info, but we must free it
   delete (GrabberPosition *) event.GetClientData();

   // No need to propagate any further
   event.Skip( true );
}

//
// Handle EVT_TOOLBAR_ENDDRAG events
//
void ToolBarDock::OnEndDrag( wxCommandEvent &event )
{
   GrabberPosition *p = (GrabberPosition *) event.GetClientData();

   // Go do the docking/floating
   SetDocked( event.GetId(), p->position, p->offset );

   // See comments in OnBeginDraw()
   // Thaw();

   // Free the position info
   delete p;

   // No need to propagate any further
   event.Skip( true );
}

// 
// Handle toolbar updates
//
void ToolBarDock::OnToolBarUpdate( wxCommandEvent &event )
{
   ToolBar *t = mBars[ event.GetId() ];

   // Resize floater window to exactly contain toolbar
   if( !t->IsDocked() )
   {
      t->GetParent()->SetClientSize( t->GetMinSize() );
   }

   // Update the layout
   LayoutToolBars();

   // Allow it to propagate to our parent
   event.Skip();
}

//
// Repaint toolbar gap lines
//
void ToolBarDock::OnPaint( wxPaintEvent &event )
{
   wxPaintDC dc( this );

   // Set the gap color
   AColor::Dark( &dc, false );

   // Draw the initial horizontal and vertical gaps
   wxSize sz = GetClientSize();
   dc.DrawLine( 0, 0, sz.GetWidth(), 0 );
   dc.DrawLine( 0, 0, 0, sz.GetHeight() );

   // Draw the gap between each bar
   int ndx, cnt = mDockedBars.GetCount();
   for( ndx = 0; ndx < cnt; ndx++ )
   {
      wxRect r = ( (ToolBar *)mDockedBars[ ndx ] )->GetRect();
      dc.DrawLine( r.GetLeft(),
                   r.GetBottom() + 1,
                   sz.GetWidth(),
                   r.GetBottom() + 1 );

      dc.DrawLine( r.GetRight() + 1,
                   r.GetTop(),
                   r.GetRight() + 1,
                   r.GetBottom() + 1 );
   }
}

////////////////////////////////////////////////////////////
/// Methods for ToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( ToolBar, wxPanel )
   EVT_PAINT( ToolBar::OnPaint )
END_EVENT_TABLE()  

//
// Constructor
//
ToolBar::ToolBar():
   wxPanel()
{
   // Initialize everything
   mParent = NULL;
   mDocked = true;
   mResizeable = false;
   mType = NoBarID;
   mTitle.Clear();
   mLabel.Clear();

   mBackgroundBitmap = NULL;
   mBackgroundHeight = 0;
   mBackgroundWidth = 0;
}

//
// Destructor
//
ToolBar::~ToolBar()
{
}

//
// Returns the toolbar title
//
wxString ToolBar::GetTitle()
{
   return mTitle;
}

//
// Returns the toolbar label
//
wxString ToolBar::GetLabel()
{
   return mLabel;
}

//
// Returns the toolbar type
//
int ToolBar::GetType()
{
   return mType;
}

//
// Returns whether the toolbar is resizeable or not
//
bool ToolBar::IsResizeable()
{
   return mResizeable;
}

//
// Returns the dock state of the toolbar
//
bool ToolBar::IsDocked()
{
   return mDocked;
}

//
// Initialize the toolbar
//
void ToolBar::InitToolBar( wxWindow *parent,
                           int type,
                           const wxString &title,
                           const wxString &label,
                           bool resizeable )
{
   // Save parameters
   mParent = parent;
   mType = type;
   mTitle = title;
   mLabel = label;
   mResizeable = resizeable;

   // Create the window and label it
   Create( mParent,
           type,
           wxDefaultPosition,
           wxDefaultSize,
           wxNO_BORDER | wxTAB_TRAVERSAL | wxCLIP_CHILDREN | wxFULL_REPAINT_ON_RESIZE,
           mTitle );
   SetLabel( mLabel );

   // Use a box sizer for laying out controls
   mHSizer = new wxBoxSizer( wxHORIZONTAL );

   // Create the grabber and add it to the sizer
   mGrabber = new Grabber( this, mType );
   mHSizer->Add( mGrabber, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP | wxRIGHT, 1 );

   // Default to docked
   SetDocked( true );

   // Go add all the rest of the gadgets
   Populate();

   // Layout and show
   SetAutoLayout( true );
   SetSizerAndFit( mHSizer );
   Layout();
   Show();
}

//
// Toggle the docked/floating state
//
void ToolBar::SetDocked( bool dock )
{
   // Remember it
   mDocked = dock;

   // Change the tooltip of the the grabber
   if( dock )
   {
#if wxUSE_TOOLTIPS
      mGrabber->SetToolTip( _("Float Toolbar") );
#endif
   }
   else
   {
#if wxUSE_TOOLTIPS
      mGrabber->SetToolTip( _("Dock Toolbar") );
#endif
   }
}

//
// Notify parent of changes
//
void ToolBar::Updated()
{
   wxCommandEvent e( EVT_TOOLBAR_UPDATED, GetId() );
   mParent->GetEventHandler()->AddPendingEvent( e );
}

//
// Returns a pointer to the main sizer
//
wxBoxSizer *ToolBar::GetSizer()
{
   return mHSizer;
}

//
// Add a window to the main sizer
//
void ToolBar::Add( wxWindow *window,
                   int proportion,
                   int flag,
                   int border,
                   wxObject* userData )
{
   mHSizer->Add( window,
                 proportion,
                 flag,
                 border,
                 userData );
}

//
// Add a child sizer to the main sizer
//
void ToolBar::Add( wxSizer *sizer,
                   int proportion,
                   int flag,
                   int border,
                   wxObject* userData )
{
   mHSizer->Add( sizer,
                 proportion,
                 flag,
                 border,
                 userData );
}

//
// Add some space to the main sizer
//
void ToolBar::Add( int width,
                   int height,
                   int proportion,
                   int flag,
                   int border,
                   wxObject* userData )
{
   mHSizer->Add( width,
                 height,
                 proportion,
                 flag,
                 border,
                 userData );
}

//
// Adds a spacer to the main sizer
//
void ToolBar::AddSpacer( int size )
{
   mHSizer->AddSpacer( size );
}

//
// Adds a strechable spacer to the main sizer
//
void ToolBar::AddStretchSpacer( int prop )
{
   mHSizer->AddStretchSpacer( prop );
}

//
// Detach a window from the main sizer
//
void ToolBar::Detach( wxWindow *window )
{
   mHSizer->Detach( window );
}

//
// Detach a child sizer from the main sizer
//
void ToolBar::Detach( wxSizer *sizer )
{
   mHSizer->Detach( sizer );
}

///This is a generic function that will make a button, given
///generic button images.
///  Parameters:
///
/// up:           An image of the blank button in its "up" state
/// down:         An image of the blank button in its "down" state
/// hilite        An image of the blank button with the hilite halo
/// foreground:   A color bitmap of the icon on the button when its
///                 enabled--Can be a solid field.
/// disabledfg:   A color bitmap of the icon when its disabled--can be a solid field.
/// alpha:        A greyscale mask that determines how much of the
///                 foreground shows through
/// id            Button ID
/// placement     Location on the toolbar
/// processdownevents      boolean that determine whether the button will process events
///                        if it is in the down position (and pop up when clicked in the down position)
/// xadjust       x-offset to adjust the icon pixmaps from, wrt a centered icon on background image
/// yadjust       y-offset to adjust the icon pixmaps from, wrt a centered icon on background image
AButton * ToolBar::MakeButton(wxImage * up,
                              wxImage * down,
                              wxImage * hilite,
                              const char **foreground,
                              const char **disabledfg,
                              const char **alpha, wxWindowID id,
                              wxPoint placement,
                              bool processdownevents, wxSize size,
                              int xadjust, int yadjust) 
{
   wxImage * color 			= new wxImage(wxBitmap(foreground).ConvertToImage());
   wxImage * color_disabled = new wxImage(wxBitmap(disabledfg).ConvertToImage());
   wxImage * mask 			= new wxImage(wxBitmap(alpha).ConvertToImage());

   //Some images need to be centered on the button.  We calculate the centered values here, and then
   //adjust by xoff/yoff, for maximum control.
 
   int xoff = (size.GetWidth() - color->GetWidth())/2;
   int yoff = (size.GetHeight() - color->GetHeight())/2;
   
   wxImage * up2        = OverlayImage(up, color, mask, xoff, yoff);
   wxImage * hilite2    = OverlayImage(hilite, color, mask, xoff, yoff);
   wxImage * down2      = OverlayImage(down, color, mask, xoff + 1, yoff + 1);
   wxImage * disable2   = OverlayImage(up, color_disabled, mask, xoff, yoff);

   AButton * button =
      new AButton(this, id, placement, size, up2, hilite2, down2,
            disable2, processdownevents);

   delete color;
   delete color_disabled;
   delete mask;
   delete up2;
   delete down2;
   delete hilite2;
   delete disable2;

   return button;
}

//
// This changes the state a button (from up to down or vice versa)
//
void ToolBar::SetButton( bool down, AButton * button )
{
   if( down )
   {
      button->PushDown();
   }
   else
   {
      button->PopUp();
   }
}

//
// This draws the background of a toolbar
//
void ToolBar::OnPaint( wxPaintEvent & evt )
{
   wxPaintDC dc( (wxWindow *) evt.GetEventObject() );

   int width, height;
   GetSize(&width, &height);

#ifdef USE_AQUA_THEME

   if (mBackgroundWidth < width) {
      if (mBackgroundBitmap)
         delete mBackgroundBitmap;
      
      wxImage *aquaImage = CreateAquaBackground(width, height, 0);
      mBackgroundBitmap = new wxBitmap(aquaImage);
      delete aquaImage;
   }

   wxMemoryDC memDC;
   memDC.SelectObject(*mBackgroundBitmap);

   dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);

#else

#endif

   // Go repaint the rest
   Repaint( &dc );
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


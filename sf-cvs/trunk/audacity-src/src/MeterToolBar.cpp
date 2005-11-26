/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterToolBar.cpp

  Dominic Mazzoni
 
  See MeterToolBar.h for details

**********************************************************************/

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/dc.h>
#include <wx/dcclient.h>
#include <wx/event.h>
#include <wx/dcclient.h> // wxPaintDC
#include <wx/gbsizer.h>
#include "MeterToolBar.h"
#include "Audacity.h"
#include "widgets/Meter.h"
#include <wx/log.h>

////////////////////////////////////////////////////////////
/// Methods for MeterToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( MeterToolBar, ToolBar )
   EVT_SIZE( MeterToolBar::OnSize )
END_EVENT_TABLE()

//Standard contructor
MeterToolBar::MeterToolBar( wxWindow * parent ):
   ToolBar()
{
   mSizer = NULL;
   mPlayMeter = NULL;
   mRecordMeter = NULL;

   InitToolBar( parent,
                MeterBarID,
                _("Audacity Meter Toolbar"),
                _("Meter"),
                true );

   // Simulate a size event to set initial meter placement/size
   wxSizeEvent dummy;
   OnSize( dummy );
}

MeterToolBar::~MeterToolBar()
{
}

void MeterToolBar::Populate()
{
   mSizer = new wxGridBagSizer();
   Add( mSizer, 1, wxEXPAND );

   mPlayMeter = new Meter( this,
                           wxID_ANY,
                           false,
                           wxDefaultPosition,
                           wxSize( 99, 55 ) );
   mPlayMeter->SetLabel( wxT("Meter-Play"));
   mSizer->Add( mPlayMeter, wxGBPosition( 0, 0 ), wxDefaultSpan, wxEXPAND );

   mRecordMeter = new Meter( this,
                             wxID_ANY,
                             true,
                             wxDefaultPosition,
                             wxSize( 99, 55 ) );
   mRecordMeter->SetLabel( wxT("Meter-Record") );
   mSizer->Add( mRecordMeter, wxGBPosition( 0, 1 ), wxDefaultSpan, wxEXPAND );

#if wxUSE_TOOLTIPS
   mPlayMeter->SetToolTip( _("Output level meter") );
   mRecordMeter->SetToolTip( _("Input level meter - click to monitor input") );
#endif
}

void MeterToolBar::OnSize( wxSizeEvent & evt )
{
   int width, height;

   // We can be resized before populating...protect against it
   if( !mSizer )
   {
      return;
   }

   // Get the usable area
   GetClientSize( &width, &height );
   width -= mSizer->GetPosition().x;

   // Default location for record meter
   wxGBPosition pos( 0, 1 );

   // Two horizontal
   if( width > height )
   {
      if( height > 120 )
      {
         // Stacked
         mPlayMeter->SetMinSize( wxSize( width - 2, ( height / 2 ) - 1 ) );
         mRecordMeter->SetMinSize( wxSize( width - 2, ( height / 2 ) - 1 ) );
         pos.SetCol( 0 );
         pos.SetRow( 1 );
      }
      else
      {
         // Side-by-side
         mPlayMeter->SetMinSize( wxSize( ( width / 2 ) - 3, height ) );
         mRecordMeter->SetMinSize( wxSize( ( width / 2 ) - 6, height ) );
      }

      mPlayMeter->SetStyle(Meter::HorizontalStereo);
      mRecordMeter->SetStyle(Meter::HorizontalStereo);
   }
   else
   {
      // Two vertical, side-by-side
      mPlayMeter->SetMinSize( wxSize( ( width / 2 ) - 2, height ) );
      mRecordMeter->SetMinSize( wxSize( ( width / 2 ) - 2, height ) );
      mPlayMeter->SetStyle(Meter::VerticalStereo);
      mRecordMeter->SetStyle(Meter::VerticalStereo);
   }

   // Position the record meter
   mSizer->SetItemPosition( mRecordMeter, pos );

   // And make it happen
   Layout();
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
// arch-tag: 3acba542-52ae-44eb-b0b3-e0645587b5c0

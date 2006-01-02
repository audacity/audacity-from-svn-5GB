/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolsToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
 
  See ToolsToolBar.h for details

**********************************************************************/

#include "Audacity.h"

#include "ToolsToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/dcmemory.h>
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/brush.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/settings.h>
#endif

#include <wx/image.h>
#include <wx/tooltip.h>
#include <wx/msgdlg.h>

#include "widgets/AButton.h"
#include "AudioIO.h"
#include "ImageManipulation.h"
#include "Prefs.h"
#include "Project.h"
#include "Track.h"

#include "AColor.h"
#include "MeterToolBar.h"

#include "../images/ToolsButtons.h"

// Strings to convert a tool number into a status message
// These MUST be in the same order as the ids above.
const wxChar * MessageOfTool[numTools] = { wxTRANSLATE("Click and drag to select audio"),
   wxTRANSLATE("Click and drag to edit the amplitude envelope"),
   wxTRANSLATE("Click and drag to edit the samples"),
#if defined( __WXMAC__ )
   wxTRANSLATE("Click to Zoom In, Shift-Click to Zoom Out"),
#elif defined( __WXMSW__ )
   wxTRANSLATE("Drag to Zoom Into Region, Right-Click to Zoom Out"),
#elif defined( __WXGTK__ )
   wxTRANSLATE("Left=Zoom In, Right=Zoom Out, Middle=Normal"),
#endif
   wxTRANSLATE("Click and drag to move a track in time"),
   wxT("") // multi-mode tool
};

////////////////////////////////////////////////////////////
/// Methods for ToolsToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ToolsToolBar, ToolBar)
   EVT_COMMAND_RANGE( firstTool,
                      lastTool,
                      wxEVT_COMMAND_BUTTON_CLICKED,
                      ToolsToolBar::OnTool )
END_EVENT_TABLE()

//Standard constructor
ToolsToolBar::ToolsToolBar( wxWindow * parent ):
   ToolBar()
{
   InitToolBar( parent,
                ToolsBarID,
                _("Audacity Tools Toolbar"),
                _("Tools") );

   //Read the following wxASSERTs as documentating a design decision
   wxASSERT( selectTool   == selectTool   - firstTool );
   wxASSERT( envelopeTool == envelopeTool - firstTool );
   wxASSERT( slideTool    == slideTool    - firstTool );
   wxASSERT( zoomTool     == zoomTool     - firstTool );
   wxASSERT( drawTool     == drawTool     - firstTool );
   wxASSERT( multiTool    == multiTool    - firstTool );

   mCurrentTool = selectTool;
   mTool[mCurrentTool]->PushDown();
}


wxImage *ToolsToolBar::MakeToolImage(wxImage * tool,
                                       wxImage * mask, int style)
{
   // This code takes the image of a tool, and its mask,
   // and creates one of four images of this tool inside
   // a little button, for the toolbar.  The tool
   // is alpha-blended onto the background.

   const char **src;

   switch(style) {
   case 1: // hilite
      src = Hilite;
      break;
   case 2: // down
      src = Down;
      break;
   default:
      src = Up;
      break;
   }

   wxImage *bkgndOriginal = new wxImage(wxBitmap(src).ConvertToImage());
   wxImage *upOriginal = new wxImage(wxBitmap(Up).ConvertToImage());

#ifdef USE_AQUA_THEME
   wxImage *background = bkgndOriginal;
#else
   wxColour backgroundColour =
       wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE);
   wxColour baseColour;
   unsigned char *data = upOriginal->GetData();
   baseColour.Set(data[28 * 3], data[28 * 3 + 1], data[28 * 3 + 2]);
   wxImage *background = ChangeImageColour(bkgndOriginal,
                                           baseColour,
                                           backgroundColour);
#endif

   // 
   // Overlay the tool on top of it
   //

   wxImage *result;
   if (style == 2)              // down
      result = OverlayImage(background, tool, mask, 1, 1);
   else
      result = OverlayImage(background, tool, mask, 0, 0);
   delete background;

   #ifndef USE_AQUA_THEME
   delete bkgndOriginal;
   delete upOriginal;
   #endif

   return result;
}

AButton *ToolsToolBar::MakeTool(const char **tool, const char **alpha,
                                  wxWindowID id, const wxChar *label)
{
   wxImage *ctr = new wxImage(wxBitmap(tool).ConvertToImage());
   wxImage *mask = new wxImage(wxBitmap(alpha).ConvertToImage());
   wxImage *up = MakeToolImage(ctr, mask, 0);
   wxImage *hilite = MakeToolImage(ctr, mask, 1);
   wxImage *down = MakeToolImage(ctr, mask, 2);
   wxImage *dis = MakeToolImage(ctr, mask, 3);

   AButton *button =
       new AButton(this, id, wxDefaultPosition, wxSize(27, 27),
                   up, hilite, down, dis, false);
   button->SetLabel( label );
   mToolSizer->Add( button );

   delete ctr;
   delete mask;
   delete up;
   delete hilite;
   delete down;
   delete dis;

   return button;
}


void ToolsToolBar::RegenerateToolsTooltips()
{

// JKC: 
//   Under Win98 Tooltips appear to be buggy, when you have a lot of
//   tooltip messages flying around.  I found that just creating a 
//   twelfth tooltip caused Audacity to crash when it tried to show 
//   any tooltip.
//
//   Win98 does NOT recover from this crash - for any application which is 
//   using tooltips will also crash thereafter...  so you must reboot.
//   Rather weird.  
//
//   Getting windows to process more of its stacked up messages seems
//   to workaround the problem.  The problem is not fully understood though
//   (as of April 2003).
   
   //	Vaughan, October 2003: Now we're crashing on Win2K if 
	// "Quit when closing last window" is unchecked, when we come back 
	// through here, on either of the wxSafeYield calls.
	// James confirms that commenting them out does not cause his original problem 
	// to reappear, so they're commented out now.
	//		wxSafeYield(); //Deal with some queued up messages...

   #if wxUSE_TOOLTIPS
   mTool[selectTool]->SetToolTip(_("Selection Tool"));
   mTool[envelopeTool]->SetToolTip(_("Envelope Tool"));
   mTool[slideTool]->SetToolTip(_("Time Shift Tool"));
   mTool[zoomTool]->SetToolTip(_("Zoom Tool"));
   mTool[drawTool]->SetToolTip(_("Draw Tool"));
   mTool[multiTool]->SetToolTip(_("Multi-Tool Mode"));
   #endif

   //		wxSafeYield();
   return;

}

void ToolsToolBar::Populate()
{
   mToolSizer = new wxGridSizer( 2, 3, 1, 1 );
   Add( mToolSizer );

   /* Tools */

   mTool[ selectTool   ] = MakeTool( IBeam, IBeamAlpha, selectTool, _("SelectionTool") );
   mTool[ envelopeTool ] = MakeTool( Envelope, EnvelopeAlpha, envelopeTool, _("TimeShiftTool") );
   mTool[ drawTool     ] = MakeTool( Draw, DrawAlpha, drawTool, _("DrawTool") );
   mTool[ zoomTool     ] = MakeTool( Zoom, ZoomAlpha, zoomTool, _("ZoomTool") );
   mTool[ slideTool    ] = MakeTool( TimeShift, TimeShiftAlpha, slideTool, _("SlideTool") );
   mTool[ multiTool    ] = MakeTool( Multi, MultiAlpha, multiTool, _("MultiTool") );

#if wxUSE_TOOLTIPS
#ifdef __WXMAC__
   wxToolTip::Enable(false);    // DM: tooltips are broken in wxMac
#else
// MB: Should make this a pref
   wxToolTip::Enable(true);     
   wxToolTip::SetDelay(1000);
#endif
#endif

   RegenerateToolsTooltips();
}

ToolsToolBar::~ToolsToolBar()
{
   for (int i = 0; i < 5; i++)
      delete mTool[i];
}

/// Gets the currently active tool
/// In Multi-mode this might not return the multi-tool itself
/// since the active tool may be changed by what you hover over.
int ToolsToolBar::GetCurrentTool()
{
   return mCurrentTool;
}

/// Sets the currently active tool
/// @param tool - The index of the tool to be used.
/// @param show - should we update the button display?
void ToolsToolBar::SetCurrentTool(int tool, bool show)
{
   //In multi-mode the current tool is shown by the 
   //cursor icon.  The buttons are not updated.

   if (tool != mCurrentTool) {
      if (show)
         mTool[mCurrentTool]->PopUp();
      mCurrentTool=tool;
      if (show)
         mTool[mCurrentTool]->PushDown();
   }
	//JKC: ANSWER-ME: Why is this RedrawAllProjects() line required?
   RedrawAllProjects();
}

bool ToolsToolBar::GetSelectToolDown()
{
   return mTool[selectTool]->IsDown();
}

bool ToolsToolBar::GetZoomToolDown()
{
   return mTool[zoomTool]->IsDown();
}

bool ToolsToolBar::GetEnvelopeToolDown()
{
   return mTool[envelopeTool]->IsDown();
}

bool ToolsToolBar::GetSlideToolDown()
{
   return mTool[slideTool]->IsDown();
}

bool ToolsToolBar::GetDrawToolDown()
{
   return mTool[drawTool ]->IsDown();
}

bool ToolsToolBar::GetMultiToolDown()
{
   return mTool[multiTool ]->IsDown();
}

const wxChar * ToolsToolBar::GetMessageForTool( int ToolNumber )
{
   wxASSERT( ToolNumber >= 0 );
   wxASSERT( ToolNumber < numTools );
   return wxGetTranslation(MessageOfTool[ ToolNumber ]);
}


void ToolsToolBar::OnTool(wxCommandEvent & evt)
{
   mCurrentTool = evt.GetId() - firstTool;
   for (int i = 0; i < numTools; i++)
      if (i == mCurrentTool) 
         mTool[i]->PushDown();
      else
         mTool[i]->PopUp();

   RedrawAllProjects();
}

void ToolsToolBar::Repaint( wxPaintDC *dc )
{
}

void ToolsToolBar::EnableDisableButtons()
{
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
// arch-tag: ebfdc42a-6a03-4826-afa2-937a48c0565b

/**********************************************************************

  Audacity: A Digital Audio Editor

  EditToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius
 
  See EditToolBar.h for details

*******************************************************************//*!

\class EditToolBar
\brief A ToolBar that has the edit buttons on it.
 
  This class, which is a child of Toolbar, creates the
  window containing interfaces to commonly-used edit
  functions that are otherwise only available through
  menus. The window can be embedded within a normal project
  window, or within a ToolbarFrame that is managed by a
  global ToolBarStub called gControlToolBarStub.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

*//*******************************************************************/


#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/tooltip.h>
#endif

#include "EditToolBar.h"

#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Internat.h"
#include "../Project.h"
#include "../Theme.h"
#include "../UndoManager.h"
#include "../widgets/AButton.h"

IMPLEMENT_CLASS(EditToolBar, ToolBar);

const int BUTTON_WIDTH = 27;
const int SEPARATOR_WIDTH = 14;

////////////////////////////////////////////////////////////
/// Methods for EditToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( EditToolBar, ToolBar )
   EVT_COMMAND_RANGE( ETBCutID,
                      ETBCutID + ETBNumButtons - 1,
                      wxEVT_COMMAND_BUTTON_CLICKED,
                      EditToolBar::OnButton )
END_EVENT_TABLE()

//Standard contructor
EditToolBar::EditToolBar()
: ToolBar(EditBarID, _NoAcc("&Edit"), wxT("Edit"))
{
}

EditToolBar::~EditToolBar()
{
   for (int i=0; i<ETBNumButtons; i++)
      delete mButtons[i];
}

void EditToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}

void EditToolBar::AddSeparator()
{
   AddSpacer();
}

/// This is a convenience function that allows for button creation in
/// MakeButtons() with fewer arguments
/// Very similar to code in ControlToolBar...
AButton *EditToolBar::AddButton(
   teBmps eFore, teBmps eDisabled,
   int id,
   const wxChar *label,
   const wxChar *tip)
{
   AButton *&r = mButtons[id];

   r = ToolBar::MakeButton(
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, bmpRecoloredHiliteSmall,
      eFore, eDisabled,
      wxWindowID(id),
      wxDefaultPosition, 
      false,
      theTheme.ImageSize( bmpRecoloredUpSmall ));

   r->SetLabel(label);
// JKC: Unlike ControlToolBar, does not have a focus rect.  Shouldn't it?
// r->SetFocusRect( r->GetRect().Deflate( 4, 4 ) );

#if wxUSE_TOOLTIPS
   r->SetToolTip(tip);
#endif
   Add( r, 0, wxALIGN_CENTER );

   return r;
}

void EditToolBar::Populate()
{
   MakeButtonBackgroundsSmall();

   /* Buttons */
   AddButton(bmpCut, bmpCutDisabled, ETBCutID,
      _("Cut"), _("Cut"));
   AddButton(bmpCopy, bmpCopyDisabled, ETBCopyID,
      _("Copy"), _("Copy"));
   AddButton(bmpPaste, bmpPasteDisabled, ETBPasteID,
      _("Paste"), _("Paste"));
   AddButton(bmpTrim, bmpTrimDisabled, ETBTrimID,
      _("Trim outside selection"),_("Trim"));
   AddButton(bmpSilence, bmpSilenceDisabled, ETBSilenceID,
      _("Silence selection"),_("Silence"));
   AddSeparator();
   AddButton(bmpUndo, bmpUndoDisabled, ETBUndoID,
      _("Undo"), _("Undo"));
   AddButton(bmpRedo, bmpRedoDisabled, ETBRedoID,
      _NoAcc("&Redo"), _NoAcc("&Redo"));
   AddSeparator();

   AddButton(bmpZoomIn, bmpZoomInDisabled, ETBZoomInID,
      _("Zoom In"),_("Zoom In"));
   AddButton(bmpZoomOut, bmpZoomOutDisabled, ETBZoomOutID,
      _("Zoom Out"),_("Zoom Out"));

   #if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
   AddButton(bmpZoomToggle, bmpZoomToggleDisabled, ETBZoomToggleID,
      _("Zoom Toggle"),_("Zoom Toggle"));
   #endif

    AddButton(bmpZoomSel, bmpZoomSelDisabled, ETBZoomSelID,
      _("Fit selection in window"),_("Fit Selection"));
   AddButton(bmpZoomFit, bmpZoomFitDisabled, ETBZoomFitID,
      _("Fit project in window"),_("Fit Project"));

   mButtons[ETBZoomInID]->SetEnabled(false);
   mButtons[ETBZoomOutID]->SetEnabled(false);

   #if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
   mButtons[ETBZoomToggleID]->SetEnabled(false);
   #endif

   mButtons[ETBZoomSelID]->SetEnabled(false);
   mButtons[ETBZoomFitID]->SetEnabled(false);
   mButtons[ETBPasteID]->SetEnabled(false);
}

void EditToolBar::OnButton(wxCommandEvent &event)
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;

   bool busy = gAudioIO->IsBusy();
   int id = event.GetId();

   switch (id) {
      case ETBCutID:
         if (!busy) p->OnCut();
         break;
      case ETBCopyID:
         if (!busy) p->OnCopy();
         break;
      case ETBPasteID:
         if (!busy) p->OnPaste();
         break;
      case ETBTrimID:
         if (!busy) p->OnTrim();
         break;
      case ETBSilenceID:
         if (!busy) p->OnSilence();
         break;
      case ETBUndoID:
         if (!busy) p->OnUndo();
         break;
      case ETBRedoID:
         if (!busy) p->OnRedo();
         break;
      case ETBZoomInID:
         p->OnZoomIn();
         break;
      case ETBZoomOutID:
         p->OnZoomOut();
         break;

#if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
      case ETBZoomToggleID:
         p->OnZoomToggle();
         break;
#endif

      case ETBZoomSelID:
         p->OnZoomSel();
         break;
      case ETBZoomFitID:
         p->OnZoomFit();
         break;
   }

   SetButton(false, mButtons[id]);
}

void EditToolBar::EnableDisableButtons()
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;

   // Is anything selected?
   bool selection = false;
   TrackListIterator iter(p->GetTracks());
   for (Track *t = iter.First(); t; t = iter.Next())
      if (t->GetSelected()) {
         selection = true;
         break;
      }
   selection &= (p->GetSel0() < p->GetSel1());
   
   mButtons[ETBCutID]->SetEnabled(selection);
   mButtons[ETBCopyID]->SetEnabled(selection);
   mButtons[ETBTrimID]->SetEnabled(selection);
   mButtons[ETBSilenceID]->SetEnabled(selection);

   mButtons[ETBUndoID]->SetEnabled(p->GetUndoManager()->UndoAvailable());
   mButtons[ETBRedoID]->SetEnabled(p->GetUndoManager()->RedoAvailable());

   bool tracks = (!p->GetTracks()->IsEmpty());

   mButtons[ETBZoomInID]->SetEnabled(tracks && (p->GetZoom() < gMaxZoom));
   mButtons[ETBZoomOutID]->SetEnabled(tracks && (p->GetZoom() > gMinZoom) );

   #if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
   mButtons[ETBZoomToggleID]->SetEnabled(tracks);
   #endif

   mButtons[ETBZoomSelID]->SetEnabled(selection);
   mButtons[ETBZoomFitID]->SetEnabled(tracks);

   mButtons[ETBPasteID]->SetEnabled(p->Clipboard());
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
// arch-tag: 55533f04-7fee-4a50-a3b6-e392ab2f8713


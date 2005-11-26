/**********************************************************************

  Audacity: A Digital Audio Editor

  EditToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
 
  See EditToolBar.h for details

**********************************************************************/

#include "EditToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/log.h>
#include <wx/event.h>
#include <wx/brush.h>
#include <wx/dcclient.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/tooltip.h>
#endif

#include <wx/image.h>

#include "AudioIO.h"
#include "ImageManipulation.h"
#include "Project.h"
#include "UndoManager.h"
#include "widgets/AButton.h"
#include "widgets/ASlider.h"

#include "../images/EditButtons.h"

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
EditToolBar::EditToolBar(wxWindow * parent):
   ToolBar()
{
   InitToolBar( parent,
                EditBarID,
                _("Audacity Edit Toolbar"),
                _("Edit") );
}


// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments
void EditToolBar::AddButton(const char **fg, const char **disabled, const char **alpha,
                            int id, const wxChar *tooltip, const wxChar *label)
{

   mButtons[id] = MakeButton(
                     upImage, downImage, hiliteImage, fg,
                     disabled, alpha,
                     wxWindowID(id),
                     wxDefaultPosition,
                     false /*No edit buttons should process down events.*/,
                     wxSize(BUTTON_WIDTH, BUTTON_WIDTH), 0, 0);
   #if wxUSE_TOOLTIPS // Not available in wxX11
   mButtons[id]->SetToolTip(tooltip);
   #endif
   mButtons[id]->SetLabel( label );
   Add( mButtons[id] );
}

void EditToolBar::AddSeparator()
{
   AddSpacer();
}

void EditToolBar::Populate()
{
   wxColour newColour =
       wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE);
   wxColour baseColour = wxColour(204, 204, 204);

   wxImage *upOriginal = new wxImage(wxBitmap(Up).ConvertToImage());
   wxImage *downOriginal = new wxImage(wxBitmap(Down).ConvertToImage());
   wxImage *hiliteOriginal = new wxImage(wxBitmap(Hilite).ConvertToImage());

#ifdef __WXGTK__
   /* dmazzoni: hack to get around XPM color bugs in GTK */
   unsigned char *data = upOriginal->GetData();
   baseColour.Set(data[28 * 3], data[28 * 3 + 1], data[28 * 3 + 2]);
#endif

   upImage = ChangeImageColour(upOriginal, baseColour, newColour);
   downImage = ChangeImageColour(downOriginal, baseColour, newColour);
   hiliteImage = ChangeImageColour(hiliteOriginal, baseColour, newColour);

   /* Buttons */

   AddButton(Cut, CutDisabled, CutAlpha, ETBCutID,
             _("Cut"), _("Cut"));
   AddButton(Copy, CopyDisabled, CopyAlpha, ETBCopyID,
             _("Copy"), _("Copy"));
   AddButton(Paste, PasteDisabled, PasteAlpha, ETBPasteID,
             _("Paste"), _("Paste"));
   AddButton(Trim, TrimDisabled, TrimAlpha, ETBTrimID,
             _("Trim outside selection"),_("Trim"));
   AddButton(Silence, SilenceDisabled, SilenceAlpha, ETBSilenceID,
             _("Silence selection"),_("Silence"));
   AddSeparator();

   AddButton(Undo, UndoDisabled, UndoAlpha, ETBUndoID,
             _("Undo"), _("Undo"));
   AddButton(Redo, RedoDisabled, RedoAlpha, ETBRedoID,
             _("Redo"), _("Redo"));
   AddSeparator();

   AddButton(ZoomIn, ZoomInDisabled, ZoomInAlpha, ETBZoomInID,
             _("Zoom In"),_("Zoom In"));
   AddButton(ZoomOut, ZoomOutDisabled, ZoomOutAlpha, ETBZoomOutID,
             _("Zoom Out"),_("Zoom Out"));

   #if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
   AddButton(ZoomToggle, ZoomToggleDisabled, ZoomToggleAlpha, ETBZoomToggleID,
             _("Zoom Toggle"),_("Zoom Toggle"));
   #endif

   AddButton(ZoomSel, ZoomSelDisabled, ZoomSelAlpha, ETBZoomSelID,
             _("Fit selection in window"),_("Fit Selection"));
   AddButton(ZoomFit, ZoomFitDisabled, ZoomFitAlpha, ETBZoomFitID,
             _("Fit project in window"),_("Fit Project"));

   mButtons[ETBZoomInID]->SetEnabled(false);
   mButtons[ETBZoomOutID]->SetEnabled(false);

   #if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
   mButtons[ETBZoomToggleID]->SetEnabled(false);
   #endif

   mButtons[ETBZoomSelID]->SetEnabled(false);
   mButtons[ETBZoomFitID]->SetEnabled(false);
   mButtons[ETBPasteID]->SetEnabled(false);

   delete upImage;
   delete downImage;
   delete hiliteImage;
   delete upOriginal;
   delete downOriginal;
   delete hiliteOriginal;
}

EditToolBar::~EditToolBar()
{
   for (int i=0; i<ETBNumButtons; i++)
      delete mButtons[i];
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


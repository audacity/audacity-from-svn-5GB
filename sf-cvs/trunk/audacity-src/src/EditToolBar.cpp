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
#endif

#include <wx/image.h>
#include <wx/tooltip.h>

#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "AudioIO.h"
#include "Project.h"
#include "UndoManager.h"

#include "../images/EditButtons.h"

const int BUTTON_WIDTH = 27;
const int SEPARATOR_WIDTH = 14;

////////////////////////////////////////////////////////////
/// Methods for EditToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(EditToolBar, wxWindow)
   EVT_PAINT(EditToolBar::OnPaint)
   EVT_CHAR(EditToolBar::OnKeyEvent)

   EVT_COMMAND_RANGE(ETBCutID, ETBCutID + ETBNumButtons-1,
         wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnButton)
END_EVENT_TABLE()

//Standard contructor
EditToolBar::EditToolBar(wxWindow * parent)
   : ToolBar(parent, -1, wxPoint(1, 1), wxSize(340, 27))
{
   InitializeEditToolBar();
}

//Another constructor
EditToolBar::EditToolBar(wxWindow * parent, wxWindowID id,
                         const wxPoint & pos, const wxSize & size)
   : ToolBar(parent, id, pos, size)
{
   InitializeEditToolBar();
}


// This sets up the EditToolBar, initializing all the important values
// and creating the buttons.
void EditToolBar::InitializeEditToolBar()
{
   mIdealSize = wxSize(340, 27);
   mTitle = "Audacity Edit Toolbar";
   mType = EditToolBarID;

   wxColour backgroundColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour origColour(204, 204, 204);

   MakeButtons();

   mBackgroundBrush.SetColour(backgroundColour);
   mBackgroundPen.SetColour(backgroundColour);

   mBackgroundBitmap = NULL;
   mBackgroundHeight = 0;
   mBackgroundWidth = 0;
}


// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments

void EditToolBar::AddButton(char **fg, char **disabled, char **alpha,
                            int id, const char *tooltip)
{
   mButtons[id] = ToolBar::MakeButton(
                     upImage, downImage, hiliteImage, (const char **) fg,
                     (const char **) disabled, (const char **) alpha,
                     wxWindowID(id), wxPoint(mButtonPos, 0),
                     wxSize(BUTTON_WIDTH, BUTTON_WIDTH), 3, 3);

   mButtons[id]->SetToolTip(tooltip);
   mButtonPos += BUTTON_WIDTH;
}

void EditToolBar::AddSeparator()
{
   mButtonPos += SEPARATOR_WIDTH;
}

void EditToolBar::MakeButtons()
{
   wxColour newColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour baseColour = wxColour(204, 204, 204);

   wxImage *upOriginal = new wxImage(Up);
   wxImage *downOriginal = new wxImage(Down);
   wxImage *hiliteOriginal = new wxImage(Hilite);

   upImage = ChangeImageColour(upOriginal, baseColour, newColour);
   downImage = ChangeImageColour(downOriginal, baseColour, newColour);
   hiliteImage = ChangeImageColour(hiliteOriginal, baseColour, newColour);

#ifdef __WXGTK__
   /* dmazzoni: hack to get around XPM color bugs in GTK */
   unsigned char *data = upOriginal->GetData();
   baseColour.Set(data[28 * 3], data[28 * 3 + 1], data[28 * 3 + 2]);
#endif

   /* Buttons */

   mButtonPos = 1;

   AddButton(Cut, CutDisabled, CutAlpha, ETBCutID,
             _("Cut selection to clipboard"));
   AddButton(Copy, CopyDisabled, CopyAlpha, ETBCopyID,
             _("Copy selection to clipboard"));
   AddButton(Paste, PasteDisabled, PasteAlpha, ETBPasteID,
             _("Paste selection"));
   AddButton(Trim, TrimDisabled, TrimAlpha, ETBTrimID,
             _("Trim everything ETBOutsIDe selection"));
   AddButton(Silence, SilenceDisabled, SilenceAlpha, ETBSilenceID,
             _("Insert Silence"));

   AddSeparator();
   AddButton(Undo, UndoDisabled, UndoAlpha, ETBUndoID, _("Undo"));
   AddButton(Redo, RedoDisabled, RedoAlpha, ETBRedoID, _("Redo"));
   AddSeparator();

   AddButton(ZoomIn, ZoomInDisabled, ZoomInAlpha, ETBZoomInID,
             _("Zoom In"));
   AddButton(ZoomOut, ZoomOutDisabled, ZoomOutAlpha, ETBZoomOutID,
             _("Zoom Out"));
   AddButton(ZoomSel, ZoomSelDisabled, ZoomSelAlpha, ETBZoomSelID,
             _("Fit selection in window"));
   AddButton(ZoomFit, ZoomFitDisabled, ZoomFitAlpha, ETBZoomFitID,
             _("Fit entire file in window"));

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
   delete[] mButtons;

   if (mBackgroundBitmap)
      delete mBackgroundBitmap;
}

void EditToolBar::OnKeyEvent(wxKeyEvent & event)
{
   event.Skip();
}

void EditToolBar::OnButton(wxCommandEvent &event)
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;

   wxCommandEvent e;
   bool busy = gAudioIO->IsBusy();
   int id = event.GetId();

   switch (id) {
      case ETBCutID:
         if (!busy) p->Cut(e);
         break;
      case ETBCopyID:
         if (!busy) p->Copy(e);
         break;
      case ETBPasteID:
         if (!busy) p->Paste(e);
         break;
      case ETBTrimID:
         if (!busy) p->Trim(e);
         break;
      case ETBSilenceID:
         if (!busy) p->OnSilence(e);
         break;
      case ETBUndoID:
         if (!busy) p->Undo(e);
         break;
      case ETBRedoID:
         if (!busy) p->Redo(e);
         break;
      case ETBZoomInID:
         p->OnZoomIn(e);
         break;
      case ETBZoomOutID:
         p->OnZoomOut(e);
         break;
      case ETBZoomSelID:
         p->OnZoomSel(e);
         break;
      case ETBZoomFitID:
         p->OnZoomFit(e);
         break;
   }

   SetButton(false, mButtons[id]);
}

void EditToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

   dc.SetBrush(mBackgroundBrush);
   dc.SetPen(mBackgroundPen);
   dc.DrawRectangle(0, 0, width, height);

   dc.SetPen(*wxBLACK_PEN);
}

void EditToolBar::EnableDisableButtons()
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;

   // Is anything selected?
   bool selection = false;
   TrackListIterator iter(p->GetTracks());
   for (VTrack *t = iter.First(); t; t = iter.Next())
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

   mButtons[ETBZoomInID]->SetEnabled(p->GetZoom() < gMaxZoom);
   mButtons[ETBZoomOutID]->SetEnabled(p->GetZoom() > gMinZoom);

   mButtons[ETBZoomSelID]->SetEnabled(selection);
   mButtons[ETBZoomFitID]->SetEnabled(tracks);
}

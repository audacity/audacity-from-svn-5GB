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
#include "ImageManipulation.h"
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
   : ToolBar(parent, -1, wxPoint(1, 1), wxSize(350, 27))
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
   mIdealSize = wxSize(350, 27);
   mTitle = _("Audacity Edit Toolbar");
   mType = EditToolBarID;
   mNumDividers = 0;

   MakeButtons();
}


// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments

void EditToolBar::AddButton(const char **fg, const char **disabled, const char **alpha,
                            int id, const char *tooltip)
{

   // Windows (TM) has a little extra room for some reason, so the top of the
   // buttons should be a little lower.
   int buttonTop = 0;
#ifdef __WXMSW__
   buttonTop=0;
#endif

   mButtons[id] = ToolBar::MakeButton(
                     upImage, downImage, hiliteImage, fg,
                     disabled, alpha,
                     wxWindowID(id), wxPoint(mButtonPos, buttonTop),
                     false /*No edit buttons should process down events.*/,
                     wxSize(BUTTON_WIDTH, BUTTON_WIDTH), 0, 0);
   mButtons[id]->SetToolTip(tooltip);
   mButtonPos += BUTTON_WIDTH;
   mDividers[mNumDividers++] = mButtonPos++;
}

void EditToolBar::AddSeparator()
{
   mButtonPos += SEPARATOR_WIDTH;
   mDividers[mNumDividers++] = mButtonPos++;
}

void EditToolBar::MakeButtons()
{
   wxColour newColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
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

   mButtonPos = 0;

   AddButton(Cut, CutDisabled, CutAlpha, ETBCutID,
             _("Cut"));
   AddButton(Copy, CopyDisabled, CopyAlpha, ETBCopyID,
             _("Copy"));
   AddButton(Paste, PasteDisabled, PasteAlpha, ETBPasteID,
             _("Paste"));
   AddButton(Trim, TrimDisabled, TrimAlpha, ETBTrimID,
             _("Trim everything outside selection"));
   AddButton(Silence, SilenceDisabled, SilenceAlpha, ETBSilenceID,
             _("Silence selection"));

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
             _("Fit entire project in window"));

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

void EditToolBar::OnKeyEvent(wxKeyEvent & event)
{
   event.Skip();
}

void EditToolBar::OnButton(wxCommandEvent &event)
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;

   bool busy = gAudioIO->IsStreamActive();
   int id = event.GetId();

   switch (id) {
      case ETBCutID:
         if (!busy) p->Cut();
         break;
      case ETBCopyID:
         if (!busy) p->Copy();
         break;
      case ETBPasteID:
         if (!busy) p->Paste();
         break;
      case ETBTrimID:
         if (!busy) p->Trim();
         break;
      case ETBSilenceID:
         if (!busy) p->OnSilence();
         break;
      case ETBUndoID:
         if (!busy) p->Undo();
         break;
      case ETBRedoID:
         if (!busy) p->Redo();
         break;
      case ETBZoomInID:
         p->OnZoomIn();
         break;
      case ETBZoomOutID:
         p->OnZoomOut();
         break;
      case ETBZoomSelID:
         p->OnZoomSel();
         break;
      case ETBZoomFitID:
         p->OnZoomFit();
         break;
   }

   SetButton(false, mButtons[id]);
}

void EditToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

   DrawBackground(dc, width, height);

   dc.SetPen(*wxBLACK_PEN);
//   for(int i=0; i<mNumDividers; i++)
//      dc.DrawLine(mDividers[i], 0, mDividers[i], mIdealSize.GetHeight());
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

   mButtons[ETBZoomInID]->SetEnabled(p->GetZoom() < gMaxZoom);
   mButtons[ETBZoomOutID]->SetEnabled(p->GetZoom() > gMinZoom);

   mButtons[ETBZoomSelID]->SetEnabled(selection);
   mButtons[ETBZoomFitID]->SetEnabled(tracks);

   mButtons[ETBPasteID]->SetEnabled(p->Clipboard());
}

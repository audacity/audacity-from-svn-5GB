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
   EVT_COMMAND(ETBCutID, wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnCut)
   EVT_COMMAND(ETBCopyID, wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnCopy)
   EVT_COMMAND(ETBPasteID, wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnPaste)
   EVT_COMMAND(ETBTrimID, wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnTrim)
   EVT_COMMAND(ETBSilenceID,
         wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnSilence)
   EVT_COMMAND(ETBUndoID, wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnUndo)
   EVT_COMMAND(ETBRedoID, wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnRedo)

   EVT_COMMAND(ETBZoomInID,
         wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnZoomIn)
   EVT_COMMAND(ETBZoomOutID,
         wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnZoomOut)
   EVT_COMMAND(ETBZoomSelID,
         wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnZoomSel)
   EVT_COMMAND(ETBZoomFitID,
         wxEVT_COMMAND_BUTTON_CLICKED, EditToolBar::OnZoomFit)
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

void EditToolBar::AddButton(AButton **button, char **fg,
                            char **disabled, char **alpha,
                            int id, const char *tooltip)
{
   *button = ToolBar::MakeButton(upImage, downImage, hiliteImage,
                (const char **) fg, (const char **) disabled,
                (const char **) alpha, wxWindowID(id), wxPoint(mButtonPos, 0),
                wxSize(BUTTON_WIDTH, BUTTON_WIDTH), 3, 3);

   (*button)->SetToolTip(tooltip);
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

   AddButton(&mCut, Cut, CutDisabled, CutAlpha, ETBCutID,
             _("Cut selection to clipboard"));
   AddButton(&mCopy, Copy, CopyDisabled, CopyAlpha, ETBCopyID,
             _("Copy selection to clipboard"));
   AddButton(&mPaste, Paste, PasteDisabled, PasteAlpha, ETBPasteID,
             _("Paste selection"));
   AddButton(&mTrim, Trim, TrimDisabled, TrimAlpha, ETBTrimID,
             _("Trim everything outside selection"));
   AddButton(&mSilence, Silence, SilenceDisabled, SilenceAlpha, ETBSilenceID,
             _("Insert Silence"));

   AddSeparator();
   AddButton(&mUndo, Undo, UndoDisabled, UndoAlpha, ETBUndoID, _("Undo"));
   AddButton(&mRedo, Redo, RedoDisabled, RedoAlpha, ETBRedoID, _("Redo"));
   AddSeparator();

   AddButton(&mZoomIn, ZoomIn, ZoomInDisabled, ZoomInAlpha, ETBZoomInID,
             _("Zoom In"));
   AddButton(&mZoomOut, ZoomOut, ZoomOutDisabled, ZoomOutAlpha, ETBZoomOutID,
             _("Zoom Out"));
   AddButton(&mZoomSel, ZoomSel, ZoomSelDisabled, ZoomSelAlpha, ETBZoomSelID,
             _("Fit selection in window"));
   AddButton(&mZoomFit, ZoomFit, ZoomFitDisabled, ZoomFitAlpha, ETBZoomFitID,
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
   delete mCut;
   delete mCopy;
   delete mPaste;
   delete mTrim;
   delete mSilence;
   delete mUndo;
   delete mRedo;
   delete mZoomIn;
   delete mZoomOut;
   delete mZoomSel;
   delete mZoomFit;

   if (mBackgroundBitmap)
      delete mBackgroundBitmap;
}

void EditToolBar::OnKeyEvent(wxKeyEvent & event)
{
   event.Skip();
}

void EditToolBar::OnCut()
{
   if (!gAudioIO->IsBusy()) {
      wxCommandEvent event;
      AudacityProject *p = GetActiveProject();
      if (p) {
         p->Cut(event);
      }
   }
   SetButton(false, mCut);
}


void EditToolBar::OnCopy()
{
   if (!gAudioIO->IsBusy()) {
      wxCommandEvent event;
      AudacityProject *p = GetActiveProject();
      if (p) {
         p->Copy(event);
      }
   }
   SetButton(false, mCopy);
}



void EditToolBar::OnPaste()
{
   if (!gAudioIO->IsBusy()) {
      wxCommandEvent event;
      AudacityProject *p = GetActiveProject();
      if (p) {
         p->Paste(event);
      }
   }
   SetButton(false, mPaste);
}

void EditToolBar::OnTrim()
{
   if (!gAudioIO->IsBusy()) {
      wxCommandEvent event;
      AudacityProject *p = GetActiveProject();
      if (p) {
         p->Trim(event);
      }
   }
   SetButton(false, mTrim);
}

void EditToolBar::OnSilence()
{
   if (!gAudioIO->IsBusy()) {
      wxCommandEvent event;
      AudacityProject *p = GetActiveProject();
      if (p) {
         p->OnSilence(event);
      }
   }
   SetButton(false, mSilence);
}

void EditToolBar::OnUndo()
{
   if (!gAudioIO->IsBusy()) {
      wxCommandEvent event;
      AudacityProject *p = GetActiveProject();
      if (p) {
         p->Undo(event);
      }
   }
   SetButton(false, mUndo);
}

void EditToolBar::OnRedo()
{
   if (!gAudioIO->IsBusy()) {
      wxCommandEvent event;
      AudacityProject *p = GetActiveProject();
      if (p) {
         p->Redo(event);
      }
   }
   SetButton(false, mRedo);
}


void EditToolBar::OnZoomIn()
{
   wxCommandEvent event;
   AudacityProject *p = GetActiveProject();
   if (p) {
      p->OnZoomIn(event);
   }

   SetButton(false, mZoomIn);
}


void EditToolBar::OnZoomOut()
{

   wxCommandEvent event;
   AudacityProject *p = GetActiveProject();
   if (p) {
      p->OnZoomOut(event);
   }

   SetButton(false, mZoomOut);
}

void EditToolBar::OnZoomSel()
{
   wxCommandEvent event;
   AudacityProject *p = GetActiveProject();
   if (p) {
      p->OnZoomSel(event);
   }

   SetButton(false, mZoomSel);
}

void EditToolBar::OnZoomFit()
{
   wxCommandEvent event;
   AudacityProject *p = GetActiveProject();
   if (p)
      p->OnZoomFit(event);

   SetButton(false, mZoomFit);
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

   bool tracks = (!p->GetTracks()->IsEmpty());

   mZoomIn->SetEnabled(tracks);
   mZoomOut->SetEnabled(tracks);
   mZoomSel->SetEnabled(tracks);
   mZoomFit->SetEnabled(tracks);

   // Is anything selected?
   bool selection = false;
   TrackListIterator iter(p->GetTracks());
   for (VTrack *t = iter.First(); t; t = iter.Next())
      if (t->GetSelected()) {
         selection = true;
         break;
      }
   selection &= (p->GetSel0() < p->GetSel1());
   
   mCut->SetEnabled(selection);
   mCopy->SetEnabled(selection);
   mTrim->SetEnabled(selection);
   mSilence->SetEnabled(selection);
   mZoomSel->SetEnabled(selection);

   mUndo->SetEnabled(p->GetUndoManager()->UndoAvailable());
   mRedo->SetEnabled(p->GetUndoManager()->RedoAvailable());

   mZoomIn->SetEnabled(p->GetZoom() < gMaxZoom);
   mZoomOut->SetEnabled(p->GetZoom() > gMinZoom);
}

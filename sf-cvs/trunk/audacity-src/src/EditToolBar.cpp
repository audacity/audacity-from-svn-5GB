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
#include "ToolBar.h"
#include "AudioIO.h"
#include "Project.h"
#include "UndoManager.h"

#include "../images/EditButtons.h"


class ToolBar;

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
AButton *EditToolBar::MakeButton(wxImage * up, wxImage * down,
                                 wxImage * hilite,
                                 char const **foreground,
                                 char const **disabled,
                                 char const **alpha, int id, int left)
{
   return ToolBar::MakeButton(up, down, hilite, foreground, disabled, alpha,
             wxWindowID(id), wxPoint(left,0), wxSize(27, 27), 3, 3);
}



void EditToolBar::MakeButtons()
{
   wxImage *upOriginal = new wxImage(Up);
   wxImage *downOriginal = new wxImage(Down);
   wxImage *hiliteOriginal = new wxImage(Hilite);

   wxColour newColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour baseColour = wxColour(204, 204, 204);

#ifdef __WXGTK__
   /* dmazzoni: hack to get around XPM color bugs in GTK */
   unsigned char *data = upOriginal->GetData();
   baseColour.Set(data[28 * 3], data[28 * 3 + 1], data[28 * 3 + 2]);
#endif

   wxImage *upPattern =
       ChangeImageColour(upOriginal, baseColour, newColour);
   wxImage *downPattern =
       ChangeImageColour(downOriginal, baseColour, newColour);
   wxImage *hilitePattern =
       ChangeImageColour(hiliteOriginal, baseColour, newColour);


   /* Buttons */

   //Cut Button
   mCut = MakeButton(upPattern, downPattern, hilitePattern,
                     (char const **) Cut,
                     (char const **) CutDisabled,
                     (char const **) CutAlpha, ETBCutID, 1);
   mCut->SetToolTip(_("Cut selection (to clipboard)"));


   //Copy Button
   mCopy = MakeButton(upPattern, downPattern, hilitePattern,
                      (char const **) Copy,
                      (char const **) CopyDisabled,
                      (char const **) CopyAlpha, ETBCopyID, 28);
   mCopy->SetToolTip(_("Copy selection to clipboard"));


   //Paste Button
   mPaste = MakeButton(upPattern, downPattern, hilitePattern,
                       (char const **) Paste,
                       (char const **) PasteDisabled,
                       (char const **) PasteAlpha, ETBPasteID, 55);
   mPaste->SetToolTip(_("Paste clipboard"));

   //Trim Button
   mTrim = MakeButton(upPattern, downPattern, hilitePattern,
                      (char const **) Trim,
                      (char const **) TrimDisabled,
                      (char const **) TrimAlpha, ETBTrimID, 82);
   mTrim->SetToolTip(_("Trim everything outside selection"));

   //Silence Button
   mSilence = MakeButton(upPattern, downPattern, hilitePattern,
                         (char const **) Silence,
                         (char const **) SilenceDisabled,
                         (char const **) SilenceAlpha, ETBSilenceID, 109);
   mSilence->SetToolTip(_("Insert Silence"));

   //Undo Button
   mUndo = MakeButton(upPattern, downPattern, hilitePattern,
                      (char const **) Undo,
                      (char const **) UndoDisabled,
                      (char const **) UndoAlpha, ETBUndoID, 150);
   mUndo->SetToolTip(_("Undo last action"));

   //Redo Button
   mRedo = MakeButton(upPattern, downPattern, hilitePattern,
                      (char const **) Redo,
                      (char const **) RedoDisabled,
                      (char const **) RedoAlpha, ETBRedoID, 177);
   mRedo->SetToolTip(_("Redo last undo"));

   //Zoom In Button
   mZoomIn = MakeButton(upPattern, downPattern, hilitePattern,
                        (char const **) ZoomIn,
                        (char const **) ZoomInDisabled,
                        (char const **) ZoomInAlpha, ETBZoomInID, 220);
   mZoomIn->SetToolTip(_("Zoom In"));

   //Zoom Out Button
   mZoomOut = MakeButton(upPattern, downPattern, hilitePattern,
                         (char const **) ZoomOut,
                         (char const **) ZoomOutDisabled,
                         (char const **) ZoomOutAlpha, ETBZoomOutID, 247);
   mZoomOut->SetToolTip(_("Zoom Out"));

   //Zoom to selection Button
   mZoomSel = MakeButton(upPattern, downPattern, hilitePattern,
                         (char const **) ZoomSel,
                         (char const **) ZoomSelDisabled,
                         (char const **) ZoomSelAlpha, ETBZoomSelID, 274);
   mZoomSel->SetToolTip(_("Fit selection in window"));

   //Zoom All Button
   mZoomFit = MakeButton(upPattern, downPattern, hilitePattern,
                         (char const **) ZoomFit,
                         (char const **) ZoomFitDisabled,
                         (char const **) ZoomFitAlpha, ETBZoomFitID, 302);
   mZoomFit->SetToolTip(_("Fit entire file in window"));

   delete upPattern;
   delete downPattern;
   delete hilitePattern;
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
   /*
   if (event.KeyCode() == WXK_SPACE) {
      if (gAudioIO->IsBusy()) {
      } else {
      }
      return;
   } else
      event.Skip();
   */
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

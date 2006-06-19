/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.cpp

  Joshua Haberman
  Leland Lucius

**********************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/log.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/intl.h>

#include "HistoryWindow.h"
#include "UndoManager.h"
#include "Project.h"
#include "ShuttleGui.h"

enum {
   ID_LIST = 1000,
   ID_DISCARD
};

BEGIN_EVENT_TABLE(HistoryWindow, wxDialog)
   EVT_SHOW(HistoryWindow::OnShow)
   EVT_SIZE(HistoryWindow::OnSize)
   EVT_CLOSE(HistoryWindow::OnCloseWindow)
   EVT_LIST_ITEM_SELECTED(wxID_ANY, HistoryWindow::OnItemSelected)
   EVT_BUTTON(ID_DISCARD, HistoryWindow::OnDiscard)
END_EVENT_TABLE()

HistoryWindow::HistoryWindow(AudacityProject *parent, UndoManager *manager):
   wxDialog((wxWindow*)parent, wxID_ANY, wxString(_("Undo History")),
      wxDefaultPosition, wxDefaultSize,
      wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxWANTS_CHARS )
{
   mManager = manager;
   mProject = parent;
   mSelected = 0;

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
      mList = S.Id(ID_LIST).AddListControlReportMode();
      mList->InsertColumn(0, _("Action"), wxLIST_FORMAT_LEFT, 250);
      mList->InsertColumn(1, _("Size"), wxLIST_FORMAT_LEFT, 65);
      mList->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(HistoryWindow::OnChar));

      S.SetBorder(1);
      S.StartHorizontalLay(wxALIGN_RIGHT, false);
         S.Id(wxID_CANCEL).AddButton(_("&Close"));
         mDiscard = S.Id(ID_DISCARD).AddButton(_("&Discard"));
      S.EndHorizontalLay();
   S.EndVerticalLay();
   DoUpdate();
   mList->SetMinSize(mList->GetSize());
   Fit();
   SetMinSize(GetSize());
   // ----------------------- End of main section --------------

   mList->SetColumnWidth(0, mList->GetClientSize().x - mList->GetColumnWidth(1));
}

HistoryWindow::~HistoryWindow()
{
   mList->Disconnect(wxEVT_KEY_DOWN, wxKeyEventHandler(HistoryWindow::OnChar));
}

void HistoryWindow::UpdateDisplay()
{
   if(IsShown())
      DoUpdate();
}

void HistoryWindow::DoUpdate()
{
   mList->DeleteAllItems();

   mSelected = mManager->GetCurrentState() - 1;
   for(unsigned int i = 0; i < mManager->GetNumStates(); i++) {
      wxString desc, size;
      wxListItem item;
      
      mManager->GetLongDescription(i + 1, &desc, &size);
      mList->InsertItem(i, desc);
      mList->SetItem(i, 1, size);
   }
   mList->SetItemState(mSelected,
                       wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                       wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

   mList->EnsureVisible(mSelected);

   mDiscard->Enable(mList->GetItemCount()!=0);
}

void HistoryWindow::OnDiscard(wxCommandEvent &event)
{
   int i;

   for (i = mList->GetItemCount() - 1; i >= 0; i--) {
      if (mList->GetItemState(i, wxLIST_STATE_SELECTED)) {
         mProject->SetStateTo(i + 1);
         mManager->RemoveStateAt(i);
         mSelected = i;
      }
   }

   i = mManager->GetNumStates();
   if (i == 0) {
      mProject->TP_PushState(_("History Cleared"),
                             _("History Cleared"),
                             false);
      mSelected = 0;
      i = mManager->GetNumStates();
   }

   if (mSelected >= i)
      mSelected = i - 1;

   mProject->SetStateTo(mSelected + 1);
   UpdateDisplay();
}

void HistoryWindow::OnItemSelected(wxListEvent &event)
{
   mSelected = event.GetIndex();
   mProject->SetStateTo(mSelected + 1);
}  

void HistoryWindow::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
  this->Show(FALSE);
}

void HistoryWindow::OnSize(wxSizeEvent & event)
{
   Layout();
   mList->SetColumnWidth(0, mList->GetClientSize().x - mList->GetColumnWidth(1));
   mList->EnsureVisible(mSelected);
}

void HistoryWindow::OnShow(wxShowEvent & event)
{
   if (event.GetShow())
      UpdateDisplay();
}

void HistoryWindow::OnChar(wxKeyEvent &event)
{
   wxListCtrl *l = wxDynamicCast( event.GetEventObject(), wxListCtrl );

   event.Skip(true);

   switch (event.GetKeyCode())
   {
      case WXK_DELETE:
      case WXK_BACK:
      {
         wxCommandEvent e(wxEVT_COMMAND_BUTTON_CLICKED, ID_DISCARD);
         l->GetEventHandler()->AddPendingEvent(e);

         event.Skip(false);
      }
      break;

      case 'A':
         if (event.ControlDown()) {
            int i;

            for (i = l->GetItemCount() - 1; i >= 0; i--) {
               l->SetItemState(i,
                               wxLIST_STATE_SELECTED,
                               wxLIST_STATE_SELECTED);
            }
            event.Skip(false);
         }
      break;
   }
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
// arch-tag: a32dc48c-50da-4cda-90a6-b5aa4fd7673e


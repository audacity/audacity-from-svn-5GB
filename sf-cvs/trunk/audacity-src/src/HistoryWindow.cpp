/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.cpp

  Joshua Haberman
  Leland Lucius

*******************************************************************//*!

\class HistoryWindow
\brief Works with UndoManager to allow user to see descriptions of 
and undo previous commands.  Also allows you to selectively clear the 
undo memory so as to free up space.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/imaglist.h>
#include <wx/intl.h>
#include <wx/listctrl.h>
#include <wx/settings.h>
#include <wx/textctrl.h>

#include "../images/Arrow.xpm"
#include "HistoryWindow.h"
#include "UndoManager.h"
#include "Project.h"
#include "ShuttleGui.h"

enum {
   ID_LEVELS = 1000,
   ID_DISCARD,
   ID_CLEAR
};

BEGIN_EVENT_TABLE(HistoryWindow, wxDialog)
   EVT_SHOW(HistoryWindow::OnShow)
   EVT_SIZE(HistoryWindow::OnSize)
   EVT_CLOSE(HistoryWindow::OnCloseWindow)
   EVT_LIST_ITEM_SELECTED(wxID_ANY, HistoryWindow::OnItemSelected)
   EVT_BUTTON(ID_CLEAR, HistoryWindow::OnClear)
   EVT_BUTTON(ID_DISCARD, HistoryWindow::OnDiscard)
END_EVENT_TABLE()

HistoryWindow::HistoryWindow(AudacityProject *parent, UndoManager *manager):
   wxDialog((wxWindow*)parent, wxID_ANY, wxString(_("Undo History")),
      wxDefaultPosition, wxDefaultSize,
      wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER ) //| wxWANTS_CHARS )
{
   mManager = manager;
   mProject = parent;
   mSelected = 0;

   wxImageList *imageList = new wxImageList(9, 16);
   imageList->Add(wxIcon(empty_9x16_xpm));
   imageList->Add(wxIcon(arrow_xpm));

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
      S.StartStatic(_("Manage History"), 1);
      {
         mList = S.AddListControlReportMode();
         mList->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(HistoryWindow::OnChar));
         mList->InsertColumn(0, _("Action"), wxLIST_FORMAT_LEFT, 250);
         mList->InsertColumn(1, _("Size"), wxLIST_FORMAT_LEFT, 65);
         mList->SetSingleStyle(wxLC_SINGLE_SEL);

         //Assign rather than set the image list, so that it is deleted later.
         mList->AssignImageList(imageList, wxIMAGE_LIST_SMALL);

         S.StartHorizontalLay(wxALIGN_CENTER, false);
            mLevels = S.Id(ID_LEVELS).AddTextBox(_("&Selected levels"), wxT("0"), 10);
            mLevels->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(HistoryWindow::OnChar));
            mDiscard = S.Id(ID_DISCARD).AddButton(_("&Discard"));
            mClear = S.Id(ID_CLEAR).AddButton(_("&Clear All"));
         S.EndHorizontalLay();
      }
      S.EndStatic();
      S.StartHorizontalLay(wxALIGN_BOTTOM | wxALIGN_RIGHT, false);
         S.Id(wxID_OK).AddButton(_("&OK"));
      S.EndHorizontalLay();
   S.EndVerticalLay();
   // ----------------------- End of main section --------------

   DoUpdate();
   mList->SetMinSize(mList->GetSize());
   Fit();
   SetMinSize(GetSize());
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
   int i;

   mList->DeleteAllItems();

   mSelected = mManager->GetCurrentState() - 1;
   for (i = 0; i < mManager->GetNumStates(); i++) {
      wxString desc, size;
      wxListItem item;
      
      mManager->GetLongDescription(i + 1, &desc, &size);
      mList->InsertItem(i, desc, i == mSelected ? 1 : 0);
      mList->SetItem(i, 1, size);
   }

   mList->SetItemState(mSelected,
                       wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                       wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

   mList->EnsureVisible(mSelected);

   mDiscard->Enable(mList->GetItemCount()!=0);
}

void HistoryWindow::OnClear(wxCommandEvent &event)
{
   mSelected = mList->GetItemCount() - 1;
   OnDiscard(event);
}

void HistoryWindow::OnDiscard(wxCommandEvent &event)
{
   int i;

   for (i = mSelected; i >= 0; i--) {
      mProject->SetStateTo(i + 1);
      mManager->RemoveStateAt(i);
      mSelected = i;
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
   int i;

   mSelected = event.GetIndex();

   for (i = 0; i < mList->GetItemCount(); i++) {
      mList->SetItemImage(i, 0);

      if (i > mSelected)
         mList->SetItemTextColour(i, *wxLIGHT_GREY);
      else
         mList->SetItemTextColour(i, mList->GetTextColour());
   }
   mList->SetItemImage(mSelected, 1);

   mProject->SetStateTo(mSelected + 1);
   mLevels->SetValue(wxString::Format(wxT("%d"), mSelected + 1));
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
   event.Skip(false);

   if (event.GetId() == ID_LEVELS) {
      return;
   }

   wxListCtrl *l = wxDynamicCast( event.GetEventObject(), wxListCtrl );

   switch (event.GetKeyCode())
   {
      case WXK_DELETE:
      case WXK_BACK:
      {
         wxCommandEvent e(wxEVT_COMMAND_BUTTON_CLICKED, ID_DISCARD);
         l->GetEventHandler()->AddPendingEvent(e);
         return;
      }
      break;
   }

   event.Skip(true);
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


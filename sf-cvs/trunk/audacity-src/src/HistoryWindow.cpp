/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/button.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/imaglist.h>
#include <wx/listctrl.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/intl.h>

#include "../images/Arrow.xpm"
#include "HistoryWindow.h"
#include "UndoManager.h"
#include "Project.h"

enum {
   HistoryListID = 1000,
   DiscardID
};

BEGIN_EVENT_TABLE(HistoryWindow, wxDialog)
   EVT_LIST_END_LABEL_EDIT(HistoryListID, HistoryWindow::OnLabelChanged)
   EVT_LIST_ITEM_SELECTED(HistoryListID, HistoryWindow::OnItemSelected)
   EVT_BUTTON(DiscardID, HistoryWindow::OnDiscard)
END_EVENT_TABLE()

HistoryWindow::HistoryWindow(AudacityProject *parent, UndoManager *manager):
wxDialog(parent, -1, _("Undo History"), wxDefaultPosition,
         wxDefaultSize, wxDIALOG_MODELESS | wxCAPTION | wxTHICK_FRAME)
{
   mTopSizer = new wxBoxSizer(wxVERTICAL);

   mList = new wxListCtrl(this, HistoryListID, wxDefaultPosition, wxSize(350, 180),
                          wxLC_REPORT /* | wxLC_EDIT_LABELS */);
   mList->SetSizeHints(350, 180);

   wxImageList *imageList = new wxImageList(24, 24); //TODO: free
   imageList->Add(wxIcon(empty_24x24_xpm));
   imageList->Add(wxIcon(arrow_xpm));
   mList->SetImageList(imageList, wxIMAGE_LIST_SMALL);
   mList->InsertColumn(0, _("Action"), wxLIST_FORMAT_LEFT, 280);
   mList->InsertColumn(1, _("Size"), wxLIST_FORMAT_LEFT);


   mTopSizer->Add(mList, 1, wxGROW|wxALL, 2);
   
   {
      wxStaticBoxSizer *purgeSizer = new wxStaticBoxSizer(
              new wxStaticBox(this, -1, _("Discard undo data")),
              wxVERTICAL);

      wxBoxSizer *firstLine = new wxBoxSizer(wxHORIZONTAL);

      purgeSizer->Add(
         mLevelsAvailable = new wxStaticText(this, -1,
            _("Undo Levels Available (lots and lots)"),
            wxDefaultPosition, wxDefaultSize, 0),
            0, wxALIGN_LEFT|wxTOP|wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 2);

      purgeSizer->Add(firstLine);

      wxBoxSizer *secondLine = new wxBoxSizer(wxHORIZONTAL);

      secondLine->Add(new wxStaticText(this, -1, _("Levels to discard: ")),
                            0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 3);

      secondLine->Add(
         mDiscardNum = new wxSpinCtrl(this, -1, "1", wxDefaultPosition, wxDefaultSize,
                        wxSP_ARROW_KEYS, 1, manager->GetCurrentState() - 1),
         0, wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, 2 );

      secondLine->Add(
         mDiscard = new wxButton(this, DiscardID, _("Discard")),
         0, wxALIGN_RIGHT|wxALL, 2 );

      purgeSizer->Add(secondLine, 0, wxGROW);

      mTopSizer->Add(purgeSizer, 0, wxGROW|wxALL, 3);
   }

   mManager = manager;
   mProject = parent;

   UpdateDisplay();

   SetAutoLayout(true);
   mTopSizer->Fit(this);
   mTopSizer->SetSizeHints(this);
   SetSizer(mTopSizer);
}

void HistoryWindow::UpdateDisplay()
{
   mList->Hide();   // to speed up the update
   
   mList->DeleteAllItems();
   for(unsigned int i = 0; i < mManager->GetNumStates(); i++) {
      wxString desc, size;
      wxListItem item;
      
      mManager->GetDescription(i + 1, &desc, &size);
      mList->InsertItem(i, desc, i == mManager->GetCurrentState() - 1 ? 1 : 0);
      mList->SetItem(i, 1, size);

      if(i > mManager->GetCurrentState() - 1) {
         item.m_itemId = i;
         item.SetTextColour(*wxLIGHT_GREY);
         mList->SetItem(item);
      }
   }

   mList->EnsureVisible(mManager->GetCurrentState() - 1);
   
   /* I may or may not like this, we'll see */
   /*
   wxListItem item;
   item.m_itemId = mSelected;
   item.m_state = wxLIST_STATE_SELECTED;
   mList->SetItem(item);
   */
   
   mList->Show();

   mLevelsAvailable->SetLabel(wxString::Format(_("Undo Levels Available: %d"),
                                              mManager->GetCurrentState() - 1));

   mDiscardNum->SetRange(1, mManager->GetCurrentState() - 1);

   mDiscard->Enable(mManager->GetCurrentState() > 1 ? true : false);

}

void HistoryWindow::OnLabelChanged(wxListEvent &event)
{
   mManager->SetDescription(event.GetIndex() + 1, event.GetItem().m_text);
   UpdateDisplay();
}

void HistoryWindow::OnDiscard(wxCommandEvent &event)
{
   mManager->RemoveStates(mDiscardNum->GetValue());
   UpdateDisplay();
}

void HistoryWindow::OnItemSelected(wxListEvent &event)
{
   mSelected = event.GetIndex();
   mProject->SetStateTo(mSelected + 1);
   UpdateDisplay();
}  

HistoryWindow::~HistoryWindow()
{
}


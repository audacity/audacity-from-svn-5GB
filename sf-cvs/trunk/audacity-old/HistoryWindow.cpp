/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/statbox.h>
#include <wx/stattext.h>

#include "HistoryWindow.h"

enum {
   HistoryListID
};

BEGIN_EVENT_TABLE(HistoryWindow, wxDialog)
   EVT_BUTTON(wxID_OK, HistoryWindow::OnOK)
   EVT_BUTTON(wxID_CANCEL, HistoryWindow::OnCancel)
END_EVENT_TABLE()

HistoryWindow::HistoryWindow(wxWindow *parent, UndoManager *manager):
wxDialog(parent, -1, "Undo History", wxDefaultPosition,
         wxDefaultSize, wxDIALOG_MODAL /* for now */ | wxCAPTION | wxTHICK_FRAME)
{
   mTopSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, "Undo Management"), wxVERTICAL );

   mList = new wxListCtrl(this, HistoryListID, wxDefaultPosition, wxSize(350, 180),
                          wxLC_REPORT | wxLC_EDIT_LABELS);
   mList->SetImageList((wxImageList *)NULL, wxIMAGE_LIST_SMALL);
   mList->InsertColumn(0, "Action", wxLIST_FORMAT_LEFT, 280);
   mList->InsertColumn(1, "Size", wxLIST_FORMAT_LEFT);

   for(unsigned int i = 0; i < manager->GetNumStates(); i++) {
      wxString desc, size;
      manager->GetDescription(i + 1, &desc, &size);
      mList->InsertItem(i, desc, 0);
      mList->SetItem(i, 1, size);
   }

   mTopSizer->Add(mList, 1, wxGROW|wxALL, 2);

   mTopSizer->Add(
      new wxStaticText(this, -1, "Here you can throw away the oldest undo data to free disk space.",
                       wxDefaultPosition, wxDefaultSize, 0),
      0, wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, 2 );

   mTopSizer->Add(
      new wxStaticText(this, -1, wxString::Format("Undo Levels Available: %d",
                                                  manager->GetCurrentState() - 1),
                       wxDefaultPosition, wxDefaultSize, 0),
      0, wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, 2 );
    
   mTopSizer->Add(
      mDiscardNum = new wxSpinCtrl(this, -1, "1", wxDefaultPosition, wxDefaultSize,
                     wxSP_ARROW_KEYS, 1, manager->GetCurrentState() - 1),
      0, wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, 2 );

   mTopSizer->Add(
      new wxButton(this, wxID_OK, "OK"),
      0, wxALIGN_CENTER|wxALL, 2 );

   mTopSizer->Add(
      new wxButton(this, wxID_CANCEL, "Cancel"),
      0, wxALIGN_CENTER|wxALL, 2 );

   mManager = manager;
   SetAutoLayout(true);
   SetSizer(mTopSizer);

   mTopSizer->Fit(this);
   mTopSizer->SetSizeHints(this);
}

void HistoryWindow::OnOK(wxCommandEvent &event)
{
   mManager->RemoveStates(mDiscardNum->GetValue());
   EndModal(0);
}

void HistoryWindow::OnCancel(wxCommandEvent &event)
{
   EndModal(0);
}

HistoryWindow::~HistoryWindow()
{
}


/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.cpp

  Brian Gunlogson

**********************************************************************/

#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/choice.h>
#include <wx/intl.h>

#include "../Prefs.h"
#include "KeyConfigPrefs.h"
#include "../Project.h"

#define HistoryListID     7001
#define DescriptionTextID 7002

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_LIST_ITEM_SELECTED(HistoryListID, KeyConfigPrefs::OnItemSelected)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, _("Configure Keyboard")),
      wxVERTICAL );

   {
      wxBoxSizer *vCategorySizer = new wxBoxSizer(wxVERTICAL);

      // BG: Create list control that will hold the commands supported under the selected category
      mCommandsList = new wxListCtrl(this, HistoryListID, wxDefaultPosition, wxSize(200, 180),
                                         wxLC_REPORT /* | wxLC_EDIT_LABELS */);

      mCommandsList->SetSizeHints(200, 180);

      mCommandsList->InsertColumn(0, _("Commands"), wxLIST_FORMAT_LEFT, 200);

      AudacityProject *mAud = GetActiveProject();
      for(int i = 0; i < mAud->GetNumCommands(); i++)
      {
         mCommandsList->InsertItem(i, mAud->GetCommandName(i));
      }

      vCategorySizer->Add(mCommandsList, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      vCategorySizer->Add(
               new wxStaticText(this, -1, _(/*"Description:\n Nothing selected."*/"THIS CODE IS A WORK IN PROGRESS")), 0,
               wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

      topSizer->Add(
         vCategorySizer, 0, 
         wxALIGN_CENTER_VERTICAL|wxALL, TOP_LEVEL_BORDER );
   }

   SetAutoLayout(true);
   SetSizer(topSizer);

   topSizer->Fit(this);
   topSizer->SetSizeHints(this);
}

void KeyConfigPrefs::OnItemSelected(wxListEvent &event)
{
   int sel = event.GetIndex();
}  

bool KeyConfigPrefs::Apply()
{
   return true;
}

KeyConfigPrefs::~KeyConfigPrefs()
{
}

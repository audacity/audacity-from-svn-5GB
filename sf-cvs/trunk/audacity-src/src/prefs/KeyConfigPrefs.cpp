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

#define CategoryChoiceID  7001
#define HistoryListID     7002
#define DescriptionTextID 7003

#define NUM_CATEGORIES 3
wxString categories[] = { "File", "View", "Audio" };

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_CHOICE(CategoryChoiceID,          KeyConfigPrefs::OnFormatChoice)
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
      wxBoxSizer *hTopCategorySizer = new wxBoxSizer(wxHORIZONTAL);

      hTopCategorySizer->Add(
               new wxStaticText(this, -1, _("Category:")), 0,
               wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   #ifdef __WXMAC__
     // This is just to work around a wxChoice auto-sizing bug
     mSelectedCategory = new wxChoice(
        this, -1, wxDefaultPosition, wxSize(200,-1),
        NUM_CATEGORIES, categories);
   #else
     mSelectedCategory = new wxChoice(
        this, -1, wxDefaultPosition, wxDefaultSize,
        NUM_CATEGORIES, categories);
   #endif

      mSelectedCategory->SetSelection(0);

      hTopCategorySizer->Add(mSelectedCategory, 0,
                          wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

      vCategorySizer->Add(hTopCategorySizer, 0,
                          wxALL|wxGROW, GENERIC_CONTROL_BORDER);


      // BG: Create list control that will hold the commands supported under the selected category
      mCategoryCommands = new wxListCtrl(this, HistoryListID, wxDefaultPosition, wxSize(200, 180),
                                         wxLC_REPORT /* | wxLC_EDIT_LABELS */);

      mCategoryCommands->SetSizeHints(200, 180);

      mCategoryCommands->InsertColumn(0, _("Commands"), wxLIST_FORMAT_LEFT, 194);

      vCategorySizer->Add(mCategoryCommands, 0,
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

void KeyConfigPrefs::OnFormatChoice(wxCommandEvent& evt)
{
   int sel = mSelectedCategory->GetSelection();
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

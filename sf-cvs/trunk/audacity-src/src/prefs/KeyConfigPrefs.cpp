/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.cpp

  Brian Gunlogson

**********************************************************************/

#include <wx/defs.h>
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

#define CommandsListID    7001
#define DescriptionTextID 7002
#define KeysListID        7003
#define CurrentComboID    7004
#define AddComboButtonID  7005

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_LIST_ITEM_SELECTED(CommandsListID, KeyConfigPrefs::OnItemSelected)
   EVT_BUTTON(AddComboButtonID, KeyConfigPrefs::AddComboToList)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent):
PrefsPanel(parent), mCommandSelected(-1)
{
   mAudacity = GetActiveProject();

   topSizer = new wxBoxSizer( wxVERTICAL );

   {
      wxBoxSizer *vKeyConfigSizer = new wxBoxSizer(wxHORIZONTAL);

      wxBoxSizer *vCommandSizer = new wxBoxSizer(wxVERTICAL);

      // BG: Create list control that will hold the commands supported under the selected category
      mCommandsList = new wxListCtrl(this, CommandsListID, wxDefaultPosition, wxSize(200, 180),
                                         wxLC_REPORT | wxLC_SINGLE_SEL /* | wxLC_EDIT_LABELS */);

      mCommandsList->SetSizeHints(200, 180);

      mCommandsList->InsertColumn(0, _("Commands"), wxLIST_FORMAT_LEFT, 200);

      //Insert supported commands into list control
      for(int i = 0; i < mAudacity->GetNumCommands(); i++)
      {
         mCommandsList->InsertItem(i, mAudacity->GetCommandName(i));
      }

      vCommandSizer->Add(mCommandsList, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      vCommandSizer->Add(
               new wxStaticText(this, DescriptionTextID, _("Description:\n Nothing selected.")), 0,
               wxALIGN_LEFT|wxALL, GENERIC_CONTROL_BORDER);

      vKeyConfigSizer->Add(
         vCommandSizer, 0, 
         wxALL, TOP_LEVEL_BORDER );

      wxBoxSizer *vKeySizer = new wxBoxSizer(wxVERTICAL);
      wxBoxSizer *hKeySizer = new wxBoxSizer(wxHORIZONTAL);

      // BG: Create list control that will hold the commands supported under the selected category
      mKeysList = new wxListCtrl(this, KeysListID, wxDefaultPosition, wxSize(200, 180),
                                         wxLC_REPORT /* | wxLC_EDIT_LABELS */);

      mKeysList->SetSizeHints(200, 180);

      mKeysList->InsertColumn(0, _("Keys"), wxLIST_FORMAT_LEFT, 200);

      vKeySizer->Add(mKeysList, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      mCurrentComboText = NULL;
      mCurrentComboText = new SysKeyTextCtrl(
         this, CurrentComboID, "",
         wxDefaultPosition, wxSize(115, -1), 0 );

      hKeySizer->Add(mCurrentComboText, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      wxButton *addComboButton =
         new wxButton(this, AddComboButtonID, _("Add"));

      hKeySizer->Add(addComboButton, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      vKeySizer->Add(
         hKeySizer, 0, 
         wxALL, 0 );

      vKeyConfigSizer->Add(
         vKeySizer, 0, 
         wxALL, TOP_LEVEL_BORDER );

      topSizer->Add(
         vKeyConfigSizer, 0, 
         wxALIGN_LEFT|wxALL, TOP_LEVEL_BORDER );
   }

   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);

   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
}

void KeyConfigPrefs::OnItemSelected(wxListEvent &event)
{
   wxWindow *wDescLabel = FindWindow(DescriptionTextID);
   mCommandSelected = event.GetIndex();

   if(wDescLabel)
   {
      // BG: Set the description
      wDescLabel->SetLabel("Description:\n " + mAudacity->GetCommandDesc(mCommandSelected));
   }

/*
   // BG: Test the function
   (this->*((wxEventFunction) (mAudacity->GetCommandFunc(mCommandSelected))))(event);
*/
}

void KeyConfigPrefs::AddComboToList(wxCommandEvent& event)
{
   wxString comboString = mCurrentComboText->GetValue();

   for(int i = 0; i < mKeysList->GetItemCount(); i++)
   {
      if(comboString == mKeysList->GetItemText(i))
         return;
   }

   mKeysList->InsertItem(mKeysList->GetItemCount(), comboString);
}

bool KeyConfigPrefs::Apply()
{
   mAudacity->RebuildMenuBar();

   return true;
}

wxListCtrl *KeyConfigPrefs::GetKeysList()
{
   return mKeysList;
}

KeyConfigPrefs::~KeyConfigPrefs()
{
}


//BG: A quick and dirty override of wxTextCtrl to capture keys like Ctrl, Alt

BEGIN_EVENT_TABLE(SysKeyTextCtrl, wxTextCtrl)
   EVT_KEY_DOWN(SysKeyTextCtrl::OnKey)
   EVT_CHAR(SysKeyTextCtrl::OnChar)
END_EVENT_TABLE()

SysKeyTextCtrl::SysKeyTextCtrl(wxWindow *parent, wxWindowID id,
                              const wxString& value,
                              const wxPoint& pos,
                              const wxSize& size,
                              long style,
                              const wxValidator& validator,
                              const wxString& name) : wxTextCtrl(parent, id, value, pos, size, style, validator, name)
{
}

SysKeyTextCtrl::~SysKeyTextCtrl()
{
}

//BG: It works on Windows, but we need to trap WM_CHAR
//DM: On Linux, now it works except for Ctrl+3...Ctrl+8
void SysKeyTextCtrl::OnKey(wxKeyEvent& event)
{
   wxString newStr = "";

   long key = event.GetKeyCode();

   if(event.ControlDown())
      newStr += "Ctrl+";

   if(event.AltDown())
      newStr += "Alt+";

   if(event.ShiftDown())
      newStr += "Shift+";

   if (event.ControlDown() && key >= 1 && key <= 26)
      newStr += (char)(64 + key);
   else if (key >= 33 && key <= 126)
      newStr += (char)key;
   else if (key == WXK_BACK)
      newStr = "Backspace";
   else if (key == WXK_DELETE)
      newStr = "Delete";
   else if (key == WXK_SPACE)
      newStr = "Spacebar";
   else
      return; // Don't change it if we don't recognize the key

   SetValue(newStr);
}

//BG: Trap WM_CHAR
void SysKeyTextCtrl::OnChar(wxKeyEvent& event)
{
}

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

#define CommandsListID         7001
#define DescriptionTextID      7002
#define KeysListID             7003
#define CurrentComboID         7004
#define RemoveComboButtonID    7005
#define ClearComboButtonID     7006
#define AddComboButtonID       7007
#define AssignDefaultsButtonID 7008

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_LIST_ITEM_SELECTED(CommandsListID, KeyConfigPrefs::OnItemSelected)
   EVT_BUTTON(RemoveComboButtonID, KeyConfigPrefs::RemoveComboFromList)
   EVT_BUTTON(ClearComboButtonID, KeyConfigPrefs::ClearComboList)
   EVT_BUTTON(AddComboButtonID, KeyConfigPrefs::AddComboToList)
   EVT_BUTTON(AssignDefaultsButtonID, KeyConfigPrefs::AssignDefaults)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent):
PrefsPanel(parent), mCommandSelected(-1)
{
   mAudacity = GetActiveProject();
   if (!mAudacity)
      return;

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
               new wxStaticText(this, DescriptionTextID,
                  _("Description:\n ") + wxString(_("Nothing selected."))),
               0, wxALIGN_LEFT|wxALL, GENERIC_CONTROL_BORDER);

      vKeyConfigSizer->Add(
         vCommandSizer, 0, 
         wxALL, TOP_LEVEL_BORDER );

      wxBoxSizer *vKeySizer = new wxBoxSizer(wxVERTICAL);
      wxBoxSizer *hKeySizer1 = new wxBoxSizer(wxHORIZONTAL);
      wxBoxSizer *hKeySizer2 = new wxBoxSizer(wxHORIZONTAL);

      // BG: Create list control that will hold the commands supported under the selected category
      mKeysList = new wxListCtrl(this, KeysListID, wxDefaultPosition, wxSize(200, 180),
                                         wxLC_REPORT /* | wxLC_EDIT_LABELS */);

      mKeysList->SetSizeHints(200, 180);

      mKeysList->InsertColumn(0, _("Keys"), wxLIST_FORMAT_LEFT, 200);

      vKeySizer->Add(mKeysList, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      hKeySizer1->Add(new wxButton(this, RemoveComboButtonID, _("Remove")), 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      hKeySizer1->Add(new wxButton(this, ClearComboButtonID, _("Clear")), 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      vKeySizer->Add(
         hKeySizer1, 0, 
         wxALL, 0 );

      mCurrentComboText = NULL;
      mCurrentComboText = new SysKeyTextCtrl(
         this, CurrentComboID, "",
         wxDefaultPosition, wxSize(115, -1), 0 );

      hKeySizer2->Add(mCurrentComboText, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      hKeySizer2->Add(new wxButton(this, AddComboButtonID, _("Add")), 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      vKeySizer->Add(
         hKeySizer2, 0, 
         wxALL, 0 );

      vKeySizer->Add(new wxButton(this, AssignDefaultsButtonID, _("Assign Defaults")), 0,
                          wxALIGN_CENTER_HORIZONTAL|wxGROW|wxALL, GENERIC_CONTROL_BORDER);

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

   mKeysList->DeleteAllItems();

   if(mAudacity->GetMenuType(mCommandSelected) == typeSeparator)
   {
      if(wDescLabel)
         wDescLabel->SetLabel("");

      return;
   }

   UpdateKeyList();

   if(wDescLabel)
   {
      // BG: Set the description
      wDescLabel->SetLabel(_("Description:\n ")
            + mAudacity->GetCommandDesc(mCommandSelected));
   }

/*
   // BG: Test the function
   (this->*((wxEventFunction) (mAudacity->GetCommandFunc(mCommandSelected))))(event);
*/
}

void KeyConfigPrefs::UpdateKeyList()
{
   gPrefs->SetPath("/Keyboard/" + wxString::Format("%i", mCommandSelected));

   long keyIndex;
   wxString keyString;

   if(gPrefs->GetFirstEntry(keyString, keyIndex))
   {
      mKeysList->InsertItem(0, keyString);
      while(gPrefs->GetNextEntry(keyString, keyIndex))
      {
         mKeysList->InsertItem(mKeysList->GetItemCount(), keyString);
      }
   }

   gPrefs->SetPath("/");
}

void KeyConfigPrefs::RemoveComboFromList(wxCommandEvent& event)
{
   long item = -1;

   for ( ;; )
   {
      item = mKeysList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);

      if ( item == -1 )
         break;

      gPrefs->DeleteEntry("/Keyboard/" + wxString::Format("%i", mCommandSelected) + "/" + mKeysList->GetItemText(item), true);

      mKeysList->DeleteItem(item);
   }

   mAudacity->TokenizeCommandStrings(mCommandSelected);
}

void KeyConfigPrefs::ClearComboList(wxCommandEvent& event)
{
   gPrefs->DeleteGroup("/Keyboard/" + wxString::Format("%i", mCommandSelected));
   mKeysList->DeleteAllItems();
   mAudacity->TokenizeCommandStrings(mCommandSelected);
}

void KeyConfigPrefs::AddComboToList(wxCommandEvent& event)
{
   wxString comboString = mCurrentComboText->GetValue();

   //BG: Cannot add blank key or empty category or seperator items
   if((!comboString.length()) || (mCommandSelected < 0) || (mAudacity->GetMenuType(mCommandSelected) == typeSeparator))
      return;

   //BG: Check to see if shortcut is in use
   if(mAudacity->FindCommandByCombos(comboString) >= 0)
      return;

   mKeysList->InsertItem(mKeysList->GetItemCount(), comboString);

   gPrefs->Write("/Keyboard/" + wxString::Format("%i", mCommandSelected) + "/" + comboString, (long)0);

   mAudacity->TokenizeCommandStrings(mCommandSelected);

   mCurrentComboText->SetValue("");
}

void KeyConfigPrefs::AssignDefaults(wxCommandEvent& event)
{
   mKeysList->DeleteAllItems();
   mAudacity->AssignDefaults();
   UpdateKeyList();
}

bool KeyConfigPrefs::Apply()
{
   mAudacity = GetActiveProject();

   if (mAudacity)	
      mAudacity->RebuildMenuBar();

   return true;
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
                              const wxString& name)
: wxTextCtrl(parent, id, value, pos, size, style, validator, name)
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
   else
   {
      switch(key)
      {
      case WXK_BACK:
         newStr += "Backspace";
         break;
      case WXK_DELETE:
         newStr += "Delete";
         break;
      case WXK_SPACE:
         newStr += "Spacebar";
         break;
      case WXK_PRIOR:
         newStr += "PageUp";
         break;
      case WXK_NEXT:
         newStr += "PageDown";
         break;
      case WXK_END:
         newStr += "End";
         break;
      case WXK_HOME:
         newStr += "Home";
         break;
      case WXK_LEFT:
         newStr += "Left";
         break;
      case WXK_UP:
         newStr += "Up";
         break;
      case WXK_RIGHT:
         newStr += "Right";
         break;
      case WXK_DOWN:
         newStr += "Down";
         break;
      case WXK_INSERT:
         newStr += "Insert";
         break;
      case WXK_NUMPAD0:
         newStr += "NUMPAD0";
         break;
      case WXK_NUMPAD1:
         newStr += "NUMPAD1";
         break;
      case WXK_NUMPAD2:
         newStr += "NUMPAD2";
         break;
      case WXK_NUMPAD3:
         newStr += "NUMPAD3";
         break;
      case WXK_NUMPAD4:
         newStr += "NUMPAD4";
         break;
      case WXK_NUMPAD5:
         newStr += "NUMPAD5";
         break;
      case WXK_NUMPAD6:
         newStr += "NUMPAD6";
         break;
      case WXK_NUMPAD7:
         newStr += "NUMPAD7";
         break;
      case WXK_NUMPAD8:
         newStr += "NUMPAD8";
         break;
      case WXK_NUMPAD9:
         newStr += "NUMPAD9";
         break;
      case WXK_MULTIPLY:
         newStr += "*";
         break;
      case WXK_ADD:
         newStr += "+";
         break;
      case WXK_SUBTRACT:
         newStr += "-";
         break;
      case WXK_DECIMAL:
         newStr += ".";
         break;
      case WXK_DIVIDE:
         newStr += "/";
         break;
      case WXK_F1:
         newStr += "F1";
         break;
      case WXK_F2:
         newStr += "F2";
         break;
      case WXK_F3:
         newStr += "F3";
         break;
      case WXK_F4:
         newStr += "F4";
         break;
      case WXK_F5:
         newStr += "F5";
         break;
      case WXK_F6:
         newStr += "F6";
         break;
      case WXK_F7:
         newStr += "F7";
         break;
      case WXK_F8:
         newStr += "F8";
         break;
      case WXK_F9:
         newStr += "F9";
         break;
      case WXK_F10:
         newStr += "F10";
         break;
      case WXK_F11:
         newStr += "F11";
         break;
      case WXK_F12:
         newStr += "F12";
         break;
      case WXK_F13:
         newStr += "F13";
         break;
      case WXK_F14:
         newStr += "F14";
         break;
      case WXK_F15:
         newStr += "F15";
         break;
      case WXK_F16:
         newStr += "F16";
         break;
      case WXK_F17:
         newStr += "F17";
         break;
      case WXK_F18:
         newStr += "F18";
         break;
      case WXK_F19:
         newStr += "F19";
         break;
      case WXK_F20:
         newStr += "F20";
         break;
      case WXK_F21:
         newStr += "F21";
         break;
      case WXK_F22:
         newStr += "F22";
         break;
      case WXK_F23:
         newStr += "F23";
         break;
      case WXK_F24:
         newStr += "F24";
         break;
      case WXK_PAGEUP:
         newStr += "PageUp";
         break;
      case WXK_PAGEDOWN:
         newStr += "PageDown";
         break;
      case WXK_NUMPAD_ENTER:
         newStr += "NUMPAD_ENTER";
         break;
      case WXK_NUMPAD_F1:
         newStr += "NUMPAD_F1";
         break;
      case WXK_NUMPAD_F2:
         newStr += "NUMPAD_F2";
         break;
      case WXK_NUMPAD_F3:
         newStr += "NUMPAD_F3";
         break;
      case WXK_NUMPAD_F4:
         newStr += "NUMPAD_F4";
         break;
      case WXK_NUMPAD_HOME:
         newStr += "NUMPAD_HOME";
         break;
      case WXK_NUMPAD_LEFT:
         newStr += "NUMPAD_LEFT";
         break;
      case WXK_NUMPAD_UP:
         newStr += "NUMPAD_UP";
         break;
      case WXK_NUMPAD_RIGHT:
         newStr += "NUMPAD_RIGHT";
         break;
      case WXK_NUMPAD_DOWN:
         newStr += "NUMPAD_DOWN";
         break;
      case WXK_NUMPAD_PRIOR:
         newStr += "NUMPAD_PAGEUP";
         break;
      case WXK_NUMPAD_PAGEUP:
         newStr += "NUMPAD_PAGEUP";
         break;
      case WXK_NUMPAD_NEXT:
         newStr += "NUMPAD_PAGEDOWN";
         break;
      case WXK_NUMPAD_PAGEDOWN:
         newStr += "NUMPAD_PAGEDOWN";
         break;
      case WXK_NUMPAD_END:
         newStr += "NUMPAD_END";
         break;
      case WXK_NUMPAD_BEGIN:
         newStr += "NUMPAD_HOME";
         break;
      case WXK_NUMPAD_INSERT:
         newStr += "NUMPAD_INSERT";
         break;
      case WXK_NUMPAD_DELETE:
         newStr += "NUMPAD_DELETE";
         break;
      case WXK_NUMPAD_EQUAL:
         newStr += "NUMPAD_EQUAL";
         break;
      case WXK_NUMPAD_MULTIPLY:
         newStr += "NUMPAD_MULTIPLY";
         break;
      case WXK_NUMPAD_ADD:
         newStr += "NUMPAD_ADD";
         break;
      case WXK_NUMPAD_SUBTRACT:
         newStr += "NUMPAD_SUBTRACT";
         break;
      case WXK_NUMPAD_DECIMAL:
         newStr += "NUMPAD_DECIMAL";
         break;
      case WXK_NUMPAD_DIVIDE:
         newStr += "NUMPAD_DIVIDE";
         break;
      default:
         return; // Don't change it if we don't recognize the key
      }
   }

   SetValue(newStr);
}

//BG: Trap WM_CHAR
void SysKeyTextCtrl::OnChar(wxKeyEvent& event)
{
}

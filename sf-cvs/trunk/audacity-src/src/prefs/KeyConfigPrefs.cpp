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
#include <wx/msgdlg.h>

#include "../Prefs.h"
#include "KeyConfigPrefs.h"
#include "../commands/CommandsCfg.h"

#define AssignDefaultsButtonID  7001
#define CurrentComboID          7002
#define RebuildMenusButtonID    7003
#define ChooseCmdsCfgLocationID 7004

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_BUTTON(AssignDefaultsButtonID, KeyConfigPrefs::AssignDefaults)
   EVT_BUTTON(RebuildMenusButtonID, KeyConfigPrefs::RebuildMenus)
   EVT_BUTTON(ChooseCmdsCfgLocationID, KeyConfigPrefs::CmdsCfgLocation)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   mAudacity = GetActiveProject();
   if (!mAudacity)
      return;

   topSizer = new wxBoxSizer( wxVERTICAL );

   {
      //Add label
      topSizer->Add(
               new wxStaticText(this, -1,
                  _("This code has undergone a rewrite. The GUI is not complete.\nYou can still define custom keybindings; edit the 'Audacity-Commands.xml' file.\nIf you need to know what a specific key combo is called, type it in the box below.\nClick Assign Defaults to revert the menus back to default.\nClick Rebuild Menus to reparse 'Audacity-Commands.xml' and rebuild the menus.")),
               0, wxALIGN_LEFT|wxALL, GENERIC_CONTROL_BORDER);

      //Add key combo text box
      mCurrentComboText = NULL;
      mCurrentComboText = new SysKeyTextCtrl(
         this, CurrentComboID, "",
         wxDefaultPosition, wxSize(115, -1), 0 );

      topSizer->Add(mCurrentComboText, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      //Add assign defaults button
      topSizer->Add(new wxButton(this, AssignDefaultsButtonID, _("Assign Defaults")), 0,
                          wxALIGN_CENTER_HORIZONTAL|wxGROW|wxALL, GENERIC_CONTROL_BORDER);

      //Add rebuild menus button
      topSizer->Add(new wxButton(this, RebuildMenusButtonID, _("Rebuild Menus")), 0,
                          wxALIGN_CENTER_HORIZONTAL|wxGROW|wxALL, GENERIC_CONTROL_BORDER);

      //Add change commands.cfg location button
      topSizer->Add(new wxButton(this, ChooseCmdsCfgLocationID, _("Change Audacity-Commands.xml Location")), 0,
                          wxALIGN_CENTER_HORIZONTAL|wxGROW|wxALL, GENERIC_CONTROL_BORDER);

      bool bFalse = false;
      //Check commands.cfg location status
      wxString locationStatus;
      if(gPrefs->Read("/QDeleteCmdCfgLocation", &bFalse))
         locationStatus += wxString("\n* ") + wxString(_("Queued for location change"));
      if(gPrefs->Read("/DeleteCmdCfgLocation", &bFalse))
         locationStatus += wxString("\n* ") + wxString(_("Location change failed"));

      //Add label
      topSizer->Add(
               new wxStaticText(this, -1,
                  gCommandsCfgLocation + locationStatus),
               0, wxALIGN_LEFT|wxALL, GENERIC_CONTROL_BORDER);
   }

   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);

   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
}

void KeyConfigPrefs::AssignDefaults(wxCommandEvent& event)
{
   for(unsigned int i = 0; i < gAudacityProjects.GetCount(); i++)
   {
      if(gAudacityProjects[i])
      {
         gAudacityProjects[i]->SetMenuBar(NULL);
         gAudacityProjects[i]->GetCommands()->AssignDefaults();
         gAudacityProjects[i]->SetMenuBar(gAudacityProjects[i]->GetCommands()->GetMenuBar("appmenu"));
         gAudacityProjects[i]->UpdateMenus();
      }
   }
}

void KeyConfigPrefs::RebuildMenus(wxCommandEvent& event)
{
   for(unsigned int i = 0; i < gAudacityProjects.GetCount(); i++)
   {
      if(gAudacityProjects[i])
      {
         gAudacityProjects[i]->SetMenuBar(NULL);
         gAudacityProjects[i]->GetCommands()->Reparse();
         gAudacityProjects[i]->SetMenuBar(gAudacityProjects[i]->GetCommands()->GetMenuBar("appmenu"));
         gAudacityProjects[i]->UpdateMenus();
      }
   }
}

void KeyConfigPrefs::CmdsCfgLocation(wxCommandEvent& event)
{
   gPrefs->Write("/QDeleteCmdCfgLocation", true);
   wxMessageBox(_("Audacity needs to be restarted for this change to take effect.\nPlease close all open Audacity projects."));
}

bool KeyConfigPrefs::Apply()
{
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

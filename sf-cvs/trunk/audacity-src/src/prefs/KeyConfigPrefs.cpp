/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.cpp

  Brian Gunlogson
  Dominic Mazzoni

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
#include <wx/menuitem.h>

#include "../Prefs.h"
#include "../commands/CommandManager.h"
#include "../commands/Keyboard.h"

#include "KeyConfigPrefs.h"

#define AssignDefaultsButtonID  7001
#define CurrentComboID          7002
#define SetButtonID             7003
#define ClearButtonID           7004
#define CommandsListID          7005


// The numbers of the columns of the mList.
enum { BlankColumn=0, CommandColumn=1, KeyComboColumn=2};

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_BUTTON(AssignDefaultsButtonID, KeyConfigPrefs::AssignDefaults)
   EVT_BUTTON(SetButtonID, KeyConfigPrefs::OnSet)
   EVT_BUTTON(ClearButtonID, KeyConfigPrefs::OnClear)
   EVT_LIST_ITEM_SELECTED(CommandsListID, KeyConfigPrefs::OnItemSelected)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   AudacityProject *project = GetActiveProject();
   if (!project)
      return;
   mManager = project->GetCommandManager();

   topSizer = new wxBoxSizer( wxVERTICAL );

   // This code for displaying keybindings is similar to code in MousePrefs.
   // Would be nice to create a new 'Bindings' class which both 
   // KeyConfigPrefs and MousePrefs use.
   mList = new wxListCtrl( this, CommandsListID ,
      wxDefaultPosition, wxSize(500,250),
      wxLC_REPORT | wxLC_HRULES | wxLC_VRULES | wxSUNKEN_BORDER 
      );

   wxASSERT( mList );

   //An empty first column is a workaround - under Win98 the first column 
   //can't be right aligned.
   mList->InsertColumn(BlankColumn,    _T(""), wxLIST_FORMAT_LEFT );
   mList->InsertColumn(CommandColumn,  _("Command"),  wxLIST_FORMAT_RIGHT );
   mList->InsertColumn(KeyComboColumn, _("Key Combination"), wxLIST_FORMAT_LEFT );

   RepopulateBindingsList();

   mList->SetColumnWidth( BlankColumn, 0 ); // First column width is zero, to hide it.
   // Would like to use wxLIST_AUTOSIZE but
   // wxWindows does not look at the size of column heading.
//   mList->SetColumnWidth( CommandColumn, 250 );
   mList->SetColumnWidth( CommandColumn, wxLIST_AUTOSIZE );
   mList->SetColumnWidth( KeyComboColumn, 115 );

   topSizer->Add( mList, 1, 
                  wxGROW | wxALL, GENERIC_CONTROL_BORDER);

   //Add key combo text box
   mCurrentComboText = new SysKeyTextCtrl(
      this, CurrentComboID, "",
      wxDefaultPosition, wxSize(115, -1), 0 );

   wxButton *pSetButton = new wxButton(this, SetButtonID, _("Set"));
   wxButton *pClearButton = new wxButton(this, ClearButtonID, _("Clear"));

   wxBoxSizer * pComboLabelSizer = new wxBoxSizer( wxHORIZONTAL );

   pComboLabelSizer->Add( mCurrentComboText, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   pComboLabelSizer->Add( pSetButton, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   pComboLabelSizer->Add( pClearButton, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   topSizer->Add(pComboLabelSizer, 0,
                       wxALL, GENERIC_CONTROL_BORDER);

   //Add assign defaults button
   topSizer->Add(new wxButton(this, AssignDefaultsButtonID, _("Assign Defaults")), 0,
                 wxALL, GENERIC_CONTROL_BORDER);

   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);

   outSizer->Fit(this);
   outSizer->SetSizeHints(this);

   mCommandSelected = -1;
}

void KeyConfigPrefs::OnSet(wxCommandEvent& event)
{
   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount())
      return;

   mList->SetItem( mCommandSelected, KeyComboColumn, mCurrentComboText->GetValue() );
}

void KeyConfigPrefs::OnClear(wxCommandEvent& event)
{
   mCurrentComboText->Clear();

   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount())
      return;

   mList->SetItem( mCommandSelected, KeyComboColumn, "" );
}

void KeyConfigPrefs::OnItemSelected(wxListEvent &event)
{
   mCommandSelected = event.GetIndex();
   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount()) {
      mCurrentComboText->SetLabel("");
      return;
   }
   
   wxString key = mManager->GetKeyFromName(mNames[mCommandSelected]);
   mCurrentComboText->Clear();
   mCurrentComboText->AppendText(key);
   mCurrentComboText->SetFocus();
}

void KeyConfigPrefs::RepopulateBindingsList()
{
   mList->DeleteAllItems(); // Delete contents, but not the column headers.
   mNames.Clear();
   mManager->GetAllCommandNames(mNames, false);
   unsigned int i;
   for(i=0; i<mNames.GetCount(); i++) {
      mList->InsertItem( i, _T("") );
      wxString label = mManager->GetLabelFromName(mNames[i]);
      label = wxMenuItem::GetLabelFromText(label.BeforeFirst('\t'));
      mList->SetItem( i, CommandColumn, label );
      mList->SetItem( i, KeyComboColumn, mManager->GetKeyFromName(mNames[i]) );
   }
}


void KeyConfigPrefs::AssignDefaults(wxCommandEvent& event)
{
   unsigned int i;

   for(i=0; i<mNames.GetCount(); i++) {
      mList->SetItem( i, 1, mManager->GetDefaultKeyFromName(mNames[i]) );
   }
}

bool KeyConfigPrefs::Apply()
{
   unsigned int i;
   wxListItem item;

   gPrefs->SetPath("/NewKeys");

   //
   // Only store the key in the preferences if it's different 
   // than the default value.
   //

   item.m_col = KeyComboColumn;
   item.m_mask = wxLIST_MASK_TEXT;
   for(i=0; i<mNames.GetCount(); i++) {
      item.SetId( i );
      mList->GetItem(item);
      wxString name = mNames[i];
      wxString key = item.m_text;
      wxString defaultKey = mManager->GetDefaultKeyFromName(name);

      if (gPrefs->HasEntry(name)) {
         wxString oldKey = gPrefs->Read(name, key);
         if (oldKey != key)  {
            gPrefs->Write(name, key);
         }
         if (key == defaultKey)   {
            gPrefs->DeleteEntry(name);
         }
      }
      else {
         if (key != defaultKey){
            gPrefs->Write(name, key);
         }
      }
   }

   gPrefs->SetPath("/");

   for(i=0; i<gAudacityProjects.GetCount(); i++)
      if(gAudacityProjects[i])
         gAudacityProjects[i]->RebuildMenuBar();

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
//DM: On Linux, now it works except for Ctrl+3...Ctrl+8 (April/2003)
void SysKeyTextCtrl::OnKey(wxKeyEvent& event)
{
   SetValue(KeyEventToKeyString(event));
}

//BG: Trap WM_CHAR
void SysKeyTextCtrl::OnChar(wxKeyEvent& event)
{
}

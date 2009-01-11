/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.cpp

  Brian Gunlogson
  Dominic Mazzoni
  James Crook

*******************************************************************//*!

\class KeyConfigPrefs
\brief A PrefsPanel for keybindings.

The code for displaying keybindings is similar to code in MousePrefs.
It would be nice to create a new 'Bindings' class which both 
KeyConfigPrefs and MousePrefs use.

*//*********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/intl.h>
#include <wx/filedlg.h>

#include "../Prefs.h"
#include "../commands/CommandManager.h"
#include "../commands/Keyboard.h"
#include "../xml/XMLFileReader.h"

#include "../Internat.h"
#include "../ShuttleGui.h"
#include "KeyConfigPrefs.h"

#include "FileDialog.h"

#define AssignDefaultsButtonID  7001
#define CurrentComboID          7002
#define SetButtonID             7003
#define ClearButtonID           7004
#define CommandsListID          7005
#define SaveButtonID            7006
#define LoadButtonID            7007

// The numbers of the columns of the mList.
enum { BlankColumn=0, CommandColumn=1, KeyComboColumn=2};

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_BUTTON(AssignDefaultsButtonID, KeyConfigPrefs::OnDefaults)
   EVT_BUTTON(SetButtonID, KeyConfigPrefs::OnSet)
   EVT_BUTTON(ClearButtonID, KeyConfigPrefs::OnClear)
   EVT_BUTTON(SaveButtonID, KeyConfigPrefs::OnSave)
   EVT_BUTTON(LoadButtonID, KeyConfigPrefs::OnLoad)
   EVT_LIST_ITEM_SELECTED(CommandsListID, KeyConfigPrefs::OnItemSelected)
   EVT_LIST_KEY_DOWN(CommandsListID, KeyConfigPrefs::OnKeyDown)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   SetLabel(_("Keyboard"));         // Provide visual label
   SetName(_("Keyboard"));          // Provide audible label
   Populate( );
}

void KeyConfigPrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.
   // These steps set up the pointer to the command manager...
   AudacityProject *project = GetActiveProject();
   if (!project)
      return;
   mManager = project->GetCommandManager();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
   CreateList();
   mCommandSelected = -1;
}

/// Normally in classes derived from PrefsPanel this function 
/// is used both to populate the panel and to exchange data with it.
/// With KeyConfigPrefs all the exchanges are handled specially,
/// so this is only used in populating the panel.
void KeyConfigPrefs::PopulateOrExchange( ShuttleGui & S )
{
   S.StartStatic( _("Key Bindings"), 1 );
   mList = S.Id( CommandsListID ).AddListControlReportMode();
   S.StartHorizontalLay( wxALIGN_LEFT, 0);
   {
      // LLL: Moved here from Populate.  On the Mac, the control
      //      would not accept focus when using the mouse, but it
      //      would when tabbing to the field.  Not really sure
      //      why...just glad it works now.  :-)

      // The SysKeyText ctrl is so special that we aren't
      // going to include it into Audacity's version of ShuttleGui.
      // So instead we create it here, and we can add it into 
      // the sizer scheme later...
      mCurrentComboText = new SysKeyTextCtrl(
            this, CurrentComboID, wxT(""),
            wxDefaultPosition, wxSize(115, -1), 0 );

      // AddWindow is a generic 'Add' for ShuttleGui.
      // It allows us to add 'foreign' controls.
      S.AddWindow( mCurrentComboText )->MoveAfterInTabOrder( mList );
      S.Id( SetButtonID ).AddButton( _("S&et"));
      S.Id( ClearButtonID ).AddButton( _("&Clear"));
   }
   S.EndHorizontalLay();
   #ifdef __WXMAC__
   S.AddFixedText( _("Note: Pressing Cmd+Q will quit. All other keys are valid."));
   #endif
   S.StartHorizontalLay( wxALIGN_LEFT, 0);
   {
      S.Id( AssignDefaultsButtonID ).AddButton( _("&Defaults"));
      S.Id( SaveButtonID ).AddButton( _("&Save..."));
      S.Id( LoadButtonID ).AddButton( _("&Load..."));
   }
   S.EndHorizontalLay();
   S.EndStatic();
   return;
}

/// Sets up mList with the right number of columns, titles,
/// fills the contents and sets column widths.
void KeyConfigPrefs::CreateList()
{
   wxASSERT( mList );

   //An empty first column is a workaround - under Win98 the first column 
   //can't be right aligned.
   mList->InsertColumn(BlankColumn,    wxT(""), wxLIST_FORMAT_LEFT );
   mList->InsertColumn(CommandColumn,  _NoAcc("&Command"),  wxLIST_FORMAT_RIGHT );
   mList->InsertColumn(KeyComboColumn, _("Key Combination"), wxLIST_FORMAT_LEFT );

   RepopulateBindingsList();

   mList->SetColumnWidth( BlankColumn, 0 ); // First column width is zero, to hide it.
   // Would like to use wxLIST_AUTOSIZE but
   // wxWindows does not look at the size of column heading.
   mList->SetColumnWidth( CommandColumn, 250 );
//   mList->SetColumnWidth( CommandColumn, wxLIST_AUTOSIZE );
   mList->SetColumnWidth( KeyComboColumn, 115 );
}

void KeyConfigPrefs::OnSave(wxCommandEvent& event)
{
   Apply();

   wxString fName = wxT("Audacity-keys.xml");
   wxString path = gPrefs->Read(wxT("/DefaultExportPath"),
                                ::wxGetCwd());

   fName = FileSelector(_("Export Keyboard Shortcuts As:"),
                        NULL,
                        fName,
                        wxT("xml"),
                        wxT("*.xml"), wxSAVE | wxOVERWRITE_PROMPT, this);

   if (!fName)
      return;

   path = wxPathOnly(fName);
   gPrefs->Write(wxT("/DefaultExportPath"), path);

   XMLFileWriter prefFile;
   
   prefFile.Open(fName, wxT("wb"));

   if (!prefFile.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName,
                   _("Error saving keyboard shortcuts"),
                   wxOK | wxCENTRE, this);
      return;
   }

   mManager->WriteXML(prefFile);

   prefFile.Close();
}

void KeyConfigPrefs::OnLoad(wxCommandEvent& event)
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),
                                ::wxGetCwd());

   wxString fileName = FileSelector(_("Select an XML file containing Audacity keyboard shortcuts..."),
                                    path,     // Path
                                    wxT(""),       // Name
                                    wxT(""),       // Extension
                                    _("XML files (*.xml)|*.xml|All files (*.*)|*.*"),
                                    0,        // Flags
                                    this);    // Parent

   if (!fileName)
      return;

   path = wxPathOnly(fileName);
   gPrefs->Write(wxT("/DefaultOpenPath"), path);

   XMLFileReader reader;
   if (!reader.Parse(mManager, fileName))
      wxMessageBox(reader.GetErrorStr(),
                   _("Error loading keyboard shortcuts"),
                   wxOK | wxCENTRE, this);

   RepopulateBindingsList();
}

void KeyConfigPrefs::OnSet(wxCommandEvent& event)
{
   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount())
      return;
   wxString newKey = mCurrentComboText->GetValue();
   
   // Check if shortcut has already been assigned
   for (int i = 0; i < mList->GetItemCount(); i++)
   {
      wxListItem item;
      item.m_col = KeyComboColumn;
      item.m_mask = wxLIST_MASK_TEXT;
      item.SetId(i);
      mList->GetItem(item);
      if (item.m_text == newKey)
      {
         wxListItem item;
         item.m_col = CommandColumn;
         item.m_mask = wxLIST_MASK_TEXT;
         item.SetId(i);
         mList->GetItem(item);
         wxString prompt = wxString::Format(
            _("The keyboard shortcut '%s' is already assigned to:\n\n'%s'"),
            newKey.c_str(),
            item.m_text.c_str());
            
         wxMessageBox(prompt, _("Error"), wxICON_STOP | wxCENTRE, this);
         
         return;
      }
   }
   
   mList->SetItem( mCommandSelected, KeyComboColumn, newKey);
}

void KeyConfigPrefs::OnClear(wxCommandEvent& event)
{
   mCurrentComboText->Clear();
   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount())
      return;
   mList->SetItem( mCommandSelected, KeyComboColumn, wxT("") );
}

void KeyConfigPrefs::OnKeyDown(wxListEvent &event)
{
   int keycode = event.GetKeyCode();
   int selected = mList->GetNextItem(-1, wxLIST_NEXT_ALL,  wxLIST_STATE_SELECTED);
   int cnt = mList->GetItemCount();
   wxListItem item;
   bool found = false;

   item.SetColumn(CommandColumn);
   item.SetMask(wxLIST_MASK_TEXT);

   for (int i = selected + 1; i < cnt; i++)
   {
      item.SetId(i);

      mList->GetItem(item);

      if (item.m_text.Left(1).IsSameAs(keycode, false)) {
         mList->SetItemState(event.GetIndex(),
                             0,
                             wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

         mList->SetItemState(i,
                             wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                             wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

         mList->EnsureVisible(i);

         found = true;

         break;
      }
   }

   if (!found) {
      for (int i = 0; i < selected; i++)
      {
         item.SetId(i);

         mList->GetItem(item);

         if (item.m_text.Left(1).IsSameAs(keycode, false)) {
            mList->SetItemState(event.GetIndex(),
                                0,
                                wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

            mList->SetItemState(i,
                                wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                                wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

            mList->EnsureVisible(i);
            break;
         }
      }
   }
}

void KeyConfigPrefs::OnItemSelected(wxListEvent &event)
{
   mCommandSelected = event.GetIndex();
   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount()) {
      mCurrentComboText->SetLabel(wxT(""));
      return;
   }
   
   wxListItem item;
   item.m_col = KeyComboColumn;
   item.m_mask = wxLIST_MASK_TEXT;
   item.SetId( mCommandSelected );
   mList->GetItem(item);

   mCurrentComboText->Clear();
   mCurrentComboText->AppendText(item.m_text);
   // JKC: July-2004
   // Under Windows 98 setting the focus to the combo box whilst
   // we are still processing an OnItemSelected event can lead
   // to a crash (try clicking on an item in the list and dragging it).
   // It's OK under WinXP.  TODO: Is there a #define that only excludes
   // WIN_98 that we could use here instead??
#ifndef __WXMSW__
   // LLL: Removed since the focus would jump away from the listctrl when
   //      using the keyboard to move up and down the list.  Every time
   //      the up/down arrows are pressed, the focus jumps aways and you have
   //      to tab back to the listctrl.
   //mCurrentComboText->SetFocus();
#else
   //JKC Something like the following might do what we want under Win98?
   //mCurrentComboText->GetEventHandler()->AddPendingEvent( wxFocusEvent(wxEVT_SET_FOCUS));
#endif
}

void KeyConfigPrefs::RepopulateBindingsList()
{
   mList->DeleteAllItems(); // Delete contents, but not the column headers.
   mNames.Clear();
   mManager->GetAllCommandNames(mNames, false);
   unsigned int i;
   for(i=0; i<mNames.GetCount(); i++) {
      mList->InsertItem( i, wxT("") );
      wxString label;
      
      // Labels for undo and redo change according to the last command
      // which can be undone/redone, so give them a special check in order
      // not to confuse users
      if (mNames[i] == wxT("Undo"))
         label = _("Undo");
      else if (mNames[i] == wxT("Redo"))
         label = _NoAcc("&Redo");
      else
         label = mManager->GetPrefixedLabelFromName(mNames[i]);
      
      label = wxMenuItem::GetLabelFromText(label.BeforeFirst('\t'));
      wxString key = mManager->GetKeyFromName(mNames[i]);

      #ifdef __WXMAC__
      // Replace Ctrl with Cmd
      if (key.Length() >= 5 && key.Left(5)==wxT("Ctrl+")) {
         key = wxT("Cmd+")+key.Right(key.Length()-5);
      }
      #endif

      mList->SetItem( i, CommandColumn, label );
      mList->SetItem( i, KeyComboColumn, key );
   }
}

void KeyConfigPrefs::OnDefaults(wxCommandEvent& event)
{
   unsigned int i;

   for(i=0; i<mNames.GetCount(); i++) {
      mList->SetItem( i, KeyComboColumn, mManager->GetDefaultKeyFromName(mNames[i]) );
   }
}

bool KeyConfigPrefs::Apply()
{
   unsigned int i;
   wxListItem item;

   /// \todo
   /// gPrefs SetPath is dodgy as we might be using gPrefs
   /// elsewhere.
   gPrefs->SetPath(wxT("/NewKeys"));

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

      #ifdef __WXMAC__
      // Replace Cmd with Ctrl
      if (key.Length() >= 4 && key.Left(4)==wxT("Cmd+")) {
         key = wxT("Ctrl+")+key.Right(key.Length()-4);
      }
      #endif

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

   gPrefs->SetPath(wxT("/"));

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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: f09afeeb-9805-463a-b3ca-e3e3bfe05549


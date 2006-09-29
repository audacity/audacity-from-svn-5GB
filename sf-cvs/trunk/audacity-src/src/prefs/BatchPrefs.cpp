/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchPrefs.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class BatchPrefs
\brief A PrefsPanel that builds up a chain of effects in BatchCommands

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/textdlg.h>

#include "BatchPrefs.h"
#include "../Envelope.h"
#include "../Languages.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../BatchCommandDialog.h"
#include "../ShuttleGui.h"

#define ChainsListID             7005
#define AddButtonID              7006
#define RemoveButtonID           7007
#define CommandsListID           7008
#define ImportButtonID           7009
#define ExportButtonID           7010
#define DefaultsButtonID         7011
#define UpButtonID               7012
#define DownButtonID             7013
#define RenameButtonID           7014

BEGIN_EVENT_TABLE(BatchPrefs, wxPanel)
   EVT_BUTTON(AddButtonID, BatchPrefs::OnAdd)
   EVT_BUTTON(RemoveButtonID, BatchPrefs::OnRemove)
   EVT_BUTTON(RenameButtonID, BatchPrefs::OnRename)
   EVT_BUTTON(UpButtonID, BatchPrefs::OnUp)
   EVT_BUTTON(DownButtonID, BatchPrefs::OnDown)
   EVT_BUTTON(ImportButtonID, BatchPrefs::OnImport)
   EVT_BUTTON(ExportButtonID, BatchPrefs::OnExport)
   EVT_BUTTON(DefaultsButtonID, BatchPrefs::OnDefaults)
   EVT_LIST_ITEM_SELECTED(ChainsListID, BatchPrefs::OnChainSelected)
   EVT_LIST_BEGIN_LABEL_EDIT(ChainsListID, BatchPrefs::OnChainsBeginEdit)
   EVT_LIST_END_LABEL_EDIT(ChainsListID, BatchPrefs::OnChainsEndEdit)
   EVT_LIST_ITEM_ACTIVATED(CommandsListID, BatchPrefs::OnItemSelected)

END_EVENT_TABLE()

enum { BlankColumn,   
   ItemNumberColumn,    
   ActionColumn, 
   ParamsColumn,
};

enum { CleanSpeechID,
   MP3ConversionID,
};

/// Constructor
BatchPrefs::BatchPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   SetLabel(_("Batch"));         // Provide visual label
   SetName(_("Batch"));          // Provide audible label
   Populate();
}

/// Creates the dialog and its contents.
void BatchPrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.
   mBatchCommands.ReadChains();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   // Set the column size for the chains list.
   PopulateChains();

   // Set the column size for the chains list.
   wxSize sz = mChains->GetClientSize();
   mChains->SetColumnWidth(0, sz.x);

   // Get and validate the currently active chain
   mActiveChain = gPrefs->Read(wxT("/Batch/ActiveChain"), wxT("CleanSpeech"));
   int item = mChains->FindItem(-1, mActiveChain);
   if (mChains->FindItem(-1, mActiveChain) == -1) {
      item = 0;
      mActiveChain = mChains->GetItemText(0);
   }

   // Load it
   mChain = mBatchCommands.GetChain(mActiveChain);

   // Select the name in the list...this will fire an event.
   mChains->SetItemState(item, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);

   // We have a bare list.  We need to add columns and content.
   PopulateList();

   // Size columns properly
   mList->SetColumnWidth( BlankColumn,  0 ); // First column width is zero, to hide it.
   mList->SetColumnWidth( ItemNumberColumn,  wxLIST_AUTOSIZE );
   mList->SetColumnWidth( ActionColumn, 110 );
   mList->SetColumnWidth( ParamsColumn, 220 );
}

/// Defines the dialog and does data exchange with it.
void BatchPrefs::PopulateOrExchange( ShuttleGui & S )
{
   S.StartHorizontalLay( wxEXPAND, 0 );
   S.SetBorder( 2 );
   S.StartStatic( _("Behaviors"),1 );
   {
      S.TieCheckBox( _("&Batch debug mode"),  
         wxT("/Batch/Debug"), false);
      S.TieCheckBox( _("&Normalize on load"), 
         wxT("/Batch/NormalizeOnLoad"), false );
      S.TieCheckBox( _("&Prompt to save, even if empty"),    
         wxT("/Batch/EmptyCanBeDirty"), false );
      S.TieCheckBox( _("Cl&eanSpeech Mode (Customized GUI)"), 
         wxT("/Batch/CleanSpeechMode"), false);
   }
   S.EndStatic();
   S.StartStatic( _("Show / Hide"),1 );
   {
      S.TieCheckBox( _("S&how MP3-ID3 Dialog"), 
         wxT("/Batch/ShowId3Dialog"), false);
   }
   S.EndStatic();
   S.EndHorizontalLay();

   S.StartHorizontalLay( wxEXPAND, 1 );
   S.StartStatic( _("Batch Options"));
   {
      // JKC: Experimenting with an alternative way to get multiline
      // translated strings to work correctly without very long lines.
      // My appologies Alexandre if this way didn't work either.
      // 
      // With this method:
      //   1) it compiles fine under windows unicode and normal mode.
      //   2) xgettext source code has handling for the trailing '\'
      //
      // It remains to see if linux and mac can cope and if xgettext 
      // actually does do fine with strings presented like this.
      // If it doesn't work out, revert to all-on-one-line.
      S.AddFixedText( 
         _("Batch mode is an\n\
experimental feature.\n\n\
Please read the release\nnotes for known limitations\n\n"));

      S.SetStyle( wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_SINGLE_SEL |
                  wxLC_EDIT_LABELS );
      mChains = S.Id( ChainsListID ).AddListControlReportMode();
      mChains->InsertColumn( 0, wxT("Chain"), wxLIST_FORMAT_LEFT );
      S.StartHorizontalLay( wxEXPAND, 0 );
      {
         S.Id( AddButtonID ).AddButton( _("&Add") );
         mRemove = S.Id( RemoveButtonID ).AddButton( _("&Remove") );
         mRename = S.Id( RenameButtonID ).AddButton( _("Rena&me") );
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();
   S.StartStatic( _("Batch Sequence (Double-Click or press SPACE to edit)"),1);
   {
      S.SetStyle( wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                  wxLC_SINGLE_SEL );
      mList = S.Id( CommandsListID ).AddListControlReportMode();

      //An empty first column is a workaround - under Win98 the first column 
      //can't be right aligned.
      mList->InsertColumn(BlankColumn,   wxT(""),         wxLIST_FORMAT_LEFT );
      mList->InsertColumn(ItemNumberColumn,    _("No"),   wxLIST_FORMAT_RIGHT );
      mList->InsertColumn(ActionColumn,  _("Command"),    wxLIST_FORMAT_RIGHT );
      mList->InsertColumn(ParamsColumn,  _("Parameters"), wxLIST_FORMAT_LEFT );

      S.StartHorizontalLay(wxEXPAND, 0);
      {
         S.Id( UpButtonID ).AddButton( _("Move &Up"), wxALIGN_LEFT );
         S.Id( DownButtonID ).AddButton( _("Move &Down"), wxALIGN_LEFT );
         mDefaults = S.Id( DefaultsButtonID ).AddButton( _("De&faults") );
         S.Id( ImportButtonID ).AddButton( _("&Import"), wxALIGN_LEFT );
         S.Id( ExportButtonID ).AddButton( _("E&xport"), wxALIGN_LEFT );
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();
   S.EndHorizontalLay();
   return;
}

/// This clears and updates the contents of mChains
void BatchPrefs::PopulateChains()
{
   wxArrayString names = mBatchCommands.GetChainNames();
   int i;

   mChains->DeleteAllItems();
   for (i = 0; i < names.GetCount(); i++)
   {
      mChains->InsertItem(i, names[i]);
   }

   mChains->Refresh(false);
}

/// This clears and updates the contents of mList
void BatchPrefs::PopulateList()
{
   wxString command;
   wxString params;
   int i;

   mList->DeleteAllItems();

   for (i = 0; i < mChain->GetCount(); i++) {
      mBatchCommands.Split( (*mChain)[i], command, params );
      AddItem( command, params );
   }

   AddItem( _("- END -"), wxT(""));
   mList->Refresh(false);
}

/// 
void BatchPrefs::OnAdd(wxCommandEvent &event)
{
   while (true) {
      wxTextEntryDialog d(this,
                          _("Enter name of new chain"),
                          _("Batch Chain"));
      wxString name;

      if (d.ShowModal() == wxID_CANCEL) {
         return;
      }

      name = d.GetValue().Strip(wxString::both);

      if (name.Length() == 0) {
         wxMessageBox(_("Name must not be blank"),
                      _("Batch Chain"),
                      wxOK | wxICON_ERROR,
                      this);
         continue;
      }

      if (name.Contains(wxT("[")) ||
          name.Contains(wxT("]")) ||
          name.Contains(wxCONFIG_PATH_SEPARATOR)) {
         wxMessageBox(wxString::Format(_("Names may not contain brackets and '%c'"), wxCONFIG_PATH_SEPARATOR),
                      _("Batch Chain"),
                      wxOK | wxICON_ERROR,
                      this);
         continue;
      }

      if (mChains->FindItem(-1, name) != -1) {
         wxMessageBox(_("Chain already exists"),
                      _("Batch Chain"),
                      wxOK | wxICON_ERROR,
                      this);
         continue;
      }

      mBatchCommands.GetChain(name);
      
      long item = mChains->InsertItem(mChains->GetItemCount(), name);
      mChains->SetItemState(item, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);

      break;
   }
}

///
void BatchPrefs::OnRemove(wxCommandEvent &event)
{
   long item = mChains->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   wxString name = mChains->GetItemText(item);
   wxMessageDialog m(this,
                     wxString::Format(_("Are you sure you want to delete %s?"), name.c_str()),
                     _("Batch Chain"),
                     wxYES_NO | wxICON_QUESTION);
   if (m.ShowModal() == wxID_NO) {
      return;
   }
}

///
void BatchPrefs::OnRename(wxCommandEvent &event)
{
   long item = mChains->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      return;
   }

   mChains->EditLabel(item);
}

///
void BatchPrefs::OnUp(wxCommandEvent &event)
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1 || item == 0 || item + 1 == mList->GetItemCount()) {
      return;
   }

   wxString commandP, paramsP, commandC, paramsC;
   wxListItem info;

   info.SetMask(wxLIST_MASK_TEXT);
   info.SetId(item - 1);

   info.SetColumn(ActionColumn);
   mList->GetItem(info);
   commandP = info.GetText();

   info.SetColumn(ParamsColumn);
   mList->GetItem(info);
   paramsP = info.GetText();

   info.SetId(item);

   info.SetColumn(ActionColumn);
   mList->GetItem(info);
   commandC = info.GetText();

   info.SetColumn(ParamsColumn);
   mList->GetItem(info);
   paramsC = info.GetText();

   SetItem(item - 1, commandC, paramsC);
   SetItem(item, commandP, paramsP);

   mList->SetItemState(item - 1, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
}

///
void BatchPrefs::OnDown(wxCommandEvent &event)
{
   long item = mList->GetNextItem(-1,
                                  wxLIST_NEXT_ALL,
                                  wxLIST_STATE_SELECTED);
   if (item == -1 || item + 2 >= mList->GetItemCount()) {
      return;
   }

   wxString commandN, paramsN, commandC, paramsC;
   wxListItem info;

   info.SetMask(wxLIST_MASK_TEXT);
   info.SetId(item + 1);

   info.SetColumn(ActionColumn);
   mList->GetItem(info);
   commandN = info.GetText();

   info.SetColumn(ParamsColumn);
   mList->GetItem(info);
   paramsN = info.GetText();

   info.SetId(item);

   info.SetColumn(ActionColumn);
   mList->GetItem(info);
   commandC = info.GetText();

   info.SetColumn(ParamsColumn);
   mList->GetItem(info);
   paramsC = info.GetText();

   SetItem(item, commandN, paramsN);
   SetItem(item + 1, commandC, paramsC);

   mList->SetItemState(item + 1, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
}

/// Loads a command chain from a file.
void BatchPrefs::OnImport(wxCommandEvent &event)
{
   mBatchCommands.ImportChain(this, mActiveChain);
   PopulateList();
}

/// Saves the current command chain to a file.
void BatchPrefs::OnExport(wxCommandEvent &event)
{
   mBatchCommands.ExportChain(this, mActiveChain);
   PopulateList();
}

/// Select the empty Command chain.
void BatchPrefs::OnDefaults(wxCommandEvent &event)
{
   mChain = mBatchCommands.RestoreChain(mActiveChain);
   PopulateList();
}

/// An item in the chains list has been selected.
void BatchPrefs::OnChainSelected(wxListEvent &event)
{
   int itemNo = event.GetIndex();

   mActiveChain = mChains->GetItemText(itemNo);
   mChain = mBatchCommands.GetChain(mActiveChain);

   if (mBatchCommands.IsFixed(mActiveChain)) {
      mRemove->Disable();
      mRename->Disable();
      mDefaults->Enable();
   }
   else {
      mRemove->Enable();
      mRename->Enable();
      mDefaults->Disable();
   }

   PopulateList();
}

///
void BatchPrefs::OnChainsBeginEdit(wxListEvent &event)
{
   int itemNo = event.GetIndex();

   wxString chain = mChains->GetItemText(itemNo);

   if (mBatchCommands.IsFixed(mActiveChain)) {
      wxBell();
      event.Veto();
   }
}

///
void BatchPrefs::OnChainsEndEdit(wxListEvent &event)
{
   if (event.IsEditCancelled()) {
      return;
   }

   wxString newname = event.GetLabel();

   mChain = mBatchCommands.RenameChain(mActiveChain, newname);
   mActiveChain = newname;
}

/// An item in the list has been selected.
/// Bring up a dialog to allow its parameters to be edited.
void BatchPrefs::OnItemSelected(wxListEvent &event)
{
   int itemNo = event.GetIndex();
   // Keep chain short.
   // We currently only store shortish chains in the prefs.
   if( itemNo > 20 )
      return;

   wxString command, params;
   BatchCommandDialog Dlg( this, -1);

   wxListItem info;
   info.SetId( itemNo );
   info.SetMask( wxLIST_MASK_TEXT );

   info.SetColumn( ActionColumn );
   mList->GetItem( info );
   command = info.GetText();

   info.SetColumn( ParamsColumn );
   mList->GetItem( info );
   params = info.GetText();
   
   Dlg.SetCommandAndParams( command, params );

   if( Dlg.ShowModal())
   {
      SetItem( 
         itemNo, 
         Dlg.mSelectedCommand,
         Dlg.mSelectedParameters);
   }
}

// This commented out code might be useful as a first step if we want an immediate response to 
// switching in and out of CleanSpeech mode.
// As things currently stand, the batch commands available will NOT reflect changes in
// CleanSpeech mode until we close and reopen the preferences dialog.
#if 0
   int mode;
   AudacityProject *proj = GetActiveProject();
   mode = gPrefs->Read(wxT("/Batch/CleanSpeechMode"), 1L);
   proj->GetControlToolBar()->SetCleanSpeechMode(mode == 1);
#endif

/// Send changed values back to Prefs, and update Audacity.
bool BatchPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   gPrefs->Write(wxT("/Batch/ActiveChain"), mActiveChain);
   mBatchCommands.WriteChains();
   mBatchCommands.FlushChains();

   unsigned int j;
   for(j = 0; j < gAudacityProjects.GetCount(); j++)
   {
      gAudacityProjects[j]->UpdateBatchPrefs();
   }

   return true;
}

///
void BatchPrefs::Cancel()
{
   mBatchCommands.FlushChains();
}

BatchPrefs::~BatchPrefs()
{
}

/// Add one item into mList
void BatchPrefs::AddItem( wxString const & Action, wxString const & Params)
{
   int i=mList->GetItemCount();
   mList->InsertItem( i, wxT("") );
   mList->SetItem( i, ItemNumberColumn, wxString::Format(wxT(" %02i"),i+1) );
   mList->SetItem( i, ActionColumn, Action );
   mList->SetItem( i, ParamsColumn, Params );
}

/// Sets one item in mList to a command/parameter pair.
void BatchPrefs::SetItem(int itemNo, const wxString command, const wxString params)
{
   // There is a 'rogue' entry of "- END -" at the end of the list
   // which users can click on to add an entry to the end.

   // There is a special entry of "No Action" in the effects list,
   // which can be used to delete an entry.

   // special case - deleting an item.
   if( command.IsSameAs(wxT("No Action")))
   {
      //Attempt to delete the 'rogue' entry at the end.
      if( (itemNo+1) >= mList->GetItemCount() )
         return;

      mChain->RemoveAt( itemNo );
      PopulateList();
      return;
   }

   // Set the item in the list.
   mList->SetItem( itemNo ,ActionColumn, command ); 
   mList->SetItem( itemNo ,ParamsColumn, params );

   // IF at end of list THEN extend list
   // else replace item.
   if( (itemNo+1) >= mList->GetItemCount() )
   {
      // Add new rogue
      AddItem( _("- END -"),wxT(""));
      mChain->Add( mBatchCommands.Join( command, params ) );
   }
   else
   {
      (*mChain)[itemNo] = mBatchCommands.Join( command, params );
   }

   mList->Refresh(false);
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
// arch-tag: 7e997d04-6b94-4abb-b3d6-748400f86598

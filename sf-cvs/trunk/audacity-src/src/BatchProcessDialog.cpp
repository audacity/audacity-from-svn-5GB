/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchProcessDialog.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//*!

\class BatchProcessDialog
\brief Shows progress in executring commands in BatchCommands.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/filedlg.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/button.h>
#include <wx/imaglist.h>
#include <wx/msgdlg.h>

#include "Prefs.h"
#include "Project.h"
#include "BatchProcessDialog.h"
#include "commands/CommandManager.h"
#include "effects/Effect.h"
#include "../images/Arrow.xpm"
#include "BatchCommands.h"

#include "Theme.h"
#include "AllThemeResources.h"


#define ChainsListID       7001
#define ApplyToProjectID   7002
#define ApplyToFilesID     7003

BEGIN_EVENT_TABLE(BatchProcessDialog, wxDialog)
   EVT_BUTTON(ApplyToProjectID, BatchProcessDialog::OnApplyToProject)
   EVT_BUTTON(ApplyToFilesID, BatchProcessDialog::OnApplyToFiles)
   EVT_BUTTON(wxID_CANCEL, BatchProcessDialog::OnCancel)
END_EVENT_TABLE()

BatchProcessDialog::BatchProcessDialog(wxWindow * parent):
   wxDialog(parent, wxID_ANY, _("Batch Processing"), wxDefaultPosition)
{
   AudacityProject * p = GetActiveProject();
   if (p->GetCleanSpeechMode())
   {
      SetTitle(_("CleanSpeech Batch Processing"));
   }

   SetLabel(_("Batch Processing"));         // Provide visual label
   SetName(_("Batch Processing"));          // Provide audible label
   Populate();

   CenterOnParent();

   mAbort = false;
}

BatchProcessDialog::~BatchProcessDialog()
{
}

void BatchProcessDialog::Populate()
{
   // First any pre-processing for constructing the GUI.
   mBatchCommands.ReadChains();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Defines the dialog and does data exchange with it.
void BatchProcessDialog::PopulateOrExchange( ShuttleGui & S )
{
   S.StartVerticalLay(true);
   {
      
      S.StartStatic(_("Select chain"), true);
      {
         S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                     wxLC_SINGLE_SEL);
         mChains = S.Id(ChainsListID).AddListControlReportMode();
         mChains->InsertColumn(0, _("Chain"), wxLIST_FORMAT_LEFT);
      }
      S.EndStatic();

      S.StartHorizontalLay(wxALIGN_RIGHT, false);
      {
         S.Id(ApplyToProjectID).AddButton(_("Apply to Current Project"));
         S.Id(ApplyToFilesID).AddButton(_("Apply to Files..."));
         S.Id(wxID_CANCEL).AddButton(_("Cancel"));
      }
      S.StartHorizontalLay();
   }
   S.EndVerticalLay();

   wxArrayString names = mBatchCommands.GetChainNames();
   for (int i = 0; i < names.GetCount(); i++)
   {
      mChains->InsertItem(i, names[i]);
   }

   // Get and validate the currently active chain
   wxString name = gPrefs->Read(wxT("/Batch/ActiveChain"), wxT("CleanSpeech"));
   int item = mChains->FindItem(-1, name);
   if (mChains->FindItem(-1, name) == -1) {
      item = 0;
      name = mChains->GetItemText(0);
   }

   // Select the name in the list...this will fire an event.
   mChains->SetItemState(item, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);

   Layout();
   Fit();
   SetSizeHints(GetSize());

   // Set the column size for the chains list.
   wxSize sz = mChains->GetClientSize();
   mChains->SetColumnWidth(0, sz.x);
}

void BatchProcessDialog::OnApplyToProject(wxCommandEvent & event)
{
   long item = mChains->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      wxMessageBox(_("No chain selected"));
      return;
   }

   wxDialog d(this, wxID_ANY, wxString(_("Batch Processing")));
   ShuttleGui S(&d, eIsCreating);

   S.StartHorizontalLay(wxCENTER, false);
   {
      S.StartStatic(_(""), 0);
         S.AddFixedText(wxT(""));
         S.AddFixedText(_("Applying chain to current project"), true);
         S.AddFixedText(wxT(""));
      S.EndStatic();
   }
   S.EndHorizontalLay();

   d.Layout();
   d.Fit();
   d.CenterOnParent();
   d.Move(-1, 0);
   d.Show();
   Hide();

   wxWindowDisabler wd;

   wxString name = mChains->GetItemText(item);

   gPrefs->Write(wxT("/Batch/ActiveChain"), name);

   if (!mBatchCommands.ApplyChain(name, wxT(""))) {
      return;
   }

   EndModal(true);
}

void BatchProcessDialog::OnApplyToFiles(wxCommandEvent & event)
{
   long item = mChains->GetNextItem(-1,
                                    wxLIST_NEXT_ALL,
                                    wxLIST_STATE_SELECTED);
   if (item == -1) {
      wxMessageBox(_("No chain selected"));
      return;
   }

   wxString name = mChains->GetItemText(item);
   gPrefs->Write(wxT("/Batch/ActiveChain"), name);

   AudacityProject *project = GetActiveProject();
   if (!project->GetIsEmpty())
   {
      wxMessageBox(_("Please save and close the current project first."));
      return;
   }

   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"), ::wxGetCwd());
   wxString prompt =  project->GetCleanSpeechMode() ? 
      _("Select vocal file(s) for batch CleanSpeech Chain...") :
      _("Select file(s) for batch processing...");
   wxString fileSelector = project->GetCleanSpeechMode() ? 
      _("Vocal files (*.wav;*.mp3)|*.wav;*.mp3|WAV files (*.wav)|*.wav|MP3 files (*.mp3)|*.mp3") :
      _("All files (*.*)|*.*|WAV files (*.wav)|*.wav|AIFF files (*.aif)|*.aif|AU files (*.au)|*.au|MP3 files (*.mp3)|*.mp3|Ogg Vorbis files (*.ogg)|*.ogg|FLAC files (*.flac)|*.flac"
       );

   wxFileDialog dlog(this, prompt,
                     path, wxT(""), fileSelector,
                     wxOPEN | wxMULTIPLE);

   if (dlog.ShowModal() != wxID_OK) {
      return;
   }

   wxArrayString files;
   dlog.GetPaths(files);

   files.Sort();

   wxDialog d(this, wxID_ANY, wxString(_("Batch Processing")));
   ShuttleGui S(&d, eIsCreating);

   S.StartVerticalLay(false);
   {
      S.StartStatic(_("Applying..."), 1);
      {
         wxImageList *imageList = new wxImageList(9, 16);
         imageList->Add(wxIcon(empty_9x16_xpm));
         imageList->Add(wxIcon(arrow_xpm));

         S.SetStyle(wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                    wxLC_SINGLE_SEL);
         mList = S.AddListControlReportMode();
         mList->AssignImageList(imageList, wxIMAGE_LIST_SMALL);
         mList->InsertColumn(0, _("File"), wxLIST_FORMAT_LEFT);
      }
      S.EndStatic();

      S.StartHorizontalLay(wxCENTER, false);
      {
         S.Id(wxID_CANCEL).AddButton(_("Cancel"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   int i;
   for (i = 0; i < files.GetCount(); i++ )
   {
      mList->InsertItem(i, files[i], i == 0);
   }

   // Set the column size for the files list.
   mList->SetColumnWidth(0, wxLIST_AUTOSIZE);

   int width = mList->GetColumnWidth(0);
   wxSize sz = mList->GetClientSize();
   if (width > sz.GetWidth() && width < 500) {
      sz.SetWidth(width);
      mList->SetBestFittingSize(sz);
   }

   d.Layout();
   d.Fit();
   d.SetSizeHints(d.GetSize());
   d.CenterOnParent();
   d.Move(-1, 0);
   d.Show();
   Hide();

   for (i = 0; i < files.GetCount(); i++)
   {
      wxWindowDisabler wd(&d);
      if (i > 0)
      {
         //Clear the arrow in previous item.
         mList->SetItemImage(i - 1, 0, 0);
      }
      mList->SetItemImage(i, 1, 1);
      mList->EnsureVisible(i);

      if (!mBatchCommands.ApplyChain(name, files[i])) {
         break;
      }

      if (!d.IsShown() || mAbort) {
         break;
      }
   }

   EndModal(true);
}

void BatchProcessDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

/////////////////////////////////////////////////////////////////////
#include <wx/textdlg.h>
#include "../BatchCommandDialog.h"
//#define ChainsListID             7005
#define AddButtonID              7006
#define RemoveButtonID           7007
#define CommandsListID           7008
#define ImportButtonID           7009
#define ExportButtonID           7010
#define DefaultsButtonID         7011
#define UpButtonID               7012
#define DownButtonID             7013
#define RenameButtonID           7014

BEGIN_EVENT_TABLE(EditChainsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, EditChainsDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, EditChainsDialog::OnCancel)
   EVT_BUTTON(AddButtonID, EditChainsDialog::OnAdd)
   EVT_BUTTON(RemoveButtonID, EditChainsDialog::OnRemove)
   EVT_BUTTON(RenameButtonID, EditChainsDialog::OnRename)
   EVT_BUTTON(UpButtonID, EditChainsDialog::OnUp)
   EVT_BUTTON(DownButtonID, EditChainsDialog::OnDown)
   EVT_BUTTON(ImportButtonID, EditChainsDialog::OnImport)
   EVT_BUTTON(ExportButtonID, EditChainsDialog::OnExport)
   EVT_BUTTON(DefaultsButtonID, EditChainsDialog::OnDefaults)
   EVT_LIST_ITEM_SELECTED(ChainsListID, EditChainsDialog::OnChainSelected)
   EVT_LIST_BEGIN_LABEL_EDIT(ChainsListID, EditChainsDialog::OnChainsBeginEdit)
   EVT_LIST_END_LABEL_EDIT(ChainsListID, EditChainsDialog::OnChainsEndEdit)
   EVT_LIST_ITEM_ACTIVATED(CommandsListID, EditChainsDialog::OnItemSelected)
END_EVENT_TABLE()

enum {
   BlankColumn,   
   ItemNumberColumn,    
   ActionColumn, 
   ParamsColumn,
};

/// Constructor
EditChainsDialog::EditChainsDialog(wxWindow * parent):
   wxDialog(parent, wxID_ANY, wxString(_("Edit Chains")))
{
   SetLabel(_("Batch"));         // Provide visual label
   SetName(_("Batch"));          // Provide audible label
   Populate();
}

EditChainsDialog::~EditChainsDialog()
{
}

/// Creates the dialog and its contents.
void EditChainsDialog::Populate( )
{
   // First any pre-processing for constructing the GUI.
   mBatchCommands.ReadChains();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreating);
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

   Layout();
   Fit();
   CenterOnParent();
}

/// Defines the dialog and does data exchange with it.
void EditChainsDialog::PopulateOrExchange( ShuttleGui & S )
{
   S.StartHorizontalLay( wxEXPAND, 1 );
   {
      S.StartStatic( _("Chains"));
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
         S.SetStyle( wxSUNKEN_BORDER | wxLC_REPORT | wxLC_HRULES | wxLC_SINGLE_SEL |
                     wxLC_EDIT_LABELS );
         mChains = S.Id( ChainsListID ).AddListControlReportMode();
         mChains->InsertColumn( 0, wxT("Chain"), wxLIST_FORMAT_LEFT );
         S.StartHorizontalLay(wxCENTER, false);
         {
            S.Id( AddButtonID ).AddButton( _("&Add") );
            mRemove = S.Id( RemoveButtonID ).AddButton( _("&Remove") );
            mRename = S.Id( RenameButtonID ).AddButton( _("Rena&me") );
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartStatic( _("Chain Sequence (Double-Click or press SPACE to edit)"),1);
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
         mList->SetSizeHints(300, 200);

         S.StartHorizontalLay(wxCENTER, false);
         {
            S.Id( UpButtonID ).AddButton( _("Move &Up"), wxALIGN_LEFT );
            S.Id( DownButtonID ).AddButton( _("Move &Down"), wxALIGN_LEFT );
            mDefaults = S.Id( DefaultsButtonID ).AddButton( _("De&faults") );
//            S.Id( ImportButtonID ).AddButton( _("&Import"), wxALIGN_LEFT );
//            S.Id( ExportButtonID ).AddButton( _("E&xport"), wxALIGN_LEFT );
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   GetSizer()->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxALIGN_RIGHT | wxBOTTOM, 10);

   return;
}

/// This clears and updates the contents of mChains
void EditChainsDialog::PopulateChains()
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
void EditChainsDialog::PopulateList()
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
void EditChainsDialog::OnAdd(wxCommandEvent &event)
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
void EditChainsDialog::OnRemove(wxCommandEvent &event)
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
void EditChainsDialog::OnRename(wxCommandEvent &event)
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
void EditChainsDialog::OnUp(wxCommandEvent &event)
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
void EditChainsDialog::OnDown(wxCommandEvent &event)
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
void EditChainsDialog::OnImport(wxCommandEvent &event)
{
   mBatchCommands.ImportChain(this, mActiveChain);
   PopulateList();
}

/// Saves the current command chain to a file.
void EditChainsDialog::OnExport(wxCommandEvent &event)
{
   mBatchCommands.ExportChain(this, mActiveChain);
   PopulateList();
}

/// Select the empty Command chain.
void EditChainsDialog::OnDefaults(wxCommandEvent &event)
{
   mChain = mBatchCommands.RestoreChain(mActiveChain);
   PopulateList();
}

/// An item in the chains list has been selected.
void EditChainsDialog::OnChainSelected(wxListEvent &event)
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
void EditChainsDialog::OnChainsBeginEdit(wxListEvent &event)
{
   int itemNo = event.GetIndex();

   wxString chain = mChains->GetItemText(itemNo);

   if (mBatchCommands.IsFixed(mActiveChain)) {
      wxBell();
      event.Veto();
   }
}

///
void EditChainsDialog::OnChainsEndEdit(wxListEvent &event)
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
void EditChainsDialog::OnItemSelected(wxListEvent &event)
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
void EditChainsDialog::OnOK(wxCommandEvent &event)
{
   gPrefs->Write(wxT("/Batch/ActiveChain"), mActiveChain);
   mBatchCommands.WriteChains();
   mBatchCommands.FlushChains();
   EndModal(true);
}

///
void EditChainsDialog::OnCancel(wxCommandEvent &event)
{
   mBatchCommands.FlushChains();
   EndModal(false);
}

/// Add one item into mList
void EditChainsDialog::AddItem( wxString const & Action, wxString const & Params)
{
   int i=mList->GetItemCount();
   mList->InsertItem( i, wxT("") );
   mList->SetItem( i, ItemNumberColumn, wxString::Format(wxT(" %02i"),i+1) );
   mList->SetItem( i, ActionColumn, Action );
   mList->SetItem( i, ParamsColumn, Params );
}

/// Sets one item in mList to a command/parameter pair.
void EditChainsDialog::SetItem(int itemNo, const wxString command, const wxString params)
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
// arch-tag: TBD



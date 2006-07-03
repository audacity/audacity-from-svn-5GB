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

#include "BatchPrefs.h"
#include "../Envelope.h"
#include "../Languages.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../BatchCommandDialog.h"
#include "../ShuttleGui.h"

#define CommandsListID          7005
#define SaveButtonID            7006
#define LoadButtonID            7007
#define CleanSpeechButtonID     7008
#define Mp3ButtonID             7009
#define EmptyChainButtonID      7010

BEGIN_EVENT_TABLE(BatchPrefs, wxPanel)
   EVT_BUTTON(SaveButtonID, BatchPrefs::OnSave)
   EVT_BUTTON(LoadButtonID, BatchPrefs::OnLoad)
   EVT_BUTTON(CleanSpeechButtonID, BatchPrefs::OnSetChainCleanSpeech)
   EVT_BUTTON(Mp3ButtonID,         BatchPrefs::OnSetChainMp3)
   EVT_BUTTON(EmptyChainButtonID,  BatchPrefs::OnSetChainEmpty)
   EVT_LIST_ITEM_ACTIVATED(CommandsListID, BatchPrefs::OnItemSelected)

// Is someone working on drag and drop????
   EVT_LIST_BEGIN_DRAG(CommandsListID, BatchPrefs::OnDrag)  // The user has started dragging an item with the left mouse button. The event handler must call wxTreeEvent::Allow() for the drag operation to continue.  
// EVT_TREE_BEGIN_RDRAG(id, func)  // The user has started dragging an item with the right mouse button. The event handler must call wxTreeEvent::Allow() for the drag operation to continue.  
// EVT_LIST_END_DRAG(CommandsListID, BatchPrefs::OnDragEnd)   // The user has released the mouse after dragging an item.  
END_EVENT_TABLE()

enum { BlankColumn,   
   ItemNumberColumn,    
   ActionColumn, 
   ParamsColumn,
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

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
   // We have a bare list.  We need to add columns and content.
   CreateList();
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
//    S.TieCheckBox( _("Show Confirmation for 'delete orphans'"), 
//       wxT("/Batch/ShowDeleteConfirmation"), true);
   }
   S.EndStatic();
   S.EndHorizontalLay();
   S.StartHorizontalLay( wxEXPAND, 1 );
   S.StartStatic( _("Batch Options"));
   {
      S.AddFixedText( 
         _("Batch mode is an\nexperimental feature.\n\nPlease read the release\nnotes for known limitations"));
      S.Id( CleanSpeechButtonID ).AddButton( _("&CleanSpeech chain") );
      S.Id( Mp3ButtonID ).AddButton( _("&MP3 conversion chain") );
      S.Id( EmptyChainButtonID ).AddButton( _("&Empty chain") );
      S.Id( LoadButtonID ).AddButton( _("&Load chain") );
      S.Id( SaveButtonID ).AddButton( _("&Save chain") );
   }
   S.EndStatic();
   S.StartStatic( _("Batch Sequence (Double-Click or press SPACE to edit)"),1);
   {
      mList = S.Id( CommandsListID ).AddListControlReportMode();
   }
   S.EndStatic();
   S.EndHorizontalLay();
   return;
}

/// Sets up mList with the right number of columns, titles,
/// fills the contents and sets column widths.
void BatchPrefs::CreateList()
{
   wxASSERT( mList );

   //An empty first column is a workaround - under Win98 the first column 
   //can't be right aligned.
   mList->InsertColumn(BlankColumn,   wxT(""),         wxLIST_FORMAT_LEFT );
   mList->InsertColumn(ItemNumberColumn,    _("No"),   wxLIST_FORMAT_RIGHT );
   mList->InsertColumn(ActionColumn,  _("Command"),    wxLIST_FORMAT_RIGHT );
   mList->InsertColumn(ParamsColumn,  _("Parameters"), wxLIST_FORMAT_LEFT );

   wxASSERT( mList );
   mBatchCommands.ReadChain();
   PopulateList();

   mList->SetColumnWidth( BlankColumn,  0 ); // First column width is zero, to hide it.
   mList->SetColumnWidth( ItemNumberColumn,  wxLIST_AUTOSIZE );
   mList->SetColumnWidth( ActionColumn, 110 );
   mList->SetColumnWidth( ParamsColumn, 220 );
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

/// This clears and updates the contents of mList
void BatchPrefs::PopulateList()
{
   unsigned int i;
   wxString command;
   wxString params;
   wxASSERT( mList );
   mList->DeleteAllItems();
   for(i=0;i<mBatchCommands.mCommandChain.GetCount();i++)
   {
      command =mBatchCommands.mCommandChain[i]; 
      params  =mBatchCommands.mParamsChain[i];
      AddItem( command, params );
   }
   AddItem( _("- END -"), wxT(""));
   mList->Refresh(false);
}

/// Select the MP3 Command chain.
void BatchPrefs::OnSetChainMp3(wxCommandEvent &event)
{
   mBatchCommands.SetWavToMp3Chain();
   PopulateList();
}

/// Select the CleanSpeech Command chain.
void BatchPrefs::OnSetChainCleanSpeech(wxCommandEvent &event)
{
   mBatchCommands.SetCleanSpeechChain();
   PopulateList();
}

/// Select the empty Command chain.
void BatchPrefs::OnSetChainEmpty(wxCommandEvent &event)
{
   mBatchCommands.ResetChain();
   PopulateList();
}

/// Loads a command chain from a file.
void BatchPrefs::OnLoad(wxCommandEvent &event)
{
   mBatchCommands.LoadChain( this );
   PopulateList();
}

/// Saves the current command chain to a file.
void BatchPrefs::OnSave(wxCommandEvent &event)
{
   mBatchCommands.SaveChain( this );
   PopulateList();
}

/// JKC: This looks like the start of some code for dragging command chains
/// into the list box...  Is anyone working on it? (June/2006)
void BatchPrefs::OnDrag(wxListEvent &event)
{
   event.Allow();
//   wxLogDebug( "From %i", event.GetItem() );
//   mDragStart = event.GetItem();
}

/// See comment in OnDrag
void BatchPrefs::OnDragEnd(wxListEvent &event)
{
   int i;
   i=7;// place for a breakpoint?
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

   unsigned int j;
   for(j = 0; j < gAudacityProjects.GetCount(); j++)
   {
      gAudacityProjects[j]->UpdateBatchPrefs();
   }
   mBatchCommands.WriteChain();
   return true;
}

BatchPrefs::~BatchPrefs()
{
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

      mBatchCommands.mCommandChain.RemoveAt( itemNo );
      mBatchCommands.mParamsChain.RemoveAt( itemNo );
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
      mBatchCommands.mCommandChain.Add( command );
      mBatchCommands.mParamsChain.Add( params );
   }
   else
   {
      mBatchCommands.mCommandChain[itemNo] = command;
      mBatchCommands.mParamsChain[itemNo] = params;
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

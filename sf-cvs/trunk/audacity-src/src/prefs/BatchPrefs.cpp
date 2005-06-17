/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#include "BatchPrefs.h"

#include "../Audacity.h"
#include "../EditToolBar.h"
#include "../Envelope.h"
#include "../Languages.h"
#include "../Prefs.h"
#include "../Project.h"  //lda
#include "../BatchCommandDialog.h"

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/radiobut.h>
#include <wx/button.h>

#define AssignDefaultsButtonID  7001
#define CurrentComboID          7002
#define SetButtonID             7003
#define ClearButtonID           7004
#define CommandsListID          7005
#define SaveButtonID            7006
#define LoadButtonID            7007
#define CleanSpeechButtonID     7008
#define Mp3ButtonID             7009
#define EmptyChainButtonID      7010

BEGIN_EVENT_TABLE(BatchPrefs, wxPanel)
//   EVT_BUTTON(AssignDefaultsButtonID, BatchPrefs::OnDefaults)
//   EVT_BUTTON(SetButtonID, BatchPrefs::OnSet)
//   EVT_BUTTON(ClearButtonID, BatchPrefs::OnClear)
//   EVT_BUTTON(SaveButtonID, BatchPrefs::OnSave)
//   EVT_BUTTON(LoadButtonID, BatchPrefs::OnLoad)
   EVT_LIST_ITEM_ACTIVATED(CommandsListID, BatchPrefs::OnItemSelected)
   EVT_LIST_BEGIN_DRAG(CommandsListID, BatchPrefs::OnDrag)  // The user has started dragging an item with the left mouse button. The event handler must call wxTreeEvent::Allow() for the drag operation to continue.  
   EVT_BUTTON(CleanSpeechButtonID, BatchPrefs::OnSetChainCleanSpeech)
   EVT_BUTTON(Mp3ButtonID,         BatchPrefs::OnSetChainMp3)
   EVT_BUTTON(EmptyChainButtonID,  BatchPrefs::OnSetChainEmpty)
//EVT_TREE_BEGIN_RDRAG(id, func)  // The user has started dragging an item with the right mouse button. The event handler must call wxTreeEvent::Allow() for the drag operation to continue.  
//   EVT_LIST_END_DRAG(CommandsListID, BatchPrefs::OnDragEnd)   // The user has released the mouse after dragging an item.  
END_EVENT_TABLE()

enum { BlankColumn,   
   ItemNumberColumn,    
   ActionColumn, 
   ParamsColumn,
};

// Titles for the lists of check boxes.
const wxString Titles[NUM_BATCH_CHECKBOX_CONTAINERS] =
{
   _("Behaviors"),  // e.g. 'Pause Always Allowed'.
   _("Show / Hide") // e.g. show/hide a toolbar or a tabbed window.
};

BatchPrefs::BatchPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   int i;

   topSizer = new wxBoxSizer( wxVERTICAL );
   // CheckSizer is a sizer that holds the left and right sizers.
   wxBoxSizer * CheckSizer = new wxBoxSizer( wxHORIZONTAL );

   // We have two containers, a left and a right one.
   for(i=0;i<NUM_BATCH_CHECKBOX_CONTAINERS;i++)
   {
      mCheckListSizers[i] = new wxStaticBoxSizer(
            new wxStaticBox(this, -1,
         Titles[i] ),
         wxVERTICAL );

#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
      mCheckListBoxes[i] = new wxCheckListBox
           (
            this,                  // parent
            -1,                    // control id
            wxPoint(10, 10),       // listbox poistion
            wxSize(245, 130),      // listbox size
            0,                     // initially no entries.
            NULL,                  // initial empty list of strings.
            0
           );
      if( mCheckListBoxes[i] )
         mCheckListSizers[i]->Add(mCheckListBoxes[i], 1, wxGROW|wxALL, 2);
#endif
      CheckSizer->Add( mCheckListSizers[i], 1, wxGROW|wxALL, 1);
   }

   // And CheckSizer is itself added in to the topSizer.
   topSizer->Add( CheckSizer, 0, wxGROW | wxALL, TOP_LEVEL_BORDER );

   // Create all the checkboxes.
   mbCreating=true;
   AllCheckBoxActions();

   // batchControlSizer is a sizer that holds the left and right sizers.
   wxBoxSizer * batchControlSizer = new wxBoxSizer( wxHORIZONTAL );

   wxStaticBoxSizer *batchOptionsSizer =
      new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Batch Options")),
            wxVERTICAL);

   wxStaticText * item = new wxStaticText(this, -1,
                     _("Batch mode is an\n experimental feature.\n\n"
                       "Please read the release\n notes for known limitations"
                       ),
                     wxDefaultPosition, wxDefaultSize, 0);

   batchOptionsSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 5);
   wxButton * btnCleanSpeech = new wxButton( this, CleanSpeechButtonID,
      _("CleanSpeech Chain"), wxDefaultPosition, wxDefaultSize, 0);

   wxButton * btnMp3 = new wxButton( this, Mp3ButtonID,
      _("Mp3 Conversion Chain"), wxDefaultPosition, wxDefaultSize, 0);

   wxButton * btnEmpty = new wxButton( this, EmptyChainButtonID,
      _("Empty Chain"), wxDefaultPosition, wxDefaultSize, 0);

   batchOptionsSizer->Add(btnCleanSpeech, 0, wxALIGN_CENTER | wxALL, 5);
   batchOptionsSizer->Add(btnMp3, 0, wxALIGN_CENTER | wxALL, 5);
   batchOptionsSizer->Add(btnEmpty, 0, wxALIGN_CENTER | wxALL, 5);
   batchControlSizer->Add( batchOptionsSizer, 1, wxGROW|wxALL, 1);
 
   wxStaticBoxSizer *batchSequenceSizer =
      new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Batch Sequence (Double-Click to edit)")),
            wxVERTICAL);

   mList = new wxListCtrl( this, CommandsListID ,
      wxDefaultPosition, wxDefaultSize,
      wxLC_REPORT | wxLC_HRULES | wxLC_VRULES | wxSUNKEN_BORDER 
      );

   wxASSERT( mList );

   //An empty first column is a workaround - under Win98 the first column 
   //can't be right aligned.
   mList->InsertColumn(BlankColumn,   "",              wxLIST_FORMAT_LEFT );
   mList->InsertColumn(ItemNumberColumn,    _("No"),   wxLIST_FORMAT_RIGHT );
   mList->InsertColumn(ActionColumn,  _("Command"),    wxLIST_FORMAT_RIGHT );
   mList->InsertColumn(ParamsColumn,  _("Parameters"), wxLIST_FORMAT_LEFT );

   wxASSERT( mList );
   mBatchCommands.ReadChain();
   PopulateList();

   mList->SetColumnWidth( BlankColumn,  0 ); // First column width is zero, to hide it.
   mList->SetColumnWidth( ItemNumberColumn,  wxLIST_AUTOSIZE );
   mList->SetColumnWidth( ActionColumn, 110 );
   mList->SetColumnWidth( ParamsColumn, 320 );

   batchSequenceSizer->Add( mList, 1, wxEXPAND );
   batchControlSizer->Add( batchSequenceSizer, 3, wxGROW|wxALL, 1);
   topSizer->Add( batchControlSizer, 1, wxEXPAND );

   // Finish layout
   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 1, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);

   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
}

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
   AddItem( _("- END -"), "");
   mList->Refresh(false);
}

void BatchPrefs::OnSetChainMp3(wxCommandEvent &event)
{
   mBatchCommands.SetWavToMp3Chain();
   PopulateList();
}

void BatchPrefs::OnSetChainCleanSpeech(wxCommandEvent &event)
{
   mBatchCommands.SetCleanSpeechChain();
   PopulateList();
}

void BatchPrefs::OnSetChainEmpty(wxCommandEvent &event)
{
   mBatchCommands.ResetChain();
   PopulateList();
}

void BatchPrefs::OnDrag(wxListEvent &event)
{
   event.Allow();
//   wxLogDebug( "From %i", event.GetItem() );
//   mDragStart = event.GetItem();
}

void BatchPrefs::OnDragEnd(wxListEvent &event)
{
   int i;
   i=7;
}

void BatchPrefs::OnItemSelected(wxListEvent &event)
{
   int itemNo = event.GetIndex();
   // Keep chain short.
   // We currently only store shortish chains in the prefs.
   if( itemNo > 20 )
      return;

   wxString command, params;

   BatchCommandDialog Dlg( this, -1, -1);

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

wxCheckBox * BatchPrefs::CreateCheckBox(const wxString Description, const bool State)
{
   wxSizer * pSizer;
   pSizer = mCheckListSizers[mCurrentCheckBoxContainer];
   wxASSERT( pSizer );
   wxCheckBox * pCheckBox = new wxCheckBox(this, -1, Description);
   pCheckBox->SetValue(State);
   pSizer->Add(pCheckBox, 0, wxGROW|wxALL, 2);
   return pCheckBox;
}

// TIDY-ME: Can CheckBoxAction be made part of the base class?
// It is used by both GuiPrefs and BatchPrefs.

// Function that deals with one checkbox, either creating it or
// using its value to update gPrefs and the GUI.
// The function is made more complex by coping with
// checkboxes in a sizer or checkboxes in a check list box.
void BatchPrefs::CheckBoxAction(
   const wxString Description,
   const wxString SettingName,
   bool bDefault,
   int mWindowID)
{
   bool bValue;
   bool bIsInListBox;

// TODO:  What should we do if on a Mac?
// wxWidgets 2.4.1 documentation suggests that wxCheckListBox isn't
// supported on Mac.
#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
   int Counter = ++(mCheckBoxCounters[mCurrentCheckBoxContainer]);

   wxCheckListBox * pCurrentList = mCheckListBoxes[mCurrentCheckBoxContainer];
   // If there was a non null list box, then it goes in it.
   bIsInListBox = ( pCurrentList != NULL );
#else
   bIsInListBox = false;
#endif

   if( !bIsInListBox )
   {
      // It's on the dialog, better make sure we
      // have a slot for a check box pointer for it.
      mCurrentCheckBox++;
      wxASSERT( mCurrentCheckBox < MAX_BATCH_CHECKBOXES );
   }

   // IF Creating THEN create the checkbox, reading from gPrefs
   if( mbCreating )
   {
      gPrefs->Read(SettingName, &bValue, bDefault);
      if( !bIsInListBox )
      {
         mCheckBoxes[ mCurrentCheckBox ] =
            CreateCheckBox( Description, bValue );
      }
      else
      {
#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
         wxASSERT( pCurrentList );
         pCurrentList->Append( Description );
         pCurrentList->Check(Counter, bValue);
#endif
      }
   }
   // ELSE we are taking the value and writing it to
   // gPrefs and then applying the settings
   else
   {
      if( !bIsInListBox )
      {
         wxASSERT( mCheckBoxes[ mCurrentCheckBox ] != NULL );
         bValue = mCheckBoxes[ mCurrentCheckBox ]->GetValue();
      }
      else
      {
#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
         wxASSERT( pCurrentList );
         bValue = pCurrentList->IsChecked( Counter );
#endif
      }
      gPrefs->Write(SettingName,bValue);
   }
}

void BatchPrefs::AddItem( wxString const & Action, wxString const & Params)
{
   int i=mList->GetItemCount();
   mList->InsertItem( i, "" );
   mList->SetItem( i, ItemNumberColumn, wxString::Format(" %02i",i+1) );
   mList->SetItem( i, ActionColumn, Action );
   mList->SetItem( i, ParamsColumn, Params );
}

/// This is a condensed list of all the check boxes.
/// This function is used in two ways :
///   a) when creating the check boxes.
///   b) when applying the settings.
/// Member variables of BatchPrefs are set before calling this function
/// so that it 'knows what to do'.
/// This removes repetitive code and guarantees that the same setting
/// names are used in saving the parameters as were used in retrieving
/// them.
void BatchPrefs::AllCheckBoxActions()
{
   // Code duplication warning: this default is repeated in Project.cpp
   // in the destructor.  -DMM
   #ifdef __WXMAC__
      const bool bQuitOnCloseDefault = false;
   #else
      const bool bQuitOnCloseDefault = true;
   #endif
   // End code duplication warning
   mCurrentCheckBox=-1;
   mCheckBoxCounters[0]=-1;
   mCheckBoxCounters[1]=-1;
   mCurrentCheckBoxContainer=0;

   CheckBoxAction(_("Batch debug mode"),  "/Batch/Debug", false);
   CheckBoxAction(_("Normalize on load"), "/Batch/NormalizeOnLoad", false );
   CheckBoxAction(_("Prompt to save, even if empty"),    "/Batch/EmptyCanBeDirty", false );
   CheckBoxAction(_("CleanSpeech Mode (Customize GUI)"), "/Batch/CleanSpeechMode", false);

   mCurrentCheckBoxContainer=1;

   CheckBoxAction(_("Show Mp3-ID3 Dialog"), "/Batch/ShowId3Dialog", false);
// CheckBoxAction(_("Show Confirmation for 'delete orphans'"), "/Batch/ShowDeleteConfirmation", true);

// Commented out code might be useful as a first step if we want an immediate response to 
// switching in and out of CleanSpeech mode.
// As things currently stand, the batch commands available will NOT reflect changes in
// CleanSpeech mode until we close and reopen the preferences dialog.
#if 0
   int mode;
   AudacityProject *proj = GetActiveProject();
   mode = gPrefs->Read(wxT("/Batch/CleanSpeechMode"), 1L);
   proj->SetCleanSpeechMode(mode == 1);
#endif
};

bool BatchPrefs::Apply()
{
   mbCreating = false;
   AllCheckBoxActions();
   unsigned int j;
   for(j = 0; j < gAudacityProjects.GetCount(); j++)
   {
      gAudacityProjects[j]->UpdateBatchPrefs();
      gAudacityProjects[j]->LayoutToolBars(); // Just to add/remove the CleanSpeech button.
   }
   mBatchCommands.SaveChain();
   return true;
}

BatchPrefs::~BatchPrefs()
{
}

void BatchPrefs::SetItem(int itemNo, const wxString command, const wxString params)
{
   // There is a 'rogue' entry of "- END -" at the end of the list
   // which users can click on to add an entry to the end.

   // There is a special entry of "No Action" in the effects list,
   // which can be used to delete an entry.

   // special case - deleting an item.
   if( command.IsSameAs("No Action"))
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
      AddItem( _("- END -"),"");
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




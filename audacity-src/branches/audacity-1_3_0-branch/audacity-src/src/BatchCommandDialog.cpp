/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommandDialog.cpp

  Dominic Mazzoni
  James Crook

**********************************************************************/
#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/button.h>
#include <wx/string.h>


#include "Audacity.h"
#include "Project.h"
#include "BatchCommandDialog.h"
#include "commands/CommandManager.h"
#include "effects/Effect.h"
#include "BatchCommands.h"


#define CommandsListID        7001
#define EditParamsButtonID    7002

BEGIN_EVENT_TABLE(BatchCommandDialog, wxDialog)
   EVT_BUTTON(wxID_OK,                     BatchCommandDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL,                 BatchCommandDialog::OnCancel)
   EVT_BUTTON(EditParamsButtonID,          BatchCommandDialog::OnEditParams)
   EVT_LIST_ITEM_SELECTED(CommandsListID,  BatchCommandDialog::OnItemSelected)
END_EVENT_TABLE();

BatchCommandDialog::BatchCommandDialog(wxWindow * parent, wxWindowID id):
   wxDialog(parent, id,
            _("Select Command"),
            wxDefaultPosition, wxSize(250,200), wxDIALOG_MODAL | wxCAPTION | wxTHICK_FRAME)
{

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   wxFlexGridSizer * gridSizer = new wxFlexGridSizer( 2,2,5,5);// 2 rows, 3 cols, 5 padding.
   gridSizer->AddGrowableCol( 1 );

   wxControl *item;
   
   item = new wxStaticText(this, -1, _("Command:"),  wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item, 0, wxALIGN_RIGHT | wxTOP, 3);

   wxBoxSizer * boxSizer = new wxBoxSizer( wxHORIZONTAL ); 
   mCommand = new wxTextCtrl(this, -1, wxT(""));
   boxSizer->Add(mCommand, 1, wxALIGN_LEFT | wxALL | wxGROW, 0);
   mEditParams = new wxButton( this, EditParamsButtonID, _("Edit Parameters"));
   boxSizer->Add(mEditParams, 0, wxALIGN_LEFT | wxLEFT, 3);
   gridSizer->Add(boxSizer, 0, wxALIGN_LEFT | wxALL | wxGROW, 0);

   item = new wxStaticText(this, -1, _("Parameters:"),  wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item, 0, wxALIGN_RIGHT | wxTOP, 3);
   mParameters = new wxTextCtrl(this, -1, wxT(""),  wxDefaultPosition, wxSize(250,-1),0);
   gridSizer->Add(mParameters, 0, wxALIGN_LEFT | wxALL | wxGROW, 0);

   mainSizer->Add(gridSizer, 0, wxALIGN_LEFT | wxALL | wxGROW, 5);

   item = new wxStaticText(this, -1,
                     _("Choose from the list below"),
                     wxDefaultPosition, wxDefaultSize, 0);

   mainSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 5);

   int i;

   mChoices = new wxListCtrl( this, CommandsListID ,
      wxDefaultPosition, wxDefaultSize,
      wxLC_LIST | wxLC_SINGLE_SEL| wxSUNKEN_BORDER 
      );

   wxASSERT( mChoices );
   for(i=0;i<99;i++)
   {
      mChoices->InsertItem( i, wxString::Format(wxT("Item%02i"),i));
   }
   PopulateCommandList();

   mainSizer->Add(mChoices, 1, wxEXPAND | wxALL, 5);

   wxBoxSizer *okSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *cancel =
       new wxButton(this, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   okSizer->Add(cancel, 0, wxALIGN_CENTRE | wxALL, 5);

   mOK = 
       new wxButton(this, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
   mOK->SetDefault();
   mOK->SetFocus();
   okSizer->Add(mOK, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(okSizer,0 , wxALIGN_CENTRE | wxALL, 5);

   SetAutoLayout(TRUE);
   SetSizer(mainSizer);
   SetSize(350, 400);
}

void BatchCommandDialog::PopulateCommandList()
{
   wxArrayString commandList = BatchCommands::GetAllCommands();

   unsigned int i;
   mChoices->DeleteAllItems();
   for( i=0;i<commandList.GetCount();i++)
   {
      mChoices->InsertItem( i, commandList[i]);
   }
}

int BatchCommandDialog::GetSelectedItem()
{
   int i;
   mSelectedCommand = wxT("");
   for(i=0;i<mChoices->GetItemCount();i++)
   {
      if( mChoices->GetItemState( i, wxLIST_STATE_FOCUSED) != 0)
      {
         mSelectedCommand = mChoices->GetItemText( i );
         return i;
      }
   }
   return -1;
}

void BatchCommandDialog::ValidateChoices()
{
   mOK->Enable(false);
}

void BatchCommandDialog::OnChoice(wxCommandEvent & event)
{
   ValidateChoices();
}

void BatchCommandDialog::OnOk(wxCommandEvent & event)
{
   mSelectedCommand = mCommand->GetValue();
   mSelectedParameters = mParameters->GetValue();
   EndModal(true );
}

void BatchCommandDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

void BatchCommandDialog::OnItemSelected(wxListEvent &event)
{
   int itemNo = event.GetIndex();
   wxString command = mChoices->GetItemText( itemNo );
   mCommand->SetValue( command );
   wxString params = BatchCommands::GetCurrentParamsFor( command );
   mParameters->SetValue( params );
}

void BatchCommandDialog::OnEditParams(wxCommandEvent &event)
{
   wxString command = mCommand->GetValue();
   wxString params  = mParameters->GetValue();
   Effect * f = BatchCommands::GetEffectFromCommandName( command );
   if( f==NULL )
      return;
   BatchCommands::SetCurrentParametersFor( f, command, params );
   if( BatchCommands::PromptForParamsFor( command ))
   {
      // we've just prompted for the parameters, so the values
      // that are current have changed.
      params = BatchCommands::GetCurrentParamsFor( command );
      mParameters->SetValue( params );
      mParameters->Refresh();
   }
}

void BatchCommandDialog::SetCommandAndParams(const wxString &Command, const wxString &Params)
{
   mCommand->SetValue( Command );
   mParameters->SetValue( Params );
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

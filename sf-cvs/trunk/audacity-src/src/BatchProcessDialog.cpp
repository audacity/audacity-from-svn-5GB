/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchProcessDialog.cpp

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
#include <wx/imaglist.h>
#include <wx/msgdlg.h>

#include "Audacity.h"
#include "Project.h"
#include "BatchProcessDialog.h"
#include "commands/CommandManager.h"
#include "effects/Effect.h"
#include "../images/Arrow.xpm"
#include "BatchCommands.h"


#define FileListID 7001

BEGIN_EVENT_TABLE(BatchProcessDialog, wxDialog)
   EVT_BUTTON(wxID_OK,            BatchProcessDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL,        BatchProcessDialog::OnCancel)
END_EVENT_TABLE()

BatchProcessDialog::BatchProcessDialog(wxWindow * parent, wxWindowID id):
   wxDialog(parent, id,
            _("Batch Processing"),
            wxPoint(20,20), wxDefaultSize, wxDIALOG_MODAL | wxCAPTION | wxTHICK_FRAME)
{
   mAbort = false;

   AudacityProject * p = GetActiveProject();
   if( p->GetCleanSpeechMode() )
   {
      SetTitle( _("CleanSpeech Batch Processing") );
   }
   mBatchCommands = new BatchCommands;
   mBatchCommands->ReadChain();

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   wxControl *item;

   item = new wxStaticText(this, -1,
                     _("Audio files to be processed"),
                     wxDefaultPosition, wxDefaultSize, 0);

   mainSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 5);

   mList = new wxListCtrl(this, FileListID, wxDefaultPosition, wxSize(350, 180),
                          wxLC_REPORT | wxLC_HRULES | wxLC_VRULES | wxSUNKEN_BORDER  /* | wxLC_EDIT_LABELS */);
   mList->SetSizeHints(350, 180);

   wxImageList *imageList = new wxImageList(9, 16);
   imageList->Add(wxIcon(empty_9x16_xpm));
   imageList->Add(wxIcon(arrow_xpm));
   mList->AssignImageList(imageList, wxIMAGE_LIST_SMALL);
   mList->InsertColumn(0, _("File"), wxLIST_FORMAT_LEFT, 280);
//   mList->InsertColumn(1, _("Size"), wxLIST_FORMAT_LEFT, 66);
//   mList->InsertColumn(2, _("Time"), wxLIST_FORMAT_LEFT, 66);

   mainSizer->Add(mList, 1, wxEXPAND | wxALL, 5);

   wxBoxSizer *okSizer = new wxBoxSizer(wxHORIZONTAL);

   mCancel =
       new wxButton(this, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   okSizer->Add(mCancel, 0, wxALIGN_CENTRE | wxALL, 5);

   mOK = 
       new wxButton(this, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
   mOK->SetDefault();
   mOK->SetFocus();
   okSizer->Add(mOK, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(okSizer,0 , wxALIGN_CENTRE | wxALL, 5);

   SetAutoLayout(TRUE);
   SetSizer(mainSizer);
   SetSize(320, 400);
}

BatchProcessDialog::~BatchProcessDialog()
{
   if( mBatchCommands )
      delete mBatchCommands;
}
void BatchProcessDialog::PopulateList(wxArrayString fileList)
{
   unsigned int i;
   mList->DeleteAllItems(); // Delete contents.
   for(i=0;i<fileList.GetCount();i++)
   {
      mList->InsertItem( i, fileList[i], i==0 );
   }
}

void BatchProcessDialog::ValidateChoices()
{
   mOK->Enable(false);
}

void BatchProcessDialog::OnChoice(wxCommandEvent & event)
{
   ValidateChoices();
}

void BatchProcessDialog::OnOk(wxCommandEvent & event)
{
   mOK->Enable(false);
   DoProcessing();
   EndModal(true);
}

void BatchProcessDialog::OnCancel(wxCommandEvent & event)
{
   // Can happen if user clicks Cancel before clicking Ok
   if( mOK->IsEnabled() )
   {
      EndModal(false);
      return;
   }

   mCancel->Enable(false);

   mAbort = true;
   mBatchCommands->AbortBatch();
}

void BatchProcessDialog::DoProcessing()
{
   int i;
   wxString Temp;
   for(i=0;i<mList->GetItemCount();i++)
   {
      if( i>0 )
      {
         //Clear the arrow in previous item.
         mList->SetItemImage( i-1,0,0);
      }
      mList->SetItemImage( i, 1,1);
      if( !ProcessOne( mList->GetItemText( i )) || mAbort )
         return;
   }
}

bool BatchProcessDialog::ProcessOne(const wxString Filename)
{
   return mBatchCommands->ApplyBatchToNamedFile( Filename );
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



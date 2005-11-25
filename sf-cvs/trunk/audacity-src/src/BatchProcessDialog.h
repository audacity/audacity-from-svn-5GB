/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchProcessDialog.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_PROCESS_DIALOG__
#define __AUDACITY_BATCH_PROCESS_DIALOG__

#include <wx/defs.h>
#include <wx/string.h>


#ifdef __WXMSW__
    #include  <wx/ownerdrw.h>
#endif

//#include  "wx/log.h"
#include  <wx/sizer.h>
#include  <wx/menuitem.h>
#include  <wx/checklst.h>

class wxWindow;
class wxCheckBox;
class wxChoice;
class wxTextCtrl;
class wxStaticText;
class wxRadioButton;
class wxListCtrl;
class wxListEvent;
class wxButton;
class BatchCommands;

class BatchProcessDialog:public wxDialog {
 public:
   // constructors and destructors
   BatchProcessDialog(wxWindow * parent, wxWindowID id);
   virtual ~BatchProcessDialog();
 public:
	bool ProcessOne( const wxString Filename );
	void DoProcessing();
   void OnChoice(wxCommandEvent & event);
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

   void ValidateChoices();
   void PopulateList(wxArrayString fileList);

   wxButton   *mOK;
   wxButton   *mCancel;
   wxListCtrl *mList;
   BatchCommands * mBatchCommands;

   bool mAbort;

   DECLARE_EVENT_TABLE()
};


#endif

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


/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommandDialog.h

  Brian Gunlogson
  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_BATCH_COMMAND_DIALOG__
#define __AUDACITY_BATCH_COMMAND_DIALOG__

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

class BatchCommandDialog:public wxDialog {
 public:
   // constructors and destructors
   BatchCommandDialog(wxWindow * parent, wxWindowID id,
                     unsigned int format);
 public:
	void SetCommandAndParams( const wxString &Command, const wxString & Params);
	void OnEditParams(wxCommandEvent &event);
   void OnChoice(wxCommandEvent & event);
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnItemSelected(wxListEvent &event);

   void ValidateChoices();
   void PopulateCommandList();
   int GetSelectedItem();

   wxString   mSelectedCommand;
   wxString   mSelectedParameters;
   wxButton   *mOK;
   wxButton   *mEditParams;
   wxListCtrl *mChoices;
   wxTextCtrl * mCommand;
   wxTextCtrl * mParameters;
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


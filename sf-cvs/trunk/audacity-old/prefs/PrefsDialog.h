/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_PREFS_DIALOG__
#define __AUDACITY_PREFS_DIALOG__

#include <wx/dialog.h>
#include <wx/window.h>
#include <wx/event.h>
#include <wx/listbox.h>
#include <wx/button.h>

class PrefsDialog:public wxDialog {

 public:
   PrefsDialog(wxWindow * parent);
   ~PrefsDialog();

   void OnCategoryChange(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
    wxListBox * mCategories;
   wxButton *mOK;
   wxButton *mCancel;

   int mSelected;

 public:
    DECLARE_EVENT_TABLE()
};

#endif

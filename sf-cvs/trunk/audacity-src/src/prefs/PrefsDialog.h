/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_PREFS_DIALOG__
#define __AUDACITY_PREFS_DIALOG__

#include <wx/dialog.h>

class wxNotebook;
class wxWindow;
class wxButton;
class wxCommandEvent;

class PrefsDialog:public wxDialog {

 public:
   PrefsDialog(wxWindow * parent);
   ~PrefsDialog();

   void OnCategoryChange(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

   void SelectPageByName(wxString pageName);
   void ShowTempDirPage();

 private:
   wxNotebook *mCategories;
   wxButton *mOK;
   wxButton *mCancel;

   int mSelected;

 public:
    DECLARE_EVENT_TABLE()
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  Warning.cpp

  Dominic Mazzoni

*******************************************************************//**

\class WarningDialog
\brief Gives a warning message, that can be dismissed, with crucially
the ability to not see similar warnings again for this session.

*//********************************************************************/



#include "../Audacity.h"

#include "Warning.h"

#include "../Prefs.h"
#include "../ShuttleGui.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

class WarningDialog : public wxDialog
{
 public:
   // constructors and destructors
   WarningDialog(wxWindow *parent, 
                 wxString message);

   bool dontShow;
   
 private:
   void OnOK(wxCommandEvent& event);

   wxCheckBox *mCheckBox;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(WarningDialog, wxDialog)
   EVT_BUTTON(wxID_OK, WarningDialog::OnOK)
END_EVENT_TABLE()

WarningDialog::WarningDialog(wxWindow *parent, wxString message)
:  wxDialog(parent, wxID_ANY, (wxString)_("Warning")),
   dontShow( false )
{
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(10);
   S.StartVerticalLay(false);
   {
      S.AddUnits(message);
      mCheckBox = S.TieCheckBox(_("Don't show this warning again"), dontShow);
   }

   S.SetBorder(0);
   S.AddStandardButtons(eOkButton);

   Fit();
}

void WarningDialog::OnOK(wxCommandEvent& event)
{
   dontShow = mCheckBox->GetValue();

   EndModal(wxID_OK);
}

void ShowWarningDialog(wxWindow *parent,
                       wxString internalDialogName,
                       wxString message)
{
   bool dontShow = false;

   gPrefs->SetPath(wxT("/Warnings"));
   gPrefs->Read(internalDialogName, &dontShow, false);
   gPrefs->SetPath(wxT("/"));

   if (dontShow)
      return;

   WarningDialog dlog(parent, message);
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.dontShow) {
      gPrefs->SetPath(wxT("/Warnings"));
      gPrefs->Write(internalDialogName, true);
      gPrefs->SetPath(wxT("/"));
   }
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
// arch-tag: b84d77e0-4375-43f0-868e-3130e18c14c8


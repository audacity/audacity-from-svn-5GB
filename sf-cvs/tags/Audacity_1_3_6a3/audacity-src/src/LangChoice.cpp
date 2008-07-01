/**********************************************************************

  Audacity: A Digital Audio Editor

  LangChoice.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class LangChoiceDialog
\brief A dialog used (at start up) to present the user with a choice
of languages for Audacity.

*//*******************************************************************/


#include "Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include "LangChoice.h"
#include "Languages.h"
#include "ShuttleGui.h"

class LangChoiceDialog:public wxDialog {
public:
   LangChoiceDialog(wxWindow * parent,
                    wxWindowID id,
                    const wxString & title);
   
   wxString GetLang() { return mLang; }

private:
   void OnOk(wxCommandEvent & event);
   
   wxChoice *mChoice;
   wxString mLang;

   int mNumLangs;
   wxArrayString mLangCodes;
   wxArrayString mLangNames;
 
   DECLARE_EVENT_TABLE()
};

wxString ChooseLanguage(wxWindow *parent)
{
   wxString returnVal;

   LangChoiceDialog dlog(parent, -1, _("Audacity First Run"));
   dlog.CentreOnParent();
   dlog.ShowModal();
   returnVal = dlog.GetLang();

   return returnVal;
}

BEGIN_EVENT_TABLE(LangChoiceDialog, wxDialog)
    EVT_BUTTON(wxID_OK, LangChoiceDialog::OnOk)
END_EVENT_TABLE()

LangChoiceDialog::LangChoiceDialog(wxWindow * parent,
                                   wxWindowID id,
                                   const wxString & title):
   wxDialog(parent, id, title)
{
   GetLanguages(mLangCodes, mLangNames);
   int ndx = mLangCodes.Index(GetSystemLanguageCode());
   wxString lang;

   if (ndx != wxNOT_FOUND) {
      lang = mLangNames[ndx];
   }

   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay(false);
   {
      S.StartHorizontalLay();
      {
         S.SetBorder(15);
         mChoice = S.AddChoice(_("Choose Language for Audacity to use:"),
                              lang,
                              &mLangNames);
      }
      S.EndVerticalLay();

      S.SetBorder(0);
      S.AddStandardButtons(eOkButton);
   }
   S.EndVerticalLay();

   Fit();
}

void LangChoiceDialog::OnOk(wxCommandEvent & event)
{
   mLang = mLangCodes[mChoice->GetSelection()];

   EndModal(true);
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
// arch-tag: 16ac4580-c6b2-4ba7-bcaf-b026f2d58cdb


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
   mNumLangs = mLangNames.GetCount();

   wxString sysLang = GetSystemLanguageCode();

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hSizer;

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   hSizer->Add(new wxStaticText(this, -1,
                                _("Choose Language for Audacity to use:")),
               0, wxALIGN_CENTRE | wxALIGN_CENTER_VERTICAL | wxALL, 8);

   wxString *langArray = new wxString[mNumLangs];
   int i;
   for(i=0; i<mNumLangs; i++)
      langArray[i] = mLangNames[i];
   mChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                          mNumLangs, langArray);
   mChoice->SetSelection(0); // in case nothing else matches
   delete[] langArray;
   for(i=0; i<mNumLangs; i++)
      if (mLangCodes[i] == sysLang)
         mChoice->SetSelection(i);
   hSizer->Add(mChoice,
               0, wxALIGN_CENTRE | wxALIGN_CENTER_VERTICAL | wxALL, 8);

   mainSizer->Add(hSizer,
                  0, wxALL, 8);

   mainSizer->Add(CreateStdButtonSizer(this, eOkButton), 0, wxEXPAND);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
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


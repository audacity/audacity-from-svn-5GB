/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/window.h>
#include <wx/dialog.h>
#include <wx/gdicmn.h>
#include <wx/listbox.h>
#include <wx/font.h>
#include <wx/msgdlg.h>

#include "../Prefs.h"

#include "PrefsDialog.h"
#include "PrefsPanel.h"

#include "AudioIOPrefs.h"
#include "SampleRatePrefs.h"
#include "FileFormatPrefs.h"
#include "SpectrumPrefs.h"
#include "DirectoriesPrefs.h"


enum {
   CategoriesID = 1000
};

BEGIN_EVENT_TABLE(PrefsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, PrefsDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, PrefsDialog::OnCancel)
END_EVENT_TABLE()



PrefsDialog::PrefsDialog(wxWindow * parent):
wxDialog(parent, -1, "Audacity Preferences", wxDefaultPosition,
         wxDefaultSize, wxDIALOG_MODAL | wxCAPTION | wxTHICK_FRAME)
{
   CentreOnParent();

   wxBoxSizer *topSizer = new wxBoxSizer(wxVERTICAL);

   mCategories = new wxNotebook(this, -1, wxDefaultPosition, wxDefaultSize,
                                wxNB_LEFT);
   wxNotebookSizer *catSizer = new wxNotebookSizer(mCategories);


   topSizer->Add(catSizer, 1, wxGROW | wxALL, 0);

   /* All panel additions belong here */
   mCategories->AddPage(new AudioIOPrefs(mCategories), "Audio I/O");
   mCategories->AddPage(new SampleRatePrefs(mCategories), "Sample Rates");
   mCategories->AddPage(new FileFormatPrefs(mCategories), "File Formats");
   mCategories->AddPage(new SpectrumPrefs(mCategories), "Spectrograms");
   mCategories->AddPage(new DirectoriesPrefs(mCategories), "Directories");

   long selected = gPrefs->Read("/Prefs/PrefsCategory", 0L);
   if (selected < 0 || selected >= mCategories->GetPageCount())
      mSelected = 0;

   mCategories->SetSelection(selected);

   mOK = new wxButton(this,
                      wxID_OK, "OK");

#ifndef TARGET_CARBON
   mOK->SetDefault();
   mOK->SetFocus();
#endif

   mCancel = new wxButton(this,
                          wxID_CANCEL,
                          "Cancel");

   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

   buttonSizer->Add(mCancel, 0, wxALL, 7);
   buttonSizer->Add(mOK, 0, wxALL, 7);
   
   topSizer->Add(buttonSizer, 0, wxALIGN_RIGHT);

   SetAutoLayout(true);
   SetSizer(topSizer);
   topSizer->Fit(this);
   topSizer->SetSizeHints(this);

   #ifdef __WXMAC__
   // Until sizing works properly on the Mac
   SetSize(500, 350);
   #endif
}


void PrefsDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(0);
}


void PrefsDialog::OnOK(wxCommandEvent & event)
{
   PrefsPanel *panel;

   gPrefs->Write("/Prefs/PrefsCategory", (long)mCategories->GetSelection());

   for (int i = 0; i < mCategories->GetPageCount(); i++) {
      panel = (PrefsPanel *) mCategories->GetPage(i);

      /* The dialog doesn't end until all the input is valid */
      if (!panel->Apply()) {
         mCategories->SetSelection(i);
         mSelected = i;
         return;
      }
   }

   EndModal(0);
}


PrefsDialog::~PrefsDialog()
{
   delete mCategories;
   delete mOK;
   delete mCancel;
}

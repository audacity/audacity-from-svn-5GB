/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/font.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/listbox.h>
#include <wx/msgdlg.h>
#include <wx/notebook.h>
#include <wx/sizer.h>

#include "../Prefs.h"

#include "PrefsDialog.h"
#include "PrefsPanel.h"

#include "AudioIOPrefs.h"
#include "DirectoriesPrefs.h"
#include "FileFormatPrefs.h"
#include "GUIPrefs.h"
#include "KeyConfigPrefs.h"
#include "QualityPrefs.h"
#include "SpectrumPrefs.h"

enum {
   CategoriesID = 1000
};

BEGIN_EVENT_TABLE(PrefsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, PrefsDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, PrefsDialog::OnCancel)
END_EVENT_TABLE()



PrefsDialog::PrefsDialog(wxWindow * parent):
wxDialog(parent, -1, _("Audacity Preferences"), wxDefaultPosition,
         wxDefaultSize, wxDIALOG_MODAL | wxCAPTION | wxTHICK_FRAME)
{
   CentreOnParent();

   wxRect rect = GetRect();
   if(rect.x < 0) rect.x = 0;
   if(rect.y < 0) rect.y = 0;
   SetSize(rect);

   wxBoxSizer *topSizer = new wxBoxSizer(wxVERTICAL);

   mCategories = new wxNotebook(this, -1, wxDefaultPosition, wxDefaultSize
#ifdef __WXGTK__
                                ,wxNB_LEFT
#endif
                                );
   wxNotebookSizer *catSizer = new wxNotebookSizer(mCategories);


   topSizer->Add(catSizer, 1, wxGROW | wxALL, 0);

   /* All panel additions belong here */
   mCategories->AddPage(new AudioIOPrefs(mCategories), _("Audio I/O"));
   mCategories->AddPage(new QualityPrefs(mCategories), _("Quality"));
   mCategories->AddPage(new FileFormatPrefs(mCategories), _("File Formats"));
   mCategories->AddPage(new SpectrumPrefs(mCategories), _("Spectrograms"));
   mCategories->AddPage(new DirectoriesPrefs(mCategories), _("Directories"));
   mCategories->AddPage(new GUIPrefs(mCategories), _("Interface"));
   mCategories->AddPage(new KeyConfigPrefs(mCategories), _("Configure Keyboard"));

   long selected = gPrefs->Read("/Prefs/PrefsCategory", 0L);
   if (selected < 0 || selected >= mCategories->GetPageCount())
      mSelected = 0;

   mCategories->SetSelection(selected);

   mOK = new wxButton(this,
                      wxID_OK, _("OK"));

#ifndef TARGET_CARBON
   mOK->SetDefault();
   mOK->SetFocus();
#endif

   mCancel = new wxButton(this,
                          wxID_CANCEL,
                          _("Cancel"));

   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

   buttonSizer->Add(mCancel, 0, wxALL, 7);
   buttonSizer->Add(mOK, 0, wxALL, 7);
   
   topSizer->Add(buttonSizer, 0, wxALIGN_RIGHT);

   wxBoxSizer *outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);
   outSizer->Fit(this);
   outSizer->SetSizeHints(this);

   #ifdef __WXMAC__
   // Until sizing works properly on the Mac
   SetSize(525, 350);
   #endif

   #ifdef __WXMSW__
   // Because it looks nice (tm)   (you can see all the tabs at once)
   SetSize(525, 363);
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
}

void PrefsDialog::SelectPageByName(wxString pageName)
{
   int n = mCategories->GetPageCount();

   int i;

   for(i=0; i<n; i++)
      if (mCategories->GetPageText(i) == pageName) {
         mCategories->SetSelection(i);
         return;
      }
}


void PrefsDialog::ShowTempDirPage()
{
   SelectPageByName(_("Directories"));   
}


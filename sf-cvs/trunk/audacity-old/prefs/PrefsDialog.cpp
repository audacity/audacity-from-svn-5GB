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

#include "PrefsDialog.h"
#include "PrefsPanel.h"

#include "AudioIOPrefs.h"
#include "SampleRatePrefs.h"
#include "FileFormatPrefs.h"
#include "SpectrumPrefs.h"


enum {
	CategoriesID = 1000
};

BEGIN_EVENT_TABLE(PrefsDialog, wxDialog)
	EVT_LISTBOX(CategoriesID, PrefsDialog::OnCategoryChange)
	EVT_BUTTON(wxID_OK, PrefsDialog::OnOK)
	EVT_BUTTON(wxID_CANCEL, PrefsDialog::OnCancel)
END_EVENT_TABLE()



PrefsDialog::PrefsDialog(wxWindow *parent):
	wxDialog(parent, -1, "Audacity Preferences", wxDefaultPosition,
			 wxSize(500, 420), wxDIALOG_MODAL)
{
    CentreOnParent();

	mCategories = new wxListBox(this,
	                            CategoriesID, 
								wxPoint(20, 20),
								wxSize(120, 350));

	mOK         = new wxButton (this, 
	                            wxID_OK,
								"OK",
								wxPoint(400, 385),
								wxSize(80, 20));

    #ifndef TARGET_CARBON
    mOK->SetDefault();
    mOK->SetFocus();							  
    #endif

	mCancel      = new wxButton(this, 
	                            wxID_CANCEL,
								"Cancel",
								wxPoint(300, 385),
								wxSize(90, 20));

	/* All panel additions belong here */
	#ifdef __WXGTK__
	mCategories->Append("Audio I/O",    new AudioIOPrefs(this));
	#endif
	mCategories->Append("Sample Rates", new SampleRatePrefs(this));
	mCategories->Append("File Formats", new FileFormatPrefs(this));
	mCategories->Append("Spectrograms", new SpectrumPrefs(this));

  PrefsPanel *panel;

	for(int i = 0; i < mCategories->Number(); i++) {
		panel = (PrefsPanel *)mCategories->GetClientData(i);
		panel->HidePrefsPanel();
  }

	mCategories->SetSelection(0);
	panel = (PrefsPanel *)mCategories->GetClientData(0);
	panel->ShowPrefsPanel();

	mSelected = 0;
}


void PrefsDialog::OnCategoryChange(wxCommandEvent& event)
{
  int newSelection = mCategories->GetSelection();
  
  if (newSelection>=0 && newSelection<mCategories->Number()
      && newSelection != mSelected) {

  	/* hide the old panel */
  	PrefsPanel *panel = (PrefsPanel *)mCategories->GetClientData(mSelected);
  	panel->HidePrefsPanel();
  	
  	/* show the new one */
  	panel=(PrefsPanel*)mCategories->GetClientData(newSelection);
  	panel->ShowPrefsPanel();

  	mSelected = newSelection;
  }
}


void PrefsDialog::OnCancel(wxCommandEvent& event)
{
	EndModal(0);
}


void PrefsDialog::OnOK(wxCommandEvent& event)
{
	PrefsPanel *panel;

	for(int i = 0; i < mCategories->Number(); i++) {
		panel = (PrefsPanel *)mCategories->GetClientData(i);

		/* The dialog doesn't end until all the input is valid */
		if(!panel->Apply()) {
			PrefsPanel *tmp = 
				(PrefsPanel*)mCategories->GetClientData(mSelected);
			tmp->HidePrefsPanel();
			panel->ShowPrefsPanel();

			mCategories->SetSelection(i);
			mSelected = i;
			return;
		}
	}

	EndModal(0);
}


PrefsDialog::~PrefsDialog()
{
	PrefsPanel *panel;
	for(int i = 0; i < mCategories->Number(); i++) {
		panel = (PrefsPanel *)mCategories->GetClientData(i);
		delete panel;
	}
	
	delete mCategories;
	delete mOK;
	delete mCancel;
}

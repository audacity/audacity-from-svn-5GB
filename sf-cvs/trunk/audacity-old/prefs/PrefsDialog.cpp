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


enum {
	CategoriesID = 1000
};

BEGIN_EVENT_TABLE(PrefsDialog, wxDialog)
	EVT_LISTBOX(CategoriesID, PrefsDialog::OnCategoryChange)
	EVT_BUTTON(wxID_OK, PrefsDialog::OnOK)
	EVT_BUTTON(wxID_CANCEL, PrefsDialog::OnCancel)
END_EVENT_TABLE()



PrefsDialog::PrefsDialog():
	wxDialog(NULL, -1, "Audacity Preferences", wxDefaultPosition,
			 wxSize(500, 400), wxDIALOG_MODAL)
{
	mCategories = new wxListBox(this,
	                            CategoriesID, 
								wxPoint(20, 20),
								wxSize(120, 350));
	mOK         = new wxButton (this, 
	                            wxID_OK,
								"OK",
								wxPoint(400, 375));
	mCancel      = new wxButton(this, 
	                            wxID_CANCEL,
								"Cancel",
								wxPoint(310, 375));
	
	/* All panel additions belong here */
	mCategories->Append("Audio I/O",    new AudioIOPrefs(this));
	mCategories->Append("Sample Rates", new SampleRatePrefs(this));
	mCategories->Append("File Formats", new FileFormatPrefs(this));

	mCategories->SetSelection(0);
	PrefsPanel *panel = (PrefsPanel *)mCategories->GetClientData(0);
	panel->Show();

	mSelected = 0;
}


void PrefsDialog::OnCategoryChange(wxCommandEvent& event)
{
	/* hide the old panel */
	PrefsPanel *panel = (PrefsPanel *)mCategories->GetClientData(mSelected);
	panel->Show(false);
	
	/* show the new one */
	panel=(PrefsPanel*)mCategories->GetClientData(mCategories->GetSelection());
	panel->Show();

	mSelected = mCategories->GetSelection();
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
			tmp->Show(false);
			panel->Show();

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

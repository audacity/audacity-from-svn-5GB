/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/dialog.h"

#include "AboutDialog.h"
#include "AudacityApp.h"

#include "xpm/AudacityLogo.xpm"

// ----------------------------------------------------------------------------
// icons
// ----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(AboutDialog, wxDialog)
        EVT_BUTTON(wxID_OK, AboutDialog::OnOK)
        EVT_BUTTON(wxID_YES, AboutDialog::OnOK)
        EVT_BUTTON(wxID_NO, AboutDialog::OnOK)
        EVT_BUTTON(wxID_CANCEL, AboutDialog::OnOK)
END_EVENT_TABLE()

IMPLEMENT_CLASS(AboutDialog, wxDialog)

AboutDialog::AboutDialog()
  : wxDialog( (wxFrame *)NULL, -1, "About Audacity...",
  wxDefaultPosition, wxSize(530, 290))
{
  wxString fullMessage("Audacity:\n"
					   "A New Digital Audio Editor\n"
					   "by Dominic Mazzoni, Roger Dannenberg, "
					   "Jason Cohen, Robert Leidle,\n"
					   "Mark Tomlinson, and Joshua Haberman.\n"
					   "Version " AUDACITY_VERSION_STRING "\n"
					   "http://www.cs.cmu.edu/~music/audacity/");
  
  wxString caption("A New Digital Audio Editor\n"
				   "by Dominic Mazzoni, Roger Dannenberg, "
				   "Jason Cohen, Robert Leidle,\n"
				   "Mark Tomlinson, and Joshua Haberman.\n"
				   "Version " AUDACITY_VERSION_STRING "\n"
				   "http://www.cs.cmu.edu/~music/audacity/");
				   

  new wxStaticText( this, -1, caption, wxPoint(8, 150), wxSize(514, 100) );
  
  wxButton *ok = new wxButton(this, wxID_OK,
							  "Audacious!",
							  wxPoint(400, 260),
							  wxSize(100, 20));
	#ifndef TARGET_CARBON
	ok->SetDefault();
	ok->SetFocus();
	#endif
	
	logo = new wxBitmap((const char **)AudacityLogo);

  icon =
	  new wxStaticBitmap(this, -1, *logo, wxPoint(0, 0), wxSize(530, 142));
}

AboutDialog::~AboutDialog()
{
  delete icon;
  delete logo;
}

void AboutDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
  EndModal( wxID_YES );
}


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

AboutDialog::AboutDialog(wxWindow *parent)
  : wxDialog(parent, -1, "About Audacity...",
  wxDefaultPosition, wxSize(400, 400))
{
  Centre();

  wxWindow *panel = new wxWindow(this, -1,
								 wxPoint(0, 0),
								 wxSize(400, 400));
  panel->SetBackgroundColour(wxColour(255, 255, 255));

  wxString topMessage("Audacity version " AUDACITY_VERSION_STRING);

  wxString botMessage("A New Digital Audio Editor\n"
					  "by Dominic Mazzoni, Roger Dannenberg, "
					  "Jason Cohen, \n"
					  "Robert Leidle, "
					  "Mark Tomlinson, and Joshua Haberman.\n"
					  "Logo by Harvey Lubin.\n\n"
					  "http://www.cs.cmu.edu/~music/audacity/");

  new wxStaticText( panel, -1, topMessage, wxPoint(10, 10), wxSize(380, 20),
					wxALIGN_CENTER);

  new wxStaticText( panel, -1, botMessage, wxPoint(10, 230), wxSize(380, 130),
					wxALIGN_CENTER);
  
  wxButton *ok = new wxButton(panel, wxID_OK,
							  "Audacious!",
							  wxPoint(150, 370),
							  wxSize(100, 20));
  #ifndef TARGET_CARBON
  ok->SetDefault();
  ok->SetFocus();
  #endif
	
  logo = new wxBitmap((const char **)AudacityLogo);

  icon =
	  new wxStaticBitmap(panel, -1, *logo, wxPoint(93, 30), wxSize(215, 190));
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


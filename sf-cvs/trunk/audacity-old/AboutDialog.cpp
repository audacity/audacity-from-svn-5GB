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
  wxDefaultPosition, wxSize(530, 262))
{
  wxString fullMessage("Audacity:\n"
					   "A New Digital Audio Editor\n"
					   "by Dominic Mazzoni, Roger Dannenberg, "
					   "Jason Cohen, Robert Leidle,"
					   "Mark Tomlinson, and Joshua Haberman.\n"
					   "Version " AUDACITY_VERSION_STRING "\n"
					   "http://www.cs.cmu.edu/~music/audacity/");
  
  wxString caption("A New Digital Audio Editor\n"
				   "by Dominic Mazzoni, Roger Dannenberg, "
				   "Jason Cohen, Robert Leidle, "
				   "Mark Tomlinson, and Joshua Haberman.\n"
				   "Version " AUDACITY_VERSION_STRING "\n"
				   "http://www.cs.cmu.edu/~music/audacity/");
				   

  new wxStaticText( this, -1, caption, wxPoint(8, 150), wxSize(514, 76) );
  
  wxButton *ok = new wxButton(this, wxID_OK,
							  "Audacious!",
							  wxPoint(400, 232),
							  wxSize(100, 20));
	#ifndef TARGET_CARBON
	ok->SetDefault();
	ok->SetFocus();
	#endif
	
	logo = new wxBitmap((const char **)AudacityLogo);

  icon =
	  new wxStaticBitmap(this, -1, *logo, wxPoint(0, 0), wxSize(530, 142));
				   
	/*			   
  
  topsizer = new wxBoxSizer( wxVERTICAL );
  
  
  
  topsizer->Add( icon, 0, wxCENTER );
  
  topsizer->Add( CreateTextSizer(caption), 0, wxCENTER, 10 );
  
  topsizer->Add( CreateButtonSizer( wxOK ),
				 0, wxCENTRE | wxALL, 10 );
  
  SetAutoLayout( TRUE );
  SetSizer( topsizer );
  
  topsizer->SetSizeHints( this );
  topsizer->Fit( this );
  wxSize size( GetSize() );
  if (size.x < size.y*3/2) {
	size.x = size.y*3/2;
	SetSize( size );
  }
  Centre( wxBOTH | wxCENTER_FRAME);
  */  
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


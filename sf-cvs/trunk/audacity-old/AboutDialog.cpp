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
  wxDefaultPosition, wxDefaultSize)
{
  wxString fullMessage("Audacity:\n"
					   "A New Digital Audio Editor\n"
					   "by Dominic Mazzoni, Roger Dannenberg, "
					   "Jason Cohen, Robert Leidle,"
					   "and Mark Tomlinson.\n"
					   "Version " AUDACITY_VERSION_STRING "\n"
					   "http://www.cs.cmu.edu/~music/audacity/");
  
  wxString caption("A New Digital Audio Editor\n"
				   "by Dominic Mazzoni, Roger Dannenberg, "
				   "Jason Cohen, Robert Leidle, "
				   "and Mark Tomlinson.\n"
				   "Version " AUDACITY_VERSION_STRING "\n"
				   "http://www.cs.cmu.edu/~music/audacity/");
  
  topsizer = new wxBoxSizer( wxVERTICAL );
  
  logo = new wxBitmap(AudacityLogo);

  icon =
	new wxStaticBitmap(this, -1, *logo, wxPoint(0, 0), wxSize(530, 142));
  
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
  
}

AboutDialog::~AboutDialog()
{
  // TODO: Is there anything we need to clean up?
}

void AboutDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
  EndModal( wxID_YES );
}


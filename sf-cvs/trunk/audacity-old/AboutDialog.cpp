/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/dialog.h"

#include "AboutDialog.h"
#include "AudacityApp.h"

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
  : wxDialog( (wxFrame *)NULL, -1, "About Audacity...")
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
  
  logo = new wxBitmap();
  
#ifdef __WXMSW__
  if (logo->LoadFile("AudacityLogo",wxBITMAP_TYPE_BMP_RESOURCE)) {
#else
#ifdef __WXMAC__
  if (logo->LoadFile("AudacityLogo",wxBITMAP_TYPE_PICT_RESOURCE)) {
#else
  if (logo->LoadFile("icons/AudacityLogo.XPM",wxBITMAP_TYPE_XPM)) {
#endif
#endif

	icon =
	  new wxStaticBitmap(this, -1, *logo, wxPoint(0, 0), wxSize(530, 142));
	
	topsizer->Add( icon, 0, wxCENTER );
	topsizer->Add( CreateTextSizer(caption), 0, wxCENTER, 10 );
  }
  else {
	topsizer->Add( CreateTextSizer(fullMessage), 0, wxCENTER, 10 );	
  }
  
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


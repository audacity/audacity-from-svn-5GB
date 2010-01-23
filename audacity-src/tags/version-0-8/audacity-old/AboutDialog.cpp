/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/dialog.h"

#include "AboutDialog.h"


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

AboutDialog::AboutDialog( wxWindow *parent,
						  wxBitmap *bitmap,
						  const wxString& message,
						  const wxPoint& pos)
  : wxDialog( parent, -1, "About", pos, wxDefaultSize, wxDEFAULT_DIALOG_STYLE )
{
    wxBeginBusyCursor();

    wxBoxSizer *topsizer = new wxBoxSizer( wxVERTICAL );

	wxStaticBitmap *icon =
	  new wxStaticBitmap(this, -1, *bitmap, wxPoint(0, 0), wxSize(530, 142));

	topsizer->Add( icon, 0, wxCENTER );
    topsizer->Add( CreateTextSizer(message), 0, wxCENTER, 10 );
    
    // 3) button
    topsizer->Add( CreateButtonSizer( wxOK ),
				   0, wxCENTRE | wxALL, 10 );

    SetAutoLayout( TRUE );
    SetSizer( topsizer );
    
    topsizer->SetSizeHints( this );
    topsizer->Fit( this );
    wxSize size( GetSize() );
    if (size.x < size.y*3/2)
    {
        size.x = size.y*3/2;
		SetSize( size );
    }

    Centre( wxBOTH | wxCENTER_FRAME);

    wxEndBusyCursor();
}

void AboutDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
    EndModal( wxID_YES );
}


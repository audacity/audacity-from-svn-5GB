/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorDialog.cpp

  Jimmy Johnson

*******************************************************************//**

\class ErrorDialog
Gives an Error message with an option for help.

*//********************************************************************/

#include "../Audacity.h"

#include <wx/button.h>

#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/utils.h>

#include "LinkingHtmlWindow.h"
#include "../ShuttleGui.h"
#include "../HelpText.h"
#include "../Internat.h"

class ErrorDialog : public wxDialog
{
   public:
   // constructors and destructors
   ErrorDialog(wxWindow *parent, 
      const wxString & dlogTitle, 
      const wxString & message, 
      const wxString & helpURL);

private:
	wxString dhelpURL;
	
   void OnOk( wxCommandEvent &event );
   void OnHelp( wxCommandEvent &event );
   DECLARE_EVENT_TABLE()
	   
};

BEGIN_EVENT_TABLE(ErrorDialog, wxDialog)
   EVT_BUTTON( wxID_OK, ErrorDialog::OnOk)
   EVT_BUTTON( wxID_HELP, ErrorDialog::OnHelp)
END_EVENT_TABLE()

ErrorDialog::ErrorDialog(
   wxWindow *parent, 
   const wxString & dlogTitle, 
   const wxString & message, 
   const wxString & helpURL):
   wxDialog(parent, (wxWindowID)-1, dlogTitle)
{
   dhelpURL = helpURL;

   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay();
   {
      S.SetBorder( 20 );
      S.AddFixedText( message );
      S.SetBorder( 2 );
      S.StartHorizontalLay( );
      S.Id( wxID_HELP ).AddButton( _("Help") );
      wxButton * pBtn = S.Id( wxID_OK ).AddButton( _("OK"));
      pBtn->SetDefault();
      pBtn->SetFocus();
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

#if 0
   // Original non ShuttleGui based code.
   // Layout did not look good on Windows.
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);
   
   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);
   
   wxStaticText *statText = new wxStaticText(this, -1, message);
   mainSizer->Add(statText, 0, wxALIGN_LEFT|wxALL, 5);
	   
   wxButton *help = new wxButton(this, wxID_HELP, _("Help"));
   hSizer->Add(help, 0, wxALIGN_LEFT|wxALL, 5);
		
   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   ok->SetFocus();
 hSizer->Add(ok, 0, wxALIGN_RIGHT|wxALL, 5);
   
   vSizer->Add(hSizer, 0, wxALIGN_CENTER|wxALL, 5);
   
   mainSizer->Add(vSizer, 0, wxALL, 15 );
   
   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
#endif
}

void ErrorDialog::OnOk(wxCommandEvent &event)
{	

   EndModal(true);
}

void ErrorDialog::OnHelp(wxCommandEvent &event)
{
   if( dhelpURL.StartsWith(wxT("innerlink:")) )
   {
      wxDialog Dlg(this, -1, _NoAcc(TitleText(dhelpURL.Mid( 10 )) ),
               wxDefaultPosition, wxDefaultSize);
      ShuttleGui S( &Dlg, eIsCreating );
      S.StartVerticalLay();
      {
         wxHtmlWindow *html = new LinkingHtmlWindow(&Dlg, -1,
                                            wxDefaultPosition,
                                            wxSize(480, 240),
                                            wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);
         html->SetPage( HelpText( dhelpURL.Mid( 10 )));

         S.Prop(1).AddWindow( html, wxEXPAND );
         S.Id( wxID_OK).AddButton( _("Close") );
      }
      S.EndVerticalLay();
      Dlg.Fit();
      Dlg.Centre();
      Dlg.ShowModal();
      return;
   }

#if defined(__WXMSW__) || defined(__WXMAC__)
   OpenInDefaultBrowser(dhelpURL);
#else
   wxLaunchDefaultBrowser(dhelpURL);
#endif
	EndModal(true);
}

void ShowErrorDialog(wxWindow *parent,
                     const wxString &dlogTitle,
                     const wxString &message, 
                     const wxString &helpURL)
{
   ErrorDialog dlog(parent, dlogTitle, message, helpURL);
   dlog.CentreOnParent();
   dlog.ShowModal();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: b84d77e0-4375-43f0-868e-3130e18c14c8

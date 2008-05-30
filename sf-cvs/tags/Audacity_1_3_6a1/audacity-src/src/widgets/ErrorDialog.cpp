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
#include <wx/icon.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/utils.h>
#include <wx/html/htmlwin.h>
#include <wx/settings.h>

#include "LinkingHtmlWindow.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../ShuttleGui.h"
#include "../HelpText.h"
#include "../Internat.h"
#include "../Project.h"

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
void ShowHtmlInBrowser( const wxString &Url );


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

void ShowHtmlText( wxWindow * pParent, const wxString &Title, const wxString &HtmlText, bool bIsFile = false )
{
   BrowserFrame * pWnd = new BrowserFrame();
   pWnd->Create(pParent, -1, Title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_FRAME_STYLE);// & ~wxSYSTEM_MENU);
   ShuttleGui S( pWnd, eIsCreating );
   LinkingHtmlWindow *html;
   S.StartVerticalLay();
   {
      S.StartHorizontalLay( wxEXPAND, 0);
      {
         wxButton * pWndBackwards = S.Id( wxID_BACKWARD ).AddButton( _("<") );
         wxButton * pWndForwards  = S.Id( wxID_FORWARD  ).AddButton( _(">") );
         pWndForwards->Enable( false );
         pWndBackwards->Enable( false );
         #if wxUSE_TOOLTIPS
         pWndForwards->SetToolTip( _("Forwards" ));
         pWndBackwards->SetToolTip( _("Backwards" ));
         #endif
      }
      S.EndHorizontalLay();
      html = new LinkingHtmlWindow(pWnd, -1,
                                         wxDefaultPosition,
                                         bIsFile ? wxSize(500, 400) : wxSize(480, 240),
                                         wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);

      html->SetRelatedFrame( pWnd, wxT("Help: %s") );
      if( bIsFile )
         html->LoadFile( HtmlText );
      else
         html->SetPage( HtmlText);

      S.Prop(1).AddWindow( html, wxEXPAND );
      S.Id( wxID_CLOSE ).AddButton( _("Close") );
   }
   S.EndVerticalLay();

   // -- START of ICON stuff -----
   // If this section (providing an icon) causes compilation errors on linux, comment it out for now.
   // it will just mean that the icon is missing.  Works OK on Windows.
   #ifdef __WXMSW__
      wxIcon ic(wxICON(AudacityLogo));
   #else
      wxIcon ic;
      ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
   #endif
   pWnd->SetIcon( ic );
   // -- END of ICON stuff -----


   pWnd->mpHtml = html;
   pWnd->SetBackgroundColour( wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
   pWnd->CreateStatusBar();
   html->SetRelatedStatusBar( 0 );
   pWnd->Fit();
   pWnd->Centre();
   pWnd->Show( true );
   pWnd->SetFocus();
   //Dlg.Show();
   return;
}



void ShowHtmlInBrowser( const wxString &Url )
{
#if defined(__WXMSW__) || defined(__WXMAC__)
   OpenInDefaultBrowser(Url);
#else
   wxLaunchDefaultBrowser(Url);
#endif
}

void ErrorDialog::OnHelp(wxCommandEvent &event)
{
   if( dhelpURL.StartsWith(wxT("innerlink:")) )
   {
      ShowHtmlText(
         this, 
         TitleText(dhelpURL.Mid( 10 ) ),
         HelpText( dhelpURL.Mid( 10 )) );
      return;
   }
   ShowHtmlInBrowser( dhelpURL );
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


void ShowHelpDialog(wxWindow *parent,
                     const wxString &localFileName,
                     const wxString &remoteURL)
{
   AudacityProject * pProj = GetActiveProject();
   wxString HelpMode = wxT("Standard");

   if( pProj )
   {
      HelpMode = pProj->mHelpPref;
   }

   if( (HelpMode == wxT("FromInternet")) && !remoteURL.IsEmpty() )
   {
      // Always go to remote URL.  Use External browser.
      ShowHtmlInBrowser( remoteURL );
   }
   else if( !wxFileExists( localFileName ))
   {
      // If you give an empty remote URL, you should have already ensured
      // that the file exists!
      wxASSERT( !remoteURL.IsEmpty() );
      // I can't find it'.
      // Use Built-in browser to suggest you use the remote url.
      wxString Text = HelpText( wxT("remotehelp") );
      Text.Replace( wxT("*URL*"), remoteURL );
      ShowHtmlText( parent, _("Help on the Internet"), Text );
   }
   else if( HelpMode == wxT("InBrowser") ) 
   {
      // Local file, External browser 
      ShowHtmlInBrowser( wxString(wxT("file:"))+localFileName );
   }
   else
   {
      // Local file, Built-in browser
      ShowHtmlText( parent, wxT(""), localFileName, true );
   }
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

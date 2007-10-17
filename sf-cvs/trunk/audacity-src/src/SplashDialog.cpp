/**********************************************************************

  Audacity: A Digital Audio Editor

  SplashDialog.cpp

  James Crook

********************************************************************//**

\class SplashDialog
\brief The SplashDialog shows help information for Audacity when 
Audacity starts up.

It was written for the benefit of new users who do not want to 
read the manual.  The text of the dialog is kept short to increase the
chance of it being read.  The content is designed to reduce the 
most commonly asked questions about Audacity.

*//********************************************************************/


#include "Audacity.h"

#include <wx/dialog.h>
#include <wx/html/htmlwin.h>
#include <wx/button.h>
#include <wx/dcclient.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/intl.h>

#include "SplashDialog.h"
#include "FileNames.h"
#include "Internat.h"
#include "ShuttleGui.h"
#include "widgets/LinkingHtmlWindow.h"

#include "Theme.h"
#include "AllThemeResources.h"
#include "Prefs.h"
#include "HelpText.h"

enum 
{
   DontShowID=1000,
};

BEGIN_EVENT_TABLE(SplashDialog, wxDialog)
   EVT_BUTTON(wxID_OK, SplashDialog::OnOK)
   EVT_CHECKBOX( DontShowID, SplashDialog::OnDontShow )
END_EVENT_TABLE()

IMPLEMENT_CLASS(SplashDialog, wxDialog)

SplashDialog::SplashDialog(wxWindow * parent)
   :  wxDialog(parent, -1, _NoAcc("&Welcome to Audacity!"),
               wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   this->SetBackgroundColour(theTheme.Colour( clrAboutBoxBackground ));
   m_pIcon = NULL;
   m_pLogo = NULL; //vvv
   ShuttleGui S( this, eIsCreating );
   Populate( S );
   Fit();
   this->Centre();
}

void SplashDialog::Populate( ShuttleGui & S )
{
   this->SetBackgroundColour(theTheme.Colour( clrAboutBoxBackground ));
   bool bShow;
   gPrefs->Read(wxT("/GUI/ShowSplashScreen"), &bShow, true );
   S.StartVerticalLay(1);

   //vvv For now, change to AudacityLogoWithName via old-fashioned ways, not Theme.
   m_pLogo = new wxBitmap((const char **) AudacityLogoWithName_xpm); //vvv
   m_pIcon =
       new wxStaticBitmap(S.GetParent(), -1, 
                          *m_pLogo, //vvv theTheme.Bitmap(bmpAudacityLogoWithName), 
                          wxDefaultPosition, wxSize(LOGOWITHNAME_WIDTH, LOGOWITHNAME_HEIGHT));
   S.Prop(0).AddWindow( m_pIcon );

   wxHtmlWindow *html = new LinkingHtmlWindow(S.GetParent(), -1,
                                         wxDefaultPosition,
                                         wxSize(LOGOWITHNAME_WIDTH, 280),
                                         wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER );
   html->SetPage(HelpText( wxT("welcome") ));
   S.Prop(1).AddWindow( html, wxEXPAND );
   S.Prop(0).StartMultiColumn(2, wxEXPAND);
   S.SetStretchyCol( 1 );// Column 1 is stretchy...
   {
      S.SetBorder( 5 );
      S.Id( DontShowID).AddCheckBox( _("Don't show this again at start up"), bShow ? wxT("false") : wxT("true") );
      wxButton *ok = new wxButton(S.GetParent(), wxID_OK, _("OK... Audacious!"));
      ok->SetDefault();
      ok->SetFocus();
      S.SetBorder( 5 );
      S.Prop(0).AddWindow( ok, wxALIGN_RIGHT| wxALL );
   }
   S.EndVerticalLay();
}

SplashDialog::~SplashDialog()
{
   delete m_pIcon;
   delete m_pLogo;
}

void SplashDialog::OnDontShow( wxCommandEvent & Evt )
{
   bool bShow = !Evt.IsChecked(); 
   gPrefs->Write(wxT("/GUI/ShowSplashScreen"), bShow );
}

void SplashDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(wxID_OK);
}


#if 0
void ShowSplashScreen( AudacityProject * pProj )
{

   SplashDialog dlog(pProj);
   dlog.ShowModal();
}
#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: a8955864-40e2-47aa-923b-cace3994493a


/**********************************************************************

  Audacity: A Digital Audio Editor

  SplashDialog.cpp

  James Crook
  Vaughan Johnson

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
#include <wx/image.h>

#include "SplashDialog.h"
#include "FileNames.h"
#include "Internat.h"
#include "ShuttleGui.h"
#include "widgets/LinkingHtmlWindow.h"

#include "Theme.h"
#include "AllThemeResources.h"
#include "Prefs.h"
#include "HelpText.h"

SplashDialog * SplashDialog::pSelf=NULL;

enum 
{
   DontShowID=1000,
   ID_BUTTON_LOGO, 
};

BEGIN_EVENT_TABLE(SplashDialog, wxDialog)
   EVT_BUTTON(wxID_OK, SplashDialog::OnOK)
	EVT_BUTTON(ID_BUTTON_LOGO, SplashDialog::OnButton_Logo)
   EVT_CHECKBOX( DontShowID, SplashDialog::OnDontShow )
END_EVENT_TABLE()

IMPLEMENT_CLASS(SplashDialog, wxDialog)

#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   #define kWidth 360 
#else
   #define kWidth LOGOWITHNAME_WIDTH 
#endif

SplashDialog::SplashDialog(wxWindow * parent)
:  wxDialog(parent, -1, 
            #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
               _NoAcc("&Jamling Audacity"),
               wxPoint(640 - kWidth, 120), 
               wxSize(kWidth, kWidth * 1.61803399),  // the golden ratio = 1.61803399
            #else
               _NoAcc("&Welcome to Audacity!"),
               wxPoint( -1, 60 ), // default x position, y position 60 pixels from top of screen.
               wxDefaultSize, 
            #endif
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   // loads either the XPM or the windows resource, depending on the platform
   #if !defined(__WXMAC__) && !defined(__WXX11__)
      #ifdef __WXMSW__
         wxIcon ic(wxICON(AudacityLogo));
      #else
         wxIcon ic(wxICON(AudacityLogo48x48));
      #endif
      SetIcon(ic);
   #endif

   // This seems to have no effect when called here, on Windows. 
   this->SetBackgroundColour(theTheme.Colour( clrAboutBoxBackground ));
   mButton_Logo = NULL;
   
   ShuttleGui S( this, eIsCreating );
   Populate( S );
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      wxRect screenRect = wxGetClientDisplayRect();;
      this->Move(screenRect.GetWidth() - kWidth - 24, 120); // off to the right, below the ruler 
   #else
      Fit();
      this->Centre();
      int x,y;
      GetPosition( &x, &y );
      Move( x, 60 );
   #endif
}

#if (AUDACITY_BRANDING != BRAND_AUDACITY)
   //#include "../images/Branding/powered_by_Audacity_162x64.xpm"
   //#include "../images/Branding/powered_by_Audacity_over_162x64.xpm"

   // Build in the brand-specific logo and mouse-over bitmaps. 
   // The data global vars in these MUST be named company_logo_xpm and company_logo_over_xpm.
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      #include "../images/Branding/Jamling_logo.xpm" // 162x64 image var is called company_logo_xpm.
      #include "../images/Branding/Jamling_logo_over.xpm" // 162x64 
   #endif
#endif

void SplashDialog::Populate( ShuttleGui & S )
{
   //#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   //   this->SetBackgroundColour(wxColour(rrr, ggg, bbb)); // same as waveform color
   //#else
   //   this->SetBackgroundColour(theTheme.Colour( clrAboutBoxBackground ));
   //#endif
   this->SetBackgroundColour(theTheme.Colour( clrAboutBoxBackground ));

   bool bShow;
   gPrefs->Read(wxT("/GUI/ShowSplashScreen"), &bShow, true );
   S.StartVerticalLay(1);

   wxImage buttonImageUp;
   wxImage buttonImageOver;
   wxSize buttonSize;
   wxBitmap* pButtonBitmap;
   wxBitmap* pButtonBitmapOver;
   //vvv Change this compile flag when other brands are supported. 
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL)) 
      //vvv Compiles only BRAND_JAMLING__EASY or BRAND_JAMLING__FULL 
      //    because we don't load any other company_logo*_xpm consts yet. 
      pButtonBitmap = new wxBitmap(company_logo_xpm);
      buttonImageUp = pButtonBitmap->ConvertToImage();
      pButtonBitmapOver = new wxBitmap(company_logo_over_xpm);
      buttonImageOver = pButtonBitmapOver->ConvertToImage();
      buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
      mButton_Logo = 
         new AButton(this, ID_BUTTON_LOGO, 
                     wxDefaultPosition, buttonSize, 
                     &buttonImageUp, &buttonImageOver, 
                     &buttonImageUp, &buttonImageUp, //v other images?
                     true); 
   #else
      //vvv For now, change to AudacityLogoWithName via old-fashioned ways, not Theme.
      pButtonBitmap = //vvv theTheme.Bitmap(bmpAudacityLogoWithName)
         new wxBitmap((const char **) AudacityLogoWithName_xpm); 
      buttonImageUp = pButtonBitmap->ConvertToImage();

      // JKC: Resize to 50% of size.  Later we may use a smaller xpm as
      // our source, but this allows us to tweak the size - if we want to.
      // It also makes it easier to revert to full size if we decide to.
      const float fScale=0.5f;// smaller size.
      buttonSize = wxSize(LOGOWITHNAME_WIDTH * fScale, LOGOWITHNAME_HEIGHT * fScale); 
      // wxIMAGE_QUALITY_HIGH not supported by wxWidgets 2.6.x, or we would use it here.
      buttonImageUp.Rescale(buttonSize.GetWidth(), buttonSize.GetHeight());

      buttonImageOver = buttonImageUp.Copy();
      buttonImageOver.RotateHue(-0.01);

      mButton_Logo = 
         new AButton(this, ID_BUTTON_LOGO, 
                     wxDefaultPosition, buttonSize, 
                     &buttonImageUp, &buttonImageOver, 
                     &buttonImageUp, &buttonImageUp, //v other images?
                     true); 
   #endif
   S.Prop(0).AddWindow(mButton_Logo);

   mpHtml = new LinkingHtmlWindow(S.GetParent(), -1,
                                         wxDefaultPosition,
                                         wxSize(kWidth, 280),
                                         wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER );
   mpHtml->SetPage(HelpText( wxT("welcome") ));
   S.Prop(1).AddWindow( mpHtml, wxEXPAND );
   S.Prop(0).StartMultiColumn(2, wxEXPAND);
   S.SetStretchyCol( 1 );// Column 1 is stretchy...
   {
      S.SetBorder( 5 );
      S.Id( DontShowID).AddCheckBox( _("Don't show this again at start up"), bShow ? wxT("false") : wxT("true") );
      wxButton *ok = new wxButton(S.GetParent(), wxID_OK);
      ok->SetDefault();
      ok->SetFocus();
      S.SetBorder( 5 );
      S.Prop(0).AddWindow( ok, wxALIGN_RIGHT| wxALL );
   }
   S.EndVerticalLay();
}

SplashDialog::~SplashDialog()
{
}

void SplashDialog::OnButton_Logo(wxCommandEvent& event)
{
   #if (AUDACITY_BRANDING == BRAND_AUDACITY)
      wxHtmlLinkInfo link(AUDACITY_URL);
   #else
      wxHtmlLinkInfo link(AUDACITY_BRANDING_BRANDURL);
   #endif
   OpenInDefaultBrowser(link);
   mButton_Logo->PopUp();
}

void SplashDialog::OnDontShow( wxCommandEvent & Evt )
{
   bool bShow = !Evt.IsChecked(); 
   gPrefs->Write(wxT("/GUI/ShowSplashScreen"), bShow );
}

void SplashDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   Show( false );

}

void SplashDialog::Show2( wxWindow * pParent )
{
   if( pSelf == NULL )
   {
      pSelf = new SplashDialog( pParent );
   }
   pSelf->mpHtml->SetPage(HelpText( wxT("welcome") ));
   pSelf->SetLabel(TitleText(wxT("welcome")));
   pSelf->Show( true );
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
// arch-tag: a8955864-40e2-47aa-923b-cace3994493a


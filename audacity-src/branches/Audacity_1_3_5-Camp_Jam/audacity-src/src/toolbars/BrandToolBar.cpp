/**********************************************************************

  Audacity: A Digital Audio Editor

  BrandToolBar.cpp

  Copyright 2008 by Vaughan Johnson
  
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

**********************************************************************/

#include "BrandToolBar.h"
#if WANT_BRAND_TOOLBAR

#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/tooltip.h>
#endif

#include <wx/button.h>
#include "../widgets/AButton.h"
#include "../widgets/LinkingHtmlWindow.h"

#include "../Project.h"

enum 
{
   ID_BUTTON_LOGO = 1300, 
   ID_BUTTON_WELCOME, 
};

#define BUTTON_WELCOME_TEXT _("Show Jamling Window") //v Jamling only for now. 

IMPLEMENT_CLASS(BrandToolBar, ToolBar);

BEGIN_EVENT_TABLE(BrandToolBar, ToolBar)
	EVT_BUTTON(ID_BUTTON_LOGO, BrandToolBar::OnButton_Logo)
	EVT_BUTTON(ID_BUTTON_WELCOME, BrandToolBar::OnButton_Welcome)
END_EVENT_TABLE()

BrandToolBar::BrandToolBar()
: ToolBar(BrandBarID, _("Brand"), wxT("Brand"))
{
   mButton_Logo = NULL; 
   mButton_Welcome = NULL;
}

BrandToolBar::~BrandToolBar()
{
}

#if (AUDACITY_BRANDING != BRAND_AUDACITY)
   // Build in the brand-specific logo and mouse-over bitmaps. 
   // The data global vars in these MUST be named company_logo_xpm and company_logo_over_xpm.
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      #include "../images/Branding/Jamling_logo.xpm" // 162x64 image var is called company_logo_xpm.
      #include "../images/Branding/Jamling_logo_over.xpm" // 162x64 
   #endif
#endif

void BrandToolBar::Populate()
{
   this->AddSpacer(5);

   wxBoxSizer* sizer = new wxBoxSizer(wxVERTICAL);
   this->Add(sizer, 1, wxEXPAND);

   wxBitmap* pButtonBitmap = new wxBitmap(company_logo_xpm);
   wxImage buttonImageUp = pButtonBitmap->ConvertToImage();
   wxBitmap* pButtonBitmapOver = new wxBitmap(company_logo_over_xpm);
   wxImage buttonImageOver = pButtonBitmapOver->ConvertToImage();
   wxSize buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
   mButton_Logo = 
      new AButton(this, ID_BUTTON_LOGO, 
                  wxDefaultPosition, buttonSize, 
                  &buttonImageUp, &buttonImageOver, 
                  &buttonImageUp, &buttonImageUp, //v other images?
                  true); 
   sizer->AddSpacer(5);
   sizer->Add(mButton_Logo, 0, wxEXPAND);

   mButton_Welcome = new wxButton(this, ID_BUTTON_WELCOME, BUTTON_WELCOME_TEXT);
   sizer->AddSpacer(5);
   sizer->Add(mButton_Welcome, 0, wxEXPAND | wxALIGN_BOTTOM);

   #if wxUSE_TOOLTIPS
      mButton_Logo->SetToolTip(AUDACITY_BRANDING_BRANDURL); // 
      mButton_Welcome->SetToolTip(BUTTON_WELCOME_TEXT);
   #endif

   this->AddSpacer(5);
}

void BrandToolBar::OnButton_Logo(wxCommandEvent& event)
{
   #if (AUDACITY_BRANDING == BRAND_AUDACITY)
      wxHtmlLinkInfo link(AUDACITY_URL);
   #else
      wxHtmlLinkInfo link(AUDACITY_BRANDING_BRANDURL);
   #endif
   OpenInDefaultBrowser(link);
   mButton_Logo->PopUp();
}

void BrandToolBar::OnButton_Welcome(wxCommandEvent& event)
{
   AudacityProject* pProj = GetActiveProject();
   pProj->OnHelpWelcome();
}

#endif // WANT_BRAND_TOOLBAR
/**********************************************************************

   Audacity: A Digital Audio Editor

   Branding.h

   Vaughan Johnson, November 2006

   class Branding: 
      Project-specific branding (brand/artist name, website URL, logo, color scheme)
   class BrandingPanel: 
      Custom build branding, based on AUDACITY_BRANDING build flag
  
**********************************************************************/

#include <wx/msgdlg.h>

#include "Branding.h"

#if (AUDACITY_BRANDING == BRAND_UMIXIT) || (AUDACITY_BRANDING == BRAND_THINKLABS)
#include <wx/image.h>

#include "AColor.h"
#include "Project.h"
#include "widgets/LinkingHtmlWindow.h"

// class BrandingPanel: 
//    Custom build branding, based on AUDACITY_BRANDING build flag

// Build in the brand-specific logo and mouse-over bitmaps.
// The data global vars in these MUST be named company_logo_xpm and 
// company_logo_over_xpm, so the code works for all.
// If you want the size the same as the "powered by Audacity" logo, it's 162x64
#if (AUDACITY_BRANDING == BRAND_UMIXIT) 
   #include "../images/Branding/UmixIt.xpm"
   #include "../images/Branding/UmixIt_over.xpm"
#elif (AUDACITY_BRANDING == BRAND_THINKLABS)
   #include "../images/Branding/Thinklabs.xpm"
   #include "../images/Branding/Thinklabs_over.xpm"
#endif

#include "../images/Branding/powered_by_Audacity.xpm"
#include "../images/Branding/powered_by_Audacity_over.xpm"

enum {
   ID_BUTTON_PROJECT_LOGO = 13010,
   ID_BUTTON_COMPANY_LOGO, 
   ID_BUTTON_AUDACITY_LOGO // "powered by Audacity" logo
};

BEGIN_EVENT_TABLE(BrandingPanel, wxPanel)
	EVT_BUTTON(ID_BUTTON_PROJECT_LOGO, BrandingPanel::OnButton_ProjectLogo)
	EVT_BUTTON(ID_BUTTON_COMPANY_LOGO, BrandingPanel::OnButton_CompanyLogo)
	EVT_BUTTON(ID_BUTTON_AUDACITY_LOGO, BrandingPanel::OnButton_AudacityLogo)
   EVT_PAINT(BrandingPanel::OnPaint)
   EVT_SIZE(BrandingPanel::OnSize)
END_EVENT_TABLE()

#define kHalfInset 2
#define kInset 4
#define kDoubleInset (2 * kInset)
#define kTripleInset (3 * kInset)
#define kQuadrupleInset (4 * kInset)

BrandingPanel::BrandingPanel(AudacityProject* pProject, 
                              const wxPoint& pos /*= wxDefaultPosition*/, 
                              const wxSize& size /*= wxDefaultSize*/) 
   : wxPanel(pProject, -1, pos, size)
{
   mProject = pProject;

   this->SetBackgroundColour(*wxWHITE); 

   // buttons
   mButton_ProjectLogo = NULL; // Won't know this until loading project.

   wxImage buttonImage;
   wxImage buttonImageOver;
   wxPoint buttonPos;
   wxSize buttonSize;
   wxBitmap* pButtonBitmap;
   wxBitmap* pButtonBitmapOver;

   mMinHeight = 0;

   // Default positioning has powered_by_Audacity_xpm rightmost and   
   // company_logo_xpm to its left.
   // Start with powered_by_Audacity_xpm rightmost. 
   pButtonBitmap = new wxBitmap(powered_by_Audacity_xpm);
   buttonImage = pButtonBitmap->ConvertToImage();
   buttonPos = wxPoint(size.GetWidth() - pButtonBitmap->GetWidth() - kInset, kInset);
   buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
   pButtonBitmapOver = new wxBitmap(powered_by_Audacity_over_xpm);
   buttonImageOver = pButtonBitmapOver->ConvertToImage();
   mButton_AudacityLogo = 
      new AButton(this, ID_BUTTON_AUDACITY_LOGO, 
                  buttonPos, buttonSize, 
                  &buttonImage, &buttonImageOver, 
                  &buttonImage, &buttonImage, //vvv other images?
                  false); // momentary button
   if (mMinHeight < buttonSize.GetHeight() + kDoubleInset)
      mMinHeight = buttonSize.GetHeight() + kDoubleInset;

   pButtonBitmap = new wxBitmap(company_logo_xpm);
   buttonImage = pButtonBitmap->ConvertToImage();
   buttonPos = 
      wxPoint(buttonPos.x - pButtonBitmap->GetWidth() - kInset, // left of powered_by_Audacity_xpm
               kInset);
   buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
   pButtonBitmapOver = new wxBitmap(company_logo_over_xpm);
   buttonImageOver = pButtonBitmapOver->ConvertToImage();
   mButton_CompanyLogo = 
      new AButton(this, ID_BUTTON_COMPANY_LOGO, 
                  buttonPos, buttonSize, 
                  &buttonImage, &buttonImageOver, 
                  &buttonImage, &buttonImage, //vvv other images?
                  false); // momentary button
   if (mMinHeight < buttonSize.GetHeight() + kDoubleInset)
      mMinHeight = buttonSize.GetHeight() + kDoubleInset;

   this->SetSizeHints(-1, mMinHeight);

   #if wxUSE_TOOLTIPS
      mButton_CompanyLogo->SetToolTip(AUDACITY_BRANDING_BRANDURL);
      mButton_AudacityLogo->SetToolTip(AUDACITY_URL);
   #endif // wxUSE_TOOLTIPS
}

void BrandingPanel::SetProjectLogo(wxFileName brandLogoFileName)
{
   if (brandLogoFileName.IsOk() && brandLogoFileName.FileExists())
   {
      if (mButton_ProjectLogo != NULL)
      {
         mButton_ProjectLogo->Destroy();
         mButton_ProjectLogo = NULL;
      }
      wxImage buttonImage(brandLogoFileName.GetFullPath());
      mButton_ProjectLogo = 
         new AButton(this, ID_BUTTON_PROJECT_LOGO, 
                     wxPoint(kInset, kInset), 
                     wxSize(buttonImage.GetWidth(), buttonImage.GetHeight()), 
                     &buttonImage, &buttonImage, 
                     &buttonImage, &buttonImage, //vvv other images?
                     false); // momentary button
      if (mMinHeight < buttonImage.GetHeight() + kDoubleInset)
         mMinHeight = buttonImage.GetHeight() + kDoubleInset;
      #if wxUSE_TOOLTIPS
         Branding* pBranding = mProject->GetBranding();
         if (pBranding)
            mButton_ProjectLogo->SetToolTip(pBranding->GetBrandURL());
      #endif // wxUSE_TOOLTIPS
      mProject->HandleResize();
   }
}


// event handlers
void BrandingPanel::OnButton_ProjectLogo(wxCommandEvent& event)
{
   Branding* pBranding = mProject->GetBranding();
   if (pBranding)
   {
      wxHtmlLinkInfo link(pBranding->GetBrandURL());
      OpenInDefaultBrowser(link);
      mButton_ProjectLogo->PopUp();
   }
}

void BrandingPanel::OnButton_CompanyLogo(wxCommandEvent& event)
{
   wxHtmlLinkInfo link(AUDACITY_BRANDING_BRANDURL);
   OpenInDefaultBrowser(link);
   mButton_CompanyLogo->PopUp();
}

void BrandingPanel::OnButton_AudacityLogo(wxCommandEvent& event)
{
   wxHtmlLinkInfo link(AUDACITY_URL);
   OpenInDefaultBrowser(link);
   mButton_AudacityLogo->PopUp();
}

void BrandingPanel::OnPaint(wxPaintEvent& evt)
{
   wxPaintDC dc(this);

   dc.BeginDrawing();
   wxSize mySize = this->GetSize();
   wxRect bev(0, 0, mySize.GetWidth() - 1, mySize.GetHeight() - 1);
   AColor::Bevel(dc, true, bev);
   dc.EndDrawing();
}

void BrandingPanel::OnSize(wxSizeEvent &evt)
{
   wxPoint buttonPos;
   wxSize evtSize = evt.GetSize();
   int yPos;

   // mButton_ProjectLogo never moves.

   wxSize buttonSize_ProjectLogo = 
      mButton_ProjectLogo ? mButton_ProjectLogo->GetSize() : wxSize(0, 0);
   wxSize buttonSize_CompanyLogo = mButton_CompanyLogo->GetSize();
   wxSize buttonSize_AudacityLogo = mButton_AudacityLogo->GetSize();

   if ((buttonSize_ProjectLogo.GetWidth() + 
         buttonSize_CompanyLogo.GetWidth() + 
         buttonSize_AudacityLogo.GetWidth() + kTripleInset) < evtSize.GetWidth())
   {
      // Can put them all side-by side. This is preferred, to take up least vertical space.
      // Start with powered_by_Audacity_xpm rightmost, centered vertically.  
      yPos = (evtSize.GetHeight() - buttonSize_AudacityLogo.GetHeight()) / 2;
      if (yPos < kInset)
         yPos = kInset;
      buttonPos = wxPoint(evtSize.GetWidth() - buttonSize_AudacityLogo.GetWidth() - kInset, yPos);
      mButton_AudacityLogo->Move(buttonPos);

      yPos = (evtSize.GetHeight() - buttonSize_CompanyLogo.GetHeight()) / 2;
      if (yPos < kInset)
         yPos = kInset;
      buttonPos = 
         wxPoint(buttonPos.x - buttonSize_CompanyLogo.GetWidth() - kInset, // left of powered_by_Audacity_xpm
                  yPos);
      mButton_CompanyLogo->Move(buttonPos);
   }
   else if (evtSize.GetHeight() >=
            (buttonSize_CompanyLogo.GetHeight() + buttonSize_AudacityLogo.GetHeight()) + kInset)
   {
      // Can stack mButton_CompanyLogo and mButton_AudacityLogo at the right side.
      yPos = (evtSize.GetHeight() / 2) - kHalfInset - buttonSize_CompanyLogo.GetHeight();
      if (yPos < 0)
         yPos = 0;
      buttonPos = wxPoint(evtSize.GetWidth() - buttonSize_CompanyLogo.GetWidth() - kInset, yPos);
      mButton_CompanyLogo->Move(buttonPos);

      yPos += buttonSize_CompanyLogo.GetHeight() + kInset;
      buttonPos = wxPoint(evtSize.GetWidth() - buttonSize_AudacityLogo.GetWidth() - kInset, yPos);
      mButton_AudacityLogo->Move(buttonPos);
   }
   else
   {
      // Just move mButton_CompanyLogo and mButton_AudacityLogo to the immediate right of mButton_ProjectLogo.
      buttonPos = wxPoint(buttonSize_ProjectLogo.GetWidth() + kDoubleInset, kInset);
      mButton_CompanyLogo->Move(buttonPos);

      if (evtSize.GetHeight() - buttonSize_CompanyLogo.GetHeight() >
            evtSize.GetWidth() - buttonSize_CompanyLogo.GetWidth())
         // Can show more of mButton_AudacityLogo below than to the right.
         buttonPos = wxPoint(kInset, buttonSize_CompanyLogo.GetHeight() + kDoubleInset);
      else 
         buttonPos = wxPoint(buttonPos.x + buttonSize_CompanyLogo.GetWidth() + kInset, kInset);
      mButton_AudacityLogo->Move(buttonPos);
   }
}

#endif // (AUDACITY_BRANDING == BRAND_UMIXIT) || (AUDACITY_BRANDING == BRAND_THINKLABS)


// class Branding: 
//    Project-specific branding (brand name, website URL, logo, color scheme)

#if (AUDACITY_BRANDING == BRAND_UMIXIT) || (AUDACITY_BRANDING == BRAND_THINKLABS)
   Branding::Branding(BrandingPanel* pBrandingPanel)
      : mBrandingPanel(pBrandingPanel)
#else
   Branding::Branding()
#endif
{
   m_strBrandName = "";
   m_strBrandURL = "";
   m_strBrandColorScheme = "";
}

bool Branding::HandleXMLTag(const char *tag, const char **attrs) 
{
   if (strcmp(tag, "branding")) return false;

   // loop through attrs, null-terminated list of attribute-value pairs
   while (*attrs) {
      const char *attr = *attrs++;
      const char *value = *attrs++;

      if (!value) 
         break;

      if (!strcmp(attr, "brandname") && XMLValueChecker::IsGoodString(value)) 
         m_strBrandName = value;
      else if (!strcmp(attr, "url") && XMLValueChecker::IsGoodString(value)) 
         m_strBrandURL = value;
      else if (!strcmp(attr, "logofilename")) 
      {
         // Logo file is supposed to be stored in the project data directory.
         wxString strDirName = GetActiveProject()->GetDirManager()->GetProjectDataDir();
         if (XMLValueChecker::IsGoodFileName(value, strDirName)) 
         {
            // Store full thing, not just file name, so don't need to add path again.
            m_BrandLogoFileName.Assign(strDirName, value);
            m_BrandLogoFileName.Normalize(wxPATH_NORM_ABSOLUTE | wxPATH_NORM_LONG);
            #if (AUDACITY_BRANDING == BRAND_UMIXIT) || (AUDACITY_BRANDING == BRAND_THINKLABS)
               mBrandingPanel->SetProjectLogo(m_BrandLogoFileName);
            #endif
         } 
         else
         {
            wxLogWarning(wxT("Could not open branding logo file: %s"), value);
            return false;
         }
      }
      else if (!strcmp(attr, "colorscheme") && XMLValueChecker::IsGoodString(value)) 
         m_strBrandColorScheme = value;
   } // while

   return true; 
}

void Branding::WriteXML(int depth, FILE *fp)
{
   int i;

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<branding ");
   fprintf(fp, "brandname=\"%s\" ", XMLEsc(m_strBrandName).c_str());
   fprintf(fp, "url=\"%s\" ", XMLEsc(m_strBrandURL).c_str());
   fprintf(fp, "logofilename=\"%s\" ", XMLEsc(m_BrandLogoFileName.GetFullName()).c_str());
   fprintf(fp, "colorscheme=\"%s\" ", XMLEsc(m_strBrandColorScheme).c_str());
   fprintf(fp, "/>\n"); // XML shorthand for childless tag
}



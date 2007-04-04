/**********************************************************************

   Audacity: A Digital Audio Editor

   Branding.h

   Vaughan Johnson, November 2006

   class Branding: 
      Project-specific branding (brand/artist name, website URL, logo, color scheme)
   class BrandingPanel: 
      Custom build branding, based on AUDACITY_BRANDING build flag

**********************************************************************/

#pragma once

#include <wx/filename.h>

#include "AudacityBranding.h"
#include "xml/XMLTagHandler.h"

#if (AUDACITY_BRANDING == BRAND_UMIXIT) || (AUDACITY_BRANDING == BRAND_THINKLABS)
   #include <wx/panel.h>
   #include "widgets/AButton.h"
   
   class AudacityProject;

   class BrandingPanel : public wxPanel {
   public: 
      BrandingPanel(AudacityProject* pProject, 
                     const wxPoint& pos = wxDefaultPosition, 
                     const wxSize& size = wxDefaultSize);
      const int GetMinHeight() { return mMinHeight; };
      void SetProjectLogo(wxFileName brandLogoFileName);

   private:
      // event handlers
      void OnButton_ProjectLogo(wxCommandEvent& event); // from project file
      void OnButton_CompanyLogo(wxCommandEvent& event); // AUDACITY_BRANDING build flag
      void OnButton_AudacityLogo(wxCommandEvent& event); // AUDACITY_BRANDING build flag

      void OnPaint(wxPaintEvent& evt);
      void OnSize(wxSizeEvent &evt);

   private:
      AudacityProject* mProject;

      AButton* mButton_ProjectLogo;
      AButton* mButton_CompanyLogo;
      AButton* mButton_AudacityLogo;

      int mMinHeight;

   public:
      DECLARE_EVENT_TABLE()
   };
#endif

class Branding : public XMLTagHandler
{
public:
   #if (AUDACITY_BRANDING == BRAND_UMIXIT) || (AUDACITY_BRANDING == BRAND_THINKLABS)
      Branding(BrandingPanel* pBrandingPanel);
   #else
      Branding();
   #endif
   
   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag) { return NULL; }; //v
   virtual void WriteXML(int depth, FILE *fp);

   wxString GetBrandName() { return m_strBrandName; }
   wxString GetBrandURL() { return m_strBrandURL; }
   wxFileName GetBrandLogoFileName() { return m_BrandLogoFileName; }
   wxString GetBrandColorScheme() { return m_strBrandColorScheme; }

private:
   wxString m_strBrandName;
   wxString m_strBrandURL;
   wxFileName m_BrandLogoFileName; // Store full thing, not just file name, so don't need to add path again.
   wxString m_strBrandColorScheme;

   #if (AUDACITY_BRANDING == BRAND_UMIXIT) || (AUDACITY_BRANDING == BRAND_THINKLABS)
      BrandingPanel* mBrandingPanel;
   #endif
};

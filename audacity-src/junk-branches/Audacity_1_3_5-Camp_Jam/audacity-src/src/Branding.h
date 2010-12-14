/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
   License: GPL v2.  See License.txt.

   Branding.h
   Vaughan Johnson, November 2006

******************************************************************//**

\class Branding
\brief Project-specific branding (brand/artist name, website URL, logo, color scheme)

\class BrandingPanel
\brief Custom build branding, based on AUDACITY_BRANDING build flag

*//*******************************************************************/

#pragma once

#include <wx/filename.h>

#include "AudacityBranding.h"
#include "xml/XMLTagHandler.h"

#if WANT_BRANDING_PANEL
   #include <wx/panel.h>
   #include "widgets/AButton.h"
   
   class AudacityProject;
   class Branding;

   class BrandingPanel : public wxPanel {
   public: 
      BrandingPanel(AudacityProject* pProject, 
                     const wxPoint& pos = wxDefaultPosition, 
                     const wxSize& size = wxDefaultSize);
      #if (AUDACITY_BRANDING == BRAND_UMIXIT)
         void SetProjectBranding(Branding* pBranding); // Updates mMinWidth/mMinHeight.
      #elif (AUDACITY_BRANDING == BRAND_THINKLABS)
         // Thinklabs custom buttons
         public:
            const int GetMinLeftSectionWidth() { return mMinLeftSectionWidth; };
            virtual void EnableDisableButtons();

         private:
            AButton* mButton_Import; // Import Audio
            AButton* mButton_Amplify; // Amplify effect
            AButton* mButton_Filter; // EQ effect
            AButton* mButton_Rate; // Change Tempo effect
            AButton* mButton_Display; // Toggle Display type between WaveformDisplay and WaveformAndSpectrumDisplay

            void AddCustomButtons(); // Updates mMinLeftSectionWidth and mMinWidth/mMinHeight.

            // event handlers
            void OnButton_Import(wxCommandEvent& event); 
            void OnButton_Amplify(wxCommandEvent& event); 
            void OnButton_Filter(wxCommandEvent& event); 
            void OnButton_Rate(wxCommandEvent& event); 
            void OnButton_Display(wxCommandEvent& event); 
      #endif // (AUDACITY_BRANDING == BRAND_THINKLABS)

   private:
      // event handlers
      void OnButton_AudacityLogo(wxCommandEvent& event); // based on AUDACITY_BRANDING build flag
      void OnButton_CompanyLogo(wxCommandEvent& event); // based on AUDACITY_BRANDING build flag
      #if (AUDACITY_BRANDING != BRAND_THINKLABS)
         void OnButton_ProjectLogo(wxCommandEvent& event); // from project file
      #endif

      void OnKeyEvent(wxKeyEvent & event);
      void OnPaint(wxPaintEvent& evt);
      void OnSize(wxSizeEvent &evt);

   private:
      AudacityProject* mProject;


      AButton* mButton_AudacityLogo;
      AButton* mButton_CompanyLogo;

      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         int mMinLeftSectionWidth;
      #else
         AButton* mButton_ProjectLogo;
         int mMinRightSectionWidth;
         int mProjectLogo_origWidth;
      #endif
      int mMinWidth;
      int mMinHeight;

   public:
      DECLARE_EVENT_TABLE()
   };
#endif

class Branding : public XMLTagHandler
{
public:
   #if WANT_BRANDING_PANEL
      Branding(BrandingPanel* pBrandingPanel);
   #else
      Branding();
   #endif
   
   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag) { return NULL; }; //v
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

   #if WANT_BRANDING_PANEL
      BrandingPanel* mBrandingPanel;
   #endif
};

/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
   License: GPL v2.  See License.txt.

   Branding.cpp
   Vaughan Johnson, November 2006

******************************************************************//**

\class Branding
\brief Project-specific branding (brand/artist name, website URL, logo, color scheme)

\class BrandingPanel
\brief Custom build branding, based on AUDACITY_BRANDING build flag

*//*******************************************************************/

#include <wx/msgdlg.h>

#include "Branding.h"

#include "AllThemeResources.h"
#include "Project.h"
#include "Theme.h"

// Note that Jamling (BRAND_JAMLING__EASY, BRAND_JAMLING__FULL) had a 
// right-aligned, vertical BrandingPanel briefly, before reverting to 
// no BrandingPanel, customizing SplashDialog instead. 
#if WANT_BRANDING_PANEL

// class BrandingPanel: 
//    Custom build branding, based on AUDACITY_BRANDING build flag

#include <wx/image.h>

#include "widgets/LinkingHtmlWindow.h"

#if (AUDACITY_BRANDING == BRAND_THINKLABS)
   #include <wx/dcmemory.h>
   #include "AudioIO.h" // gAudioIO for enabling/disabling custom buttons
   #include "WaveTrack.h"

   // Thinklabs custom buttons (all 64x42)
   #include "../images/Branding/Thinklabs_buttons.h" 
#elif ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL)) 
   //v Add MixerBoard if easy.
#else
   #include "MixerBoard.h"
#endif

// Build in the brand-specific logo and mouse-over bitmaps. 
// The data global vars in these MUST be named company_logo_xpm and company_logo_over_xpm.
#if (AUDACITY_BRANDING == BRAND_UMIXIT) 
   #include "../images/Branding/UmixIt.xpm" // 162x64
   #include "../images/Branding/UmixIt_over.xpm" // 162x64
#elif (AUDACITY_BRANDING == BRAND_THINKLABS)
   #include "../images/Branding/Thinklabs.xpm" // 182x42
   #include "../images/Branding/Thinklabs_over.xpm" // 182x42
#elif ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   #include "../images/Branding/Jamling_logo.xpm" // 162x64 
   #include "../images/Branding/Jamling_logo_over.xpm" // 162x64 
#endif

// Build in the "powered by Audacity" logos preferred for each. 
// The data global vars in these MUST be named 
// powered_by_Audacity_xpm and powered_by_Audacity_over_xpm, 
// so the code works for any size.
#if (AUDACITY_BRANDING == BRAND_UMIXIT)
   // bigger ones for UmixIt
   #include "../images/Branding/powered_by_Audacity_162x64.xpm"
   #include "../images/Branding/powered_by_Audacity_over_162x64.xpm"
#else
   // for Thinklabs, and default, 106x42
   #include "../images/Branding/powered_by_Audacity.xpm"
   #include "../images/Branding/powered_by_Audacity_over.xpm"
#endif

enum {
   ID_BUTTON_PROJECT_LOGO = 13010,
   ID_BUTTON_COMPANY_LOGO, 
   ID_BUTTON_AUDACITY_LOGO, // "powered by Audacity" logo
   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      // Thinklabs custom buttons
      ID_BUTTON_IMPORT, 
      ID_BUTTON_AMPLIFY, 
      ID_BUTTON_FILTER, 
      ID_BUTTON_RATE, 
      ID_BUTTON_DISPLAY, 
   #endif // (AUDACITY_BRANDING == BRAND_THINKLABS)
};

BEGIN_EVENT_TABLE(BrandingPanel, wxPanel)
   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      // Thinklabs custom buttons
   	EVT_BUTTON(ID_BUTTON_IMPORT, BrandingPanel::OnButton_Import)
   	EVT_BUTTON(ID_BUTTON_AMPLIFY, BrandingPanel::OnButton_Amplify)
   	EVT_BUTTON(ID_BUTTON_FILTER, BrandingPanel::OnButton_Filter)
   	EVT_BUTTON(ID_BUTTON_RATE, BrandingPanel::OnButton_Rate)
   	EVT_BUTTON(ID_BUTTON_DISPLAY, BrandingPanel::OnButton_Display)
   #else
	   EVT_BUTTON(ID_BUTTON_PROJECT_LOGO, BrandingPanel::OnButton_ProjectLogo)
   #endif 
	EVT_BUTTON(ID_BUTTON_COMPANY_LOGO, BrandingPanel::OnButton_CompanyLogo)
	EVT_BUTTON(ID_BUTTON_AUDACITY_LOGO, BrandingPanel::OnButton_AudacityLogo)
   EVT_CHAR(BrandingPanel::OnKeyEvent)
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

   //v 
   //    #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   //       this->SetBackgroundColour(theTheme.Colour(clrSample)); // same as waveform color
   this->SetBackgroundColour(*wxWHITE); 

   mMinWidth = 0;
   mMinHeight = 0;
   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      mMinLeftSectionWidth = 0;
      this->AddCustomButtons();
   #else
      mButton_ProjectLogo = NULL; // Won't know this until loading project.
      mMinRightSectionWidth = 0;
      mProjectLogo_origWidth = 0;
   #endif

   wxImage buttonImageUp;
   wxImage buttonImageOver;
   wxPoint buttonPos = wxDefaultPosition;
   wxSize buttonSize = wxSize(0, 0);
   wxBitmap* pButtonBitmap;
   wxBitmap* pButtonBitmapOver;

   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL)) 
      // No powered_by_Audacity_xpm.
      mButton_AudacityLogo = NULL;
   #else
      // Default positioning has powered_by_Audacity_xpm rightmost and   
      // company_logo_xpm to its left.
      // Start with powered_by_Audacity_xpm rightmost. 
      pButtonBitmap = new wxBitmap(powered_by_Audacity_xpm);
      buttonImageUp = pButtonBitmap->ConvertToImage();
      pButtonBitmapOver = new wxBitmap(powered_by_Audacity_over_xpm);
      buttonImageOver = pButtonBitmapOver->ConvertToImage();
      buttonPos = wxPoint(size.GetWidth() - pButtonBitmap->GetWidth() - kInset, kInset);
      buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
      mButton_AudacityLogo = 
         new AButton(this, ID_BUTTON_AUDACITY_LOGO, 
                     buttonPos, buttonSize, 
                     &buttonImageUp, &buttonImageOver, 
                     &buttonImageUp, &buttonImageUp, //v other images?
                     true); 
      if (mMinWidth < buttonSize.GetWidth() + kQuadrupleInset)
         mMinWidth = buttonSize.GetWidth() + kQuadrupleInset;
      if (mMinHeight < buttonSize.GetHeight() + kQuadrupleInset)
         mMinHeight = buttonSize.GetHeight() + kQuadrupleInset;
   #endif

   pButtonBitmap = new wxBitmap(company_logo_xpm);
   buttonImageUp = pButtonBitmap->ConvertToImage();
   pButtonBitmapOver = new wxBitmap(company_logo_over_xpm);
   buttonImageOver = pButtonBitmapOver->ConvertToImage();

   int left = 0;
   if (mButton_AudacityLogo)
      left = buttonPos.x - pButtonBitmap->GetWidth() - kInset;          // left of powered_by_Audacity_xpm
   else 
   {
      #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
         left = (size.GetWidth() - pButtonBitmap->GetWidth()) / 2;      // centered
      #else 
         left = size.GetWidth() - pButtonBitmap->GetWidth() - kInset;   // right side of panel
         if (left < kInset)
            left = kInset;
      #endif
   }
   buttonPos = wxPoint(left, kInset);

   buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
   mButton_CompanyLogo = 
      new AButton(this, ID_BUTTON_COMPANY_LOGO, 
                  buttonPos, buttonSize, 
                  &buttonImageUp, &buttonImageOver, 
                  &buttonImageUp, &buttonImageUp, //v other images?
                  true); 
   if (mMinWidth < buttonSize.GetWidth() + kQuadrupleInset)
      mMinWidth = buttonSize.GetWidth() + kQuadrupleInset;
   if (mMinHeight < buttonSize.GetHeight() + kQuadrupleInset)
      mMinHeight = buttonSize.GetHeight() + kQuadrupleInset;

   #if wxUSE_TOOLTIPS
      mButton_CompanyLogo->SetToolTip(AUDACITY_BRANDING_BRANDURL);
      if (mButton_AudacityLogo) 
         mButton_AudacityLogo->SetToolTip(AUDACITY_URL);
   #endif // wxUSE_TOOLTIPS

   this->SetSizeHints(mMinWidth, mMinHeight);
}

// utility to draw a 2-pixel bevel that shows up against white background
static void Bevel2OnWhite(wxDC & dc, bool up, wxRect & r) {
   if (up)
      dc.SetPen(*wxLIGHT_GREY_PEN);
   else
      dc.SetPen(*wxMEDIUM_GREY_PEN);

   // top 
   dc.DrawLine(r.x,     r.y,     r.x + r.width,       r.y);
   dc.DrawLine(r.x + 1, r.y + 1, r.x + r.width - 1,   r.y + 1);

   // left
   dc.DrawLine(r.x,     r.y,     r.x,     r.y + r.height);
   dc.DrawLine(r.x + 1, r.y + 1, r.x + 1, r.y + r.height - 2);

   if (!up)
      dc.SetPen(*wxLIGHT_GREY_PEN);
   else
      dc.SetPen(*wxMEDIUM_GREY_PEN);

   // right
   dc.DrawLine(r.x + r.width - 1, r.y,       r.x + r.width - 1,   r.y + r.height - 1);
   dc.DrawLine(r.x + r.width - 2, r.y + 1,   r.x + r.width - 2,   r.y + r.height - 2);

   // bottom
   dc.DrawLine(r.x,     r.y + r.height - 1,  r.x + r.width - 1,   r.y + r.height - 1);
   dc.DrawLine(r.x + 1, r.y + r.height - 2,  r.x + r.width - 2,   r.y + r.height - 2);
};

#if (AUDACITY_BRANDING == BRAND_UMIXIT)
   void BrandingPanel::SetProjectBranding(Branding* pBranding)
   {
      wxFileName brandLogoFileName = pBranding->GetBrandLogoFileName();
      if (brandLogoFileName.IsOk() && brandLogoFileName.FileExists())
      {
         if (mButton_ProjectLogo != NULL)
         {
            mButton_ProjectLogo->Destroy();
            mButton_ProjectLogo = NULL;
         }
         wxImage buttonImageUp(brandLogoFileName.GetFullPath());
         mButton_ProjectLogo = 
            new AButton(this, ID_BUTTON_PROJECT_LOGO, 
                        wxPoint(2, 2),  // inset for bevel
                        wxSize(buttonImageUp.GetWidth(), buttonImageUp.GetHeight()), 
                        &buttonImageUp, &buttonImageUp, 
                        &buttonImageUp, &buttonImageUp, //v other images?
                        true); 
         if (mMinWidth < buttonImageUp.GetWidth() + kQuadrupleInset)
            mMinWidth = buttonImageUp.GetWidth() + kQuadrupleInset;
         if (mMinHeight < buttonImageUp.GetHeight() + kInset) // height plus bevel
            mMinHeight = buttonImageUp.GetHeight() + kInset;
         mProjectLogo_origWidth = buttonImageUp.GetWidth(); //vvv These appear only for wxALIGN_TOP panels. 

         #if wxUSE_TOOLTIPS
            mButton_ProjectLogo->SetToolTip(pBranding->GetBrandURL());
         #endif // wxUSE_TOOLTIPS

         mProject->HandleResize();
      }
   }

#elif (AUDACITY_BRANDING == BRAND_THINKLABS)
   void BrandingPanel::EnableDisableButtons()
   {
      bool bAudioIONotBusy = !gAudioIO->IsBusy();
      bool bTimeSelected = (mProject->GetSel0() < mProject->GetSel1());

      bool bWaveTracksSelected = false;
      TrackList* pTracks = mProject->GetTracks();
      TrackListIterator iter(pTracks);
      for (Track* t = iter.First(); t; t = iter.Next())
         if (t->GetSelected())
         {
            bWaveTracksSelected = true;
            break;
         }

      mButton_Import->SetEnabled(bAudioIONotBusy);

      bool bEnableEffects = bAudioIONotBusy && bTimeSelected && bWaveTracksSelected;
      mButton_Amplify->SetEnabled(bEnableEffects);
      mButton_Filter->SetEnabled(bEnableEffects);
      mButton_Rate->SetEnabled(bEnableEffects);

      mButton_Display->SetEnabled(!pTracks->IsEmpty());
   }

   void BrandingPanel::AddCustomButtons()
   {
      wxRect bev;
      wxImage buttonImageUp;
      wxImage buttonImageOver;
      wxImage buttonImageDown;
      wxImage buttonImageDis;
      wxPoint buttonPos(kInset, kInset);
      wxSize buttonSize;
      wxMemoryDC dc;
      wxBitmap* pButtonBitmap;

      mMinLeftSectionWidth = kTripleInset;

      // Import button
      // Make up and down from same image, with different bevel
      pButtonBitmap = new wxBitmap(Thinklabs_button_Import_xpm);
      dc.SelectObject(*pButtonBitmap);
      bev = wxRect(0, 0, pButtonBitmap->GetWidth() - 1, pButtonBitmap->GetHeight() - 1);
      Bevel2OnWhite(dc, true, bev);
      buttonImageUp = pButtonBitmap->ConvertToImage();
      Bevel2OnWhite(dc, false, bev);
      buttonImageDown = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Import_over_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageOver = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Import_dis_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageDis = pButtonBitmap->ConvertToImage();

      buttonPos.x = mMinLeftSectionWidth;
      buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
      mButton_Import = 
         new AButton(this, ID_BUTTON_IMPORT, 
                     buttonPos, buttonSize, 
                     &buttonImageUp, &buttonImageOver, 
                     &buttonImageDown, &buttonImageDis,
                     true); 
      if (mMinHeight < buttonSize.GetHeight() + kDoubleInset)
         mMinHeight = buttonSize.GetHeight() + kDoubleInset;
      mMinLeftSectionWidth += buttonSize.GetWidth() + kInset;


      // Amplify button
      pButtonBitmap = new wxBitmap(Thinklabs_button_Amplify_xpm);
      dc.SelectObject(*pButtonBitmap);
      bev = wxRect(0, 0, pButtonBitmap->GetWidth() - 1, pButtonBitmap->GetHeight() - 1);
      Bevel2OnWhite(dc, true, bev);
      buttonImageUp = pButtonBitmap->ConvertToImage();
      Bevel2OnWhite(dc, false, bev);
      buttonImageDown = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Amplify_over_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageOver = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Amplify_dis_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageDis = pButtonBitmap->ConvertToImage();

      buttonPos.x = mMinLeftSectionWidth;
      buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
      mButton_Amplify = 
         new AButton(this, ID_BUTTON_AMPLIFY, 
                     buttonPos, buttonSize, 
                     &buttonImageUp, &buttonImageOver, 
                     &buttonImageDown, &buttonImageDis,
                     true); 
      mButton_Amplify->Disable();
      if (mMinHeight < buttonSize.GetHeight() + kDoubleInset)
         mMinHeight = buttonSize.GetHeight() + kDoubleInset;
      mMinLeftSectionWidth += buttonSize.GetWidth() + kInset;


      // Filter button
      pButtonBitmap = new wxBitmap(Thinklabs_button_Filter_xpm);
      dc.SelectObject(*pButtonBitmap);
      bev = wxRect(0, 0, pButtonBitmap->GetWidth() - 1, pButtonBitmap->GetHeight() - 1);
      Bevel2OnWhite(dc, true, bev);
      buttonImageUp = pButtonBitmap->ConvertToImage();
      Bevel2OnWhite(dc, false, bev);
      buttonImageDown = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Filter_over_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageOver = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Filter_dis_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageDis = pButtonBitmap->ConvertToImage();

      buttonPos.x = mMinLeftSectionWidth;
      buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
      mButton_Filter = 
         new AButton(this, ID_BUTTON_FILTER, 
                     buttonPos, buttonSize, 
                     &buttonImageUp, &buttonImageOver, 
                     &buttonImageDown, &buttonImageDis,
                     true); 
      mButton_Filter->Disable();
      if (mMinHeight < buttonSize.GetHeight() + kDoubleInset)
         mMinHeight = buttonSize.GetHeight() + kDoubleInset;
      mMinLeftSectionWidth += buttonSize.GetWidth() + kInset;


      // Rate button
      pButtonBitmap = new wxBitmap(Thinklabs_button_Rate_xpm);
      dc.SelectObject(*pButtonBitmap);
      bev = wxRect(0, 0, pButtonBitmap->GetWidth() - 1, pButtonBitmap->GetHeight() - 1);
      Bevel2OnWhite(dc, true, bev);
      buttonImageUp = pButtonBitmap->ConvertToImage();
      Bevel2OnWhite(dc, false, bev);
      buttonImageDown = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Rate_over_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageOver = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Rate_dis_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageDis = pButtonBitmap->ConvertToImage();

      buttonPos.x = mMinLeftSectionWidth;
      buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
      mButton_Rate = 
         new AButton(this, ID_BUTTON_RATE, 
                     buttonPos, buttonSize, 
                     &buttonImageUp, &buttonImageOver, 
                     &buttonImageDown, &buttonImageDis,
                     true); 
      mButton_Rate->Disable();
      if (mMinHeight < buttonSize.GetHeight() + kDoubleInset)
         mMinHeight = buttonSize.GetHeight() + kDoubleInset;
      mMinLeftSectionWidth += buttonSize.GetWidth() + kInset;


      // Display button
      pButtonBitmap = new wxBitmap(Thinklabs_button_Display_xpm);
      dc.SelectObject(*pButtonBitmap);
      bev = wxRect(0, 0, pButtonBitmap->GetWidth() - 1, pButtonBitmap->GetHeight() - 1);
      Bevel2OnWhite(dc, true, bev);
      buttonImageUp = pButtonBitmap->ConvertToImage();
      Bevel2OnWhite(dc, false, bev);
      buttonImageDown = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Display_over_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageOver = pButtonBitmap->ConvertToImage();

      pButtonBitmap = new wxBitmap(Thinklabs_button_Display_dis_xpm);
      dc.SelectObject(*pButtonBitmap);
      Bevel2OnWhite(dc, true, bev);
      buttonImageDis = pButtonBitmap->ConvertToImage();

      buttonPos.x = mMinLeftSectionWidth;
      buttonSize = wxSize(pButtonBitmap->GetWidth(), pButtonBitmap->GetHeight()); 
      mButton_Display = 
         new AButton(this, ID_BUTTON_DISPLAY, 
                     buttonPos, buttonSize, 
                     &buttonImageUp, &buttonImageOver, 
                     &buttonImageDown, &buttonImageDis,
                     true); 
      mButton_Display->Disable();
      if (mMinHeight < buttonSize.GetHeight() + kDoubleInset)
         mMinHeight = buttonSize.GetHeight() + kDoubleInset;
      mMinLeftSectionWidth += buttonSize.GetWidth() + kInset;

      #if wxUSE_TOOLTIPS
         mButton_Import->SetToolTip(_("Import Audio (Add sound file to current Project)... (Ctrl+I)"));
         mButton_Amplify->SetToolTip(_("Amplify/Attenuate selected waveform..."));
         mButton_Filter->SetToolTip(_("Filter selected waveform..."));
         mButton_Rate->SetToolTip(_("Slow down selected waveform..."));
         mButton_Display->SetToolTip(_("Waveform Display for All Tracks"));
      #endif // wxUSE_TOOLTIPS
   }

   // event handlers
   void BrandingPanel::OnButton_Import(wxCommandEvent& event)
   {
      mProject->OnImport();
      mButton_Import->PopUp();
   }

   void BrandingPanel::OnButton_Amplify(wxCommandEvent& event)
   {
      Effect* pEffect = Effect::GetEffectByName(_("Amplify..."), PROCESS_EFFECT | BUILTIN_EFFECT);
      if (pEffect)
         mProject->OnProcessEffect(pEffect->GetID());

      mButton_Amplify->PopUp();
   }

   void BrandingPanel::OnButton_Filter(wxCommandEvent& event)
   {
      Effect* pEffect = Effect::GetEffectByName(_("Equalization..."), PROCESS_EFFECT | BUILTIN_EFFECT);
      if (pEffect)
         mProject->OnProcessEffect(pEffect->GetID());

      mButton_Filter->PopUp();
   }

   void BrandingPanel::OnButton_Rate(wxCommandEvent& event)
   {
      Effect* pEffect = Effect::GetEffectByName(_("Change Tempo..."), PROCESS_EFFECT | BUILTIN_EFFECT);
      if (pEffect)
         mProject->OnProcessEffect(pEffect->GetID());

      mButton_Rate->PopUp();
   }

   void BrandingPanel::OnButton_Display(wxCommandEvent& event)
   {
      static bool bWantWaveformDisplay = true; // Start with WaveformAndSpectrumDisplay.
      bWantWaveformDisplay = !bWantWaveformDisplay;

      TrackList* pTracks = mProject->GetTracks();
      TrackListIterator iter(pTracks);
      for (Track* pTrack = iter.First(); pTrack; pTrack = iter.Next())
         if (pTrack->GetKind() == Track::Wave)
         {
            WaveTrack* pWaveTrack = (WaveTrack*)pTrack;
            if (bWantWaveformDisplay)
               pWaveTrack->SetDisplay(WaveTrack::WaveformDisplay);
            else
               pWaveTrack->SetDisplay(WaveTrack::WaveformAndSpectrumDisplay);
         }

      #if wxUSE_TOOLTIPS
         if (bWantWaveformDisplay)
            mButton_Display->SetToolTip(_("Waveform and Spectrum Display for All Tracks"));
         else
            mButton_Display->SetToolTip(_("Waveform Display for All Tracks"));
      #endif // wxUSE_TOOLTIPS

      mButton_Display->PopUp();

      // Update the TrackPanel correspondingly. 
      // Calling RedrawProject is inefficient relative to sending a msg to TrackPanel 
      // for a particular track and control, but not a real performance hit.
      mProject->RedrawProject();
   }

#endif // (AUDACITY_BRANDING == BRAND_THINKLABS)


// event handlers
#if (AUDACITY_BRANDING != BRAND_THINKLABS)
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
#endif

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

void BrandingPanel::OnKeyEvent(wxKeyEvent & event)
{
   event.Skip(true); // Ignore them. //v
}

void BrandingPanel::OnPaint(wxPaintEvent& evt)
{
   wxPaintDC dc(this);

   dc.BeginDrawing();
   wxSize mySize = this->GetSize();
   wxRect bev(0, 0, mySize.GetWidth(), mySize.GetHeight());
   Bevel2OnWhite(dc, true, bev);
   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      if (mButton_ProjectLogo)
      {
         // Also bevel the cluster at right, when Project Logo is showing.
         // Whether stacked or side-by-side, mButton_AudacityLogo is always at the righttmost edge.
         bev = 
            wxRect(mySize.GetWidth() - mMinRightSectionWidth + 2, 0, 
                     mMinRightSectionWidth, mySize.GetHeight() - 1);
         Bevel2OnWhite(dc, true, bev);
      }
   #endif
   dc.EndDrawing();
}

void BrandingPanel::OnSize(wxSizeEvent &evt)
{
   wxPoint buttonPos;
   wxSize evtSize = evt.GetSize();
   int yPos;

   wxSize buttonSize_CompanyLogo = mButton_CompanyLogo->GetSize();
   wxSize buttonSize_AudacityLogo = 
      mButton_AudacityLogo ? mButton_AudacityLogo->GetSize() : wxSize(0, 0);
   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      // mButton_ProjectLogo never moves from (2, 2).
      wxSize buttonSize_ProjectLogo = 
         mButton_ProjectLogo ? mButton_ProjectLogo->GetSize() : wxSize(0, 0);
      int newProjectLogoWidth = 0;
      mMinRightSectionWidth = buttonSize_AudacityLogo.GetWidth() + kDoubleInset;
   #endif

   if (mButton_AudacityLogo && 
         ((buttonSize_CompanyLogo.GetHeight() + buttonSize_AudacityLogo.GetHeight() + 4) <= evtSize.GetHeight()))
   {
      // Stack the logos vertically with 2-pixel bevel.
      // Can stack mButton_CompanyLogo and mButton_AudacityLogo at the right side.
      yPos = (evtSize.GetHeight() / 2) - buttonSize_CompanyLogo.GetHeight();
      if (yPos < 2) // 2-pixel bevel
         yPos = 2;
      buttonPos = wxPoint(evtSize.GetWidth() - buttonSize_CompanyLogo.GetWidth() - kInset, yPos);
      mButton_CompanyLogo->Move(buttonPos);

      yPos += buttonSize_CompanyLogo.GetHeight() - 1; // no vertical gap
      buttonPos = wxPoint(evtSize.GetWidth() - buttonSize_AudacityLogo.GetWidth() - kInset, yPos);
      mButton_AudacityLogo->Move(buttonPos);

      #if (AUDACITY_BRANDING != BRAND_THINKLABS)
         if (mButton_ProjectLogo)
         {
            if (mMinRightSectionWidth < buttonSize_CompanyLogo.GetWidth() + kDoubleInset)
               mMinRightSectionWidth = buttonSize_CompanyLogo.GetWidth() + kDoubleInset;
            newProjectLogoWidth = evtSize.GetWidth() - mMinRightSectionWidth;
            if (newProjectLogoWidth > mProjectLogo_origWidth)
               newProjectLogoWidth = mProjectLogo_origWidth;
            mButton_ProjectLogo->SetSize(2, 2, newProjectLogoWidth, -1);
         }
      #endif
   }
   else if (mButton_AudacityLogo && 
            (
               #if (AUDACITY_BRANDING == BRAND_THINKLABS)
                  mMinLeftSectionWidth + 
               #else 
                  buttonSize_ProjectLogo.GetWidth() + 
               #endif
               buttonSize_CompanyLogo.GetWidth() + 
               buttonSize_AudacityLogo.GetWidth() + kTripleInset) < evtSize.GetWidth())
   {
      // Put the company and Audacity logos side-by side. 
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
      #if (AUDACITY_BRANDING != BRAND_THINKLABS)
         if (mButton_ProjectLogo)
         {
            mMinRightSectionWidth += buttonSize_CompanyLogo.GetWidth() + kInset;
            newProjectLogoWidth = evtSize.GetWidth() - mMinRightSectionWidth;
            if (newProjectLogoWidth > mProjectLogo_origWidth)
               newProjectLogoWidth = mProjectLogo_origWidth;
            mButton_ProjectLogo->SetSize(2, 2, newProjectLogoWidth, -1);
         }
      #endif
   }
   else
   {
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         // Just move mButton_CompanyLogo and mButton_AudacityLogo to the immediate right of left section.
         buttonPos = wxPoint(mMinLeftSectionWidth + kDoubleInset, kInset);
      #else 
         // Just move mButton_CompanyLogo and mButton_AudacityLogo to the immediate right of mButton_ProjectLogo.
         buttonPos = wxPoint(buttonSize_ProjectLogo.GetWidth() + kDoubleInset, kInset);
      #endif
      mButton_CompanyLogo->Move(buttonPos);

      if (mButton_AudacityLogo)
      {
         if (evtSize.GetHeight() - buttonSize_CompanyLogo.GetHeight() >
               evtSize.GetWidth() - buttonSize_CompanyLogo.GetWidth())
            // Can show more of mButton_AudacityLogo below than to the right.
            buttonPos = wxPoint(kInset, buttonSize_CompanyLogo.GetHeight() + kDoubleInset);
         else 
            buttonPos = wxPoint(buttonPos.x + buttonSize_CompanyLogo.GetWidth() + kInset, kInset);
         mButton_AudacityLogo->Move(buttonPos);
      }
   }
}

// end of class BrandingPanel
#endif // WANT_BRANDING_PANEL


// class Branding: 
//    Project-specific branding (brand name, website URL, logo, color scheme)

#if WANT_BRANDING_PANEL
   Branding::Branding(BrandingPanel* pBrandingPanel)
      : mBrandingPanel(pBrandingPanel)
#else
   Branding::Branding()
#endif
{
   m_strBrandName = wxT("");
   m_strBrandURL = wxT("");
   m_strBrandColorScheme = wxT("");
}

bool Branding::HandleXMLTag(const wxChar *tag, const wxChar **attrs) 
{
   if (wxStrcmp(tag, wxT("branding"))) 
      return false;

   // loop through attrs, null-terminated list of attribute-value pairs
   while (*attrs) {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value) 
         break;

      const wxString strValue = value;
      if (!wxStrcmp(attr, wxT("brandname")) && XMLValueChecker::IsGoodString(strValue)) 
         m_strBrandName = value;
      else if (!wxStrcmp(attr, wxT("url")) && XMLValueChecker::IsGoodString(strValue)) 
         m_strBrandURL = value;
      else if (!wxStrcmp(attr, wxT("logofilename"))) 
      {
         // Logo file is supposed to be stored in the project data directory.
         AudacityProject* pProject = GetActiveProject();
         wxString strDirName = pProject->GetDirManager()->GetProjectDataDir();
         if (XMLValueChecker::IsGoodFileName(value, strDirName)) 
         {
            // Store full thing, not just file name, so don't need to add path again.
            m_BrandLogoFileName.Assign(strDirName, value);
            m_BrandLogoFileName.Normalize(wxPATH_NORM_ABSOLUTE | wxPATH_NORM_LONG);
            #if (AUDACITY_BRANDING == BRAND_UMIXIT)
               mBrandingPanel->SetProjectBranding(this);
               pProject->GetMixerBoard()->SetProjectBranding(this);
            #endif
         } 
         else
         {
            wxLogWarning(wxT("Could not open branding logo file: %s"), value);
            return false;
         }
      }
      else if (!wxStrcmp(attr, wxT("colorscheme")) && XMLValueChecker::IsGoodString(strValue)) 
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



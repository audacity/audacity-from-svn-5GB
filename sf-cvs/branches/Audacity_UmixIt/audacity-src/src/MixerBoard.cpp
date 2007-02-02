/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerBoard.cpp

  Vaughan Johnson, January 2007

**********************************************************************/

#include <math.h>

#include "AColor.h"
#include "Branding.h"
#include "MixerBoard.h"
#include "Project.h"

#define kInset 4

// class MixerTrackPanel

#define TITLE_BAR_HEIGHT 18
#define MUTE_SOLO_HEIGHT 16
#define PAN_HEIGHT 24

enum {
   ID_TOGGLEBUTTON_MUTE = 13000,
   ID_TOGGLEBUTTON_SOLO,
   ID_ASLIDER_PAN,
   ID_SLIDER_GAIN,
   ID_METER,
};

BEGIN_EVENT_TABLE(MixerTrackPanel, wxPanel)
   EVT_TOGGLEBUTTON(ID_TOGGLEBUTTON_MUTE, MixerTrackPanel::OnButton_Mute)
   EVT_TOGGLEBUTTON(ID_TOGGLEBUTTON_SOLO, MixerTrackPanel::OnButton_Solo)
   EVT_SLIDER(ID_ASLIDER_PAN, MixerTrackPanel::OnSlider_Pan)
   EVT_SLIDER(ID_SLIDER_GAIN, MixerTrackPanel::OnSlider_Gain)
   EVT_COMMAND_SCROLL(ID_SLIDER_GAIN, MixerTrackPanel::OnSliderScroll_Gain)

   EVT_PAINT(MixerTrackPanel::OnPaint)
END_EVENT_TABLE()

IMPLEMENT_CLASS(MixerTrackPanel, wxPanel)


MixerTrackPanel::MixerTrackPanel(MixerBoard* parent, AudacityProject* project, 
                                 WaveTrack* pLeftTrack, WaveTrack* pRightTrack /*= NULL*/, 
                                 const wxPoint& pos /*= wxDefaultPosition*/, 
                                 const wxSize& size /*= wxDefaultSize*/) : 
   wxPanel(parent, -1, pos, size)
{
   wxASSERT(parent);
   mMixerBoard = parent;

   wxASSERT(project);
   mProject = project;

   wxASSERT(pLeftTrack);
   mLeftTrack = pLeftTrack;

   mRightTrack = pRightTrack;


   // CREATE THE CONTROLS PROGRAMMATICALLY.
   
   //vvv For some reason the sizers aren't getting offset vertically, 
   // so positions are calculated explicitly below, but sizers are still in.
   wxBoxSizer* pBoxSizer_MixerTrackPanel = new wxBoxSizer(wxVERTICAL);

	// track name
   wxPoint ctrlPos(kInset, kInset);
   wxSize ctrlSize(size.GetWidth() - (2 * kInset), TITLE_BAR_HEIGHT);
   mStaticText_TrackName = 
      new wxStaticText(this, -1, mLeftTrack->GetName(), ctrlPos, ctrlSize, wxALIGN_CENTRE | wxSUNKEN_BORDER);
   mStaticText_TrackName->SetBackgroundColour(this->GetTrackColor());
   pBoxSizer_MixerTrackPanel->Add(mStaticText_TrackName, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   // mute/solo buttons
   ctrlPos.x = (size.GetWidth() / 8) + kInset;
   ctrlPos.y += TITLE_BAR_HEIGHT + (2 * kInset);
   ctrlSize = wxSize(MUTE_SOLO_HEIGHT, MUTE_SOLO_HEIGHT);
   mToggleButton_Mute = 
      new wxToggleButton(this, ID_TOGGLEBUTTON_MUTE, _("M"), /* i18n-hint: One-letter abbreviation for "Mute" */
                           ctrlPos, ctrlSize); 
                           //vvv Available on wxButton, not wxToggleButton: wxDefaultSize, wxBU_EXACTFIT);

   ctrlPos.x = size.GetWidth() * 5 / 8;
   mToggleButton_Solo = 
      new wxToggleButton(this, ID_TOGGLEBUTTON_SOLO, _("S"),  /* i18n-hint: One-letter abbreviation for "Solo" */
                           ctrlPos, ctrlSize); 
                           //vvv Available on wxButton, not wxToggleButton: wxDefaultSize, wxBU_EXACTFIT);

   //vvvvv No dc to call AColor::Solo(), so just hard code UmixIt prefs for now.  
   mToggleButton_Mute->SetForegroundColour(wxColour(255, 255, 0)); // yellow
   mToggleButton_Solo->SetForegroundColour(wxColour(0, 255, 0)); // green

   wxBoxSizer* pBoxSizer_MuteSolo = new wxBoxSizer(wxHORIZONTAL);
   pBoxSizer_MuteSolo->Add(mToggleButton_Mute, 0, wxALIGN_CENTER | wxALL, kInset);
   pBoxSizer_MuteSolo->Add((2 * kInset), 0, 0); // horizontal spacer
   pBoxSizer_MuteSolo->Add(mToggleButton_Solo, 0, wxALIGN_CENTER | wxALL, kInset);
   pBoxSizer_MixerTrackPanel->Add(pBoxSizer_MuteSolo, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   // pan slider
   ctrlPos.x = (size.GetWidth() / 10);
   ctrlPos.y += MUTE_SOLO_HEIGHT + (4 * kInset);
   ctrlSize = wxSize((size.GetWidth() * 4 / 5), PAN_HEIGHT);
   /* i18n-hint: Title of the Pan slider, used to move the sound left or right stereoscopically */
   mSlider_Pan = new ASlider(this, ID_ASLIDER_PAN, _("Pan"), ctrlPos, ctrlSize, PAN_SLIDER);
   pBoxSizer_MixerTrackPanel->Add(mSlider_Pan, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   // gain slider & level meter
   ctrlPos.x = (2 * kInset);
   ctrlPos.y += PAN_HEIGHT + (4 * kInset);
   ctrlSize = wxSize((size.GetWidth() / 3), //vvv * 2 / 5), 
                     (size.GetHeight() - ctrlPos.y - (4 * kInset)));
   const int kSliderMin = -6, kSliderMax = 36; // wxSlider has min at top, so this is [-36dB,6dB]. 
   int nSliderValue = 
      // Analog to LWSlider::Set() calc for DB_SLIDER. Negate because wxSlider has min at top.
      -(int)(20.0f * log10(mLeftTrack->GetGain()));
   if (nSliderValue < kSliderMin)
      nSliderValue = kSliderMin;
   if (nSliderValue > kSliderMax)
      nSliderValue = kSliderMax;
   mSlider_Gain = 
      // ASlider doesn't do vertical.  
      /* i18n-hint: Title of the Gain slider, used to adjust the volume */
      //    new ASlider(this, ID_SLIDER_GAIN, _("Gain"), ctrlPos, ctrlSize, DB_SLIDER);
      new wxSlider(this, ID_SLIDER_GAIN, nSliderValue, // wxWindow* parent, wxWindowID id, int value 
                     kSliderMin, kSliderMax, // int minValue, int maxValue, 
                     ctrlPos, ctrlSize, // const wxPoint& point = wxDefaultPosition, const wxSize& size = wxDefaultSize, 
                     wxSL_VERTICAL | wxSL_AUTOTICKS); // long style = wxSL_HORIZONTAL, ...

   ctrlPos.x += ctrlSize.GetWidth() + kInset;
   ctrlSize = wxSize(((size.GetWidth() / 2) - kInset), ctrlSize.GetHeight());
   mMeter = new Meter(this, ID_METER, false, Meter::MixerTrackPanel, ctrlPos, ctrlSize, this->GetTrackColor());
   mMeter->Reset(mLeftTrack->GetRate(), true);

   wxBoxSizer* pBoxSizer_GainAndMeter = new wxBoxSizer(wxHORIZONTAL);
   pBoxSizer_GainAndMeter->Add(mSlider_Gain, 0, wxALIGN_CENTER | wxALL, kInset);
   pBoxSizer_GainAndMeter->Add(mMeter, 0, wxALIGN_CENTER | wxALL, kInset);
   pBoxSizer_MixerTrackPanel->Add(pBoxSizer_GainAndMeter, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   #if wxUSE_TOOLTIPS
      mStaticText_TrackName->SetToolTip(_T("Track Name"));
      mToggleButton_Mute->SetToolTip(_T("Mute"));
      mToggleButton_Solo->SetToolTip(_T("Solo"));
      // LWSlider already shows the value, so don't do this:   mSlider_Pan->SetToolTip(_T("Pan"));
      
      wxScrollEvent dummy;
      this->OnSliderScroll_Gain(dummy); // Set the tooltip to show the current value.

      mMeter->SetToolTip(_T("Level Meter"));
   #endif // wxUSE_TOOLTIPS


   //this->SetSizer(pBoxSizer_MixerTrackPanel);
   //vvv Don't want to shrink to minimum for sizer. 
   //pBoxSizer_MixerTrackPanel->Fit(this);
   //pBoxSizer_MixerTrackPanel->SetSizeHints(this);
}

wxColour MixerTrackPanel::GetTrackColor()
{
   //vvv This doesn't work right when switching back and forth between two projects 
   // when one is branded and the other is not, because for some reason, OnActivate 
   // isn't always called, so gActiveProject isn't updated. 
   Branding* pBranding = mProject->GetBranding();
   if (pBranding && (pBranding->GetBrandColorScheme() == "UmixIt")) //vvv UmixIt 
      return AColor::GetTrackColor((void*)mLeftTrack);
   return wxColour(102, 255, 102); // same as Meter playback color
}

void MixerTrackPanel::Update(double t, bool bForce /*= false*/)
{
   // This value 256 is analogous to typical value I saw for framesPerBuffer in audacityAudioCallback, 
   // but a smaller number gives better performance.
   const int kWidth = 256; // analog of mid.width in TrackArtist::DrawWaveform

   int numChannels = (mRightTrack != NULL) ? 2 : 1; // Doesn't handle more than 2 channels, as per kMaxMeterBars.
   
   // Arrays containing the shape of the waveform - each array has one value per pixel.
   float* min = new float[kWidth]; // Don't need separate left & right ones, because it's ignored.
   float* maxLeft = new float[kWidth];
   float* rmsLeft = new float[kWidth];
   float* maxRight = (numChannels == 1) ? NULL : new float[kWidth];
   float* rmsRight = (numChannels == 1) ? NULL : new float[kWidth];
   sampleCount *where = new sampleCount[kWidth+1]; // Don't need separate left & right ones, because it's ignored.

   double pps = mLeftTrack->GetRate(); 
   
   // The WaveTrack class handles the details of computing the shape
   // of the waveform.  The only way GetWaveDisplay will fail is if
   // there's a serious error, like some of the waveform data can't
   // be loaded.  So if the function returns false, just skip it.
   wxASSERT(mLeftTrack);
   if (mLeftTrack->GetWaveDisplay(min, maxLeft, rmsLeft, where, kWidth, t, pps) && 
         ((mRightTrack == NULL) || 
            mRightTrack->GetWaveDisplay(min, maxRight, rmsRight, where, kWidth, t, pps)))
   {
      wxASSERT(mMeter);
      mMeter->UpdateDisplay(numChannels, kWidth, maxLeft, rmsLeft, maxRight, rmsRight);
   }
   delete[] min;
   delete[] maxLeft;
   delete[] rmsLeft;
   delete[] maxRight;
   delete[] rmsRight;
   delete[] where;

   wxPaintEvent dummyEvt;
   this->OnPaint(dummyEvt);
}

// event handlers
void MixerTrackPanel::OnButton_Mute(wxCommandEvent& event)
{
   wxASSERT(mLeftTrack);
   //vvv Not handling shift-click as in TrackPanel. Tell parent to handle it?
   mLeftTrack->SetMute(mToggleButton_Mute->GetValue());
}

void MixerTrackPanel::OnButton_Solo(wxCommandEvent& event)
{
   wxASSERT(mLeftTrack);
   //vvv Not handling shift-click as in TrackPanel. Tell parent to handle it?
   mLeftTrack->SetSolo(mToggleButton_Solo->GetValue());
}

void MixerTrackPanel::OnSlider_Pan(wxCommandEvent& event)
{
   wxASSERT(mLeftTrack);
   float fValue = mSlider_Pan->Get();
   mLeftTrack->SetPan(fValue);
   if (mRightTrack != NULL)
      mRightTrack->SetPan(fValue);
   mProject->TP_PushState(_("Moved pan slider"), _("Pan"), true /* consolidate */);
}

void MixerTrackPanel::OnSlider_Gain(wxCommandEvent& event)
{
   wxASSERT(mLeftTrack);

   // Analog to LWSlider::Set() calc for DB_SLIDER. Negate because wxSlider has min at top.
   // mSlider_Gain->GetValue() is in [-6,36]. wxSlider has min at top, so this is [-36dB,6dB]. 
   //vvv ASlider version:    mSlider_Gain->Get();
   float fValue = pow(10.0f, -(float)(mSlider_Gain->GetValue()) / 20.0f); 
   mLeftTrack->SetGain(fValue);
   if (mRightTrack != NULL)
      mRightTrack->SetGain(fValue);
   mProject->TP_PushState(_("Moved gain slider"), _("Gain"), true /* consolidate */);
}

void MixerTrackPanel::OnSliderScroll_Gain(wxScrollEvent& event)
{
   mSlider_Gain->SetToolTip(wxString::Format(_T("Gain=%ddB"), -mSlider_Gain->GetValue()));
}

void MixerTrackPanel::OnPaint(wxPaintEvent &evt)
{
   wxPaintDC dc(this);

   dc.BeginDrawing();

   wxRect bev = this->GetRect();
   bev.x = 0;
   bev.Inflate(-2, -2);
   AColor::Bevel(dc, true, bev);

   //wxASSERT(mLeftTrack);

   //vvv this->DrawTitleBar(&dc, bev);
   //vvv this->DrawMuteSolo(&dc, bev, mLeftTrack->GetMute(), false);  // mute button
   //vvv this->DrawMuteSolo(&dc, bev, mLeftTrack->GetSolo(), true);   // solo button
   //vvv this->DrawPan(&dc, bev);
   //vvv mSlider_Pan->OnPaint(dc, mLeftTrack->GetSelected());
   
   dc.EndDrawing();
}


// class MixerBoard

BEGIN_EVENT_TABLE(MixerBoard, wxFrame)
   EVT_CHAR(MixerBoard::OnKeyEvent)
   EVT_CLOSE(MixerBoard::OnCloseWindow)
END_EVENT_TABLE()

#define MIXER_TRACK_PANEL_MIN_WIDTH 40 //vvv
const wxSize gSize = wxSize(640, 440); // default

MixerBoard::MixerBoard(AudacityProject *parent):
  wxFrame(parent, -1, _("Audacity Mixer Board"), wxDefaultPosition,  gSize, 
            //vvv No resize for now.   wxDEFAULT_FRAME_STYLE | ((parent == NULL) ? 0x0 : wxFRAME_FLOAT_ON_PARENT))
            wxCAPTION | wxSYSTEM_MENU | wxFRAME_TOOL_WINDOW | ((parent == NULL) ? 0x0 : wxFRAME_FLOAT_ON_PARENT))
{
   mProject = parent;
   mSize = gSize;
   mT = -1.0;

   //vvv Using wxFRAME_TOOL_WINDOW eliminates the icon and its menu.
   //   // loads either the XPM or the windows resource, depending on the platform
   //#if !defined(__WXMAC__) && !defined(__WXX11__)
   //   #ifdef __WXMSW__
   //      wxIcon ic(wxICON(AudacityLogo));
   //   #else
   //      wxIcon ic(wxICON(AudacityLogo48x48));
   //   #endif
   //   SetIcon(ic);
   //#endif
}

MixerBoard::~MixerBoard()
{}

void MixerBoard::Update(double t, bool bForce /* = false */)
{
   if ((t == mT) && !bForce)
      return;

   mT = t;

   TrackList* pTrackList = mProject->GetTracks();
   if (pTrackList->IsEmpty())
      return;

   int nMixerTrackPanelWidth = (mSize.GetX() - kInset) / (2 * kInset);
   if (nMixerTrackPanelWidth < MIXER_TRACK_PANEL_MIN_WIDTH) 
      nMixerTrackPanelWidth = MIXER_TRACK_PANEL_MIN_WIDTH;
   
   //vvv Need to reorder when track order changes. This just makes sure all are visible, 
   //    and relies (via count) on them staying in order.
   unsigned int count = 0;
   MixerTrackPanel* pMixerTrackPanel;
   Track* pLeftTrack;
   Track* pRightTrack;
   MixerTrackPanelHash::iterator iterHash;
   TrackListIterator iterTracks(pTrackList);

   pLeftTrack = iterTracks.First();
   while (pLeftTrack) {
      pRightTrack = NULL;
      if (pLeftTrack->GetLinked()) 
         pRightTrack = iterTracks.Next();

      if (pLeftTrack->GetKind() == Track::Wave) {
         iterHash = mMixerTrackPanels.find(pLeftTrack);
         if (iterHash != mMixerTrackPanels.end())
         {  // Found it.
            pMixerTrackPanel = &(*iterHash->second);
            pMixerTrackPanel->Update(t, bForce);
         }
         else
         {
            wxPoint panelPos(count * nMixerTrackPanelWidth, 0); // y should always be 0. 
            wxSize panelSize(nMixerTrackPanelWidth, gSize.GetY() - 22);

            pMixerTrackPanel = 
               new MixerTrackPanel(this, mProject, 
                                    (WaveTrack*)pLeftTrack, (WaveTrack*)pRightTrack, 
                                    panelPos, panelSize);
            if (pMixerTrackPanel)
               mMixerTrackPanels[(void*)pLeftTrack] = pMixerTrackPanel;
         }
         count++;
      }
      pLeftTrack = iterTracks.Next();
   }
}

// event handlers
void MixerBoard::OnKeyEvent(wxKeyEvent & event)
{
   GetActiveProject()->HandleKeyDown(event);
}

void MixerBoard::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
  this->Hide();
}


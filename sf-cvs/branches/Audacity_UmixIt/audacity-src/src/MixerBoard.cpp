/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerBoard.cpp

  Vaughan Johnson, January 2007

**********************************************************************/

#include <math.h>

#include <wx/dcmemory.h>
#include <wx/settings.h> // for wxSystemSettings::GetSystemColour
#include <wx/sizer.h>

#include "AColor.h"
#include "Branding.h"
#include "MixerBoard.h"
#include "Project.h"

#include "../images/MusicalInstruments.h"

// class MixerTrackCluster

#define kInset 4
#define TITLE_BAR_HEIGHT 18
#define MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH 48
#define MUTE_SOLO_HEIGHT 16
#define PAN_HEIGHT 24

enum {
   ID_TOGGLEBUTTON_MUTE = 13000,
   ID_TOGGLEBUTTON_SOLO,
   ID_ASLIDER_PAN,
   ID_SLIDER_GAIN,
};

BEGIN_EVENT_TABLE(MixerTrackCluster, wxPanel)
   EVT_COMMAND(ID_TOGGLEBUTTON_MUTE, wxEVT_COMMAND_BUTTON_CLICKED, MixerTrackCluster::OnButton_Mute)
   EVT_COMMAND(ID_TOGGLEBUTTON_SOLO, wxEVT_COMMAND_BUTTON_CLICKED, MixerTrackCluster::OnButton_Solo)
   EVT_SLIDER(ID_ASLIDER_PAN, MixerTrackCluster::OnSlider_Pan)
   EVT_SLIDER(ID_SLIDER_GAIN, MixerTrackCluster::OnSlider_Gain)
   EVT_COMMAND_SCROLL(ID_SLIDER_GAIN, MixerTrackCluster::OnSliderScroll_Gain)

   EVT_PAINT(MixerTrackCluster::OnPaint)
END_EVENT_TABLE()

IMPLEMENT_CLASS(MixerTrackCluster, wxPanel)

const int kSliderMin = -6, kSliderMax = 36; // wxSlider has min at top, so this is [-36dB,6dB]. 

MixerTrackCluster::MixerTrackCluster(wxScrolledWindow* parent, 
                                       MixerBoard* grandParent, AudacityProject* project, 
                                       WaveTrack* pLeftTrack, WaveTrack* pRightTrack /*= NULL*/, 
                                       const wxPoint& pos /*= wxDefaultPosition*/, 
                                       const wxSize& size /*= wxDefaultSize*/) : 
   wxPanel(parent, -1, pos, size)
{
   mMixerBoard = grandParent;
   mProject = project;
   mLeftTrack = pLeftTrack;
   mRightTrack = pRightTrack;

   // CREATE THE CONTROLS PROGRAMMATICALLY.
   
   //vvv For some reason the sizers aren't getting offset vertically, 
   // probably because I'm not using wxDefaultPosition, 
   // so positions are calculated explicitly below, but sizers are still in.
   wxBoxSizer* pBoxSizer_MixerTrackCluster = new wxBoxSizer(wxVERTICAL);
   wxColour trackColor = this->GetTrackColor();

	// track name
   wxPoint ctrlPos(kInset, kInset);
   wxSize ctrlSize(size.GetWidth() - (2 * kInset), TITLE_BAR_HEIGHT);
   mStaticText_TrackName = 
      new wxStaticText(this, -1, mLeftTrack->GetName(), ctrlPos, ctrlSize, 
                        wxALIGN_CENTRE | wxST_NO_AUTORESIZE | wxSUNKEN_BORDER);
   mStaticText_TrackName->SetBackgroundColour(trackColor);
   pBoxSizer_MixerTrackCluster->Add(mStaticText_TrackName, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   // musical instrument image
   ctrlPos.x = (size.GetWidth() - MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH) / 2; // center
   ctrlPos.y += TITLE_BAR_HEIGHT + (2 * kInset);
   ctrlSize = wxSize(MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH, MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH);
   wxBitmap* bitmap = mMixerBoard->GetMusicalInstrumentBitmap(mLeftTrack);
   wxASSERT(bitmap);
   mStaticBitmap_MusicalInstrument = 
      new wxStaticBitmap(this, -1, *bitmap, ctrlPos, ctrlSize, wxSUNKEN_BORDER);
   pBoxSizer_MixerTrackCluster->Add(mStaticBitmap_MusicalInstrument, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   // mute/solo buttons
   ctrlPos.x = (size.GetWidth() / 8) + kInset;
   ctrlPos.y += MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH + (4 * kInset);
   ctrlSize = wxSize(MUTE_SOLO_HEIGHT, MUTE_SOLO_HEIGHT);
   mToggleButton_Mute = 
      new AButton(this, ID_TOGGLEBUTTON_MUTE, 
                  ctrlPos, ctrlSize, 
                  mMixerBoard->mImageMuteUp, mMixerBoard->mImageMuteOver, 
                  mMixerBoard->mImageMuteDown, mMixerBoard->mImageMuteDisabled, 
                  true); // toggle button
   mToggleButton_Mute->SetAlternateImages(
      mMixerBoard->mImageMuteUp, mMixerBoard->mImageMuteOver, 
      mMixerBoard->mImageMuteDownWhileSolo, mMixerBoard->mImageMuteDisabled);

   ctrlPos.x = size.GetWidth() * 5 / 8;
   mToggleButton_Solo = 
      new AButton(this, ID_TOGGLEBUTTON_SOLO, 
                  ctrlPos, ctrlSize, 
                  mMixerBoard->mImageSoloUp, mMixerBoard->mImageSoloOver, 
                  mMixerBoard->mImageSoloDown, mMixerBoard->mImageSoloDisabled, 
                  true); // toggle button

   wxBoxSizer* pBoxSizer_MuteSolo = new wxBoxSizer(wxHORIZONTAL);
   pBoxSizer_MuteSolo->Add(mToggleButton_Mute, 0, wxALIGN_CENTER | wxALL, kInset);
   pBoxSizer_MuteSolo->Add((2 * kInset), 0, 0); // horizontal spacer
   pBoxSizer_MuteSolo->Add(mToggleButton_Solo, 0, wxALIGN_CENTER | wxALL, kInset);
   pBoxSizer_MixerTrackCluster->Add(pBoxSizer_MuteSolo, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   // pan slider
   ctrlPos.x = (size.GetWidth() / 10);
   ctrlPos.y += MUTE_SOLO_HEIGHT + (4 * kInset);
   ctrlSize = wxSize((size.GetWidth() * 4 / 5), PAN_HEIGHT);
   /* i18n-hint: Title of the Pan slider, used to move the sound left or right stereoscopically */
   mSlider_Pan = new ASlider(this, ID_ASLIDER_PAN, _("Pan"), ctrlPos, ctrlSize, PAN_SLIDER);
   pBoxSizer_MixerTrackCluster->Add(mSlider_Pan, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   // gain slider & level meter
   ctrlPos.x = (2 * kInset);
   ctrlPos.y += PAN_HEIGHT + (4 * kInset);
   ctrlSize = wxSize((size.GetWidth() / 3), 
                     (size.GetHeight() - ctrlPos.y - (4 * kInset)));
   mSlider_Gain = 
      // ASlider doesn't do vertical.  
      /* i18n-hint: Title of the Gain slider, used to adjust the volume */
      //    new ASlider(this, ID_SLIDER_GAIN, _("Gain"), ctrlPos, ctrlSize, DB_SLIDER);
      new wxSlider(this, ID_SLIDER_GAIN, // wxWindow* parent, wxWindowID id, 
                     this->GetGainToSliderValue(),  // int value, 
                     kSliderMin, kSliderMax, // int minValue, int maxValue, 
                     ctrlPos, ctrlSize, // const wxPoint& point = wxDefaultPosition, const wxSize& size = wxDefaultSize, 
                     wxSL_VERTICAL | wxSL_AUTOTICKS | wxSUNKEN_BORDER); // long style = wxSL_HORIZONTAL, ...
   //vvv mSlider_Gain->SetBackgroundColour(trackColor);
   //vvv mSlider_Gain->SetBackgroundColour(wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW));
   mSlider_Gain->SetBackgroundColour(wxColour(192, 192, 192));

   ctrlPos.x += ctrlSize.GetWidth() + kInset;
   ctrlSize = wxSize(((size.GetWidth() / 2) - kInset), ctrlSize.GetHeight());
   mMeter = new Meter(this, -1, false, Meter::MixerTrackCluster, ctrlPos, ctrlSize, trackColor);
   mMeter->HandleLayout();
   //this->ResetMeter();

   wxBoxSizer* pBoxSizer_GainAndMeter = new wxBoxSizer(wxHORIZONTAL);
   pBoxSizer_GainAndMeter->Add(mSlider_Gain, 0, wxALIGN_CENTER | wxALL, kInset);
   pBoxSizer_GainAndMeter->Add(mMeter, 0, wxALIGN_CENTER | wxALL, kInset);
   pBoxSizer_MixerTrackCluster->Add(pBoxSizer_GainAndMeter, 0, wxALIGN_CENTER | wxALL, (2 * kInset));


   #if wxUSE_TOOLTIPS
      mStaticText_TrackName->SetToolTip(_T("Track Name"));
      mToggleButton_Mute->SetToolTip(_T("Mute"));
      mToggleButton_Solo->SetToolTip(_T("Solo"));
      // LWSlider already shows the value, so don't do this:   mSlider_Pan->SetToolTip(_T("Pan"));
      
      wxScrollEvent dummy;
      this->OnSliderScroll_Gain(dummy); // Set the tooltip to show the current value.

      mMeter->SetToolTip(_T("Level Meter"));
   #endif // wxUSE_TOOLTIPS


   this->SetSizer(pBoxSizer_MixerTrackCluster);
   //vvv Don't want to shrink to minimum for sizer. 
   //pBoxSizer_MixerTrackCluster->Fit(this);
   //pBoxSizer_MixerTrackCluster->SetSizeHints(this);
}

void MixerTrackCluster::ResetMeter()
{
   mMeter->Reset(mLeftTrack->GetRate(), true);
}

void MixerTrackCluster::UpdateName()
{
   mStaticText_TrackName->SetLabel(mLeftTrack->GetName()); 
}

void MixerTrackCluster::UpdateMute()
{
   mToggleButton_Mute->SetAlternate(mLeftTrack->GetSolo());
   if (mLeftTrack->GetMute())
      mToggleButton_Mute->PushDown(); 
   else 
      mToggleButton_Mute->PopUp(); 
}

void MixerTrackCluster::UpdateSolo()
{
   bool bValue = mLeftTrack->GetSolo();
   if (bValue)
      mToggleButton_Solo->PushDown(); 
   else 
      mToggleButton_Solo->PopUp(); 
   mMixerBoard->IncrementSoloCount(bValue ? 1 : -1);
   mToggleButton_Mute->SetAlternate(bValue);
}

void MixerTrackCluster::UpdatePan()
{
   mSlider_Pan->Set(mLeftTrack->GetPan());
}

void MixerTrackCluster::UpdateGain()
{
   mSlider_Gain->SetValue(this->GetGainToSliderValue());
}

void MixerTrackCluster::UpdateMeter(double t)
{
   if ((t < 0.0) || // bad time value
         ((mMixerBoard->HasSolo() || mLeftTrack->GetMute()) && !mLeftTrack->GetSolo()))
   {
      this->ResetMeter();
      return;
   }

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
   if (mLeftTrack->GetWaveDisplay(min, maxLeft, rmsLeft, where, kWidth, t, pps) && 
         ((mRightTrack == NULL) || 
            mRightTrack->GetWaveDisplay(min, maxRight, rmsRight, where, kWidth, t, pps)))
   {
      mMeter->UpdateDisplay(numChannels, kWidth, maxLeft, rmsLeft, maxRight, rmsRight);
   }
   delete[] min;
   delete[] maxLeft;
   delete[] rmsLeft;
   delete[] maxRight;
   delete[] rmsRight;
   delete[] where;
}

// private

int MixerTrackCluster::GetGainToSliderValue()
{
   int nSliderValue = 
      // Analog to LWSlider::Set() calc for DB_SLIDER. Negate because wxSlider has min at top.
      -(int)(20.0f * log10(mLeftTrack->GetGain()));
   if (nSliderValue < kSliderMin)
      nSliderValue = kSliderMin;
   if (nSliderValue > kSliderMax)
      nSliderValue = kSliderMax;
   return nSliderValue;
}

wxColour MixerTrackCluster::GetTrackColor()
{
   //vvv This doesn't work right when switching back and forth between two projects 
   // when one is branded and the other is not, because for some reason, OnActivate 
   // isn't always called, so gActiveProject isn't updated. 
   Branding* pBranding = mProject->GetBranding();
   if (pBranding && (pBranding->GetBrandColorScheme() == "UmixIt")) //vvv UmixIt 
      return AColor::GetTrackColor((void*)mLeftTrack);
   return wxColour(102, 255, 102); // same as Meter playback color
}

// event handlers
void MixerTrackCluster::OnButton_Mute(wxCommandEvent& event)
{
   // Shift-click mutes this track and unmutes other tracks. Tell mMixerBoard to handle it.
   if (mToggleButton_Mute->WasShiftDown())
   {
      mMixerBoard->UniquelyMuteOrSolo(mLeftTrack, false);
      return;
   }

   mLeftTrack->SetMute(mToggleButton_Mute->IsDown());
   mToggleButton_Mute->SetAlternate(mLeftTrack->GetSolo());

   // Update the TrackPanel correspondingly. 
   // Calling RedrawProject is inefficient relative to sending a msg to TrackPanel 
   // for a particular track and control, but not a real performance hit.
   mProject->RedrawProject();
}

void MixerTrackCluster::OnButton_Solo(wxCommandEvent& event)
{
   // Shift-click solos this track and unsolos other tracks. Tell mMixerBoard to handle it.
   if (mToggleButton_Solo->WasShiftDown())
   {
      mMixerBoard->UniquelyMuteOrSolo(mLeftTrack, true);
      return;
   }

   bool bValue = mToggleButton_Solo->IsDown();
   mLeftTrack->SetSolo(bValue);
   mMixerBoard->IncrementSoloCount(bValue ? 1 : -1);
   mToggleButton_Mute->SetAlternate(bValue);

   // Update the TrackPanel correspondingly. 
   // Calling RedrawProject is inefficient relative to sending a msg to TrackPanel 
   // for a particular track and control, but not a real performance hit.
   mProject->RedrawProject(); 
}

void MixerTrackCluster::OnSlider_Pan(wxCommandEvent& event)
{
   float fValue = mSlider_Pan->Get();
   mLeftTrack->SetPan(fValue);
   if (mRightTrack != NULL)
      mRightTrack->SetPan(fValue);
   mProject->TP_PushState(_("Moved pan slider"), _("Pan"), true /* consolidate */);

   // Update the TrackPanel correspondingly. 
   // Calling RedrawProject is inefficient relative to sending a msg to TrackPanel 
   // for a particular track and control, but not a real performance hit.
   mProject->RedrawProject();
}

void MixerTrackCluster::OnSlider_Gain(wxCommandEvent& event)
{
   // Analog to LWSlider::Set() calc for DB_SLIDER. Negate because wxSlider has min at top.
   // mSlider_Gain->GetValue() is in [-6,36]. wxSlider has min at top, so this is [-36dB,6dB]. 
   float fValue = pow(10.0f, -(float)(mSlider_Gain->GetValue()) / 20.0f); 
   mLeftTrack->SetGain(fValue);
   if (mRightTrack != NULL)
      mRightTrack->SetGain(fValue);
   mProject->TP_PushState(_("Moved gain slider"), _("Gain"), true /* consolidate */);

   // Update the TrackPanel correspondingly. 
   // Calling RedrawProject is inefficient relative to sending a msg to TrackPanel 
   // for a particular track and control, but not a real performance hit.
   mProject->RedrawProject();
}

void MixerTrackCluster::OnSliderScroll_Gain(wxScrollEvent& event)
{
   mSlider_Gain->SetToolTip(wxString::Format(_T("Gain=%ddB"), -mSlider_Gain->GetValue()));
}

void MixerTrackCluster::OnPaint(wxPaintEvent &evt)
{
   wxPaintDC dc(this);

   dc.BeginDrawing();

   wxRect bev = this->GetRect();
   bev.x = 0;
   bev.Inflate(-2, -2);
   AColor::Bevel(dc, true, bev);

   dc.EndDrawing();
}


// class MixerBoard

BEGIN_EVENT_TABLE(MixerBoard, wxFrame)
   EVT_CHAR(MixerBoard::OnKeyEvent)
   EVT_CLOSE(MixerBoard::OnCloseWindow)
END_EVENT_TABLE()

#define MIXER_TRACK_PANEL_MIN_WIDTH 96
#define DEFAULT_NUM_TRACKCLUSTERS 8 // Default to fitting 8 tracks.
const wxSize kDefaultSize = wxSize((DEFAULT_NUM_TRACKCLUSTERS * MIXER_TRACK_PANEL_MIN_WIDTH) + (2 * kInset), 480); 

MixerBoard::MixerBoard(AudacityProject* parent):
  wxFrame(parent, -1, _("Audacity Mixer Board - ") + parent->GetName(), 
            wxDefaultPosition,  kDefaultSize, 
            //vvv frame tool: wxCAPTION | wxSYSTEM_MENU | wxFRAME_TOOL_WINDOW | 
            //vvv No resize yet:   wxDEFAULT_FRAME_STYLE | 
            wxCAPTION | wxMINIMIZE_BOX | wxSYSTEM_MENU | 
               ((parent == NULL) ? 0x0 : wxFRAME_FLOAT_ON_PARENT))
{
   // public data members
   // mute & solo button images: Create once and store on MixerBoard for use in all MixerTrackClusters.
   mImageMuteUp = NULL;
   mImageMuteOver = NULL;
   mImageMuteDown = NULL;
   mImageMuteDownWhileSolo = NULL;
   mImageMuteDisabled = NULL;
   mImageSoloUp = NULL;
   mImageSoloOver = NULL;
   mImageSoloDown = NULL;
   mImageSoloDisabled = NULL;

   // private data members
   mMixerTrackClusterWidth = MIXER_TRACK_PANEL_MIN_WIDTH; 
   
   mMusicalInstrumentBitmaps = new wxBitmap((const char**)grand_xpm);
   wxMemoryDC dc;
   dc.SelectObject(*mMusicalInstrumentBitmaps);
   wxRect bev(0, 0, MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH - 1, MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH - 1);
   AColor::Bevel(dc, false, bev);

   mProject = parent;
   
   mScrolledWindow = 
      new wxScrolledWindow(this, -1, // wxWindow* parent, wxWindowID id = -1, 
                           this->GetClientAreaOrigin(), // const wxPoint& pos = wxDefaultPosition, 
                           this->GetClientSize(), // const wxSize& size = wxDefaultSize, 
                           wxHSCROLL); // long style = wxHSCROLL | wxVSCROLL, const wxString& name = "scrolledWindow")
   mScrolledWindow->SetBackgroundColour(wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW));
   mScrolledWindow->SetScrollRate(10, 0); // no vertical scroll
   //vvv Default width should be 
   //    this->GetClientSize().GetWidth()
   // but that gives no horizontal scrollbar, so then added track chops off bottom of 
   // existing MixerTrackClusters. So, until they know how to resize, always start with a horizontal scroll bar.
   mScrolledWindow->SetVirtualSize(this->GetClientSize().GetWidth() + (4 * kInset), 
                                    this->GetClientSize().GetHeight());

   mSoloCount = 0;
   mT = -1.0;

   // loads either the XPM or the windows resource, depending on the platform
   #if !defined(__WXMAC__) && !defined(__WXX11__)
      #ifdef __WXMSW__
         wxIcon ic(wxICON(AudacityLogo));
      #else
         wxIcon ic(wxICON(AudacityLogo48x48));
      #endif
      SetIcon(ic);
   #endif
}

MixerBoard::~MixerBoard()
{
   // public data members
   delete mImageMuteUp;
   delete mImageMuteOver;
   delete mImageMuteDown;
   delete mImageMuteDownWhileSolo;
   delete mImageMuteDisabled;
   delete mImageSoloUp;
   delete mImageSoloOver;
   delete mImageSoloDown;
   delete mImageSoloDisabled;

   // private data members
   delete mMusicalInstrumentBitmaps; //vvvvv Will become some storage for several.
}

void MixerBoard::AddTrackClusters() // Add clusters for any tracks we're not yet showing.
{
   //vvv Need to reorder when track order changes. This just makes sure all are visible, 
   // and relies on them staying in order.

   TrackList* pTrackList = mProject->GetTracks();
   if (pTrackList->IsEmpty())
      return;

   if (mImageMuteUp == NULL) 
      this->CreateMuteSoloImages();

   const int kClusterHeight = mScrolledWindow->GetClientSize().GetHeight();
   unsigned int count = mMixerTrackClusters.size();
   MixerTrackClusterHash::iterator iterHash;
   TrackListIterator iterTracks(pTrackList);
   MixerTrackCluster* pMixerTrackCluster = NULL;
   Track* pLeftTrack;
   Track* pRightTrack;

   pLeftTrack = iterTracks.First();
   while (pLeftTrack) {
      pRightTrack = NULL;
      if (pLeftTrack->GetLinked()) 
         pRightTrack = iterTracks.Next();

      if (pLeftTrack->GetKind() == Track::Wave) {
         iterHash = mMixerTrackClusters.find(pLeftTrack);
         if (iterHash == mMixerTrackClusters.end())
         {  // Not already showing it.
            wxPoint clusterPos(count * mMixerTrackClusterWidth, 0); // y should always be 0. 
            wxSize clusterSize(mMixerTrackClusterWidth, kClusterHeight);
            pMixerTrackCluster = 
               new MixerTrackCluster(mScrolledWindow, this, mProject, 
                                       (WaveTrack*)pLeftTrack, (WaveTrack*)pRightTrack, 
                                       clusterPos, clusterSize);
            if (pMixerTrackCluster)
            {
               mMixerTrackClusters[(void*)pLeftTrack] = pMixerTrackCluster;
               count++;
               this->IncrementSoloCount((int)(pLeftTrack->GetSolo()));
            }
         }
      }
      pLeftTrack = iterTracks.Next();
   }

   if (pMixerTrackCluster)
   {  // Added at least one MixerTrackCluster.
      int width; 
      int height;
      mScrolledWindow->GetVirtualSize(&width, &height);
      int reqVirtualWidth = count * mMixerTrackClusterWidth;
      if (reqVirtualWidth > width)
         width = reqVirtualWidth;
      mScrolledWindow->SetVirtualSize(width, this->GetClientSize().GetHeight());
      mScrolledWindow->Refresh(false); 
   }
}

wxBitmap* MixerBoard::GetMusicalInstrumentBitmap(const WaveTrack* pLeftTrack)
{
   return mMusicalInstrumentBitmaps; //vvvvv Will become some storage for several.
}

bool MixerBoard::HasSolo() 
{  
   return (mSoloCount > 0); 
}

void MixerBoard::IncrementSoloCount(int nIncrement /*= 1*/) 
{  
   mSoloCount += nIncrement; 
}

void MixerBoard::ResetMeters()
{
   MixerTrackClusterHash::iterator iterHash;
   MixerTrackCluster* pMixerTrackCluster;
   for (iterHash = mMixerTrackClusters.begin(); iterHash != mMixerTrackClusters.end(); ++iterHash)
   {
      pMixerTrackCluster = &(*iterHash->second);
      pMixerTrackCluster->ResetMeter();
   }
}

void MixerBoard::UniquelyMuteOrSolo(const WaveTrack* pTargetLeftTrack, bool bSolo)
{
   TrackList* pTrackList = mProject->GetTracks();
   wxASSERT(pTrackList && !pTrackList->IsEmpty());
   TrackListIterator iterTracks(pTrackList);
   Track* pLeftTrack = iterTracks.First();
   while (pLeftTrack) {
      if (pLeftTrack->GetKind() == Track::Wave) {
         if (bSolo)
            pLeftTrack->SetSolo(pLeftTrack == pTargetLeftTrack);
         else 
            pLeftTrack->SetMute(pLeftTrack == pTargetLeftTrack);
      }
      if (pLeftTrack->GetLinked()) 
         pLeftTrack = iterTracks.Next(); // Skip the right track.
      pLeftTrack = iterTracks.Next();
   }

   if (bSolo)
      this->UpdateSolo(); // Update all the MixerTrackCluster solo buttons.
   else 
      this->UpdateMute(); // Update all the MixerTrackCluster mute buttons.
   mProject->RedrawProject(); // Update all the TrackLabel mute buttons.
}

void MixerBoard::UpdateName(const WaveTrack* pLeftTrack)
{
   MixerTrackClusterHash::iterator iterHash = mMixerTrackClusters.find(pLeftTrack);
   if (iterHash != mMixerTrackClusters.end())
   {  // Found it.
      MixerTrackCluster* pMixerTrackCluster = &(*iterHash->second);
      pMixerTrackCluster->UpdateName();
   }
}

void MixerBoard::UpdateMute(const WaveTrack* pLeftTrack /*= NULL*/) // NULL means update for all tracks.
{
   MixerTrackClusterHash::iterator iterHash;
   MixerTrackCluster* pMixerTrackCluster;
   if (pLeftTrack == NULL) 
   {
      for (iterHash = mMixerTrackClusters.begin(); iterHash != mMixerTrackClusters.end(); ++iterHash)
      {
         pMixerTrackCluster = &(*iterHash->second);
         pMixerTrackCluster->UpdateMute();
      }
   }
   else 
   {
      iterHash = mMixerTrackClusters.find(pLeftTrack);
      if (iterHash != mMixerTrackClusters.end())
      {  // Found it.
         pMixerTrackCluster = &(*iterHash->second);
         pMixerTrackCluster->UpdateMute();
      }
   }
}

void MixerBoard::UpdateSolo(const WaveTrack* pLeftTrack /*= NULL*/) // NULL means update for all tracks.
{
   MixerTrackClusterHash::iterator iterHash;
   MixerTrackCluster* pMixerTrackCluster;
   if (pLeftTrack == NULL) 
   {
      for (iterHash = mMixerTrackClusters.begin(); iterHash != mMixerTrackClusters.end(); ++iterHash)
      {
         pMixerTrackCluster = &(*iterHash->second);
         pMixerTrackCluster->UpdateSolo();
      }
   }
   else 
   {
      iterHash = mMixerTrackClusters.find(pLeftTrack);
      if (iterHash != mMixerTrackClusters.end())
      {  // Found it.
         pMixerTrackCluster = &(*iterHash->second);
         pMixerTrackCluster->UpdateSolo();
      }
   }
}

void MixerBoard::UpdatePan(const WaveTrack* pLeftTrack)
{
   MixerTrackClusterHash::iterator iterHash = mMixerTrackClusters.find(pLeftTrack);
   if (iterHash != mMixerTrackClusters.end())
   {  // Found it.
      MixerTrackCluster* pMixerTrackCluster = &(*iterHash->second);
      pMixerTrackCluster->UpdatePan();
   }
}

void MixerBoard::UpdateGain(const WaveTrack* pLeftTrack)
{
   MixerTrackClusterHash::iterator iterHash = mMixerTrackClusters.find(pLeftTrack);
   if (iterHash != mMixerTrackClusters.end())
   {  // Found it.
      MixerTrackCluster* pMixerTrackCluster = &(*iterHash->second);
      pMixerTrackCluster->UpdateGain();
   }
}

void MixerBoard::UpdateMeters(double t)
{
   if (t == mT)
      return;

   mT = t;

   MixerTrackClusterHash::iterator iterHash;
   MixerTrackCluster* pMixerTrackCluster;
   for (iterHash = mMixerTrackClusters.begin(); iterHash != mMixerTrackClusters.end(); ++iterHash)
   {
      pMixerTrackCluster = &(*iterHash->second);
      pMixerTrackCluster->UpdateMeter(t);
   }
}

// private methods
void MixerBoard::CreateMuteSoloImages()
{
   // Much of this is taken TrackLabel::DrawMuteSolo 
   wxBitmap bitmap(MUTE_SOLO_HEIGHT, MUTE_SOLO_HEIGHT);
   wxMemoryDC dc;
   dc.SelectObject(bitmap);

   wxRect bev(0, 0, MUTE_SOLO_HEIGHT - 1, MUTE_SOLO_HEIGHT - 1);

   // mute button images
   AColor::Mute(&dc, false, true, false);
   dc.DrawRectangle(bev);

   wxString str = _("M"); /* i18n-hint: One-letter abbreviation for "Mute" */
   long textWidth, textHeight;
   AColor::SetLabelFont(dc);
   dc.GetTextExtent(str, &textWidth, &textHeight);
   wxCoord x = bev.x + (bev.width - textWidth) / 2;
   wxCoord y = bev.y + (bev.height - textHeight) / 2;
   dc.DrawText(str, x, y);

   AColor::Bevel(dc, true, bev);

   mImageMuteUp = new wxImage(bitmap.ConvertToImage());
   mImageMuteOver = new wxImage(bitmap.ConvertToImage()); // Same as up, for now.

   AColor::Mute(&dc, true, true, false);
   dc.DrawRectangle(bev);
   dc.DrawText(str, x, y);
   AColor::Bevel(dc, false, bev);
   mImageMuteDown = new wxImage(bitmap.ConvertToImage());

   AColor::Mute(&dc, true, true, true);
   dc.DrawRectangle(bev);
   dc.DrawText(str, x, y);
   AColor::Bevel(dc, false, bev);
   mImageMuteDownWhileSolo = new wxImage(bitmap.ConvertToImage());

   mImageMuteDisabled = new wxImage(MUTE_SOLO_HEIGHT, MUTE_SOLO_HEIGHT); // Leave empty because unused.


   // solo button images
   AColor::Solo(&dc, false, true);
   dc.DrawRectangle(bev);

   str = _("S");  /* i18n-hint: One-letter abbreviation for "Solo" */
   dc.GetTextExtent(str, &textWidth, &textHeight);
   x = bev.x + (bev.width - textWidth) / 2;
   y = bev.y + (bev.height - textHeight) / 2;
   dc.DrawText(str, x, y);

   AColor::Bevel(dc, true, bev);

   mImageSoloUp = new wxImage(bitmap.ConvertToImage());
   mImageSoloOver = new wxImage(bitmap.ConvertToImage()); // Same as up, for now.

   AColor::Solo(&dc, true, true);
   dc.DrawRectangle(bev);
   dc.DrawText(str, x, y);
   AColor::Bevel(dc, false, bev);
   mImageSoloDown = new wxImage(bitmap.ConvertToImage());

   mImageSoloDisabled = new wxImage(MUTE_SOLO_HEIGHT, MUTE_SOLO_HEIGHT); // Leave empty because unused.
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


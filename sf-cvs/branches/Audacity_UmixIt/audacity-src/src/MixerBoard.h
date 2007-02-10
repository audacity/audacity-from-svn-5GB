/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerBoard.h

  Vaughan Johnson, January 2007

**********************************************************************/

#ifndef __AUDACITY_MIXER_BOARD__
#define __AUDACITY_MIXER_BOARD__

#include <wx/frame.h>
#include <wx/hashmap.h>
#include <wx/image.h>
#include <wx/panel.h>
#include <wx/scrolwin.h>
#include <wx/slider.h>
#include <wx/statbmp.h>
#include <wx/stattext.h>

#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "widgets/Meter.h"

class AudacityProject;
class MixerBoard;
class WaveTrack;

class MixerTrackCluster : public wxPanel { 
   DECLARE_DYNAMIC_CLASS(MixerTrackCluster)

public:
   MixerTrackCluster(wxScrolledWindow* parent, 
                     MixerBoard* grandParent, AudacityProject* project, 
                     WaveTrack* pLeftTrack, WaveTrack* pRightTrack = NULL, 
                     const wxPoint& pos = wxDefaultPosition, 
                     const wxSize& size = wxDefaultSize);
   ~MixerTrackCluster() {};

   void ResetMeter();

   void UpdateName();
   void UpdateMute();
   void UpdateSolo();
   void UpdatePan();
   void UpdateGain();
   void UpdateMeter(double t);

private:
   int GetGainToSliderValue();
   wxColour GetTrackColor();

   // event handlers
   void OnButton_Mute(wxCommandEvent& event);
   void OnButton_Solo(wxCommandEvent& event);
   void OnSlider_Pan(wxCommandEvent& event);
   void OnSlider_Gain(wxCommandEvent& event);
   void OnSliderScroll_Gain(wxScrollEvent& event);

   void OnPaint(wxPaintEvent &evt);

private:
   MixerBoard* mMixerBoard;
   AudacityProject* mProject;

   WaveTrack* mLeftTrack;
   WaveTrack* mRightTrack;

   // controls
   wxStaticText* mStaticText_TrackName;
   wxStaticBitmap* mStaticBitmap_MusicalInstrument;
   AButton* mToggleButton_Mute;
   AButton* mToggleButton_Solo;
   ASlider* mSlider_Pan;
   wxSlider* mSlider_Gain; //vvv ASlider* mSlider_Gain;
   Meter* mMeter;

public:
   DECLARE_EVENT_TABLE()
};

WX_DECLARE_VOIDPTR_HASH_MAP(MixerTrackCluster*, MixerTrackClusterHash);

class MixerBoard : public wxFrame { 
public:
   MixerBoard(AudacityProject* parent);
   ~MixerBoard();

   void AddTrackClusters(); // Add clusters for any tracks we're not yet showing.
   //vvv Also need to remove clusters for any removed tracks. 

   wxBitmap* GetMusicalInstrumentBitmap(const WaveTrack* pLeftTrack);

   bool HasSolo();
   void IncrementSoloCount(int nIncrement = 1);

   void ResetMeters();

   void UniquelyMuteOrSolo(const WaveTrack* pTargetLeftTrack, bool bSolo);

   void UpdateName(const WaveTrack* pLeftTrack);
   void UpdateMute(const WaveTrack* pLeftTrack = NULL); // NULL means update for all tracks.
   void UpdateSolo(const WaveTrack* pLeftTrack = NULL); // NULL means update for all tracks.
   void UpdatePan(const WaveTrack* pLeftTrack);
   void UpdateGain(const WaveTrack* pLeftTrack);
   
   void UpdateMeters(double t);

private:
   void CreateMuteSoloImages();

   // event handlers
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));
   void OnKeyEvent(wxKeyEvent & event);

public:
   // mute & solo button images: Create once and store on MixerBoard for use in all MixerTrackClusters.
   wxImage* mImageMuteUp;
   wxImage* mImageMuteOver;
   wxImage* mImageMuteDown;
   wxImage* mImageMuteDownWhileSolo; // the one actually alternate image
   wxImage* mImageMuteDisabled;
   wxImage* mImageSoloUp;
   wxImage* mImageSoloOver;
   wxImage* mImageSoloDown;
   wxImage* mImageSoloDisabled;

private:
   MixerTrackClusterHash   mMixerTrackClusters; // Hash the panels based on the left WaveTrack* they're showing.
   int                     mMixerTrackClusterWidth;
   wxBitmap*               mMusicalInstrumentBitmaps; //vvvvv Will become some storage for several.
   AudacityProject*        mProject;
   wxScrolledWindow*       mScrolledWindow; // Holds the MixerTrackClusters and handles scrolling.
   unsigned int            mSoloCount;
   double                  mT;

public:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_MIXER_BOARD__

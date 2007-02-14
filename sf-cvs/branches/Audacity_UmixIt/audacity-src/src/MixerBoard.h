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
   void OnKeyEvent(wxKeyEvent & event);
   void OnPaint(wxPaintEvent &evt);

   void OnButton_Mute(wxCommandEvent& event);
   void OnButton_Solo(wxCommandEvent& event);
   void OnSlider_Pan(wxCommandEvent& event);
   void OnSlider_Gain(wxCommandEvent& event);
   void OnSliderScroll_Gain(wxScrollEvent& event);

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


class TrackList;

WX_DECLARE_VOIDPTR_HASH_MAP(MixerTrackCluster*, MixerTrackClusterHash);

class MusicalInstrument {
public:
   MusicalInstrument(wxBitmap* pBitmap, const wxString strXPMfilename);
   ~MusicalInstrument();

   wxBitmap*      mBitmap;
   wxArrayString  mKeywords;
};
WX_DECLARE_OBJARRAY(MusicalInstrument, MusicalInstrumentArray);

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
   void LoadMusicalInstruments();

   // event handlers
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));

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

   int mMuteSoloWidth;

private:
   MixerTrackClusterHash   mMixerTrackClusters; // Hash clusters based on the left WaveTrack* they're showing.
   int                     mMixerTrackClusterWidth;
   MusicalInstrumentArray  mMusicalInstruments; 
   AudacityProject*        mProject;
   wxScrolledWindow*       mScrolledWindow; // Holds the MixerTrackClusters and handles scrolling.
   unsigned int            mSoloCount;
   double                  mT;
   TrackList*              mTracks;

public:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_MIXER_BOARD__

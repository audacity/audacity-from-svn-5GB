/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerBoard.h

  Vaughan Johnson, January 2007

**********************************************************************/

#ifndef __AUDACITY_MIXER_BOARD__
#define __AUDACITY_MIXER_BOARD__

#include <wx/frame.h>
#include <wx/hashmap.h>
#include <wx/panel.h>

// controls
#include <wx/stattext.h>
#include <wx/tglbtn.h>
#include <wx/sizer.h>
#include <wx/slider.h>

#include "widgets/ASlider.h"
#include "widgets/Meter.h"

class AudacityProject;
class MixerBoard;
class WaveTrack;

class MixerTrackPanel : public wxPanel { 
   DECLARE_DYNAMIC_CLASS(MixerTrackPanel)

 public:
   MixerTrackPanel(MixerBoard* parent, AudacityProject* project, 
                     WaveTrack* pLeftTrack, WaveTrack* pRightTrack = NULL, 
                     const wxPoint& pos = wxDefaultPosition, 
                     const wxSize& size = wxDefaultSize);
   ~MixerTrackPanel() {};

   wxColour GetTrackColor();
   void Update(double t, bool bForce = false);

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
   wxToggleButton* mToggleButton_Mute;
   wxToggleButton* mToggleButton_Solo;
   ASlider* mSlider_Pan;
   wxSlider* mSlider_Gain; //vvv ASlider* mSlider_Gain;
   Meter* mMeter;

 public:
   DECLARE_EVENT_TABLE()
};

WX_DECLARE_VOIDPTR_HASH_MAP(MixerTrackPanel*, MixerTrackPanelHash);

class MixerBoard : public wxFrame { //vvv or wxScrolledWindow ?
 public:
   MixerBoard(AudacityProject* parent);
   ~MixerBoard();

   void Update(double t, bool bForce = false);

 private:
   // event handlers
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));
   void OnKeyEvent(wxKeyEvent & event);

   MixerTrackPanelHash  mMixerTrackPanels; // Hash the panels based on the left WaveTrack* they're showing.
   AudacityProject*     mProject;
   wxSize               mSize;
   double               mT;

 public:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_MIXER_BOARD__

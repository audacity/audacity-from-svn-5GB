/**********************************************************************

  Audacity: A Digital Audio Editor

  TranscriptionToolBar.h

  Shane T. Mueller

**********************************************************************/

#ifndef __AUDACITY_TRANSCRIPTION_TOOLBAR__
#define __AUDACITY_TRANSCRIPTION_TOOLBAR__

#include "ToolBar.h"
#include "Sequence.h" // for sampleCount

#include <wx/brush.h>
#include <wx/gdicmn.h>
#include <wx/pen.h>

class AButton;
class ASlider;
class VoiceKey;
class WaveTrack;
class TimeTrack;


//TTB 0-8 are button-ids, which also correspond to their
//position in mButtons.  9 & 10 are ids for sliders, which aren't
//in the button array.
enum
   {
      TTB_PlaySpeed,
      TTB_StartOn,
      TTB_EndOn,
      TTB_StartOff,
      TTB_EndOff,
      TTB_Calibrate,
      TTB_AutomateSelection,
      TTB_MakeLabel,
      TTB_SensitivitySlider,
      TTB_PlaySpeedSlider,
   };

#define TTBNumButtons 8

class TranscriptionToolBar:public ToolBar {
 public:
  
   TranscriptionToolBar(wxWindow * parent);
   TranscriptionToolBar(wxWindow * parent, wxWindowID id,
                        const wxPoint & pos, const wxSize & size);
   
   virtual ~ TranscriptionToolBar();
   

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnKeyEvent(wxKeyEvent & event);

   virtual void OnPlaySpeed(wxCommandEvent & event);
   virtual void OnSpeedSlider(wxCommandEvent& event);
   virtual void OnStartOn(wxCommandEvent &event);
   virtual void OnStartOff(wxCommandEvent &event);
   virtual void OnEndOn(wxCommandEvent &event);
   virtual void OnEndOff(wxCommandEvent &event);
   virtual void OnCalibrate(wxCommandEvent &event);
   virtual void OnMakeLabel(wxCommandEvent &event);
   virtual void OnAutomateSelection(wxCommandEvent &event);
   virtual double GetSensitivity();
   virtual void OnSensitivitySlider(wxCommandEvent& evt);

   virtual void EnableDisableButtons();
   virtual void PlaceButton(int i, wxWindow *pWind);

 private:
   void InitializeTranscriptionToolBar();
   void AddButton(const char **fg, const char **disabled, const char **alpha,
                  int id, const char *tooltip, const char *label);
   void MakeButtons();
   void GetSamples(WaveTrack *t, sampleCount *s0, sampleCount *slen);
   void SetButton(bool newstate, AButton* button); 
   
   AButton * mButtons[TTBNumButtons];
   int mxButtonPos;
   wxImage *upImage;
   wxImage *downImage;
   wxImage *hiliteImage;
   
   ASlider * mPlaySpeedSlider;
   double mPlaySpeed;
   ASlider * mSensitivitySlider;
   double mSensitivity;
   VoiceKey *vk;
   
   wxBrush mBackgroundBrush;
   wxPen mBackgroundPen;
   wxBitmap *mBackgroundBitmap;
   int mBackgroundWidth;
   int mBackgroundHeight;

   TimeTrack * mTimeTrack;
   
   
DECLARE_EVENT_TABLE()
};


#define COMMAND_LINE_LOG_TRACE    TRUE
#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: ToDo


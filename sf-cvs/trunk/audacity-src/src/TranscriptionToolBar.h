/**********************************************************************

  Audacity: A Digital Audio Editor

  AVoiceKeyPalette.h

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

enum
   {
      TTB_StartOn,
      TTB_EndOn,
      TTB_StartOff,
      TTB_EndOff,
      TTB_Calibrate,
      TTB_AutomateSelection,
      TTB_MakeLabel,
      TTB_SensitivitySlider
   };

#define TTBNumButtons 7

class TranscriptionToolBar:public ToolBar {
 public:
  
   TranscriptionToolBar(wxWindow * parent);
   TranscriptionToolBar(wxWindow * parent, wxWindowID id,
                        const wxPoint & pos, const wxSize & size);
   
   virtual ~ TranscriptionToolBar();
   

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnKeyEvent(wxKeyEvent & event);

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

 private:
   void InitializeTranscriptionToolBar();
   void AddButton(const char **fg, const char **disabled, const char **alpha,
                  int id, const char *tooltip);
   void MakeButtons();
   void GetSamples(WaveTrack *t, sampleCount *s0, sampleCount *slen);
   void SetButton(bool newstate, AButton* button); 
   
   AButton * mButtons[TTBNumButtons];
   int mButtonPos;
   wxImage *upImage;
   wxImage *downImage;
   wxImage *hiliteImage;
   
   int mDividers[100];
   int mNumDividers;

   ASlider * mSensitivitySlider;
   double mSensitivity;
   VoiceKey *vk;
   

   wxBrush mBackgroundBrush;
   wxPen mBackgroundPen;
   wxBitmap *mBackgroundBitmap;
   int mBackgroundWidth;
   int mBackgroundHeight;


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
// arch-tag:


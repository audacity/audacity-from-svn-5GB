/**********************************************************************

  Audacity: A Digital Audio Editor

  AVoiceKeyPalette.h

  Shane T. Mueller

  This class manages the miniframe window (aka floating window)
  which contains the tool selection (ibeam, envelope, move, zoom),
  the play/stop/record buttons, and the volume control.  All of the
  controls in this window were custom-written for Audacity - they
  are not native controls on any platform - however, it is intended
  that the images could be easily replaced to allow "skinning" or
  just customization to match the look and feel of each platform.

**********************************************************************/

#ifndef __AUDACITY_TRANSCRIPTION_TOOLBAR__
#define __AUDACITY_TRANSCRIPTION_TOOLBAR__

#include <wx/brush.h>
#include <wx/gdicmn.h>
#include <wx/pen.h>
#include <wx/minifram.h>
#include <wx/file.h>

#include "Prefs.h"
#include "Project.h"
//#include "VoiceKey.h"
#include "ToolBar.h"

class VoiceKey;
class AButton;
class ASlider;
class ToolBar;
class TranscriptionToolBarFrame;
 

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


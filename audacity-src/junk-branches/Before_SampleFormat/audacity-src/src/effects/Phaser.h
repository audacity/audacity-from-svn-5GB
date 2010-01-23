/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser

  Effect programming:
  Nasca Octavian Paul

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_PHASER__
#define __AUDACITY_EFFECT_PHASER__

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/spinbutt.h>
#include <wx/spinctrl.h>
#include <wx/intl.h>

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectPhaser:public Effect {

 public:
   EffectPhaser();

   virtual wxString GetEffectName() {
      return wxString(_("Phaser..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Applying Phaser"));
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();

/*
    Phaser Parameters        

 freq       - Phaser's LFO frequency
 startphase - Phaser's LFO startphase (radians), needed for stereo Phasers
 depth      - Phaser depth (0 - no depth, 255 - max depth)
 stages     - Phaser stages (recomanded from 2 to 16-24, and EVEN NUMBER)
 drywet     - Dry/wet mix, (0 - dry, 128 - dry=wet, 255 - wet)
 fb         - Phaser FeedBack (0 - no feedback, 100 = 100% Feedback,
                               -100 = -100% FeedBack)
*/

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len,
                   float startphase);
 
   float freq;
   float startphase;
   float fb;

   int depth;
   int stages;
   int drywet;
};

// Declare window functions

#define ID_TEXT 10000
#define ID_STAGES 10001
#define ID_DRYWET 10002
#define ID_FREQTEXT 10003
#define ID_FREQSLIDER 10004
#define ID_PHASETEXT 10005
#define ID_PHASESLIDER 10006
#define ID_DEPTHTEXT 10007
#define ID_DEPTHSLIDER 10008
#define ID_FEEDBACKTEXT 10009
#define ID_FEEDBACKSLIDER 10010
wxSizer *CreatePhaserDialog(wxPanel * parent, bool call_fit =
                            TRUE, bool set_sizer = TRUE);

// WDR: class declarations

//----------------------------------------------------------------------------
// PhaserDialog
//----------------------------------------------------------------------------

class PhaserDialog:public wxDialog {
 public:
   // constructors and destructors
   PhaserDialog(wxWindow * parent, wxWindowID id, const wxString & title,
                const wxPoint & pos = wxDefaultPosition,
                const wxSize & size = wxDefaultSize,
                long style = wxDEFAULT_DIALOG_STYLE);

   // WDR: method declarations for PhaserDialog
   wxSlider *GetFeedbackSlider() {
      return (wxSlider *) FindWindow(ID_FEEDBACKSLIDER);
   } wxSlider *GetDepthSlider() {
      return (wxSlider *) FindWindow(ID_DEPTHSLIDER);
   }
   wxSlider *GetPhaseSlider() {
      return (wxSlider *) FindWindow(ID_PHASESLIDER);
   }
   wxSlider *GetFreqSlider() {
      return (wxSlider *) FindWindow(ID_FREQSLIDER);
   }
   wxTextCtrl *GetFeedbackText() {
      return (wxTextCtrl *) FindWindow(ID_FEEDBACKTEXT);
   }
   wxTextCtrl *GetDepthText() {
      return (wxTextCtrl *) FindWindow(ID_DEPTHTEXT);
   }
   wxTextCtrl *GetPhaseText() {
      return (wxTextCtrl *) FindWindow(ID_PHASETEXT);
   }
   wxTextCtrl *GetFreqText() {
      return (wxTextCtrl *) FindWindow(ID_FREQTEXT);
   }
   wxSlider *GetDryWet() {
      return (wxSlider *) FindWindow(ID_DRYWET);
   }
   wxSpinCtrl *GetStages() {
      return (wxSpinCtrl *) FindWindow(ID_STAGES);
   }
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
   // WDR: member variable declarations for PhaserDialog

 private:
   // WDR: handler declarations for PhaserDialog
   void OnFeedbackSlider(wxCommandEvent & event);
   void OnDepthSlider(wxCommandEvent & event);
   void OnPhaseSlider(wxCommandEvent & event);
   void OnFreqSlider(wxCommandEvent & event);
   void OnFeedbackText(wxCommandEvent & event);
   void OnDepthText(wxCommandEvent & event);
   void OnPhaseText(wxCommandEvent & event);
   void OnFreqText(wxCommandEvent & event);
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()

 public:
   float freq;
   float startphase;
   float fb;

   int depth;
   int stages;
   int drywet;
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser

  Effect programming:
  Nasca Octavian Paul

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_PHASER__
#define __AUDACITY_EFFECT_PHASER__

#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>

class wxString;
class wxTextCtrl;
class wxSizer;
class wxSpinCtrl;

#include "SimpleMono.h"

class WaveTrack;

class EffectPhaser:public EffectSimpleMono {

 public:
   EffectPhaser();

   virtual wxString GetEffectName() {
      return wxString(_("Phaser..."));
   }
   
   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Phaser"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Phaser"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

 protected:
   virtual bool NewTrackSimpleMono();

   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);
   
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
 
   // parameters
   float freq;
   float startphase;
   float fb;
   int depth;
   int stages;
   int drywet;

   // state variables
   unsigned long skipcount;
   float old[24]; // must be as large as MAX_STAGES
   float gain;
   float fbout;
   float lfoskip;
   float phase;

friend class PhaserDialog;
};

// Declare window functions

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

//----------------------------------------------------------------------------
// PhaserDialog
//----------------------------------------------------------------------------

class PhaserDialog:public wxDialog {
 public:
   // constructors and destructors
   PhaserDialog(EffectPhaser * effect, 
						wxWindow * parent, wxWindowID id, const wxString & title,
						const wxPoint & pos = wxDefaultPosition,
						const wxSize & size = wxDefaultSize,
						long style = wxDEFAULT_DIALOG_STYLE);

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
   // WDR: handler declarations for PhaserDialog
   void OnFeedbackSlider(wxCommandEvent & event);
   void OnDepthSlider(wxCommandEvent & event);
   void OnPhaseSlider(wxCommandEvent & event);
   void OnFreqSlider(wxCommandEvent & event);
   void OnFeedbackText(wxCommandEvent & event);
   void OnDepthText(wxCommandEvent & event);
   void OnPhaseText(wxCommandEvent & event);
   void OnFreqText(wxCommandEvent & event);
   void OnPreview(wxCommandEvent &event);
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
	EffectPhaser * m_pEffect;

 public:
   float freq;
   float startphase;
   float fb;

   int depth;
   int stages;
   int drywet;

 private:
   DECLARE_EVENT_TABLE()
};

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
// arch-tag: 6520f24a-ad8d-4394-a0b1-fa339bb95875


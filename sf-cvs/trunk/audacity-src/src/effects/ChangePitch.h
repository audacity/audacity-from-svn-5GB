/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangePitch.h

  Vaughan Johnson, Dominic Mazzoni
  
  Change Pitch effect, that allows raising or lowering 
  the pitch without changing the tempo.

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGEPITCH__
#define __AUDACITY_EFFECT_CHANGEPITCH__

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>

#include <wx/intl.h>

#include "SoundTouchEffect.h"

#define ID_TEXT 10000

#define ID_TEXT_FROMPITCH 10001
#define ID_TEXT_TOPITCH 10002

#define ID_TEXT_SEMITONESCHANGE 10003	

#define ID_TEXT_FROMFREQUENCY 10004
#define ID_TEXT_TOFREQUENCY 10005

#define ID_TEXT_PERCENTCHANGE 10006
#define ID_SLIDER_PERCENTCHANGE 10007


class WaveTrack;
class wxString;


class EffectChangePitch:public EffectSoundTouch {

 public:
   EffectChangePitch();

   virtual wxString GetEffectName() {
      return wxString(_("Change Pitch..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Changing Pitch"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool Init();

   virtual bool PromptUser();
   
 private:
   unsigned int	m_FromPitch;			// user-set pitch. Zero means not yet set.
   unsigned int	m_ToPitch;				// Zero value means not yet set.

	double			m_SemitonesChange;	// how many semitones to change pitch
	
   unsigned int	m_FromFrequency;		// starting frequency of selection
   unsigned int	m_ToFrequency;			// target frequency of selection

   double			m_PercentChange;		// percent change to apply to pitch
};

//----------------------------------------------------------------------------
// ChangePitchDialog
//----------------------------------------------------------------------------

class ChangePitchDialog:public wxDialog {
 public:
   ChangePitchDialog(wxWindow * parent,
							wxWindowID id, 
							const wxString & title, 
							const wxPoint & pos = wxDefaultPosition, 
							const wxSize & size = wxDefaultSize, 
							long style = wxDEFAULT_DIALOG_STYLE);

   // accessors
   wxTextCtrl * GetTextCtrl_FromPitch() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_FROMPITCH);
   }
   wxTextCtrl * GetTextCtrl_ToPitch() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_TOPITCH);
   }
   
   wxTextCtrl * GetTextCtrl_SemitonesChange() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_SEMITONESCHANGE);
   }

	wxTextCtrl * GetTextCtrl_FromFrequency() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_FROMFREQUENCY);
   }
   wxTextCtrl * GetTextCtrl_ToFrequency() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_TOFREQUENCY);
   }
   
	wxTextCtrl * GetTextCtrl_PercentChange() {
      return (wxTextCtrl *) FindWindow(ID_TEXT_PERCENTCHANGE);
   }
   wxSlider * GetSlider_PercentChange() {
      return (wxSlider *) FindWindow(ID_SLIDER_PERCENTCHANGE);
   }
   
	wxButton * GetOK() {
      return (wxButton *) FindWindow(wxID_OK);
   }

   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
	// handlers
   void OnText_FromPitch(wxCommandEvent & event); 
   void OnText_ToPitch(wxCommandEvent & event); 

   void OnText_SemitonesChange(wxCommandEvent & event); 
   
	void OnText_FromFrequency(wxCommandEvent & event); 
   void OnText_ToFrequency(wxCommandEvent & event); 

	void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);

   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

	// helper fns
	void Update_Text_ToPitch(); // Use m_FromPitch & m_PercentChange to set new m_ToPitch & control.
	void Update_Text_SemitonesChange(); // Use m_PercentChange to set new m_SemitonesChange & control.
	void Update_Text_ToFrequency(); // Use m_FromFrequency & m_PercentChange to set new m_ToFrequency & control.

	void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.

 private:
   bool m_bLoopDetect;
   DECLARE_EVENT_TABLE()

 public:
   unsigned int	m_FromPitch;			// user-set pitch. Zero means not yet set.
   unsigned int	m_ToPitch;				// Zero value means not yet set.

	double			m_SemitonesChange;	// how many semitones to change pitch
	
   unsigned int	m_FromFrequency;		// starting frequency of selection
   unsigned int	m_ToFrequency;			// target frequency of selection

   double			m_PercentChange;		// percent change to apply to pitch
													// Slider is (-100, 200], but textCtrls can set higher.
};


#endif // __AUDACITY_EFFECT_CHANGEPITCH__

#endif // USE_SOUNDTOUCH

/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangePitch.h

  Vaughan Johnson, Dominic Mazzoni
  
  Change Pitch effect provides raising or lowering 
  the pitch without changing the tempo.

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGEPITCH__
#define __AUDACITY_EFFECT_CHANGEPITCH__

// wxWindows controls 
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/radiobox.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include <wx/intl.h>

#include "SoundTouchEffect.h"

// Make these first 3 ID_'s match ChangeSpeed and ChangeTempo to avoid compiler warnings.
#define ID_TEXT 10000

#define ID_TEXT_PERCENTCHANGE 10001
#define ID_SLIDER_PERCENTCHANGE 10002

#define ID_CHOICE_FROMPITCH 10003
#define ID_RADIOBOX_PITCHUPDOWN 10004
#define ID_CHOICE_TOPITCH 10005

#define ID_TEXT_SEMITONESCHANGE 10006

#define ID_TEXT_FROMFREQUENCY 10007
#define ID_TEXT_TOFREQUENCY 10008



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

	// DeduceFrequency is Dominic's extremely cool trick (Vaughan sez so!) 
	// to set deduce m_FromFrequency from the samples at the beginning of 
	// the selection. Then we set some other params accordingly.
	virtual void DeduceFrequencies(); 

   virtual bool PromptUser();
   
 private:
   int				m_FromPitchIndex;		// pitch index, per PitchIndex
	bool				m_bWantPitchDown;		// up to ToPitchNum if false (default), else down
   int				m_ToPitchIndex;		// pitch index, per PitchIndex

	double			m_SemitonesChange;	// how many semitones to change pitch
	
   float				m_FromFrequency;		// starting frequency of selection
   float				m_ToFrequency;			// target frequency of selection

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
   wxChoice * GetChoice_FromPitch() { 
      return (wxChoice *) FindWindow(ID_CHOICE_FROMPITCH);
   }
	wxRadioBox * GetRadioBox_PitchUpDown() {
		return (wxRadioBox *) FindWindow(ID_RADIOBOX_PITCHUPDOWN);
	}
   wxChoice * GetChoice_ToPitch() { 
      return (wxChoice *) FindWindow(ID_CHOICE_TOPITCH);
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
	// calculations
	void Calc_ToFrequency(); // Update m_ToFrequency from m_FromFrequency & m_PercentChange.
	void Calc_ToPitchIndex(); // Update m_ToPitchIndex from new m_SemitonesChange.
	void Calc_SemitonesChange_fromPitches(); // Update m_SemitonesChange from new m_*PitchIndex-es.
	void Calc_SemitonesChange_fromPercentChange(); // Update m_SemitonesChange from new m_PercentChange.
	void Calc_PercentChange(); // Update m_PercentChange based on new m_SemitonesChange.

	// handlers
   void OnChoice_FromPitch(wxCommandEvent & event); 
	void OnRadioBox_PitchUpDown(wxCommandEvent & event);
   void OnChoice_ToPitch(wxCommandEvent & event); 

   void OnText_SemitonesChange(wxCommandEvent & event); 
   
	void OnText_FromFrequency(wxCommandEvent & event); 
   void OnText_ToFrequency(wxCommandEvent & event); 

	void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);

   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

	// helper fns for controls
	void Update_RadioBox_PitchUpDown();
	void Update_Choice_ToPitch(); 

	void Update_Text_SemitonesChange(); 
	
	void Update_Text_ToFrequency(); 

	void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.

 private:
   bool m_bLoopDetect;
   DECLARE_EVENT_TABLE()

 public:
   int				m_FromPitchIndex;		// pitch index, per PitchIndex
	bool				m_bWantPitchDown;		// up to ToPitchNum if false (default), else down
   int				m_ToPitchIndex;		// pitch index, per PitchIndex

	double			m_SemitonesChange;	// how many semitones to change pitch
	
   float				m_FromFrequency;		// starting frequency of selection
   float				m_ToFrequency;			// target frequency of selection

   double			m_PercentChange;		// percent change to apply to pitch
													// Slider is (-100, 200], but textCtrls can set higher.
};


#endif // __AUDACITY_EFFECT_CHANGEPITCH__

#endif // USE_SOUNDTOUCH

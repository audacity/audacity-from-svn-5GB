/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.h

  Vaughan Johnson, Dominic Mazzoni
  
  Change Tempo effect, that allows speeding up or 
  slowing down tempo without changing pitch.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGETEMPO__
#define __AUDACITY_EFFECT_CHANGETEMPO__

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>

#include <wx/intl.h>

#include "SimpleMono.h"

#define ID_TEXT 10000
#define ID_TEXT_PERCENTCHANGE 10001
#define ID_SLIDER_PERCENTCHANGE 10002
#define ID_TEXT_FROMBPM 10003
#define ID_TEXT_TOBPM 10004
#define ID_TEXT_FROMLENGTH 10005
#define ID_TEXT_TOLENGTH 10006


class WaveTrack;
class wxString;


class EffectChangeTempo:public EffectSimpleMono {

 public:
   EffectChangeTempo();

   virtual wxString GetEffectName() {
      return wxString(_("Change Tempo..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Changing Tempo"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool Init();

   virtual bool PromptUser();
   
 protected:
   virtual bool ProcessSimpleMono(float * buffer, sampleCount len);

 private:
   double			m_PercentChange;	// percent change to apply to tempo
												// -100% is meaningless, but sky's the upper limit
   unsigned int	m_FromBPM;			// user-set beats-per-minute. Zero means not yet set.
   unsigned int	m_ToBPM;				// Zero value means not yet set.
   double			m_FromLength;		// starting length of selection
   double			m_ToLength;			// target length of selection
};

//----------------------------------------------------------------------------
// ChangeTempoDialog
//----------------------------------------------------------------------------

class ChangeTempoDialog:public wxDialog {
 public:
   ChangeTempoDialog(wxWindow * parent,
							wxWindowID id, 
							const wxString & title, 
							const wxPoint & pos = wxDefaultPosition, 
							const wxSize & size = wxDefaultSize, 
							long style = wxDEFAULT_DIALOG_STYLE);

   // accessors
   wxTextCtrl * GetTextCtrl_PercentChange() {
      return (wxTextCtrl *) FindWindow(ID_TEXT_PERCENTCHANGE);
   }
   wxSlider * GetSlider_PercentChange() {
      return (wxSlider *) FindWindow(ID_SLIDER_PERCENTCHANGE);
   }
   wxTextCtrl * GetTextCtrl_FromBPM() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_FROMBPM);
   }
   wxTextCtrl * GetTextCtrl_ToBPM() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_TOBPM);
   }
   wxTextCtrl * GetTextCtrl_FromLength() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_FROMLENGTH);
   }
   wxTextCtrl * GetTextCtrl_ToLength() { 
      return (wxTextCtrl *) FindWindow(ID_TEXT_TOLENGTH);
   }
   wxButton * GetOK() {
      return (wxButton *) FindWindow(wxID_OK);
   }

   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
	// handlers
	void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);
   void OnText_FromBPM(wxCommandEvent & event); 
   void OnText_ToBPM(wxCommandEvent & event); 
   void OnText_ToLength(wxCommandEvent & event); 

   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

	// helper fns
	void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
	void Update_Text_ToBPM(); // Use m_FromBPM & m_PercentChange to set new m_ToBPM & control.
	void Update_Text_ToLength(); // Use m_FromLength & m_PercentChange to set new m_ToLength & control.

 private:
   bool m_bLoopDetect;
   DECLARE_EVENT_TABLE()

 public:
   double			m_PercentChange;	// percent change to apply to tempo
												// -100% is meaningless, but sky's the upper limit.
												// Slider is (-100, 200], but textCtrls can set higher.
   unsigned int	m_FromBPM;			// user-set beats-per-minute. Zero means not yet set.
   unsigned int	m_ToBPM;				// Zero value means not yet set.
   double			m_FromLength;		// starting length of selection
   double			m_ToLength;			// target length of selection
};


#endif // __AUDACITY_EFFECT_CHANGETEMPO__

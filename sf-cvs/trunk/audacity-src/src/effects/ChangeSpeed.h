/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.h

  Vaughan Johnson, Dominic Mazzoni
  
  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGESPEED__
#define __AUDACITY_EFFECT_CHANGESPEED__

#include <wx/button.h>
#include <wx/choice.h>
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
#define ID_CHOICE_FROMVINYL 10003
#define ID_CHOICE_TOVINYL 10004


class WaveTrack;
class wxString;


class EffectChangeSpeed:public EffectSimpleMono {

 public:
   EffectChangeSpeed();

   virtual wxString GetEffectName() {
      return wxString(_("Change Speed..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Changing Speed"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool Init();

   virtual bool PromptUser();
   
 protected:
   virtual bool ProcessSimpleMono(float * buffer, sampleCount len);

 private:
   double	m_PercentChange;	// percent change to apply to tempo
										// -100% is meaningless, but sky's the upper limit.
										// Slider is (-100, 200], but textCtrls can set higher.
   int		m_FromVinyl;		// from standard vinyl speed (RPM)
   int		m_ToVinyl;			// to standard vinyl speed (RPM)
};

//----------------------------------------------------------------------------
// ChangeSpeedDialog
//----------------------------------------------------------------------------

class ChangeSpeedDialog:public wxDialog {
 public:
   ChangeSpeedDialog(wxWindow * parent,
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
   wxChoice * GetChoice_FromVinyl() { 
      return (wxChoice *) FindWindow(ID_CHOICE_FROMVINYL);
   }
   wxChoice * GetChoice_ToVinyl() { 
      return (wxChoice *) FindWindow(ID_CHOICE_TOVINYL);
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
   void OnChoice_FromVinyl(wxCommandEvent & event); 
   void OnChoice_ToVinyl(wxCommandEvent & event); 

   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

	// helper fns
	void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
	void Update_Vinyl(); // Update Vinyl controls for new percent change.
	void Update_PercentChange(); // Update percent change controls for new Vinyl values.

 private:
	bool m_bLoopDetect;
   DECLARE_EVENT_TABLE()

 public:
   double	m_PercentChange;	// percent change to apply to tempo
										// -100% is meaningless, but sky's the upper limit.
										// Slider is (-100, 200], but textCtrls can set higher.
   int		m_FromVinyl;		// from standard vinyl speed (RPM)
   int		m_ToVinyl;			// to standard vinyl speed (RPM)
};


#endif // __AUDACITY_EFFECT_CHANGESPEED__

/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.h

  Dominic Mazzoni
  
  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AMPLIFY__
#define __AUDACITY_EFFECT_AMPLIFY__

#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>

class wxCheckBox;
class wxButton;
class wxStaticText;
class wxTextCtrl;

// Declare window functions

#define ID_AMP_TEXT 10001
#define ID_PEAK_TEXT 10002
#define ID_AMP_SLIDER 10003
#define ID_CLIP_CHECKBOX 10004

class wxString;

#include "SimpleMono.h"

class WaveTrack;

class EffectAmplify:public EffectSimpleMono {

 public:
   EffectAmplify();

   virtual wxString GetEffectName() {
      return wxString(_("Amplify..."));
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Amplify"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Amplifying"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool Init();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   
 protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

 private:
   float ratio;
   float peak;


friend class AmplifyDialog;
};

//----------------------------------------------------------------------------
// AmplifyDialog
//----------------------------------------------------------------------------

class AmplifyDialog:public wxDialog {
 public:
   // constructors and destructors
   AmplifyDialog(EffectAmplify * effect, 
						wxWindow * parent, wxWindowID id,
						const wxString & title, 
						const wxPoint & pos = wxDefaultPosition, 
						const wxSize & size = wxDefaultSize, 
						long style = wxDEFAULT_DIALOG_STYLE);

   // control accessors
   wxSlider *GetAmpSlider() {
      return (wxSlider *) FindWindow(ID_AMP_SLIDER);
   }
   wxTextCtrl *GetAmpText() {
      return (wxTextCtrl *) FindWindow(ID_AMP_TEXT);
   }
   wxTextCtrl *GetPeakText() {
      return (wxTextCtrl *) FindWindow(ID_PEAK_TEXT);
   }
   wxCheckBox *GetClipCheckBox() {
      return (wxCheckBox *) FindWindow(ID_CLIP_CHECKBOX);
   }
   wxButton *GetOK() {
      return (wxButton *) FindWindow(wxID_OK);
   }
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
	// handlers
   void OnAmpText(wxCommandEvent & event);
   void OnPeakText(wxCommandEvent & event);
   void OnAmpSlider(wxCommandEvent & event);
   void OnClipCheckBox(wxCommandEvent & event);
   void OnPreview( wxCommandEvent &event );
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

   void CheckClip();

 private:
   bool mLoopDetect;
	EffectAmplify * m_pEffect;
   DECLARE_EVENT_TABLE()

 public:
   float ratio;
   float peak;
   bool noclip;
};

#endif // __AUDACITY_EFFECT_AMPLIFY__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: c05d7383-e7cd-410e-b7b8-f45f47c9e283


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

#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/intl.h>

// Declare window functions

#define ID_TEXT 10000
#define ID_AMP_TEXT 10001
#define ID_PEAK_TEXT 10002
#define ID_AMP_SLIDER 10003
#define ID_CLIP_CHECKBOX 10004

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectAmplify:public Effect {

 public:
   EffectAmplify();

   virtual wxString GetEffectName() {
      return wxString(_("Amplify..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Amplifying"));
   }
   
   virtual bool Init();

   virtual bool PromptUser();
   
   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

 private:
   float ratio;
   float peak;
};

//----------------------------------------------------------------------------
// AmplifyDialog
//----------------------------------------------------------------------------

wxSizer *MakeAmplifyDialog(wxPanel * parent, bool call_fit,
                           bool set_sizer);

class AmplifyDialog:public wxDialog {
 public:
   // constructors and destructors
   AmplifyDialog(wxWindow * parent, wxWindowID id,
                 const wxString & title, const wxPoint & pos =
                 wxDefaultPosition, const wxSize & size =
                 wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE);

   // WDR: method declarations for BassBoostDialog
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
   // WDR: member variable declarations for BassBoostDialog

 private:
   // WDR: handler declarations for BassBoostDialog
   void OnAmpText(wxCommandEvent & event);
   void OnPeakText(wxCommandEvent & event);
   void OnAmpSlider(wxCommandEvent & event);
   void OnClipCheckBox(wxCommandEvent & event);
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

   void CheckClip();

 private:
   bool mLoopDetect;
   DECLARE_EVENT_TABLE()

 public:
   float ratio;
   float peak;
};


#endif

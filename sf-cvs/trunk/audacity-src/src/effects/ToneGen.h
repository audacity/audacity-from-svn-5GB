/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly
  
  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include <math.h>
#include "../FFT.h"

#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/intl.h>

#include "SimpleMono.h"

#define __UNINITIALIZED__ (-1)

class WaveTrack;

class EffectToneGen:public EffectSimpleMono {

 public:
   EffectToneGen();


   virtual wxString GetEffectName() {
      return wxString(_("Tone Generator..."));
   } virtual wxString GetEffectAction() {
      return wxString(_("Generating Tone"));
   }

   virtual bool PromptUser();

 protected:
   bool NewTrackSimpleMono(int count);

   bool ProcessSimpleMono(float *buffer,
                          sampleCount len, double samplerate);
 private:
   int waveform;
   float frequency;
   float amplitude;
   bool mix;
   int mSample;
};

// Declare window functions

#define ID_TEXT 10000
#define ID_WAVEFORM 10001
#define ID_AMPTEXT 10002
#define ID_FREQTEXT 10003
#define ID_MIX 10004
wxSizer *CreateToneGenDialog(wxWindow * parent, bool call_fit =
                             TRUE, bool set_sizer = TRUE);

// WDR: class declarations

//----------------------------------------------------------------------------
// ToneGenDialog
//----------------------------------------------------------------------------

class ToneGenDialog:public wxDialog {
 public:
   // constructors and destructors
   ToneGenDialog(wxWindow * parent, wxWindowID id, const wxString & title,
                 const wxPoint & pos = wxDefaultPosition,
                 const wxSize & size = wxDefaultSize,
                 long style = wxDEFAULT_DIALOG_STYLE);

   wxSizer *MakeToneGenDialog(wxWindow * parent, bool call_fit = TRUE,
                              bool set_sizer = TRUE);

   wxTextCtrl *GetFreqText() {
      return (wxTextCtrl *) FindWindow(ID_FREQTEXT);
   } wxTextCtrl *GetAmpText() {
      return (wxTextCtrl *) FindWindow(ID_AMPTEXT);
   }
   wxChoice *GetWaveformChoice() {
      return (wxChoice *) FindWindow(ID_WAVEFORM);
   }
   wxCheckBox *GetMixChoice() {
      return (wxCheckBox *) FindWindow(ID_MIX);
   }
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   wxButton *mCreateToneButton;

 private:
   // WDR: handler declarations for FilterDialog
   void OnCreateTone(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()

 public:
   int waveform;
   float frequency;
   float amplitude;
   bool mix;
};

#endif

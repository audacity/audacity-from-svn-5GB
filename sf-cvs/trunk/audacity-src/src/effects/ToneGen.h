/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly
  
  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include "SimpleMono.h"
#include "../FFT.h"

#include <wx/dialog.h>
#include <math.h>

class wxString;
class wxButton;
class wxCheckBox;
class wxChoice;
class wxSizer;
class wxTextCtrl;

#define __UNINITIALIZED__ (-1)

class WaveTrack;

class EffectToneGen:public Effect {

 public:
   EffectToneGen();

   virtual wxString GetEffectName() {
      return wxString(_("Tone..."));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating Tone"));
   }

   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | INSERT_EFFECT;
   }

   virtual bool PromptUser();

   virtual bool Process();

 protected:
   virtual bool MakeTone(float *buffer, sampleCount len);

 private:
   int waveform;
   float frequency;
   float amplitude;
   bool mix;
   int mSample;
   double mCurRate;
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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: b0b0e585-3416-4d56-aaa1-0efaffbc44f7


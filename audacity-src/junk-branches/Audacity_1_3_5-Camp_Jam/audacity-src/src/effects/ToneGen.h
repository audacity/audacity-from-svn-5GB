/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly

  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include "Effect.h"
#include "../widgets/TimeTextCtrl.h"
#include "../Experimental.h"

#include <wx/dialog.h>

class wxString;
class wxChoice;
class wxTextCtrl;
class ShuttleGui;

#define __UNINITIALIZED__ (-1)

class WaveTrack;


class EffectToneGen:public Effect {

 public:
   EffectToneGen();
   // A 'Chirp' is a tone that changes in frequency.
   EffectToneGen & EnableForChirps(){mbChirp=true;return *this;};

   virtual wxString GetEffectName() {
      return wxString(mbChirp? _("Chirp..."):_("Tone..."));
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(mbChirp ? wxT("Chirp") : wxT("Tone"));
   }

   virtual wxString GetEffectAction() {
      return wxString(mbChirp? _("Generating Chirp") : _("Generating Tone"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription(); 

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | INSERT_EFFECT;
   }

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Process();

 protected:
   virtual bool MakeTone(float *buffer, sampleCount len);

 private:

   longSampleCount numSamples;
   bool mbChirp;
   bool mbLogInterpolation;

   double mPositionInCycles;

   // If we made these static variables, 
   // Tone and Chirp would share the same parameters.
   int waveform;
   float frequency[2];
   float amplitude[2];
   double length;
   float logFrequency[2];
   double mCurRate;
#ifdef LOGARITHMIC_TONE_CHIRP
   int interpolation;
#endif

   // mSample is an external placeholder to remember the last "buffer"
   // position so we use it to reinitialize from where we left
   int mSample;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// ToneGenDialog
//----------------------------------------------------------------------------

class ToneGenDialog:public EffectDialog {
 public:
   // constructors and destructors
   ToneGenDialog(wxWindow * parent, const wxString & title);

   // WDR: method declarations
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   void PopulateOrExchangeStandard(ShuttleGui & S);
   void PopulateOrExchangeExtended(ShuttleGui & S);
   void OnTimeCtrlUpdate(wxCommandEvent & event);
   DECLARE_EVENT_TABLE()

 public:
   bool mbChirp;
   wxArrayString *waveforms;
   int waveform;
   double frequency[2];
   double amplitude[2];
   double length;
   bool isSelection;
#ifdef LOGARITHMIC_TONE_CHIRP
   int interpolation;
   wxArrayString *interpolations;
#endif

 private:
   TimeTextCtrl *mToneDurationT;
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


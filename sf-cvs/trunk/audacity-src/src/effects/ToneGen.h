/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly
  
  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include "Effect.h"

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

   virtual wxString GetEffectName() {
      return wxString(_("&Tone..."));
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
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Process();

 protected:
   virtual bool MakeTone(float *buffer, sampleCount len);

 private:
   int waveform;
   float frequency;
   float amplitude;
   double length;
   int mSample;
   double mCurRate;
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
   // WDR: handler declarations

   wxChoice *mWaveform;
   wxTextCtrl *mFreq;
   wxTextCtrl *mAmp;
   wxTextCtrl *mLength;

 public:
   wxArrayString *waveforms;
   int waveform;
   double frequency;
   double amplitude;
   double length;
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


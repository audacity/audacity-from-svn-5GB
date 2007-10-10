/**********************************************************************

  Audacity: A Digital Audio Editor

  DtmfGen.h

  Salvo Ventura
  Dec 2006

  An effect for the "Generator" menu to generate DTMF tones

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DTMF__
#define __AUDACITY_EFFECT_DTMF__

#include <wx/defs.h>
#include <wx/intl.h>
#include "../widgets/TimeTextCtrl.h"

#include "Effect.h"

class wxString;
class wxChoice;
class wxTextCtrl;
class ShuttleGui;
class wxSizer;

#define __UNINITIALIZED__ (-1)

class WaveTrack;


class EffectDtmf:public Effect {

 public:
   EffectDtmf() {
   }

   virtual wxString GetEffectName() {
      return wxString(_("DTMF Tones..."));
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("DTMFTone"));
   }

   virtual wxString GetEffectDescription() {
      return wxString::Format(_("Applied effect: Generate DTMF tones, %.6lf seconds"), dtmfDuration);
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating DTMF tones"));
   }

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | INSERT_EFFECT;
   }

   virtual bool PromptUser();
   virtual bool Process();
   virtual bool TransferParameters( Shuttle & shuttle );

 private:
   longSampleCount numSamplesSequence, numSamplesTone, numSamplesSilence;

   wxString dtmfString;       // dtmf tone string
   int    dtmfNTones;         // total number of tones to generate
   double dtmfTone;           // duration of a single tone in ms
   double dtmfSilence;        // duration of silence between tones in ms
   double dtmfDuration;       // duration of the whole dtmf tone sequence in seconds
   double dtmfDutyCycle;      // ratio of dtmfTone/(dtmfTone+dtmfSilence)
   double dtmfAmplitude;      // amplitude of dtmf tone sequence, restricted to (0-1)

 protected:
   virtual bool MakeDtmfTone(float *buffer, sampleCount len, float fs,
                             wxChar tone, sampleCount last,
                             longSampleCount total, float amplitude);

 // friendship ...
 friend class DtmfDialog;

};

//----------------------------------------------------------------------------
// DtmfDialog
//----------------------------------------------------------------------------

// Declare window functions

class DtmfDialog:public EffectDialog {
 public:
   // constructors and destructors
   DtmfDialog(wxWindow * parent, const wxString & title);

   // method declarations
   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   void OnDtmfStringText(wxCommandEvent & event);
   void OnDtmfDurationText(wxCommandEvent & event);
   void OnDutyCycleSlider(wxCommandEvent & event);
   void OnTimeCtrlUpdate(wxCommandEvent & event);
   void Recalculate(void);

 private:
   wxSlider   *mDtmfDutyS;
   wxTextCtrl *mDtmfStringT;
   TimeTextCtrl *mDtmfDurationT;
   wxStaticText *mDtmfToneT;
   wxStaticText *mDtmfSilenceT;
   wxStaticText *mDtmfDutyT;

   DECLARE_EVENT_TABLE()

 public:
   wxString dString;       // dtmf tone string
   int    dNTones;         // total number of tones to generate
   double dTone;           // duration of a single tone
   double dSilence;        // duration of silence between tones
   double dDuration;       // duration of the whole dtmf tone sequence
   double dDutyCycle;      // ratio of dTone/(dTone+dSilence)
   double dAmplitude;      // amplitude of dtmf tone sequence, restricted to (0-1)
   bool   dIsSelection;    // true if duration comes from selection

};

#endif

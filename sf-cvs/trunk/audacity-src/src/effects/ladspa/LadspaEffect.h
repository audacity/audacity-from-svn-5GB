/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.h

  Dominic Mazzoni

**********************************************************************/

class wxSlider;
class wxStaticText;
class wxTextCtrl;

#include <wx/dialog.h>

#include "../Effect.h"
#include "ladspa.h"

void LoadLadspaPlugins();

class LadspaEffect:public Effect {

 public:

   LadspaEffect(const LADSPA_Descriptor *data);
   virtual ~LadspaEffect();

   virtual wxString GetEffectName();
   
   virtual wxString GetEffectAction();

   virtual int GetEffectFlags() {
      return flags;
   }
 
   virtual bool Init();

   virtual bool PromptUser();
   
   virtual bool Process();
   
   virtual void End();

 private:
   bool ProcessStereo(int count, WaveTrack * left, WaveTrack *right,
                      longSampleCount lstart, longSampleCount rstart,
                      sampleCount len);

   void GetSamples(WaveTrack *track,
                   longSampleCount *start,
                   sampleCount *len);
 
   wxString pluginName;
   int flags;

   const LADSPA_Descriptor *mData;
   sampleCount mBlockSize;
   float *buffer;
   float **fInBuffer;
   float **fOutBuffer;
   unsigned long inputs;
   unsigned long outputs;
   unsigned long numInputControls;
   unsigned long *inputPorts;
   unsigned long *outputPorts;
   float *inputControls;
   float *outputControls;
   int mainRate;
};

class LadspaEffectDialog:public wxDialog {
   DECLARE_DYNAMIC_CLASS(LadspaEffectDialog)

 public:
   LadspaEffectDialog(LadspaEffect *effect,
                      wxWindow * parent,
                      const LADSPA_Descriptor *data,
                      float *inputControls,
                      int sampleRate);

   ~LadspaEffectDialog();

   void OnSlider(wxCommandEvent & event);
   void OnTextCtrl(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);

   DECLARE_EVENT_TABLE()

 private:
   void HandleSlider();
   void HandleText();

   bool inSlider;
   bool inText;
      
   int sampleRate;
   const LADSPA_Descriptor *mData;
   wxSlider **sliders;
   wxSlider *targetSlider;
   wxTextCtrl **fields;
   wxStaticText **labels;
   unsigned long *ports;
   unsigned long numParams;
   float *inputControls;
   LadspaEffect *effect;
};

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: d8622b59-5c08-4e7f-a170-2502ff8af8e5


/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/

class wxSlider;
class wxStaticText;
class wxTextCtrl;

#include <wx/dialog.h>

#include "../Effect.h"

class LadspaEffect:public Effect {

 public:

   LadspaEffect(const LADSPA_Descriptor *data);

   virtual wxString GetEffectName();
   
   virtual wxString GetEffectAction();
   
   virtual bool Init();

   virtual bool PromptUser();
   
   virtual bool Process();
   
   virtual void End();

 private:
   bool ProcessStereo(int count, WaveTrack * left, WaveTrack *right,
                      sampleCount lstart, sampleCount rstart, sampleCount len);
 
   wxString pluginName;

   const LADSPA_Descriptor *mData;
   sampleCount mBlockSize;
   sampleType *buffer;
   float **fInBuffer;
   float **fOutBuffer;
   int inputs;
   int outputs;
   int numInputControls;
   unsigned long *inputPorts;
   unsigned long *outputPorts;
   float *inputControls;
   float *outputControls;
};

class LadspaEffectDialog:public wxDialog {
   DECLARE_DYNAMIC_CLASS(LadspaEffectDialog)

 public:
   LadspaEffectDialog(wxWindow * parent,
                      const LADSPA_Descriptor *data,
                      float *inputControls);

   ~LadspaEffectDialog();

   void OnSlider(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

    DECLARE_EVENT_TABLE()

 private:
    const LADSPA_Descriptor *mData;
    wxSlider **sliders;
    wxTextCtrl **fields;
    wxStaticText **labels;
    unsigned long *ports;
    unsigned long numParams;
    float *inputControls;
};

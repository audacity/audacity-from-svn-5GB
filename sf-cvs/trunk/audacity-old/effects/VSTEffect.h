/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/

struct AEffect;
class wxSlider;
class labels;

#include <wx/dialog.h>
#include <wx/stattext.h>

#include "Effect.h"

class VSTEffect:public Effect {

 public:

   VSTEffect(wxString pluginName, AEffect * aEffect);

   virtual wxString GetEffectName();

   virtual bool Begin(wxWindow * parent);
   virtual bool DoIt(WaveTrack * t, sampleCount start, sampleCount len);
   virtual void End();

 private:
    bool isOpened;
   wxString pluginName;
   AEffect *aEffect;

   sampleCount mBlockSize;
   sampleType *buffer;
   float **fInBuffer;
   float **fOutBuffer;
   int inputs;
   int outputs;
};

class VSTEffectDialog:public wxDialog {
   DECLARE_DYNAMIC_CLASS(VSTEffectDialog)

 public:
   VSTEffectDialog(wxWindow * parent,
                   wxString effectName,
                   int numParams,
                   AEffect * aEffect,
                   const wxPoint & pos = wxDefaultPosition);

   ~VSTEffectDialog();

   void OnSlider(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

    DECLARE_EVENT_TABLE()

 private:
    AEffect * aEffect;
   wxSlider **sliders;
   wxStaticText **labels;
   int numParams;
};

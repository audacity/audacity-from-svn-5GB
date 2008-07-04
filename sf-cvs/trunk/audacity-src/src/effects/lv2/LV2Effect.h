/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class wxCheckBox;

#include <wx/dialog.h>

#include <slv2/slv2.h>

#include "../Effect.h"

void LoadLadspaPlugins();

class LV2Effect:public Effect {

 public:

   LV2Effect(SLV2Plugin plug,
             const std::set<wxString>& categories = std::set<wxString>());
   virtual ~LV2Effect();

   virtual wxString GetEffectName();
   
   virtual std::set<wxString> GetEffectCategories();

   virtual wxString GetEffectIdentifier();
   
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

   SLV2Plugin mData;
   sampleCount mBlockSize;
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
   double mLength;

   std::set<wxString> mCategories;

};

class LV2EffectDialog:public wxDialog {
   DECLARE_DYNAMIC_CLASS(LV2EffectDialog)

 public:
   LV2EffectDialog(LV2Effect *effect,
                   wxWindow * parent,
                   SLV2Plugin data,
                   float *inputControls,
                   int sampleRate,
                   double length);

   ~LV2EffectDialog();

   void OnCheckBox(wxCommandEvent & event);
   void OnSlider(wxCommandEvent & event);
   void OnTextCtrl(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);
   void ControlSetFocus(wxFocusEvent & event);

   double GetLength();

   DECLARE_EVENT_TABLE()

 private:
   void HandleText();
   void ConnectFocus(wxControl *c);
   void DisconnectFocus(wxControl *c);
   bool inSlider;
   bool inText;

   double mLength;
   int sampleRate;
   SLV2Plugin mData;
   wxSlider **sliders;
   wxTextCtrl **fields;
   wxStaticText **labels;
   wxCheckBox **toggles;
   unsigned long *ports;
   unsigned long numParams;
   float *inputControls;
   LV2Effect *effect;
   wxTextCtrl *mSeconds;
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


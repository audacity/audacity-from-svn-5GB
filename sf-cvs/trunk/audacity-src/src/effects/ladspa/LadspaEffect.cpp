/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.cpp

  Dominic Mazzoni
  
  This class implements a Ladspa Plug-in effect.

*******************************************************************//**

\class LadspaEffect
\brief An Effect that calls up a LADSPA plug in, i.e. many possible 
effects from this one class.

*//****************************************************************//**

\class LadspaEffectDialog
\brief Dialog used with Effect

*//*******************************************************************/


#include "../Audacity.h"

#include "ladspa.h"

#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/intl.h>

#include "../Effect.h"          // Audacity Effect base class
#include "LadspaEffect.h"       // This class's header file
#include "../Internat.h"

LadspaEffect::LadspaEffect(const LADSPA_Descriptor *data)
{
   mData = data;
   pluginName = LAT1CTOWX(data->Name);

   buffer = NULL;
   fInBuffer = NULL;
   fOutBuffer = NULL;

   inputs = 0;
   outputs = 0;
   numInputControls = 0;

   unsigned long p;

   inputPorts = new unsigned long [mData->PortCount];
   outputPorts = new unsigned long [mData->PortCount];
   inputControls = new float [mData->PortCount];
   outputControls = new float [mData->PortCount];

   for(p=0; p<mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_AUDIO(d)) {
         if (LADSPA_IS_PORT_INPUT(d)) {
            inputPorts[inputs] = p;
            inputs++;
         }
         else if (LADSPA_IS_PORT_OUTPUT(d)) {
            outputPorts[outputs] = p;
            outputs++;
         }
      }
      if (LADSPA_IS_PORT_CONTROL(d) &&
          LADSPA_IS_PORT_INPUT(d)) {
         numInputControls++;

         float val = float(1.0);
         LADSPA_PortRangeHint hint = mData->PortRangeHints[p];

         if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor) &&
             val < hint.LowerBound)
            val = hint.LowerBound;

         if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor) &&
             val > hint.UpperBound)
            val = hint.UpperBound;

         if (LADSPA_IS_HINT_DEFAULT_MINIMUM(hint.HintDescriptor))
            val = hint.LowerBound;

         if (LADSPA_IS_HINT_DEFAULT_LOW(hint.HintDescriptor))
            val = hint.LowerBound * 0.75f + hint.UpperBound * 0.25f;

         if (LADSPA_IS_HINT_DEFAULT_MIDDLE(hint.HintDescriptor))
            val = hint.LowerBound * 0.5f + hint.UpperBound * 0.5f;

         if (LADSPA_IS_HINT_DEFAULT_HIGH(hint.HintDescriptor))
            val = hint.LowerBound * 0.25f + hint.UpperBound * 0.75f;

         if (LADSPA_IS_HINT_DEFAULT_MAXIMUM(hint.HintDescriptor))
            val = hint.UpperBound;

         if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor))
            val *= mProjectRate;

         if (LADSPA_IS_HINT_DEFAULT_0(hint.HintDescriptor))
            val = 0.0f;

         if (LADSPA_IS_HINT_DEFAULT_1(hint.HintDescriptor))
            val = 1.0f;

         if (LADSPA_IS_HINT_DEFAULT_100(hint.HintDescriptor))
            val = 100.0f;

         if (LADSPA_IS_HINT_DEFAULT_440(hint.HintDescriptor))
            val = 440.0f;

         inputControls[p] = val;
      }
   }

   flags = PLUGIN_EFFECT;

   if (inputs == 0)
      flags |= INSERT_EFFECT;
   else if (outputs == 0)
      flags |= ANALYZE_EFFECT;
   else
      flags |= PROCESS_EFFECT;   
}

LadspaEffect::~LadspaEffect()
{
   delete[] inputPorts;
   delete[] outputPorts;
   delete[] inputControls;
   delete[] outputControls;
}

wxString LadspaEffect::GetEffectName()
{
   if (numInputControls > 0)
      return pluginName + wxT("...");
   else
      return pluginName;
}

wxString LadspaEffect::GetEffectAction()
{
   return wxString::Format(_("Performing Effect: %s"), 
                           pluginName.c_str());
}

bool LadspaEffect::Init()
{
   mBlockSize = 0;
   mainRate = 0;

   TrackListIterator iter(mWaveTracks);
   Track *left = iter.First();
   while(left) {
      if (mainRate == 0)
         mainRate = (int)(((WaveTrack *)left)->GetRate() + 0.5);
      
      if (left->GetLinked()) {
         Track *right = iter.Next();
         
         if (((WaveTrack *)left)->GetRate() !=
             ((WaveTrack *)right)->GetRate()) {
            wxMessageBox(_("Sorry, Plug-in Effects cannot be performed \
                            on stereo tracks where the individual \
                            channels of the track do not match."));
            return false;
         }
      }
      
      left = iter.Next();
   }

   if (mainRate<=0)
      mainRate = (int)(mProjectRate + 0.5);

   return true;
}

bool LadspaEffect::PromptUser()
{
   if (numInputControls > 0) {
      LadspaEffectDialog dlog(this, mParent, mData, inputControls, mainRate);
      dlog.CentreOnParent();
      dlog.ShowModal();
      
      if (!dlog.GetReturnCode())
         return false;
   }

   return true;
}

void LadspaEffect::GetSamples(WaveTrack *track,
                              longSampleCount *start,
                              sampleCount *len)
{
   double trackStart = track->GetStartTime();
   double trackEnd = track->GetEndTime();
   double t0 = mT0 < trackStart? trackStart: mT0;
   double t1 = mT1 > trackEnd? trackEnd: mT1;
   
   if (t1 > t0) {
      *start = track->TimeToLongSamples(t0);
      longSampleCount end = track->TimeToLongSamples(t1);
      *len = (sampleCount)(end - *start);
   }
   else {
      *start = 0;
      *len  = 0;
   }
}

bool LadspaEffect::Process()
{
   TrackListIterator iter(mWaveTracks);
   int count = 0;
   Track *left = iter.First();
   Track *right;
   while(left) {
      longSampleCount lstart, rstart;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);
      
      right = NULL;
      if (left->GetLinked() && inputs>1) {
         right = iter.Next();         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      bool success = false;

      if (inputs < 2 && right) {
         // If the effect is mono, apply to each channel separately

         success = ProcessStereo(count, (WaveTrack *)left, NULL,
                                 lstart, 0, len);
         if (success)
            success = ProcessStereo(count, (WaveTrack *)right, NULL,
                                    rstart, 0, len);
      }
      else success = ProcessStereo(count,
                                   (WaveTrack *)left, (WaveTrack *)right,
                                   lstart, rstart, len);
      if (!success)
         return false;
   
      left = iter.Next();
      count++;
   }

   return true;
}

bool LadspaEffect::ProcessStereo(int count, WaveTrack *left, WaveTrack *right,
                                 longSampleCount lstart, 
                                 longSampleCount rstart,
                                 sampleCount len)
{
   /* Allocate buffers */
   if (mBlockSize == 0) {
      mBlockSize = left->GetMaxBlockSize() * 2;

      buffer = new float[mBlockSize];
      fInBuffer = new float *[inputs];
      unsigned long i;
      for (i = 0; i < inputs; i++)
         fInBuffer[i] = new float[mBlockSize];
      fOutBuffer = new float *[outputs];
      for (i = 0; i < outputs; i++)
         fOutBuffer[i] = new float[mBlockSize];
   }

   /* Instantiate the plugin */

   unsigned long rate = (unsigned long)(left->GetRate() + 0.5);
   LADSPA_Handle handle = mData->instantiate(mData, rate);

   unsigned long p;
   for(p=0; p<inputs; p++) {
      mData->connect_port(handle, inputPorts[p], fInBuffer[p]);
   }
   for(p=0; p<outputs; p++) {
      mData->connect_port(handle, outputPorts[p], fOutBuffer[p]);
   }

   for(p=0; p<mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d)) {
         if (LADSPA_IS_PORT_INPUT(d)) {
            mData->connect_port(handle, p, &inputControls[p]);
         }
         else
            mData->connect_port(handle, p, &outputControls[p]);
      }
   }
   
   if (mData->activate)
      mData->activate(handle);

   // Actually perform the effect here

   sampleCount originalLen = len;
   longSampleCount ls = lstart;
   longSampleCount rs = rstart;
   while (len) {
      int block = mBlockSize;
      int i;
      if (block > len)
         block = len;

      if (left && inputs > 0) {
         left->Get((samplePtr)buffer, floatSample, ls, block);
         for (i = 0; i < block; i++)
            fInBuffer[0][i] = buffer[i];
      }
      if (right && inputs > 1) {
         right->Get((samplePtr)buffer, floatSample, rs, block);
         for (i = 0; i < block; i++)
            fInBuffer[1][i] = buffer[i];
      }

      mData->run(handle, block);

      if (left && outputs > 0) {
         for (i = 0; i < block; i++)
            buffer[i] = fOutBuffer[0][i];
         left->Set((samplePtr)buffer, floatSample, ls, block);
      }      
      
      if (right && outputs > 1) {
         for (i = 0; i < block; i++)
            buffer[i] = fOutBuffer[1][i];
         right->Set((samplePtr)buffer, floatSample, rs, block);
      }      

      len -= block;
      ls += block;
      rs += block;
      
      if (inputs > 1) {
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen))
            return false;
      }
      else {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen))
            return false;
      }
   }

   if (mData->deactivate)
      mData->deactivate(handle);

   if (mData->cleanup)
      mData->cleanup(handle);

   return true;
}

void LadspaEffect::End()
{
   if (buffer) {
      unsigned long i;

      delete[]buffer;
      for (i = 0; i < inputs; i++) {
         delete fInBuffer[i];
      }
      for (i = 0; i < outputs; i++) {
         delete fOutBuffer[i];
      }

      delete[] fInBuffer;
      delete[] fOutBuffer;
   }
   buffer = NULL;
   fInBuffer = NULL;
   fOutBuffer = NULL;
}

class Slider:public wxSlider
{
 public:
   Slider(wxWindow *parent, wxWindowID id,
          int value, int minValue, int maxValue,
          const wxPoint& pos = wxDefaultPosition,
          const wxSize& size = wxDefaultSize,
          long style = wxSL_HORIZONTAL,
          const wxValidator& validator = wxDefaultValidator,
          const wxString& name = wxSliderNameStr)
   : wxSlider(parent, id, value, minValue, maxValue,
              pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent &event)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      event.Skip();

      int y;
      int yppu;
      p->GetScrollPixelsPerUnit(NULL, &yppu);

      if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom()) {
         return;
      }

      if (r.y < rv.y) {
         p->CalcUnscrolledPosition(NULL, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else {
         p->CalcUnscrolledPosition(NULL, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(Slider, wxSlider)
    EVT_SET_FOCUS(Slider::OnSetFocus)
END_EVENT_TABLE()

class TextCtrl:public wxTextCtrl
{
 public:
   TextCtrl(wxWindow *parent, wxWindowID id,
            const wxString& value = wxEmptyString,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxTextCtrlNameStr)
   : wxTextCtrl(parent, id, value,
                pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent &event)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      event.Skip();

      int y;
      int yppu;
      p->GetScrollPixelsPerUnit(NULL, &yppu);

      if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom()) {
         return;
      }

      if (r.y < rv.y) {
         p->CalcUnscrolledPosition(NULL, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else {
         p->CalcUnscrolledPosition(NULL, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(TextCtrl, wxTextCtrl)
    EVT_SET_FOCUS(TextCtrl::OnSetFocus)
END_EVENT_TABLE()

const int LADSPA_SLIDER_ID = 13100;
const int LADSPA_TEXTCTRL_ID = 13101;
const int LADSPA_PREVIEW_ID = 13102;

BEGIN_EVENT_TABLE(LadspaEffectDialog, wxDialog)
    EVT_BUTTON(wxID_OK, LadspaEffectDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, LadspaEffectDialog::OnCancel)
    EVT_BUTTON(LADSPA_PREVIEW_ID, LadspaEffectDialog::OnPreview)
    EVT_SLIDER(LADSPA_SLIDER_ID, LadspaEffectDialog::OnSlider)
    EVT_TEXT(LADSPA_TEXTCTRL_ID, LadspaEffectDialog::OnTextCtrl)
END_EVENT_TABLE()

IMPLEMENT_CLASS(LadspaEffectDialog, wxDialog)

LadspaEffectDialog::LadspaEffectDialog(LadspaEffect *eff,
                                       wxWindow * parent,
                                       const LADSPA_Descriptor *data,
                                       float *inputControls,
                                       int sampleRate)
   :wxDialog(parent, -1, LAT1CTOWX(data->Name),
             wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    effect(eff)
{
   numParams = 0;
   this->mData = data;
   this->inputControls = inputControls;
   this->sampleRate = sampleRate;
	#ifdef __WXMSW__
		// On Windows, for some reason, wxWindows calls OnTextCtrl during creation
		// of the text control, and LadspaEffectDialog::OnTextCtrl calls HandleText, 
		// which assumes all the fields have been initialized. 
		// This can give us a bad pointer crash, so manipulate inSlider to 
		// no-op HandleText during creation.
		inSlider = true;
	#else
		inSlider = false;
	#endif
   inText = false;
   targetSlider = NULL;

   sliders = new wxSlider*[mData->PortCount];
   fields = new wxTextCtrl*[mData->PortCount];
	labels = new wxStaticText*[mData->PortCount];
   ports = new unsigned long [mData->PortCount];

   unsigned long p;
   for(p=0; p<mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d) &&
          LADSPA_IS_PORT_INPUT(d)) {
         ports[numParams] = p;
         numParams++;
      }
   }

   wxControl *item;

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);

   if (mData->Maker &&
       mData->Maker[0] && 
       LAT1CTOWX(mData->Maker) != wxString(_("None"))) {       
      item = new wxStaticText(this, 0,
                              wxString(_("Author: "))+LAT1CTOWX(mData->Maker));
      vSizer->Add(item, 0, wxALL, 5);
   }
   
   if (mData->Copyright &&
       mData->Copyright[0] && 
       LAT1CTOWX(mData->Copyright) != wxString(_("None"))) {
      
      item = new wxStaticText(this, 0,
                              LAT1CTOWX(mData->Copyright));
      vSizer->Add(item, 0, wxALL, 5);
   }

   wxScrolledWindow *w = new wxScrolledWindow(this,
                                              wxID_ANY,
                                              wxDefaultPosition,
                                              wxDefaultSize,
                                              wxVSCROLL | wxTAB_TRAVERSAL);
   w->SetScrollRate(0, 20);
   vSizer->Add(w, 1, wxEXPAND|wxALL, 5);

   wxBoxSizer *okSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *button;

   button = new wxButton(this, LADSPA_PREVIEW_ID, effect->GetPreviewName());
   okSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   button = new wxButton(this, wxID_CANCEL, _("&Cancel"));
   okSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   button = new wxButton(this, wxID_OK, _("&OK"));
   button->SetDefault();
   okSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   vSizer->Add(okSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   SetSizer(vSizer);

   wxSizer *paramSizer =
      new wxStaticBoxSizer(wxVERTICAL, w, _("Effect Settings"));

   wxFlexGridSizer *gridSizer =
      new wxFlexGridSizer(3, 0, 0);
   gridSizer->AddGrowableCol(2);

   for (p = 0; p < numParams; p++) {

      item = new wxStaticText(w, 0, wxString(mData->PortNames[ports[p]], wxConvISO8859_1));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

      wxString fieldText;
      LADSPA_PortRangeHint hint = mData->PortRangeHints[ports[p]];
      if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor))
         fieldText.Printf(wxT("%d"), (int)(inputControls[ports[p]] + 0.5));
      else
         fieldText = Internat::ToDisplayString(inputControls[ports[p]]);

      fields[p] = new TextCtrl(w, LADSPA_TEXTCTRL_ID, fieldText);
      gridSizer->Add(fields[p], 0, wxALL, 5);

      sliders[p] =
          new Slider(w, LADSPA_SLIDER_ID,
                       0, 0, 1000,
                       wxDefaultPosition,
                       wxSize(200, -1));
      gridSizer->Add(sliders[p], 0, wxEXPAND|wxALL, 5);
   }

   // Set all of the sliders based on the value in the
   // text fields
	inSlider = false; // Now we're ready for HandleText to actually do something.
   HandleText();
   
   paramSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);
   w->SetSizer(paramSizer);

   Layout();
   Fit();
   SetSizeHints(GetSize());
}

LadspaEffectDialog::~LadspaEffectDialog()
{
   delete[]sliders;
   delete[]fields;
   delete[]labels;
}

void LadspaEffectDialog::OnSlider(wxCommandEvent &event)
{
   targetSlider = (wxSlider *)event.GetEventObject();
   HandleSlider();
   targetSlider = NULL;
}

void LadspaEffectDialog::OnTextCtrl(wxCommandEvent & WXUNUSED(event))
{
	HandleText();
}

void LadspaEffectDialog::HandleSlider()
{
   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.
   if (inText)
      return;
   inSlider = true;

   for (unsigned long p = 0; p < numParams; p++) {
      if (targetSlider && targetSlider!=sliders[p])
         continue;

      float val;
      float lower = float(0.0);
      float upper = float(10.0);
      float range;

      LADSPA_PortRangeHint hint = mData->PortRangeHints[ports[p]];
      if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
         lower = hint.LowerBound;
      if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
         upper = hint.UpperBound;
      if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
         lower *= sampleRate;
         upper *= sampleRate;
      }

      range = upper - lower;

      val = (sliders[p]->GetValue() / 1000.0) * range + lower;

      wxString str;
      if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor))
         str.Printf(wxT("%d"), (int)(val + 0.5));
      else
         str = Internat::ToDisplayString(val);

      fields[p]->SetValue(str);

      inputControls[ports[p]] = val;
   }

   inSlider = false;
}

void LadspaEffectDialog::HandleText()
{
   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.

   if (inSlider)
      return;
   inText = true;
   for (unsigned long p = 0; p < numParams; p++) {
      double dval;
      float val;
      float lower = float(0.0);
      float upper = float(10.0);
      float range;

      dval = Internat::CompatibleToDouble(fields[p]->GetValue());
      val = dval;

      LADSPA_PortRangeHint hint = mData->PortRangeHints[ports[p]];
      if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
         lower = hint.LowerBound;
      if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
         upper = hint.UpperBound;      
      if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
         lower *= sampleRate;
         upper *= sampleRate;
      }         
      range = upper - lower;

      if (val < lower)
         val = lower;
      if (val > upper)
         val = upper;

      inputControls[ports[p]] = val;

      sliders[p]->SetValue((int)(((val-lower)/range) * 1000.0 + 0.5));      
   }

   inText = false;
}

void LadspaEffectDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(TRUE);
}

void LadspaEffectDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(FALSE);
}

void LadspaEffectDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   effect->Preview();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 7e4a0346-c3ec-45de-9f71-818c6e34a094


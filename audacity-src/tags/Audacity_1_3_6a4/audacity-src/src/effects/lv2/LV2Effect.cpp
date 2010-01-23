/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*******************************************************************//**

\class LV2Effect
\brief Objects of this class are used to represent LV2 plugin instances.

They act as adapters between Audacity's effect interface and the LV2 API.

*//****************************************************************//**

\class LV2EffectDialog
\brief Dialogs used with LV2 effects

Dialogs of this class are displayed to allow the user to modify the settings
of an LV2 plugin before processing.

*//*******************************************************************/

#include <cmath>

#include "../Audacity.h"

#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

#include "../Effect.h"
#include "LoadLV2.h"
#include "LV2Effect.h"
#include "../Internat.h"


LV2Effect::LV2Effect(SLV2Plugin data,
                     const std::set<wxString>& categories)
   : mCategories(categories) {
   
   mData = data;
   pluginName = 
      wxString::FromUTF8(slv2_value_as_string(slv2_plugin_get_name(mData)));
   
   fInBuffer = NULL;
   fOutBuffer = NULL;
   
   inputs = 0;
   outputs = 0;
   numInputControls = 0;
   mLength = 0;

   uint32_t p;
   
   // Allocate buffers for the port indices and the default control values
   uint32_t numPorts = slv2_plugin_get_num_ports(mData);
   inputPorts = new unsigned long [numPorts];
   outputPorts = new unsigned long [numPorts];
   inputControls = new float [numPorts];
   outputControls = new float [numPorts];
   float* minimumValues = new float [numPorts];
   float* maximumValues = new float [numPorts];
   float* defaultValues = new float [numPorts];
   
   // Retrieve the port ranges for all ports (some values may be NaN)
   slv2_plugin_get_port_ranges_float(mData, minimumValues, 
                                     maximumValues, defaultValues);
   
   gAudioPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_AUDIO);
   gControlPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_CONTROL);
   gInputPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_INPUT);
   gOutputPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_OUTPUT);

   // Get info about all ports
   for(p = 0; p < numPorts; p++) {
      SLV2Port port = slv2_plugin_get_port_by_index(mData, p);
      if (slv2_port_is_a(mData, port, gAudioPortClass)) {
         if (slv2_port_is_a(mData, port, gInputPortClass)) {
            inputPorts[inputs] = p;
            inputs++;
         }
         else if (slv2_port_is_a(mData, port, gOutputPortClass)) {
            outputPorts[outputs] = p;
            outputs++;
         }
      }
      if (slv2_port_is_a(mData, port, gControlPortClass) &&
          slv2_port_is_a(mData, port, gInputPortClass)) {
         numInputControls++;
         float val = float(1.0);
         if (std::isfinite(defaultValues[p]))
            val = defaultValues[p];
         else if (std::isfinite(minimumValues[p]))
            val = minimumValues[p];
         else if (std::isfinite(maximumValues[p]))
            val = maximumValues[p];
         inputControls[p] = val;
      }
   }
   
   // Determine whether the plugin is a generator, effect or analyser 
   // depending on the number of ports of each type (not completely accurate,
   // but works most of the time)
   flags = PLUGIN_EFFECT;
   if (inputs == 0)
      flags |= INSERT_EFFECT;
   else if (outputs == 0)
      flags |= ANALYZE_EFFECT;
   else
      flags |= PROCESS_EFFECT;   
}

LV2Effect::~LV2Effect()
{
   delete[] inputPorts;
   delete[] outputPorts;
   delete[] inputControls;
   delete[] outputControls;
}

wxString LV2Effect::GetEffectName()
{
   if (numInputControls > 0)
      return pluginName + wxT("...");
   else
      return pluginName;
}

std::set<wxString> LV2Effect::GetEffectCategories()
{
   return mCategories;
}

wxString LV2Effect::GetEffectIdentifier()
{
   wxStringTokenizer st(pluginName, wxT(" "));
   wxString id;

   // CamelCase the name
   while (st.HasMoreTokens()) {
      wxString tok = st.GetNextToken();

      id += tok.Left(1).MakeUpper() + tok.Mid(1);
   }

   return id;
}

wxString LV2Effect::GetEffectAction()
{
   return wxString::Format(_("Performing Effect: %s"), 
                           pluginName.c_str());
}

bool LV2Effect::Init()
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
            wxMessageBox(_("Sorry, Plug-in Effects cannot be performed on stereo tracks where the individual channels of the track do not match."));
            return false;
         }
      }
      
      left = iter.Next();
   }

   if (mainRate<=0)
      mainRate = (int)(mProjectRate + 0.5);

   return true;
}

bool LV2Effect::PromptUser()
{
   if (numInputControls > 0) {
      double length = mT1 > mT0 ? mT1 - mT0 : sDefaultGenerateLen;

      LV2EffectDialog dlog(this, mParent, mData, inputControls, mainRate, length);
      dlog.CentreOnParent();
      dlog.ShowModal();
      
      if (!dlog.GetReturnCode())
         return false;

      mLength = dlog.GetLength();
   }
   return true;
}

void LV2Effect::GetSamples(WaveTrack *track,
                              sampleCount *start,
                              sampleCount *len)
{
   double trackStart = track->GetStartTime();
   double trackEnd = track->GetEndTime();
   double t0 = mT0 < trackStart? trackStart: mT0;
   double t1 = mT1 > trackEnd? trackEnd: mT1;

   if (flags & INSERT_EFFECT) {
      t1 = t0 + mLength;
      if (mT0 == mT1) {
         track->InsertSilence(t0, t1);
      }
   }

   if (t1 > t0) {
      *start = track->TimeToLongSamples(t0);
      sampleCount end = track->TimeToLongSamples(t1);
      *len = (sampleCount)(end - *start);
   }
   else {
      *start = 0;
      *len  = 0;
   }
}

bool LV2Effect::Process()
{
   this->CopyInputWaveTracks(); // Set up mOutputWaveTracks.
   bool bGoodResult = true;

   TrackListIterator iter(mOutputWaveTracks);
   int count = 0;
   Track *left = iter.First();
   Track *right;
   while(left) {
      sampleCount lstart, rstart;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);
      
      right = NULL;
      if (left->GetLinked() && inputs>1) {
         right = iter.Next();         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      if (inputs < 2 && right) {
         // If the effect is mono, apply to each channel separately

         bGoodResult = ProcessStereo(count, (WaveTrack *)left, NULL,
                                 lstart, 0, len) && 
                        ProcessStereo(count, (WaveTrack *)right, NULL,
                                    rstart, 0, len);
      }
      else bGoodResult = ProcessStereo(count,
                                   (WaveTrack *)left, (WaveTrack *)right,
                                   lstart, rstart, len);
      if (!bGoodResult)
         break;
   
      left = iter.Next();
      count++;
   }

   this->ReplaceProcessedWaveTracks(bGoodResult); 
   return bGoodResult;
}

bool LV2Effect::ProcessStereo(int count, WaveTrack *left, WaveTrack *right,
                              sampleCount lstart, 
                              sampleCount rstart,
                              sampleCount len)
{
   /* Allocate buffers */
   if (mBlockSize == 0) {
      mBlockSize = left->GetMaxBlockSize() * 2;

      fInBuffer = new float *[inputs];
      unsigned long i;
      for (i = 0; i < inputs; i++)
         fInBuffer[i] = new float[mBlockSize];
      fOutBuffer = new float *[outputs];
      for (i = 0; i < outputs; i++)
         fOutBuffer[i] = new float[mBlockSize];
   }

   /* Instantiate the plugin */
   SLV2Instance handle = slv2_plugin_instantiate(mData, left->GetRate(), 0);
   
   unsigned long p;
   for(p = 0; p < inputs; p++) {
      slv2_instance_connect_port(handle, inputPorts[p], fInBuffer[p]);
   }
   for(p = 0; p < outputs; p++) {
      slv2_instance_connect_port(handle, outputPorts[p], fOutBuffer[p]);
   }

   for (p = 0; p < slv2_plugin_get_num_ports(mData); p++) {
      SLV2Port port = slv2_plugin_get_port_by_index(mData, p);
      if (slv2_port_is_a(mData, port, gControlPortClass)) {
         if (slv2_port_is_a(mData, port, gInputPortClass)) {
            slv2_instance_connect_port(handle, p, &inputControls[p]);
         }
         else if (slv2_port_is_a(mData, port, gOutputPortClass)) {
            slv2_instance_connect_port(handle, p, &outputControls[p]);
         }
      }
   }
   
   slv2_instance_activate(handle);

   // Actually perform the effect here

   sampleCount originalLen = len;
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   while (len) {
      int block = mBlockSize;
      if (block > len)
         block = len;

      if (left && inputs > 0) {
         left->Get((samplePtr)fInBuffer[0], floatSample, ls, block);
      }
      if (right && inputs > 1) {
         right->Get((samplePtr)fInBuffer[1], floatSample, rs, block);
      }
      
      slv2_instance_run(handle, block);

      if (left && outputs > 0) {
         left->Set((samplePtr)fOutBuffer[0], floatSample, ls, block);
      }
      
      if (right && outputs > 1) {
         right->Set((samplePtr)fOutBuffer[1], floatSample, rs, block);
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
   
   slv2_instance_deactivate(handle);
   slv2_instance_free(handle);
   
   return true;
}

void LV2Effect::End()
{
   unsigned long i;

   if (fInBuffer) {
      for (i = 0; i < inputs; i++) {
         if (fInBuffer[i]) {
            delete [] fInBuffer[i];
         }
      }
      delete [] fInBuffer;
      fInBuffer = NULL;
   }

   if (fOutBuffer) {
      for (i = 0; i < outputs; i++) {
         if (fOutBuffer[i]) {
            delete [] fOutBuffer[i];
         }
      }
      delete [] fOutBuffer;
      fOutBuffer = NULL;
   }
}

// This should be moved to its own source file, it's in LadspaEffect.cpp 
// as well
class LV2Slider:public wxSlider
{
 public:
   LV2Slider(wxWindow *parent, wxWindowID id,
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
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(LV2Slider, wxSlider)
    EVT_SET_FOCUS(LV2Slider::OnSetFocus)
END_EVENT_TABLE()

class LV2TextCtrl:public wxTextCtrl
{
 public:
   LV2TextCtrl(wxWindow *parent, wxWindowID id,
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
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(LV2TextCtrl, wxTextCtrl)
    EVT_SET_FOCUS(LV2TextCtrl::OnSetFocus)
END_EVENT_TABLE()

static const int LADSPA_SECONDS_ID = 13101;

BEGIN_EVENT_TABLE(LV2EffectDialog, wxDialog)
    EVT_BUTTON(wxID_OK, LV2EffectDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, LV2EffectDialog::OnCancel)
    EVT_BUTTON(ID_EFFECT_PREVIEW, LV2EffectDialog::OnPreview)
    EVT_SLIDER(wxID_ANY, LV2EffectDialog::OnSlider)
    EVT_TEXT(wxID_ANY, LV2EffectDialog::OnTextCtrl)
    EVT_CHECKBOX(wxID_ANY, LV2EffectDialog::OnCheckBox)
END_EVENT_TABLE()

IMPLEMENT_CLASS(LV2EffectDialog, wxDialog)

LV2EffectDialog::LV2EffectDialog(LV2Effect *eff,
                                 wxWindow * parent,
                                 SLV2Plugin data,
                                 float *inputControls,
                                 int sampleRate,
                                 double length)
   :wxDialog(parent, -1, 
             LAT1CTOWX(slv2_value_as_string(slv2_plugin_get_name(data))),
             wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    effect(eff)
{
   mLength = length;
   numParams = 0;
   this->mData = data;
   this->inputControls = inputControls;
   this->sampleRate = sampleRate;
	#ifdef __WXMSW__
		// On Windows, for some reason, wxWindows calls OnTextCtrl during creation
		// of the text control, and LV2EffectDialog::OnTextCtrl calls HandleText, 
		// which assumes all the fields have been initialized. 
		// This can give us a bad pointer crash, so manipulate inSlider to 
		// no-op HandleText during creation.
		inSlider = true;
	#else
		inSlider = false;
	#endif
   inText = false;
   
   // Allocate memory for the user parameter controls
   uint32_t numPorts = slv2_plugin_get_num_ports(mData);
   toggles = new wxCheckBox*[numPorts];
   sliders = new wxSlider*[numPorts];
   fields = new wxTextCtrl*[numPorts];
   labels = new wxStaticText*[numPorts];
   ports = new unsigned long [numPorts];
   
   uint32_t p;
   for (p = 0; p < numPorts; p++) {
      SLV2Port port = slv2_plugin_get_port_by_index(mData, p);
      if (slv2_port_is_a(mData, port, gControlPortClass) &&
          slv2_port_is_a(mData, port, gInputPortClass)) {
         ports[numParams] = p;
         numParams++;
      }
   }
   
   wxControl *item;

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);
   
   // Add information about the plugin
   SLV2Value tmpValue = slv2_plugin_get_author_name(data);
   if (tmpValue) {
      const char* author = slv2_value_as_string(tmpValue);
      item = new wxStaticText(this, 0,
                              wxString(_("Author: "))+LAT1CTOWX(author));
      vSizer->Add(item, 0, wxALL, 5);
      slv2_value_free(tmpValue);
   }
   
   // XXX Get the license here as well - every plugin has one, but there
   // is no SLV2 function that returns it
   
   wxScrolledWindow *w = new wxScrolledWindow(this,
                                              wxID_ANY,
                                              wxDefaultPosition,
                                              wxDefaultSize,
                                              wxVSCROLL | wxTAB_TRAVERSAL);

   // Try to give the window a sensible default/minimum size
   w->SetMinSize(wxSize(
      wxMax(600, parent->GetSize().GetWidth() * 2/3),
      parent->GetSize().GetHeight() / 2));
                                              
   w->SetScrollRate(0, 20);
   vSizer->Add(w, 1, wxEXPAND|wxALL, 5);

   // Preview, OK, & Cancel buttons
   vSizer->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);

   SetSizer(vSizer);

   wxSizer *paramSizer =
      new wxStaticBoxSizer(wxVERTICAL, w, _("Effect Settings"));

   wxFlexGridSizer *gridSizer =
      new wxFlexGridSizer(5, 0, 0);
   gridSizer->AddGrowableCol(3);
   
   SLV2Value portToggled = 
      slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#toggled");
   SLV2Value portIsInteger = 
      slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#integer");
   SLV2Value portIsSampleRate = 
      slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#sampleRate");
   
   float* minimumValues = new float [numPorts];
   float* maximumValues = new float [numPorts];
   float* defaultValues = new float [numPorts];
   
   // Retrieve the port ranges for all ports (some values may be NaN)
   slv2_plugin_get_port_ranges_float(mData, minimumValues, 
                                     maximumValues, defaultValues);

   // Create user parameter controls
   for (p = 0; p < numParams; p++) {
      SLV2Port port = slv2_plugin_get_port_by_index(data, ports[p]);
      SLV2Value tmpValue = slv2_port_get_name(data, port);
      wxString labelText = LAT1CTOWX(slv2_value_as_string(tmpValue));
      slv2_value_free(tmpValue);
      item = new wxStaticText(w, 0, labelText + wxT(":"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      wxString fieldText;
      
      if (slv2_port_has_property(data, port, portToggled)) {
         toggles[p] = new wxCheckBox(w, p, wxT(""));
         toggles[p]->SetName(labelText);
         toggles[p]->SetValue(inputControls[ports[p]] > 0);
         gridSizer->Add(toggles[p], 0, wxALL, 5);
         ConnectFocus(toggles[p]);

         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);
      }
      
      else {
         if (slv2_port_has_property(data, port, portIsInteger))
            fieldText.Printf(wxT("%d"), (int)(inputControls[ports[p]] + 0.5));
         else
            fieldText = Internat::ToDisplayString(inputControls[ports[p]]);

         fields[p] = new wxTextCtrl(w, p, fieldText);
         fields[p]->SetName(labelText);
         gridSizer->Add(fields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
         ConnectFocus(fields[p]);

         wxString bound;
         double lower = 0.0;
         double upper = 0.0;
         bool haslo = false;
         bool hashi = false;
         bool forceint = false;
         
         if (!std::isnan(minimumValues[p])) {
            lower = minimumValues[p];
            haslo = true;
         }
         if (!std::isnan(maximumValues[p])) {
            upper = maximumValues[p];
            hashi = true;
         }
         
         if (slv2_port_has_property(data, port, portIsSampleRate)) {
            lower *= sampleRate * 1000;
            upper *= sampleRate;
            forceint = true;
         }

         wxString str;
         if (haslo) {
            if (slv2_port_has_property(data, port, portIsInteger) || forceint)
               str.Printf(wxT("%d"), (int)(lower + 0.5));
            else
               str = Internat::ToDisplayString(lower);
            item = new wxStaticText(w, 0, str);
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
         }
         else {
            gridSizer->Add(1, 1, 0);
         }

         sliders[p] =
             new wxSlider(w, p,
                          0, 0, 1000,
                          wxDefaultPosition,
                          wxSize(200, -1));
         sliders[p]->SetName(labelText);
         gridSizer->Add(sliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);
         ConnectFocus(sliders[p]);

         if (hashi) {
            if (slv2_port_has_property(data, port, portIsInteger) || forceint)
               str.Printf(wxT("%d"), (int)(upper + 0.5));
            else
               str = Internat::ToDisplayString(upper);
            item = new wxStaticText(w, 0, str);
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
         }
         else {
            gridSizer->Add(1, 1, 0);
         }
      }
   }
   
   // Now add the length control
   if (effect->GetEffectFlags() & INSERT_EFFECT) {
      item = new wxStaticText(w, 0, _("Length (seconds)"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      mSeconds = new wxTextCtrl(w, LADSPA_SECONDS_ID, Internat::ToDisplayString(length));
      mSeconds->SetName(_("Length (seconds)"));
      gridSizer->Add(mSeconds, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mSeconds);
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

LV2EffectDialog::~LV2EffectDialog()
{
   delete[]sliders;
   delete[]fields;
   delete[]labels;
}

void LV2EffectDialog::OnCheckBox(wxCommandEvent &event)
{
   int p = event.GetId();

   inputControls[ports[p]] = toggles[p]->GetValue();
}

void LV2EffectDialog::OnSlider(wxCommandEvent &event)
{
   int p = event.GetId();

   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.
   if (inText)
      return;
   inSlider = true;

   float val;
   float lower = float(0.0);
   float upper = float(10.0);
   float range;
   bool forceint = false;
   
   // Get the port range
   SLV2Port port = slv2_plugin_get_port_by_index(mData, p);
   // XXX This is slow - store the values somewhere [larsl]
   SLV2Value portIsSampleRate = 
      slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#sampleRate");
   SLV2Value portIsInteger = 
      slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#integer");
   int numPorts = slv2_plugin_get_num_ports(mData);
   float* minimumValues = new float [numPorts];
   float* maximumValues = new float [numPorts];
   slv2_plugin_get_port_ranges_float(mData, minimumValues, 
                                     maximumValues, 0);
   if (std::isfinite(minimumValues[p]))
      lower = minimumValues[p];
   if (std::isfinite(maximumValues[p]))
      lower = maximumValues[p];
   if (slv2_port_has_property(mData, port, portIsSampleRate) || forceint) {
      lower *= sampleRate;
      upper *= sampleRate;
      forceint = true;
   }
   
   range = upper - lower;

   val = (sliders[p]->GetValue() / 1000.0) * range + lower;

   // Force the value to an integer if requested
   wxString str;
   if (slv2_port_has_property(mData, port, portIsInteger) || forceint)
      str.Printf(wxT("%d"), (int)(val + 0.5));
   else
      str = Internat::ToDisplayString(val);

   fields[p]->SetValue(str);

   inputControls[ports[p]] = val;

   inSlider = false;
}

void LV2EffectDialog::OnTextCtrl(wxCommandEvent & WXUNUSED(event))
{
   HandleText();
}

void LV2EffectDialog::HandleText()
{
   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.

   SLV2Value portToggled = 
      slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#toggled");
   SLV2Value portIsSampleRate = 
      slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#sampleRate");

   if (inSlider)
      return;
   inText = true;
   
   uint32_t numPorts = slv2_plugin_get_num_ports(mData);
   float* minimumValues = new float [numPorts];
   float* maximumValues = new float [numPorts];
   float* defaultValues = new float [numPorts];
   
   // Retrieve the port ranges for all ports (some values may be NaN)
   slv2_plugin_get_port_ranges_float(mData, minimumValues, 
                                     maximumValues, defaultValues);

   for (uint32_t p = 0; p < numParams; p++) {
      
      SLV2Port port = slv2_plugin_get_port_by_index(mData, p);
      
      double dval;
      float val;
      float lower = float(0.0);
      float upper = float(10.0);
      float range;

      if (slv2_port_has_property(mData, port, portToggled))
         continue;

      dval = Internat::CompatibleToDouble(fields[p]->GetValue());
      val = dval;
      
      
      
      if (!std::isnan(minimumValues[p]))
         lower = minimumValues[p];
      if (!std::isnan(maximumValues[p]))
         upper = maximumValues[p];
      if (slv2_port_has_property(mData, port, portIsSampleRate)) {
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

void LV2EffectDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(TRUE);
}

void LV2EffectDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(FALSE);
}

void LV2EffectDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   effect->Preview();
}

void LV2EffectDialog::ConnectFocus(wxControl *c)
{
   c->GetEventHandler()->Connect(wxEVT_SET_FOCUS,
                                 wxFocusEventHandler(LV2EffectDialog::ControlSetFocus));
}

void LV2EffectDialog::DisconnectFocus(wxControl *c)
{
   c->GetEventHandler()->Disconnect(wxEVT_SET_FOCUS,
                                    wxFocusEventHandler(LV2EffectDialog::ControlSetFocus));
}

void LV2EffectDialog::ControlSetFocus(wxFocusEvent &event)
{
   wxControl *c = (wxControl *) event.GetEventObject();
   wxScrolledWindow *p = (wxScrolledWindow *) c->GetParent();
   wxRect r = c->GetRect();
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
      p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
      y = r.y / yppu;
   }
   else {
      p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
      y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
   }

   p->Scroll(-1, y);
};

double LV2EffectDialog::GetLength()
{
   if (effect->GetEffectFlags() & INSERT_EFFECT) {
      mLength = Internat::CompatibleToDouble(mSeconds->GetValue());
   }

   return mLength;
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


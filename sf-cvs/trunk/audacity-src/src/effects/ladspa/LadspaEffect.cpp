/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.cpp

  Dominic Mazzoni
  
  This class implements a Ladspa Plug-in effect.

**********************************************************************/

#include "ladspa.h"

#include <wx/button.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "../Effect.h"          // Audacity Effect base class
#include "LadspaEffect.h"       // This class's header file

LadspaEffect::LadspaEffect(const LADSPA_Descriptor *data)
{
   mData = data;
   pluginName = data->Name;

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
      }
   }
}

wxString LadspaEffect::GetEffectName()
{
   if (numInputControls > 0)
      return pluginName + "...";
   else
      return pluginName;
}

wxString LadspaEffect::GetEffectAction()
{
   return "Performing Ladspa Effect: \""+pluginName+"\"";
}

bool LadspaEffect::Init()
{
   mBlockSize = 0;

   if (inputs > 1) {
      TrackListIterator iter(mWaveTracks);
      VTrack *left = iter.First();
      while(left) {
         sampleCount lstart, rstart, llen, rlen;
         GetSamples((WaveTrack *)left, &lstart, &llen);
         
         if (left->GetLinked()) {
            VTrack *right = iter.Next();
            GetSamples((WaveTrack *)right, &rstart, &rlen);
            
            if (llen != rlen ||
                ((WaveTrack *)left)->GetRate() !=
                ((WaveTrack *)right)->GetRate()) {
               wxMessageBox("Sorry, Ladspa Effects cannot be performed "
                            "on stereo tracks where the individual "
                            "channels of the track do not match.");
               return false;
            }
         }
         
         left = iter.Next();
      }
   }

   return true;
}

bool LadspaEffect::PromptUser()
{
   if (numInputControls > 0) {
      LadspaEffectDialog dlog(mParent, mData, inputControls);
      dlog.CentreOnParent();
      dlog.ShowModal();
      
      if (!dlog.GetReturnCode())
         return false;
   }

   return true;
}

bool LadspaEffect::Process()
{
   TrackListIterator iter(mWaveTracks);
   int count = 0;
   VTrack *left = iter.First();
   VTrack *right;
   while(left) {
      sampleCount lstart, rstart, len;
      GetSamples((WaveTrack *)left, &lstart, &len);
      
      right = NULL;
      if (left->GetLinked() && inputs>1) {
         right = iter.Next();         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      bool success;

      if (inputs == 1 && right) {
         // If the effect is mono, apply to each channel separately

         success = ProcessStereo(count, (WaveTrack *)left, NULL,
                                 lstart, 0, len);
         if (success)
            success = ProcessStereo(count, (WaveTrack *)right, NULL,
                                    rstart, 0, len);
      }
      else {
         bool success = ProcessStereo(count,
                                      (WaveTrack *)left, (WaveTrack *)right,
                                      lstart, rstart, len);
      }
         
      if (!success)
         return false;
   
      left = iter.Next();
      count++;
   }

   return true;
}

bool LadspaEffect::ProcessStereo(int count, WaveTrack *left, WaveTrack *right,
                                 sampleCount lstart, sampleCount rstart,
                                 sampleCount len)
{
   /* Allocate buffers */
   if (mBlockSize == 0) {
      mBlockSize = left->GetMaxBlockSize() * 2;

      buffer = new sampleType[mBlockSize];
      fInBuffer = new float *[inputs];
      int i;
      for (i = 0; i < inputs; i++)
         fInBuffer[i] = new float[mBlockSize];
      fOutBuffer = new float *[outputs];
      for (i = 0; i < outputs; i++)
         fOutBuffer[i] = new float[mBlockSize];
   }

   /* Instantiate the plugin */

   unsigned long rate = (unsigned long)(left->GetRate() + 0.5);
   LADSPA_Handle handle = mData->instantiate(mData, rate);

   mData->connect_port(handle, inputPorts[0], fInBuffer[0]);
   mData->connect_port(handle, outputPorts[0], fOutBuffer[0]);
   if (inputs>1 && outputs>1 && right) {
      mData->connect_port(handle, inputPorts[1], fInBuffer[1]);
      mData->connect_port(handle, outputPorts[1], fOutBuffer[1]);
   }

   unsigned long p;
   for(p=0; p<mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d)) {
         if (LADSPA_IS_PORT_INPUT(d)) {
            inputControls[p] = 0.5; //mData->PortRangeHints[p].LowerBound;
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
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   while (len) {
      int block = mBlockSize;
      int i;
      if (block > len)
         block = len;

      left->Get(buffer, ls, block);
      for (i = 0; i < block; i++)
         fInBuffer[0][i] = float (buffer[i] / 32767.);
      if (right) {
         right->Get(buffer, rs, block);
         for (i = 0; i < block; i++)
            fInBuffer[1][i] = float (buffer[i] / 32767.);
      }

      mData->run(handle, block);

      for (i = 0; i < block; i++)
         buffer[i] = (sampleType) (fOutBuffer[0][i] * 32767.);
      left->Set(buffer, ls, block);
      
      if (right) {
         for (i = 0; i < block; i++)
            buffer[i] = (sampleType) (fOutBuffer[1][i] * 32767.);
         right->Set(buffer, rs, block);
      }      

      len -= block;
      ls += block;
      rs += block;
      
      if (inputs > 1)
         TrackGroupProgress(count, (ls-lstart)/(double)originalLen);
      else
         TrackProgress(count, (ls-lstart)/(double)originalLen);
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
      int i;

      delete[]buffer;
      for (i = 0; i < inputs; i++) {
         delete fInBuffer[i];
      }
      for (i = 0; i < outputs; i++) {
         delete fOutBuffer[i];
      }

      delete[] fInBuffer;
      delete[] fOutBuffer;
      delete[] inputPorts;
      delete[] outputPorts;
      delete[] inputControls;
      delete[] outputControls;
   }
   buffer = NULL;
   fInBuffer = NULL;
   fOutBuffer = NULL;
}

const int LadspaEFFECT_SLIDER_ID = 13100;

BEGIN_EVENT_TABLE(LadspaEffectDialog, wxDialog)
    EVT_BUTTON(wxID_OK, LadspaEffectDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, LadspaEffectDialog::OnCancel)
    EVT_COMMAND_SCROLL(LadspaEFFECT_SLIDER_ID, LadspaEffectDialog::OnSlider)
    EVT_SLIDER(LadspaEFFECT_SLIDER_ID, LadspaEffectDialog::OnSlider)
END_EVENT_TABLE()

IMPLEMENT_CLASS(LadspaEffectDialog, wxDialog)

   LadspaEffectDialog::LadspaEffectDialog(wxWindow * parent,
                                          const LADSPA_Descriptor *data,
                                          float *inputControls)
      :wxDialog(parent, -1, data->Name,
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE)
{
   numParams = 0;
   this->mData = data;
   this->inputControls = inputControls;

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

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxControl *item;

   item = new wxStaticText(this, 0,
                           wxString("Author: ")+mData->Maker);
   mainSizer->Add(item, 0, wxALL, 5);
   
   if (mData->Copyright &&
       mData->Copyright[0] && 
       mData->Copyright != wxString("None")) {
      
      item = new wxStaticText(this, 0,
                              mData->Copyright);
      mainSizer->Add(item, 0, wxALL, 5);
   }

   wxSizer *paramSizer =
      new wxStaticBoxSizer(new wxStaticBox(this, -1,
                                           "Ladspa Effect Settings"),
                           wxVERTICAL );

   wxFlexGridSizer *gridSizer =
      new wxFlexGridSizer(3, 0, 0);

   for (p = 0; p < numParams; p++) {

      item = new wxStaticText(this, 0, mData->PortNames[ports[p]]);
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

      wxString fieldText;
      fieldText.Printf("%f", inputControls[ports[p]]);
      fields[p] = new wxTextCtrl(this, 0, fieldText);
      gridSizer->Add(fields[p], 0, wxALL, 5);

      sliders[p] =
          new wxSlider(this, LadspaEFFECT_SLIDER_ID,
                       0, 0, 1000,
                       wxDefaultPosition,
                       wxSize(200, -1));
      gridSizer->Add(sliders[p], 0, wxALL, 5);
   }
   
   paramSizer->Add(gridSizer, 1, wxALL, 5);
   mainSizer->Add(paramSizer, 1, wxALL, 5);

   wxBoxSizer *okSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *button;

   button = new wxButton(this, wxID_OK, "OK");
   button->SetDefault();
   button->SetFocus();
   okSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   button = new wxButton(this, wxID_CANCEL, "Cancel");
   okSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(okSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   SetAutoLayout(TRUE);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

LadspaEffectDialog::~LadspaEffectDialog()
{
   // TODO: proper disposal here

   delete[]sliders;
   delete[]labels;
}

void LadspaEffectDialog::OnSlider(wxCommandEvent & WXUNUSED(event))
{
#if 0
   for (int p = 0; p < numParams; p++) {
      float val;

      val = sliders[p]->GetValue() / 1000.;
      aEffect->setParameter(aEffect, p, val);

      char label[256];
      aEffect->dispatcher(aEffect, effGetParamDisplay, p, 0,
                          (void *) label, 0.0);
      char units[256];
      aEffect->dispatcher(aEffect, effGetParamLabel, p, 0, (void *) units,
                          0.0);
      labels[p]->SetLabel(wxString::Format("%s %s", label, units));
   }
#endif
}

void LadspaEffectDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(TRUE);
}

void LadspaEffectDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(FALSE);
}

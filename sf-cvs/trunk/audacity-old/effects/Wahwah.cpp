/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah.cpp

  Effect programming:
  Nasca Octavian Paul

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)

**********************************************************************/

#include <math.h>

#include "Wahwah.h"
#include "../WaveTrack.h"

//
// EffectWahwah
//

#define lfoskipsamples 30

EffectWahwah::EffectWahwah()
{
   freq = 1.5;
   startphase = 0;
   depth = 0.7;
   freqofs = 0.3;
   res = 2.5;
}

bool EffectWahwah::Begin(wxWindow * parent)
{
   WahwahDialog dlog(parent, -1, "Wahwah");

   dlog.freq = freq;
   dlog.freqoff = freqofs * 100;
   dlog.startphase = startphase * 180 / M_PI;
   dlog.res = res;
   dlog.depth = depth * 100;

   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   freq = dlog.freq;
   freqofs = dlog.freqoff / 100;
   startphase = dlog.startphase * M_PI / 180;
   res = dlog.res;
   depth = dlog.depth / 100;

   return true;
}

bool EffectWahwah::DoIt(WaveTrack * t, sampleCount start, sampleCount len)
{
   float samplerate = (float) (t->rate);

   /*Wahwah initialisation */
   float lfoskip = freq * 2 * 3.141592653589 / samplerate;
   unsigned long skipcount = 0;
   float xn1 = 0, xn2 = 0, yn1 = 0, yn2 = 0;
   float frequency, omega, sn, cs, alpha, b0, b1, b2, a0, a1, a2;
   float in, out;

   sampleCount s = start;
   sampleCount blockSize = t->GetIdealBlockSize();

   sampleType *buffer = new sampleType[blockSize];

   while (len) {
      int block = blockSize;
      if (block > len)
         block = len;

      t->Get(buffer, s, block);

      for (int i = 0; i < block; i++) {
         in = buffer[i];

         if ((skipcount++) % lfoskipsamples == 0) {
            frequency = (1 + cos(skipcount * lfoskip + startphase)) / 2;
            frequency = frequency * depth * (1 - freqofs) + freqofs;
            frequency = exp((frequency - 1) * 6);
            omega = 3.141592653589 * frequency;
            sn = sin(omega);
            cs = cos(omega);
            alpha = sn / (2 * res);
            b0 = (1 - cs) / 2;
            b1 = 1 - cs;
            b2 = (1 - cs) / 2;
            a0 = 1 + alpha;
            a1 = -2 * cs;
            a2 = 1 - alpha;
         };
         out = (b0 * in + b1 * xn1 + b2 * xn2 - a1 * yn1 - a2 * yn2) / a0;
         xn2 = xn1;
         xn1 = in;
         yn2 = yn1;
         yn1 = out;

         // Prevents clipping
         if (out < -32768)
            out = -32768;
         else if (out > 32767)
            out = 32767;

         buffer[i] = (sampleType) out;
      }

      t->Set(buffer, s, block);

      len -= block;
      s += block;
   }

   delete[]buffer;

   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// WahwahDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 40
#define FREQOFF_MIN 0
#define FREQOFF_MAX 100
#define PHASE_MIN 0
#define PHASE_MAX 359
#define DEPTH_MIN 0
#define DEPTH_MAX 100
#define RES_MIN 0
#define RES_MAX 100

// WDR: event table for WahwahDialog

BEGIN_EVENT_TABLE(WahwahDialog, wxDialog)
    EVT_BUTTON(wxID_OK, WahwahDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, WahwahDialog::OnCancel)
    EVT_TEXT(ID_FREQTEXT, WahwahDialog::OnFreqText)
    EVT_TEXT(ID_FREQOFFTEXT, WahwahDialog::OnFreqOffText)
    EVT_TEXT(ID_PHASETEXT, WahwahDialog::OnPhaseText)
    EVT_TEXT(ID_DEPTHTEXT, WahwahDialog::OnDepthText)
    EVT_TEXT(ID_RESONANCETEXT, WahwahDialog::OnResonanceText)
    EVT_SLIDER(ID_FREQSLIDER, WahwahDialog::OnFreqSlider)
    EVT_SLIDER(ID_FREQOFFSLIDER, WahwahDialog::OnFreqOffSlider)
    EVT_SLIDER(ID_PHASESLIDER, WahwahDialog::OnPhaseSlider)
    EVT_SLIDER(ID_DEPTHSLIDER, WahwahDialog::OnDepthSlider)
    EVT_SLIDER(ID_RESONANCESLIDER, WahwahDialog::OnResonanceSlider)
END_EVENT_TABLE()

WahwahDialog::WahwahDialog(wxWindow * parent, wxWindowID id, const wxString & title, const wxPoint & position, const wxSize & size, long style):
wxDialog(parent, id, title, position, size, style)
{
   CreateWahwahDialog(this, TRUE);
}

bool WahwahDialog::Validate()
{
   return TRUE;
}

bool WahwahDialog::TransferDataToWindow()
{
   wxTextCtrl *text = GetFreqText();
   if (text) {
      wxString str;
      str.Printf("%.1f", freq);
      text->SetValue(str);
   }

   text = GetFreqOffText();
   if (text) {
      wxString str;
      str.Printf("%d", (int) freqoff);
      text->SetValue(str);
   }

   text = GetPhaseText();
   if (text) {
      wxString str;
      str.Printf("%d", (int) startphase);
      text->SetValue(str);
   }

   text = GetDepthText();
   if (text) {
      wxString str;
      str.Printf("%d", (int) depth);
      text->SetValue(str);
   }

   text = GetResonanceText();
   if (text) {
      wxString str;
      str.Printf("%.1f", res);
      text->SetValue(str);
   }

   return TRUE;
}

bool WahwahDialog::TransferDataFromWindow()
{
   wxTextCtrl *c;
   long x;

   c = GetFreqText();
   if (c) {
      double d;
      c->GetValue().ToDouble(&d);
      freq = TrapDouble(d * 10, FREQ_MIN, FREQ_MAX) / 10;
   }

   c = GetFreqOffText();
   if (c) {
      double d;
      c->GetValue().ToDouble(&d);
      freqoff = TrapDouble(d, FREQOFF_MIN, FREQOFF_MAX);
   }

   c = GetPhaseText();
   if (c) {
      c->GetValue().ToLong(&x);
      startphase = TrapLong(x, PHASE_MIN, PHASE_MAX);
   }

   c = GetDepthText();
   if (c) {
      c->GetValue().ToLong(&x);
      depth = TrapLong(x, DEPTH_MIN, DEPTH_MAX);
   }

   c = GetResonanceText();
   if (c) {
      double d;
      c->GetValue().ToDouble(&d);
      res = TrapDouble(d * 10, RES_MIN, RES_MAX) / 10;
   }

   return TRUE;
}

// WDR: handler implementations for WahwahDialog

void WahwahDialog::OnResonanceSlider(wxCommandEvent & event)
{
   wxString str;
   long res = GetResonanceSlider()->GetValue();
   str.Printf("%.1f", res / 10.0);
   GetResonanceText()->SetValue(str);
}

void WahwahDialog::OnDepthSlider(wxCommandEvent & event)
{
   wxString str;
   long depth = GetDepthSlider()->GetValue();
   str.Printf("%d", depth);
   GetDepthText()->SetValue(str);
}

void WahwahDialog::OnPhaseSlider(wxCommandEvent & event)
{
   wxString str;
   long phase = GetPhaseSlider()->GetValue();
   phase = ((phase + 5) / 10) * 10;     // round to nearest multiple of 10
   str.Printf("%d", phase);
   GetPhaseText()->SetValue(str);
}

void WahwahDialog::OnFreqSlider(wxCommandEvent & event)
{
   wxString str;
   long freql = GetFreqSlider()->GetValue();
   str.Printf("%.1f", freql / 10.0);
   GetFreqText()->SetValue(str);
}

void WahwahDialog::OnFreqOffSlider(wxCommandEvent & event)
{
   wxString str;
   long freqoff = GetFreqOffSlider()->GetValue();
   str.Printf("%d", (int) freqoff);
   GetFreqOffText()->SetValue(str);
}

void WahwahDialog::OnResonanceText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetResonanceText();
   if (c) {
      double resd;

      c->GetValue().ToDouble(&resd);
      res = resd;
      res = TrapDouble(resd * 10, RES_MIN, RES_MAX) / 10.0;

      wxSlider *slider = GetResonanceSlider();
      if (slider)
         slider->SetValue(res * 10);
   }
}

void WahwahDialog::OnDepthText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetDepthText();
   if (c) {
      long depth;

      c->GetValue().ToLong(&depth);
      depth = TrapLong(depth, DEPTH_MIN, DEPTH_MAX);

      wxSlider *slider = GetDepthSlider();
      if (slider)
         slider->SetValue(depth);
   }
}

void WahwahDialog::OnPhaseText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetPhaseText();
   if (c) {
      long phase;

      c->GetValue().ToLong(&phase);
      phase = TrapLong(phase, PHASE_MIN, PHASE_MAX);

      wxSlider *slider = GetPhaseSlider();
      if (slider)
         slider->SetValue(phase);
   }
}

void WahwahDialog::OnFreqText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetFreqText();
   if (c) {
      long freql;
      double freqd;

      c->GetValue().ToDouble(&freqd);
      freq = freqd;
      freql = TrapLong(((long) (freq * 10)), FREQ_MIN, FREQ_MAX);

      wxSlider *slider = GetFreqSlider();
      if (slider)
         slider->SetValue(freql);
   }
}

void WahwahDialog::OnFreqOffText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetFreqOffText();
   if (c) {
      double freqoff;

      c->GetValue().ToDouble(&freqoff);
      freqoff = TrapDouble(freqoff, FREQOFF_MIN, FREQOFF_MAX);

      wxSlider *slider = GetFreqOffSlider();
      if (slider)
         slider->SetValue(freqoff);
   }
}

void WahwahDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void WahwahDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

// Implement window functions

wxSizer *CreateWahwahDialog(wxPanel * parent, bool call_fit,
                            bool set_sizer)
{
   wxBoxSizer *item0 = new wxBoxSizer(wxVERTICAL);

   wxStaticText *item1 =
       new wxStaticText(parent, ID_TEXT, "Wahwah by Nasca Octavian Paul",
                        wxDefaultPosition, wxDefaultSize, 0);
   item0->Add(item1, 0, wxALIGN_CENTRE | wxALL, 5);

   wxFlexGridSizer *item10 = new wxFlexGridSizer(3, 0, 0);

   wxStaticText *item11 =
       new wxStaticText(parent, ID_TEXT, "LFO Frequency (Hz):",
                        wxDefaultPosition, wxDefaultSize, 0);
   item10->Add(item11, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
               5);

   wxTextCtrl *item12 =
       new wxTextCtrl(parent, ID_FREQTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item10->Add(item12, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item13 =
       new wxSlider(parent, ID_FREQSLIDER, 100, FREQ_MIN, FREQ_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   item10->Add(item13, 0, wxALIGN_CENTRE | wxALL, 5);

   wxStaticText *item14 =
       new wxStaticText(parent, ID_TEXT, "LFO Start Phase (deg.):",
                        wxDefaultPosition, wxDefaultSize, 0);
   item10->Add(item14, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
               5);

   wxTextCtrl *item15 =
       new wxTextCtrl(parent, ID_PHASETEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item10->Add(item15, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item16 =
       new wxSlider(parent, ID_PHASESLIDER, 0, PHASE_MIN, PHASE_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   item10->Add(item16, 0, wxALIGN_CENTRE | wxALL, 5);

   wxStaticText *item17 =
       new wxStaticText(parent, ID_TEXT, "Depth (%):", wxDefaultPosition,
                        wxDefaultSize, wxALIGN_RIGHT);
   item10->Add(item17, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
               5);

   wxTextCtrl *item18 =
       new wxTextCtrl(parent, ID_DEPTHTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item10->Add(item18, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item19 =
       new wxSlider(parent, ID_DEPTHSLIDER, 0, DEPTH_MIN, DEPTH_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   item10->Add(item19, 0, wxALIGN_CENTRE | wxALL, 5);

   wxStaticText *item20 =
       new wxStaticText(parent, ID_TEXT, "Resonance:", wxDefaultPosition,
                        wxDefaultSize, wxALIGN_RIGHT);
   item10->Add(item20, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
               5);

   wxTextCtrl *item21 =
       new wxTextCtrl(parent, ID_RESONANCETEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item10->Add(item21, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item22 =
       new wxSlider(parent, ID_RESONANCESLIDER, 0, RES_MIN, RES_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   item10->Add(item22, 0, wxALIGN_CENTRE | wxALL, 5);

   wxStaticText *item30 =
       new wxStaticText(parent, ID_TEXT, "Wah Frequency Offset (%):",
                        wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT);
   item10->Add(item30, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
               5);

   wxTextCtrl *item31 =
       new wxTextCtrl(parent, ID_FREQOFFTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item10->Add(item31, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item32 =
       new wxSlider(parent, ID_FREQOFFSLIDER, 0, FREQOFF_MIN, FREQOFF_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   item10->Add(item32, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *item23 = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item24 =
       new wxButton(parent, wxID_OK, "OK", wxDefaultPosition,
                    wxDefaultSize, 0);
   item24->SetDefault();
   item23->Add(item24, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item25 =
       new wxButton(parent, wxID_CANCEL, "Cancel", wxDefaultPosition,
                    wxDefaultSize, 0);
   item23->Add(item25, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item23, 0, wxALIGN_CENTRE | wxALL, 5);

   if (set_sizer) {
      parent->SetAutoLayout(TRUE);
      parent->SetSizer(item0);
      if (call_fit) {
         item0->Fit(parent);
         item0->SetSizeHints(parent);
      }
   }

   return item0;
}

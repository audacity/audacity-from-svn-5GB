/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Dominic Mazzoni

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

**********************************************************************/

#include <math.h>
#include <wx/intl.h>

#include "Amplify.h"
#include "../WaveTrack.h"

//
// EffectAmplify
//

EffectAmplify::EffectAmplify()
{
   ratio = 1.0;
}

bool EffectAmplify::Init()
{
   peak = 0.0;

   TrackListIterator iter(mWaveTracks);
   VTrack *t = iter.First();
   int count = 0;
   while(t) {
      sampleCount start, len;
      float min, max;
      GetSamples((WaveTrack *)t, &start, &len);
      ((WaveTrack *)t)->GetMinMax(start, len, &min, &max);
      float newpeak = (fabs(min) > fabs(max) ? fabs(min) : fabs(max));
      
      if (newpeak > peak)
         peak = newpeak;
   
      t = iter.Next();
      count++;
   }
   
   return true;

}

bool EffectAmplify::PromptUser()
{
   AmplifyDialog dlog(mParent, -1, _("Amplify"));
   dlog.peak = peak;
   dlog.ratio = 1.0 / peak;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   ratio = dlog.ratio;

   return true;
}

bool EffectAmplify::Process()
{
   TrackListIterator iter(mWaveTracks);
   VTrack *t = iter.First();
   int count = 0;
   while(t) {
      sampleCount start, len;
      GetSamples((WaveTrack *)t, &start, &len);
      bool success = ProcessOne(count, (WaveTrack *)t, start, len);
      
      if (!success)
         return false;
   
      t = iter.Next();
      count++;
   }
   
   return true;
}

bool EffectAmplify::ProcessOne(int count, WaveTrack *t,
                               sampleCount start, sampleCount len)
{
   sampleCount s = start;
   sampleCount originalLen = len;
   sampleCount blockSize = t->GetMaxBlockSize();

   float *buffer = new float[blockSize];
   
   while (len) {
      sampleCount block = t->GetBestBlockSize(s);
      if (block > len)
         block = len;

      t->Get(buffer, s, block);
      for (int i = 0; i < block; i++) {
         buffer[i] = (buffer[i] * ratio);
      }
      t->Set(buffer, s, block);

      len -= block;
      s += block;
      
      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[] buffer;

   return true;
}

//----------------------------------------------------------------------------
// AmplifyDialog
//----------------------------------------------------------------------------

#define AMP_MIN -240
#define AMP_MAX 240

// WDR: event table for AmplifyDialog

BEGIN_EVENT_TABLE(AmplifyDialog, wxDialog)
    EVT_BUTTON(wxID_OK, AmplifyDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, AmplifyDialog::OnCancel)
    EVT_SLIDER(ID_AMP_SLIDER, AmplifyDialog::OnAmpSlider)
    EVT_TEXT(ID_AMP_TEXT, AmplifyDialog::OnAmpText)
    EVT_TEXT(ID_PEAK_TEXT, AmplifyDialog::OnPeakText)
    EVT_CHECKBOX(ID_CLIP_CHECKBOX, AmplifyDialog::OnClipCheckBox)
END_EVENT_TABLE()

AmplifyDialog::AmplifyDialog(wxWindow * parent, wxWindowID id, const wxString & title, const wxPoint & position, const wxSize & size, long style):
wxDialog(parent, id, title, position, size, style)
{
   ratio = 1.0;
   peak = 0.0;
   mLoopDetect = false;

   MakeAmplifyDialog(this, TRUE, TRUE);
}

bool AmplifyDialog::Validate()
{
   return TRUE;
}

bool AmplifyDialog::TransferDataToWindow()
{
   wxSlider *slider;

   slider = GetAmpSlider();
   if (slider)
      slider->SetValue((int)(100*log10(ratio)+0.5));

   mLoopDetect = true;

   wxTextCtrl *text = GetAmpText();
   if (text) {
      wxString str;
      str.Printf("%.1f", 10*log10(ratio));
      text->SetValue(str);
   }

   text = GetPeakText();
   if (text) {
      wxString str;
      str.Printf("%.1f", 10*log10(ratio*peak));
      text->SetValue(str);
   }

   mLoopDetect = false;

   return TRUE;
}

bool AmplifyDialog::TransferDataFromWindow()
{
   wxTextCtrl *c = GetAmpText();
   if (c) {
      double r;

      wxString val = c->GetValue();
      val.ToDouble(&r);
      ratio = pow(10,TrapDouble(r*10, AMP_MIN, AMP_MAX)/100.0);
   }

   return TRUE;
}

// WDR: handler implementations for AmplifyDialog

void AmplifyDialog::OnAmpText(wxCommandEvent & event)
{
   if (mLoopDetect)
      return;

   wxTextCtrl *c = GetAmpText();
   if (c) {
      double r;

      mLoopDetect = true;

      wxString val = c->GetValue();
      val.ToDouble(&r);
      ratio = pow(10,TrapDouble(r*10, AMP_MIN, AMP_MAX)/100.0);

      wxSlider *slider = GetAmpSlider();
      if (slider)
         slider->SetValue((int)(100*log10(ratio)+0.5));

      wxString str;
      str.Printf("%.1f", 10*log10(ratio*peak));
      GetPeakText()->SetValue(str);

      mLoopDetect = false;
   }
   
   CheckClip();
}

void AmplifyDialog::OnPeakText(wxCommandEvent & event)
{
   if (mLoopDetect)
      return;

   wxTextCtrl *c = GetPeakText();
   if (c) {
      double r;

      mLoopDetect = true;

      wxString val = c->GetValue();
      val.ToDouble(&r);

      ratio = pow(10, r/10.0) / peak;
      
      double dB = TrapDouble(100*log10(ratio), AMP_MIN, AMP_MAX)/10.0;
      ratio = pow(10, dB/10.0);

      wxSlider *slider = GetAmpSlider();
      if (slider)
         slider->SetValue((int)(10*dB+0.5));
      
      wxString str;
      str.Printf("%.1f", dB);
      GetAmpText()->SetValue(str);
      
      mLoopDetect = false;
   }
   
   CheckClip();
}

void AmplifyDialog::OnAmpSlider(wxCommandEvent & event)
{
   if (mLoopDetect)
      return;

   mLoopDetect = true;

   wxString str;
   double dB = GetAmpSlider()->GetValue() / 10.0;
   ratio = pow(10,TrapDouble(dB, AMP_MIN, AMP_MAX)/10.0);
   
   double dB2 = (GetAmpSlider()->GetValue()-1) / 10.0;
   double ratio2 = pow(10,TrapDouble(dB2, AMP_MIN, AMP_MAX)/10.0);

   if (GetClipCheckBox()->GetValue() &&
       ratio * peak > 1.0 &&
       ratio2 * peak < 1.0)
      ratio = 1.0 / peak;
   
   str.Printf("%.1f", 10*log10(ratio));
   GetAmpText()->SetValue(str);
   str.Printf("%.1f", 10*log10(ratio*peak));
   GetPeakText()->SetValue(str);

   mLoopDetect = false;

   CheckClip();
}

void AmplifyDialog::OnClipCheckBox(wxCommandEvent & event)
{
   CheckClip();
}

void AmplifyDialog::CheckClip()
{
   if (GetClipCheckBox()->GetValue() == true) {
      GetOK()->Enable(ratio * peak <= 1.0);
   }
   else {
      GetOK()->Enable(true);
   }
}

void AmplifyDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   if (GetClipCheckBox()->GetValue() == true) {
     if (ratio * peak > 1.0)
        ratio = 1.0 / peak;
   }

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void AmplifyDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

wxSizer *MakeAmplifyDialog(wxWindow * parent, bool call_fit,
                             bool set_sizer)
{
   wxBoxSizer *item0 = new wxBoxSizer(wxVERTICAL);

   wxStaticText *item1 =
       new wxStaticText(parent, ID_TEXT,
                        _("Amplify by Dominic Mazzoni"),
                        wxDefaultPosition, wxDefaultSize, 0);
   item0->Add(item1, 0, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *item2 = new wxBoxSizer(wxHORIZONTAL);

   item2->Add( 10, 10, 1, wxGROW | wxALL, 5 );
   
   wxStaticText *item3 =
       new wxStaticText(parent, ID_TEXT,
                        _("Amplification (dB):"),
                        wxDefaultPosition, wxDefaultSize, 0);
   item2->Add(item3, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

   wxTextCtrl *item4 =
       new wxTextCtrl(parent, ID_AMP_TEXT, "", wxDefaultPosition,
                      wxSize(60, -1), 0);
   item2->Add(item4, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

   item0->Add(item2, 0, wxGROW | wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item5 =
       new wxSlider(parent, ID_AMP_SLIDER, 0, AMP_MIN, AMP_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   item0->Add(item5, 1, wxGROW | wxALIGN_CENTRE | wxLEFT | wxRIGHT, 5);

   wxBoxSizer *item6 = new wxBoxSizer(wxHORIZONTAL);
   
   item6->Add( 10, 10, 1, wxGROW | wxALL, 5 );
   
   wxStaticText *item7 =
       new wxStaticText(parent, ID_TEXT,
                        _("New Peak Amplitude (dB):"),
                        wxDefaultPosition, wxDefaultSize, 0);
   item6->Add(item7, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

   wxTextCtrl *item8 =
       new wxTextCtrl(parent, ID_PEAK_TEXT, "", wxDefaultPosition,
                      wxSize(60, -1), 0);
   item6->Add(item8, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

   item0->Add(item6, 0, wxGROW | wxALIGN_CENTRE | wxALL, 5);

   wxCheckBox *item8b = new wxCheckBox(parent, ID_CLIP_CHECKBOX,
                        _("Don't allow clipping"),
                        wxDefaultPosition, wxDefaultSize, 0);
   item8b->SetValue(true);

   item0->Add(item8b, 0, wxALIGN_LEFT | wxALL, 5);

   wxBoxSizer *item9 = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item10 =
       new wxButton(parent, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item10->SetDefault();
   item10->SetFocus();
   item9->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item11 =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item9->Add(item11, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item9, 0, wxALIGN_CENTRE | wxALL, 5);

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



/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.cpp

  Steve Jolly

  This class implements a tone generator effect.

**********************************************************************/

#include "ToneGen.h"
#include "../WaveTrack.h"

//
// EffectToneGen
//

EffectToneGen::EffectToneGen()
{
   frequency = 1000.0;          //Hz
   waveform = 0;                //sine
   amplitude = 1.0;
   mix = false;
}

bool EffectToneGen::NewTrackSimpleMono()
{
   mSample = 0; //used to keep track of the current sample number
   //between calls to EffectToneGen::ProcessSimpleMono
   //to avoid phase jumps at block boundaries
   return true;
}

bool EffectToneGen::PromptUser()
{
   ToneGenDialog dlog(mParent, -1, _("Tone Generator"));
   dlog.frequency = frequency;
   dlog.waveform = waveform;
   dlog.amplitude = amplitude;
   dlog.mix = mix;
   dlog.GetWaveformChoice()->Append(_("Sine"));
   dlog.GetWaveformChoice()->Append(_("Square"));
   dlog.GetWaveformChoice()->Append(_("Sawtooth"));
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == 0)
      return false;

   frequency = dlog.frequency;
   waveform = dlog.waveform;
   amplitude = dlog.amplitude;
   mix = dlog.mix;

   return true;
}

bool EffectToneGen::ProcessSimpleMono(float *buffer, sampleCount len)
{
   double throwaway = 0;        //passed to modf but never used
   int i;

   switch (waveform) {
   case 0:                     //sine
      if (mix)
         for (i = 0; i < len; i++)
            buffer[i] =
                (buffer[i] +
                 amplitude * (float) sin(2 * M_PI * (i + mSample) * frequency / mCurRate)) / 2;
      else
         for (i = 0; i < len; i++)
            buffer[i] =
                amplitude * (float) sin(2 * M_PI * (i + mSample) * frequency / mCurRate);
      mSample += len;
      break;

   case 1:                     //square
      if (mix)
         for (i = 0; i < len; i++) {
            if (modf(((i + mSample) * frequency / mCurRate), &throwaway) < 0.5)
               buffer[i] = (buffer[i] + amplitude) / 2;
            else
               buffer[i] = (buffer[i] - amplitude) / 2;
      } else
         for (i = 0; i < len; i++) {
            if (modf(((i + mSample) * frequency / mCurRate), &throwaway) < 0.5)
               buffer[i] = amplitude;
            else
               buffer[i] = -amplitude;
         }
      mSample += len;
      break;

   case 2:                     //sawtooth
      if (mix)
         for (i = 0; i < len; i++)
            buffer[i] = amplitude * ((((i + mSample) % (int) (mCurRate / frequency)) / (mCurRate / frequency)) - 0.5) + buffer[i] / 2;      //hmm
      else
         for (i = 0; i < len; i++)
            buffer[i] = 2 * amplitude * ((((i + mSample) % (int) (mCurRate / frequency)) / (mCurRate / frequency)) - 0.5);
      mSample += len;
      break;

   default:
      break;
   }
   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// ToneGenDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 20000
#define AMP_MIN 0
#define AMP_MAX 1
#define WAVEFORM_MIN 0
#define WAVEFORM_MAX 2

// WDR: event table for PhaserDialog

BEGIN_EVENT_TABLE(ToneGenDialog, wxDialog)
    EVT_BUTTON(wxID_OK, ToneGenDialog::OnCreateTone)
EVT_BUTTON(wxID_CANCEL, ToneGenDialog::OnCancel)
//   EVT_TEXT(ID_FREQTEXT, ToneGenDialog::OnFreqText)
//   EVT_CHOICE(ID_WAVEFORM, ToneGenDialog::OnWaveformChoice)
END_EVENT_TABLE()

ToneGenDialog::ToneGenDialog(wxWindow * parent, wxWindowID id, const wxString & title, const wxPoint & position, const wxSize & size, long style):
wxDialog(parent, id, title, position, size, style)
{
   CreateToneGenDialog(this, TRUE);

}

bool ToneGenDialog::Validate()
{
   return TRUE;
}

bool ToneGenDialog::TransferDataToWindow()
{
   wxChoice *choice;
   wxTextCtrl *text;
   wxCheckBox *checkbox;

   choice = GetWaveformChoice();
   if (choice)
      choice->SetSelection(waveform);

   text = GetFreqText();
   if (text) {
      wxString str;
      str.Printf("%.2f", frequency);
      text->SetValue(str);
   }

   text = GetAmpText();
   if (text) {
      wxString str;
      str.Printf("%.2f", amplitude);
      text->SetValue(str);
   }

   checkbox = GetMixChoice();
   if (checkbox)
      checkbox->SetValue(mix);

   return TRUE;
}

bool ToneGenDialog::TransferDataFromWindow()
{
   wxTextCtrl *t;

   t = GetAmpText();
   if (t) {
      double d;
      t->GetValue().ToDouble(&d);
      amplitude = TrapDouble(d, AMP_MIN, AMP_MAX);
   }

   t = GetFreqText();
   if (t) {
      double d;
      t->GetValue().ToDouble(&d);
      frequency = TrapDouble(d, FREQ_MIN, FREQ_MAX);
   }

   wxChoice *c = GetWaveformChoice();
   if (c)
      waveform = TrapLong(c->GetSelection(), WAVEFORM_MIN, WAVEFORM_MAX);

   wxCheckBox *cb = GetMixChoice();
   if (cb)
      mix = cb->GetValue();

   return TRUE;
}

// WDR: handler implementations for ToneGenDialog

void ToneGenDialog::OnCreateTone(wxCommandEvent & event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void ToneGenDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

wxSizer *CreateToneGenDialog(wxWindow * parent, bool call_fit,
                             bool set_sizer)
{
   wxBoxSizer *item0 = new wxBoxSizer(wxVERTICAL);


   wxBoxSizer *item2 = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item3 =
       new wxStaticText(parent, ID_TEXT, _("Waveform:"), wxDefaultPosition,
                        wxDefaultSize, 0);
   item2->Add(item3, 0, wxALIGN_CENTRE | wxALL, 5);

   wxChoice *item4 = new wxChoice(parent, ID_WAVEFORM, wxDefaultPosition,
                                  wxSize(80, -1), 0, NULL);
   item2->Add(item4, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item2, 1, wxALIGN_CENTRE | wxALL, 5);


   wxBoxSizer *item5 = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item6 =
       new wxStaticText(parent, ID_TEXT, _("Frequency / Hz"),
                        wxDefaultPosition,
                        wxDefaultSize, 0);
   item5->Add(item6, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item7 =
       new wxTextCtrl(parent, ID_FREQTEXT, "", wxDefaultPosition,
                      wxSize(60, -1), 0);
   item5->Add(item7, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item5, 1, wxALIGN_CENTRE | wxALL, 5);


   wxBoxSizer *item8 = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item9 =
       new wxStaticText(parent, ID_TEXT, _("Amplitude (0-1)"),
                        wxDefaultPosition,
                        wxDefaultSize, 0);
   item8->Add(item9, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item10 =
       new wxTextCtrl(parent, ID_AMPTEXT, "", wxDefaultPosition,
                      wxSize(60, -1), 0);
   item8->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item8, 1, wxALIGN_CENTRE | wxALL, 5);


   wxBoxSizer *item14 = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item15 =
       new wxStaticText(parent, ID_TEXT, _("Mix Output"),
                        wxDefaultPosition,
                        wxDefaultSize, 0);
   item14->Add(item15, 0, wxALIGN_CENTRE | wxALL, 5);

   wxCheckBox *item16 =
       new wxCheckBox(parent, ID_MIX, "", wxDefaultPosition,
                      wxDefaultSize, 0);
   item14->Add(item16, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item14, 0, wxALIGN_CENTRE | wxALL, 5);


   wxBoxSizer *item11 = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item12 =
       new wxButton(parent, wxID_OK, _("Create Tone"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item12->SetDefault();
   item11->Add(item12, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item13 =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item11->Add(item13, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item11, 0, wxALIGN_CENTRE | wxALL, 5);

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

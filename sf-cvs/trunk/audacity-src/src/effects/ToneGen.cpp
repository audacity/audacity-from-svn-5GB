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
}

bool EffectToneGen::PromptUser()
{
   ToneGenDialog dlog(mParent, -1, _("Tone Generator"));
   dlog.frequency = frequency;
   dlog.waveform = waveform;
   dlog.amplitude = amplitude;
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

   return true;
}

bool EffectToneGen::Process()
{
   TrackListIterator iter(mWaveTracks);
   VTrack *t = iter.First();
   int count = 0;
   while (t) {
      sampleCount start, len;
      GetSamples((WaveTrack *) t, &start, &len);
      bool success = ProcessOne(count, (WaveTrack *) t, start, len);

      if (!success)
         return false;

      t = iter.Next();
      count++;
   }

   return true;
}

bool EffectToneGen::ProcessOne(int count, WaveTrack * t,
                               sampleCount start, sampleCount len)
{
   int i;
   float *buffer = new float[len];
   double samplerate = t->GetRate();
   double throwaway = 0;        //passed to modf but never used
   float increment = 2 * amplitude * frequency / samplerate;

   switch (waveform) {
   case 0:                     //sine
      for (i = 0; i < len; i++) {
         buffer[i] =
             amplitude * (float) sin(2 * M_PI * i * frequency /
                                     samplerate);
      }
      break;
   case 1:                     //square
      for (i = 0; i < len; i++) {
         if (modf((i * frequency / samplerate), &throwaway) < 0.5)
            buffer[i] = amplitude;
         else
            buffer[i] = -amplitude;
      }
      break;
   case 2:                     //sawtooth
      buffer[0] = 0;
      for (i = 1; i < len; i++) {
         if ((buffer[i - 1] + increment) > amplitude)
            buffer[i] = -amplitude;
         else
            buffer[i] = buffer[i - 1] + increment;
      }
      break;
   default:
      break;
   }

   t->Set(buffer, start, len);

   delete[]buffer;

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
   if (c) {
      waveform = TrapLong(c->GetSelection(), WAVEFORM_MIN, WAVEFORM_MAX);
   }

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

   item0->Add(item2, 1, wxGROW | wxALIGN_CENTRE | wxALL, 5);


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


   wxBoxSizer *item23 = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item24 =
       new wxButton(parent, wxID_OK, _("Create Tone"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item24->SetDefault();
   item23->Add(item24, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item25 =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
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

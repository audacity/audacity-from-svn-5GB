/**********************************************************************

  Audacity: A Digital Audio Editor

  BassBoost.cpp

  Effect programming:
  Nasca Octavian Paul

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)

**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>

#include "BassBoost.h"
#include "../WaveTrack.h"

//
// EffectBassBoost
//

EffectBassBoost::EffectBassBoost()
{
   frequency = 200;
   dB_boost = 12;
}

bool EffectBassBoost::PromptUser()
{
   BassBoostDialog dlog(mParent, -1, "BassBoost");
   dlog.freq = frequency;
   dlog.boost = dB_boost;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   frequency = dlog.freq;
   dB_boost = dlog.boost;

   return true;
}

bool EffectBassBoost::Process()
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

bool EffectBassBoost::ProcessOne(int count, WaveTrack * t,
                                 sampleCount start, sampleCount len)
{
   float samplerate = (float) (t->rate);

   /* Compute coefficents of the biquand IIR filter */
   float omega = 2 * 3.141592653589 * frequency / samplerate;
   float sn = sin(omega);
   float cs = cos(omega);
   float a = exp(log(10) * dB_boost / 40);
   float shape = 1.0;           /*Low Shelf filter's shape, if this is too large
                                   or too small it will result an unstable filter */
   float beta = sqrt((a * a + 1) / shape - (pow((a - 1), 2)));
   /*  Coefficients  */
   float b0 = a * ((a + 1) - (a - 1) * cs + beta * sn);
   float b1 = 2 * a * ((a - 1) - (a + 1) * cs);
   float b2 = a * ((a + 1) - (a - 1) * cs - beta * sn);
   float a0 = ((a + 1) + (a - 1) * cs + beta * sn);
   float a1 = -2 * ((a - 1) + (a + 1) * cs);
   float a2 = (a + 1) + (a - 1) * cs - beta * sn;
   /* initialise the filter */
   float xn1 = 0, xn2 = 0, yn1 = 0, yn2 = 0;

   float out, in = 0;

   sampleCount s = start;
   sampleCount originalLen = len;
   sampleCount blockSize = t->GetMaxBlockSize();

   sampleType *buffer = new sampleType[blockSize];

   while (len) {
      unsigned int block = t->GetBestBlockSize(s);
      if (block > len)
         block = len;

      t->Get(buffer, s, block);

      for (unsigned int i = 0; i < block; i++) {
         in = buffer[i];
         out = (b0 * in + b1 * xn1 + b2 * xn2 - a1 * yn1 - a2 * yn2) / a0;
         xn2 = xn1;
         xn1 = in;
         yn2 = yn1;
         yn1 = out;

         if (out < -32768)
            out = -32768;
         else if (out > 32767)
            out = 32767;        //Prevents clipping

         buffer[i] = (sampleType) out;
      }

      t->Set(buffer, s, block);
      
      len -= block;
      s += block;
      
      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[]buffer;

   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// BassBoostDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 1000
#define BOOST_MIN 0
#define BOOST_MAX 36

// WDR: event table for BassBoostDialog

BEGIN_EVENT_TABLE(BassBoostDialog, wxDialog)
    EVT_BUTTON(wxID_OK, BassBoostDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, BassBoostDialog::OnCancel)
    EVT_SLIDER(ID_FREQ_SLIDER, BassBoostDialog::OnFreqSlider)
    EVT_SLIDER(ID_BOOST_SLIDER, BassBoostDialog::OnBoostSlider)
    EVT_TEXT(ID_FREQ_TEXT, BassBoostDialog::OnFreqText)
    EVT_TEXT(ID_BOOST_TEXT, BassBoostDialog::OnBoostText)
END_EVENT_TABLE()

BassBoostDialog::BassBoostDialog(wxWindow * parent, wxWindowID id, const wxString & title, const wxPoint & position, const wxSize & size, long style):
wxDialog(parent, id, title, position, size, style)
{
   freq = 200;
   boost = 12;

   MakeBassBoostDialog(this, TRUE, TRUE);
}

bool BassBoostDialog::Validate()
{
   return TRUE;
}

bool BassBoostDialog::TransferDataToWindow()
{
   wxSlider *slider;

   slider = GetBoostSlider();
   if (slider)
      slider->SetValue(boost);

   slider = GetFreqSlider();
   if (slider)
      slider->SetValue(freq);

   wxTextCtrl *text = GetBoostText();
   if (text) {
      wxString str;
      str.Printf("%d", (int) boost);
      text->SetValue(str);
   }

   text = GetFreqText();
   if (text) {
      wxString str;
      str.Printf("%d", (int) freq);
      text->SetValue(str);
   }

   return TRUE;
}

bool BassBoostDialog::TransferDataFromWindow()
{
   wxTextCtrl *c = GetBoostText();
   if (c) {
      long b;

      wxString val = c->GetValue();
      val.ToLong(&b);
      boost = TrapLong(b, BOOST_MIN, BOOST_MAX);
   }

   c = GetFreqText();
   if (c) {
      long f;

      wxString val = c->GetValue();
      val.ToLong(&f);
      freq = TrapLong(f, FREQ_MIN, FREQ_MAX);
   }

   return TRUE;
}

// WDR: handler implementations for BassBoostDialog

void BassBoostDialog::OnBoostText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetBoostText();
   if (c) {
      long boost;

      c->GetValue().ToLong(&boost);
      freq = TrapLong(boost, BOOST_MIN, BOOST_MAX);

      if (boost < BOOST_MIN)
         boost = BOOST_MIN;
      else if (boost > BOOST_MAX)
         boost = BOOST_MAX;

      wxSlider *slider = GetBoostSlider();
      if (slider)
         slider->SetValue(boost);
   }
}

void BassBoostDialog::OnFreqText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetFreqText();
   if (c) {
      long freq;

      c->GetValue().ToLong(&freq);
      freq = TrapLong(freq, FREQ_MIN, FREQ_MAX);

      wxSlider *slider = GetFreqSlider();
      if (slider)
         slider->SetValue(freq);
   }
}

void BassBoostDialog::OnBoostSlider(wxCommandEvent & event)
{
   wxString str;
   str.Printf("%d", GetBoostSlider()->GetValue());
   GetBoostText()->SetValue(str);
}

void BassBoostDialog::OnFreqSlider(wxCommandEvent & event)
{
   wxString str;
   long freq = GetFreqSlider()->GetValue();
   freq = ((freq + 5) / 10) * 10;       // round to nearest multiple of 10
   str.Printf("%d", freq);
   GetFreqText()->SetValue(str);
}

void BassBoostDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void BassBoostDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

wxSizer *MakeBassBoostDialog(wxPanel * parent, bool call_fit,
                             bool set_sizer)
{
   wxBoxSizer *item0 = new wxBoxSizer(wxVERTICAL);

   wxStaticText *item1 =
       new wxStaticText(parent, ID_TEXT,
                        "BassBoost by Nasca Octavian Paul",
                        wxDefaultPosition, wxDefaultSize, 0);
   item0->Add(item1, 0, wxALIGN_CENTRE | wxALL, 5);

   wxFlexGridSizer *item2 = new wxFlexGridSizer(3, 0, 0);

   wxStaticText *item3 =
       new wxStaticText(parent, ID_TEXT, "Frequency (Hz):",
                        wxDefaultPosition, wxDefaultSize, 0);
   item2->Add(item3, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item4 =
       new wxTextCtrl(parent, ID_FREQ_TEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item2->Add(item4, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item5 =
       new wxSlider(parent, ID_FREQ_SLIDER, 0, FREQ_MIN, FREQ_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   //item5->SetValue(200);
   item2->Add(item5, 0, wxALIGN_CENTRE | wxALL, 5);

   wxStaticText *item6 =
       new wxStaticText(parent, ID_TEXT, "Boost (dB):", wxDefaultPosition,
                        wxDefaultSize, 0);
   item2->Add(item6, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item7 =
       new wxTextCtrl(parent, ID_BOOST_TEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item2->Add(item7, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item8 =
       new wxSlider(parent, ID_BOOST_SLIDER, 0, BOOST_MIN, BOOST_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   //item8->SetValue(12);
   item2->Add(item8, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item2, 0, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *item9 = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item10 =
       new wxButton(parent, wxID_OK, "OK", wxDefaultPosition,
                    wxDefaultSize, 0);
   item10->SetDefault();
   item10->SetFocus();
   item9->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item11 =
       new wxButton(parent, wxID_CANCEL, "Cancel", wxDefaultPosition,
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

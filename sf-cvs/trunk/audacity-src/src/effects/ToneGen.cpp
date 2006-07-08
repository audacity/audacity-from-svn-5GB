/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.cpp

  Steve Jolly

  This class implements a tone generator effect.

*******************************************************************//**

\class EffectToneGen
\brief An Effect.

*//****************************************************************//**

\class ToneGenDialog
\brief Dialog used with EffectToneGen

*//*******************************************************************/


#include "../Audacity.h"

#include "ToneGen.h"
#include "../FFT.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/textctrl.h>

#include <math.h>

//
// EffectToneGen
//

EffectToneGen::EffectToneGen()
{
   frequency = float(440.0);          //Hz
   waveform = 0;                //sine
   amplitude = float(1.0);
   length = sDefaultGenerateLen;
}

wxString EffectToneGen::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   const wxChar* waveformNames[] = {wxT("sine"), wxT("square"), wxT("sawtooth")};
   return wxString::Format(_("Applied effect: Generate %s wave tone, frequency = %.2f Hz, amplitude = %.2f, %.6lf seconds"), 
                           waveformNames[waveform], frequency, amplitude, length); 
} 

bool EffectToneGen::PromptUser()
{
   wxArrayString waveforms;
   if (mT1 > mT0)
      length = mT1 - mT0;

   waveforms.Add(_("Sine"));
   waveforms.Add(_("Square"));
   waveforms.Add(_("Sawtooth"));

   ToneGenDialog dlog(mParent, _("Tone Generator"));
   dlog.frequency = frequency;
   dlog.waveform = waveform;
   dlog.amplitude = amplitude;
   dlog.length = length;
   dlog.waveforms = &waveforms;
   dlog.Init();
   dlog.TransferDataToWindow();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   frequency = dlog.frequency;
   waveform = dlog.waveform;
   amplitude = dlog.amplitude;
   length = dlog.length;

   return true;
}

bool EffectToneGen::TransferParameters( Shuttle & shuttle )
{  
//   shuttle.TransferInt("",,0);
   return true;
}

bool EffectToneGen::MakeTone(float *buffer, sampleCount len)
{
   double throwaway = 0;        //passed to modf but never used
   int i;

   switch (waveform) {
   case 0:                     //sine
      for (i = 0; i < len; i++)
         buffer[i] =
             amplitude * (float) sin(2 * M_PI * (i + mSample) * frequency / mCurRate);
      mSample += len;
      break;

   case 1:                     //square
      for (i = 0; i < len; i++) {
         if (modf(((i + mSample) * frequency / mCurRate), &throwaway) < 0.5)
            buffer[i] = amplitude;
         else
            buffer[i] = -amplitude;
      }
      mSample += len;
      break;

   case 2:                     //sawtooth
      for (i = 0; i < len; i++)
         buffer[i] = 2 * amplitude * ((((i + mSample) % (int) (mCurRate / frequency)) / (mCurRate / frequency)) - 0.5);
      mSample += len;
      break;

   default:
      break;
   }
   return true;
}

bool EffectToneGen::Process()
{
   if (length <= 0.0)
      length = sDefaultGenerateLen;

   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      mSample = 0;
      WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat());
      mCurRate = track->GetRate();
      tmp->SetRate(mCurRate);
      longSampleCount numSamples =
         (longSampleCount)(length * mCurRate + 0.5);
      longSampleCount i = 0;
      float *data = new float[tmp->GetMaxBlockSize()];
      sampleCount block;

      while(i < numSamples) {
         block = tmp->GetBestBlockSize(i);
         if (block > (numSamples - i))
             block = numSamples - i;
         MakeTone(data, block);
         tmp->Append((samplePtr)data, floatSample, block);
         i += block;
      }
      delete[] data;

      tmp->Flush();
      track->Clear(mT0, mT1);
      track->Paste(mT0, tmp);
      delete tmp;
      
      //Iterate to the next track
      track = (WaveTrack *)iter.Next();
   }

	mT1 = mT0 + length; // Update selection.

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

ToneGenDialog::ToneGenDialog(wxWindow * parent, const wxString & title)
: EffectDialog(parent, title, INSERT_EFFECT)
{
}

void ToneGenDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartMultiColumn(2, wxCENTER);
   {
#if 1
      S.AddPrompt(_("Waveform:"));
      mWaveform = new wxChoice(this,
                               wxID_ANY,
                               wxDefaultPosition,
                               wxDefaultSize,
                               *waveforms);
      mWaveform->SetSizeHints(-1,-1);
      mWaveform->SetStringSelection(waveforms->Item(waveform));
      S.AddWindow(mWaveform);
#else
      mWaveform = S.AddChoice(_("Waveform:"),
                              waveforms->Item(waveform),
                              waveforms);
      mWaveform->SetSizeHints(-1,-1);
#endif
      mFreq = S.AddTextBox(_("Frequency / Hz"),
                           wxT(""),
                           5);
      mAmp = S.AddTextBox(_("Amplitude (0-1)"),
                          wxT(""),
                          5);
      mLength = S.AddTextBox(_("Length (seconds)"),
                             wxT(""),
                             5);
   }
   S.EndMultiColumn();
}

bool ToneGenDialog::TransferDataToWindow()
{
   mWaveform->SetSelection(waveform);
   mFreq->SetValue(wxString::Format(wxT("%.2f"), frequency));
   mAmp->SetValue(wxString::Format(wxT("%.2f"), amplitude));
   mLength->SetValue(wxString::Format(wxT("%.2f"), length));

   return true;
}

bool ToneGenDialog::TransferDataFromWindow()
{
   mLength->GetValue().ToDouble(&length);

   mAmp->GetValue().ToDouble(&amplitude);
   amplitude = TrapDouble(amplitude, AMP_MIN, AMP_MAX);

   mFreq->GetValue().ToDouble(&frequency);
   frequency = TrapDouble(frequency, FREQ_MIN, FREQ_MAX);

   waveform = TrapLong(mWaveform->GetSelection(), WAVEFORM_MIN, WAVEFORM_MAX);

   return true;
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
// arch-tag: 04ea2450-8127-45c4-8702-6aaf5b60ed8c


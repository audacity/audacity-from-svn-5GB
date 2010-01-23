/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.cpp

  Steve Jolly
  James Crook (Adapted for 'Chirps')

  This class implements a tone generator effect.

*******************************************************************//**

\class EffectToneGen
\brief An Effect that can generate a sine, square or sawtooth wave.
An extended mode of EffectToneGen supports 'chirps' where the
frequency changes smoothly during the tone.

*//****************************************************************//**

\class ToneGenDialog
\brief Dialog used with EffectToneGen

*//*******************************************************************/

#include "../Audacity.h"
#include "ToneGen.h"
#include "../FFT.h"
#include "../Project.h"
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
   mbChirp=false;
   mbLogInterpolation=false;
   waveform = 0;                //sine
   frequency[0] = float(440.0);          //Hz
   frequency[1] = float(1320.0);          //Hz
   amplitude[0] = float(0.8);
   amplitude[1] = float(0.8);
//   EnableForChirps();
   length = sDefaultGenerateLen;
#ifdef LOGARITHMIC_TONE_CHIRP
   interpolation=0;
#endif
}

wxString EffectToneGen::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   /// \todo update to include *all* chirp parameters??
   const wxChar* waveformNames[] = {wxT("sine"), wxT("square"), wxT("sawtooth"), wxT("square, no alias")};
#ifdef LOGARITHMIC_TONE_CHIRP
   const wxChar* interpolationNames[] = {wxT("linear"), wxT("logarithmic")};
#endif
   return wxString::Format(_("Applied effect: Generate %s wave %s, frequency = %.2f Hz, amplitude = %.2f, %.6lf seconds"), 
      waveformNames[waveform], mbChirp ? wxT("chirp") : wxT("tone"), frequency[0], amplitude[0], length); 
} 

bool EffectToneGen::PromptUser()
{
   wxArrayString waveforms;
#ifdef LOGARITHMIC_TONE_CHIRP
   wxArrayString interpolations;
   interpolations.Add(_("Linear"));
   interpolations.Add(_("Logarithmic"));
#endif
   ToneGenDialog dlog(mParent, mbChirp ? _("Chirp Generator") : _("Tone Generator"));
   waveforms.Add(_("Sine"));
   waveforms.Add(_("Square"));
   waveforms.Add(_("Sawtooth"));
   waveforms.Add(_("Square, no alias"));
   dlog.isSelection= false;

   if (mT1 > mT0) {
      length = mT1 - mT0;
      dlog.isSelection= true;
   }
  
   dlog.mbChirp = mbChirp;
   dlog.waveform = waveform;
   dlog.frequency[0] = frequency[0];
   dlog.frequency[1] = frequency[1];
   dlog.amplitude[0] = amplitude[0];
   dlog.amplitude[1] = amplitude[1];
   dlog.length = length;
   dlog.waveforms = &waveforms;
#ifdef LOGARITHMIC_TONE_CHIRP
   dlog.interpolation = interpolation;
   dlog.interpolations = &interpolations;
#endif
   dlog.Init();
   dlog.TransferDataToWindow();
   dlog.Fit();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   waveform = dlog.waveform;
   frequency[0] = dlog.frequency[0];
   frequency[1] = dlog.frequency[1];
   amplitude[0] = dlog.amplitude[0];
   amplitude[1] = dlog.amplitude[1];
#ifdef LOGARITHMIC_TONE_CHIRP
	interpolation = dlog.interpolation;
   if (interpolation==0)
      mbLogInterpolation = false;
   if (interpolation==1)
      mbLogInterpolation = true;
#endif
   if( !mbChirp )
   {
      frequency[1] = frequency[0];
      amplitude[1] = amplitude[0];
   }
   length = dlog.length;
   return true;
}

bool EffectToneGen::TransferParameters( Shuttle & shuttle )
{
/// \todo this should in time be using ShuttleGui too.
//   shuttle.TransferInt("",,0);
   return true;
}

bool EffectToneGen::MakeTone(float *buffer, sampleCount len)
{
   double throwaway = 0;        //passed to modf but never used
   int i;
   double f = 0.0;

   double BlendedFrequency;
   double BlendedAmplitude;

   // calculate delta, and reposition from where we left
   double amplitudeQuantum = (amplitude[1]-amplitude[0]) / numSamples;
   double frequencyQuantum = (frequency[1]-frequency[0]) / numSamples;
   BlendedAmplitude = amplitude[0] + amplitudeQuantum * mSample;

   // precalculations:
   double pre2PI = 2 * M_PI;
   double pre4divPI = 4. / M_PI;

   /*
    Duplicate the code for the two cases, log interpolation and linear interpolation
    I hope this is more readable, a bit faster (only one branching in mbLogInterpolation)
    Local variables are declared inside respective branch, globals are declared up.
   */

   // this for log interpolation
   if( mbLogInterpolation )
   {
      logFrequency[0] = log10( frequency[0] );
      logFrequency[1] = log10( frequency[1] );
      // calculate delta, and reposition from where we left
      double logfrequencyQuantum = (logFrequency[1]-logFrequency[0]) / numSamples;
      double BlendedLogFrequency = logFrequency[0] + logfrequencyQuantum * mSample;

      for (i = 0; i < len; i++)
      {
         BlendedFrequency = pow( 10.0, (double)BlendedLogFrequency );
         switch (waveform) {
            case 0:    //sine
               f = (float) sin(pre2PI * mPositionInCycles/mCurRate);
               break;
            case 1:    //square
               f = (modf(mPositionInCycles/mCurRate, &throwaway) < 0.5) ? 1.0f :-1.0f;
               break;
            case 2:    //sawtooth
               f = (2 * modf(mPositionInCycles/mCurRate+0.5f, &throwaway)) -1.0f;
               break;
            default:
               break;
         }
         // insert value in buffer
         buffer[i] = BlendedAmplitude * f;
         // update freq,amplitude
         mPositionInCycles += BlendedFrequency;
         BlendedAmplitude += amplitudeQuantum;
         BlendedLogFrequency += logfrequencyQuantum;
      }

   } else {
      // this for regular case, linear interpolation
      BlendedFrequency = frequency[0] + frequencyQuantum * mSample;
      double a,b;
      int k;
      for (i = 0; i < len; i++)
      {
         switch (waveform) {
            case 0:    //sine
               f = (float) sin(pre2PI * mPositionInCycles/mCurRate);
               break;
            case 1:    //square
               f = (modf(mPositionInCycles/mCurRate, &throwaway) < 0.5) ? 1.0f :-1.0f;
               break;
            case 2:    //sawtooth
               f = (2 * modf(mPositionInCycles/mCurRate+0.5f, &throwaway)) -1.0f;
               break;
            case 3:    //square, no alias.  Good down to 110Hz @ 44100Hz sampling.
               //do fundamental (k=1) outside loop
               b = (1. + cos((pre2PI * BlendedFrequency)/mCurRate))/pre4divPI;  //scaling
               f = (float) pre4divPI * sin(pre2PI * mPositionInCycles/mCurRate);
               for(k=3; (k<200) && (k * BlendedFrequency < mCurRate/2.); k+=2)
               {
                  //Hanning Window in freq domain
                  a = 1. + cos((pre2PI * k * BlendedFrequency)/mCurRate);
                  //calc harmonic, apply window, scale to amplitude of fundamental
                  f += (float) a * sin(pre2PI * mPositionInCycles/mCurRate * k)/(b*k);
               }
               break;

            default:
               break;
         }
         // insert value in buffer
         buffer[i] = BlendedAmplitude * f;
         // update freq,amplitude
         mPositionInCycles += BlendedFrequency;
         BlendedFrequency += frequencyQuantum;
         BlendedAmplitude += amplitudeQuantum;
      }
   }
   // update external placeholder
   mSample += len;
   return true;
}



bool EffectToneGen::Process()
{
   if (length <= 0.0)
      length = sDefaultGenerateLen;

   mPositionInCycles = 0.0;
   //Iterate over each track
   int ntrack = 0;
   this->CopyInputWaveTracks(); // Set up m_pOutputWaveTracks.
   bool bGoodResult = true;

   TrackListIterator iter(m_pOutputWaveTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      mSample = 0;
      mCurRate = track->GetRate();
      WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat(), mCurRate);
      numSamples = (longSampleCount)(length * mCurRate + 0.5);
      longSampleCount i = 0;
      float *data = new float[tmp->GetMaxBlockSize()];
      sampleCount block;

      while ((i < numSamples) && bGoodResult) {
         block = tmp->GetBestBlockSize(i);
         if (block > (numSamples - i))
             block = numSamples - i;
         MakeTone(data, block);
         tmp->Append((samplePtr)data, floatSample, block);
         i += block;

         //Update the Progress meter
         if (TrackProgress(ntrack, (double)i / numSamples))
            bGoodResult = false;
      }
      delete[] data;

      tmp->Flush();
      track->Clear(mT0, mT1);
      track->Paste(mT0, tmp);
      delete tmp;

      if (!bGoodResult)
         break;

      //Iterate to the next track
      ntrack++;
      track = (WaveTrack *)iter.Next();
   }

   if (bGoodResult)
	   mT1 = mT0 + length; // Update selection.

   this->ReplaceProcessedWaveTracks(bGoodResult); 
   return bGoodResult;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// ToneGenDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define AMP_MIN 0
#define AMP_MAX 1

BEGIN_EVENT_TABLE(ToneGenDialog, EffectDialog)
    EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, ToneGenDialog::OnTimeCtrlUpdate)
END_EVENT_TABLE()

ToneGenDialog::ToneGenDialog(wxWindow * parent, const wxString & title)
: EffectDialog(parent, title, INSERT_EFFECT)
{
   mToneDurationT = NULL;
   mbChirp = false;
}

/// Populates simple dialog that has a single tone.
void ToneGenDialog::PopulateOrExchangeStandard( ShuttleGui & S )
{
   S.StartMultiColumn(2, wxCENTER);
   {
      S.TieChoice(_("Waveform") + wxString(wxT(":")), waveform,  waveforms);
      S.SetSizeHints(-1, -1);
      S.TieTextBox(_("Frequency (Hz)"),frequency[0], 5);
      S.TieTextBox(_("Amplitude (0-1)"),amplitude[0], 5);
      S.AddFixedText(_("Duration"), false);
      if (mToneDurationT == NULL)
      {
         mToneDurationT = new
         TimeTextCtrl(this,
                      wxID_ANY,
                      wxT(""),
                      length,
                      44100,
                      wxDefaultPosition,
                      wxDefaultSize,
                      true);
         mToneDurationT->SetFormatString(mToneDurationT->GetBuiltinFormat(isSelection==true?(wxT("hh:mm:ss + samples")):(wxT("seconds"))));
         mToneDurationT->EnableMenu();
      }
      S.AddWindow(mToneDurationT);
   }
   S.EndMultiColumn();
}

/// Populates more complex dialog that has a chirp.
void ToneGenDialog::PopulateOrExchangeExtended( ShuttleGui & S )
{
   S.StartMultiColumn(2, wxCENTER);
   {
      S.TieChoice(_("Waveform:"), waveform,  waveforms);
      S.SetSizeHints(-1, -1);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(3, wxCENTER);
   {
      S.AddFixedText(wxT(""));
      S.AddTitle(_("Start"));
      S.AddTitle(_("End"));
      S.TieTextBox(_("Frequency (Hz)"),frequency[0], 10);
      S.TieTextBox(wxT(""), frequency[1], 10);
      S.TieTextBox(_("Amplitude (0-1)"),amplitude[0], 10);
      S.TieTextBox(wxT(""), amplitude[1], 10);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxCENTER);
   {
#ifdef LOGARITHMIC_TONE_CHIRP
      S.TieChoice(_("Interpolation:"), interpolation,  interpolations);
#endif
      S.AddFixedText(_("Duration"), false);
      if (mToneDurationT == NULL)
      {
         mToneDurationT = new
         TimeTextCtrl(this,
                      wxID_ANY,
                      wxT(""),
                      length,
                      44100,
                      wxDefaultPosition,
                      wxDefaultSize,
                      true);
         mToneDurationT->SetFormatString(mToneDurationT->GetBuiltinFormat(isSelection==true?(wxT("hh:mm:ss + samples")):(wxT("seconds"))));
         mToneDurationT->EnableMenu();
      }
      S.AddWindow(mToneDurationT);
   }
   S.EndMultiColumn();
}

void ToneGenDialog::PopulateOrExchange(ShuttleGui & S)
{
   if( !mbChirp )
      PopulateOrExchangeStandard( S );
   else
      PopulateOrExchangeExtended( S );
}

bool ToneGenDialog::TransferDataToWindow()
{
   EffectDialog::TransferDataToWindow();

   // Must handle this ourselves since ShuttleGui doesn't know about it
   mToneDurationT->SetTimeValue(length);

   return true;
}

bool ToneGenDialog::TransferDataFromWindow()
{
   EffectDialog::TransferDataFromWindow();

   amplitude[0] = TrapDouble(amplitude[0], AMP_MIN, AMP_MAX);
   frequency[0] = TrapDouble(frequency[0], FREQ_MIN, (float)(GetActiveProject()->GetRate())/2.);
   amplitude[1] = TrapDouble(amplitude[1], AMP_MIN, AMP_MAX);
   frequency[1] = TrapDouble(frequency[1], FREQ_MIN, (float)(GetActiveProject()->GetRate())/2.);

   // Must handle this ourselves since ShuttleGui doesn't know about it
   length = mToneDurationT->GetTimeValue();

   return true;
}

void ToneGenDialog::OnTimeCtrlUpdate(wxCommandEvent & event) {
   Fit();
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


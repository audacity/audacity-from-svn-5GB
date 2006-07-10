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
   frequency[1] = float(440.0);          //Hz
   amplitude[0] = float(0.8);
   amplitude[1] = float(0.8);
//   EnableForChirps();
   length = sDefaultGenerateLen;
}

wxString EffectToneGen::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   /// \todo update to include *all* chirp parameters??
   const wxChar* waveformNames[] = {wxT("sine"), wxT("square"), wxT("sawtooth")};
   return wxString::Format(_("Applied effect: Generate %s wave %s, frequency = %.2f Hz, amplitude = %.2f, %.6lf seconds"), 
      waveformNames[waveform], mbChirp ? wxT("chirp") : wxT("tone"), frequency[0], amplitude[0], length); 
} 

bool EffectToneGen::PromptUser()
{
   wxArrayString waveforms;
   if (mT1 > mT0)
      length = mT1 - mT0;

   waveforms.Add(_("Sine"));
   waveforms.Add(_("Square"));
   waveforms.Add(_("Sawtooth"));

   ToneGenDialog dlog(mParent, mbChirp ? _("Chirp Generator") : _("Tone Generator"));
   dlog.mbChirp = mbChirp;
   dlog.waveform = waveform;
   dlog.frequency[0] = frequency[0];
   dlog.frequency[1] = frequency[1];
   dlog.amplitude[0] = amplitude[0];
   dlog.amplitude[1] = amplitude[1];
   dlog.length = length;
   dlog.waveforms = &waveforms;
   dlog.Init();
   dlog.TransferDataToWindow();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   waveform = dlog.waveform;
   frequency[0] = dlog.frequency[0];
   frequency[1] = dlog.frequency[1];
   amplitude[0] = dlog.amplitude[0];
   amplitude[1] = dlog.amplitude[1];
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

   int iSample;
   double f;

   // t is our Blend parameter, 0.0 at left end, 1.0 at right end 
   // of the complete tone.
   double t;
   double BlendedAmplitude;
   double BlendedFrequency;
   double BlendedLogFrequency;

   // Do our divisions here, outside the loop, for greater efficiency.

   // Multiplier to convert from the sample number to the position in blend.
   double BlendMultiplier = 1.0f/numSamples;
   // Multiplier to convert from cycles-per-second to cycles-per-sample.
   double PositionMultiplier = 1.0f/ mCurRate;

   if( mbLogInterpolation )
   {
      logFrequency[0] = log10( frequency[0] );
      logFrequency[1] = log10( frequency[1] );
   }

   for (i = 0; i < len; i++)
   {
      iSample = i+mSample;
      t = iSample * BlendMultiplier;
      BlendedAmplitude = amplitude[0] + (amplitude[1]-amplitude[0]) * t;

      if( i == (len/2) )
         i=i;

      // Log interpolation is not currently used.
      // A linear shift in frequency is what most people expect, and
      // gives straight lines on a frequency plot.
      if( mbLogInterpolation )
      {
         BlendedLogFrequency = logFrequency[0] + (logFrequency[1]-logFrequency[0]) * t;
         BlendedFrequency = pow( 10.0, (double)BlendedLogFrequency );
      }
      else
      {
         BlendedFrequency = frequency[0] + (frequency[1]-frequency[0]) * t;
      }

      // Add cycles/second * second/samples * 1-sample
      mPositionInCycles += BlendedFrequency * PositionMultiplier;

      switch (waveform) {
         case 0:    //sine
            f = (float) sin(2 * M_PI * mPositionInCycles);
            break;
         case 1:    //square
            f = (modf(mPositionInCycles, &throwaway) < 0.5) ? 1.0f :-1.0f;
            break;
         case 2:    //sawtooth
            f = (2 * modf(mPositionInCycles+0.5f, &throwaway)) -1.0f;
            break;
         default:
            break;
      }
      buffer[i] = BlendedAmplitude * f;
   }
   mSample += len;
   return true;
}

bool EffectToneGen::Process()
{
   if (length <= 0.0)
      length = sDefaultGenerateLen;

   mPositionInCycles = 0.0;
   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      mSample = 0;
      WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat());
      mCurRate = track->GetRate();
      tmp->SetRate(mCurRate);
      numSamples = (longSampleCount)(length * mCurRate + 0.5);
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

ToneGenDialog::ToneGenDialog(wxWindow * parent, const wxString & title)
: EffectDialog(parent, title, INSERT_EFFECT)
{
   mbChirp=false;
}

/// Populates simple dialog that has a single tone.
void ToneGenDialog::PopulateOrExchangeStandard( ShuttleGui & S )
{
   S.StartMultiColumn(2, wxCENTER);
   {
      S.TieChoice( _("Waveform:"), waveform,  waveforms);
      S.SetSizeHints(-1,-1);
      S.TieTextBox( _("Frequency / Hz"),frequency[0], 5);
      S.TieTextBox( _("Amplitude (0-1)"),amplitude[0], 5);
      S.TieTextBox( _("Length (seconds)"),length, 5 );
   }
   S.EndMultiColumn();
}

/// Populates more complex dialog that has a chirp.
void ToneGenDialog::PopulateOrExchangeExtended( ShuttleGui & S )
{
   S.StartMultiColumn(2, wxCENTER);
   {
      S.TieChoice( _("Waveform:"), waveform,  waveforms);
      S.SetSizeHints(-1,-1);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(3, wxCENTER);
   {
      S.AddFixedText(wxT(""));
      S.AddTitle( _("Start"));
      S.AddTitle( _("End") );
      S.TieTextBox( _("Frequency / Hz"),frequency[0], 10);
      S.TieTextBox( wxT(""),frequency[1], 10);
      S.TieTextBox( _("Amplitude (0-1)"),amplitude[0], 10);
      S.TieTextBox( wxT(""),amplitude[1], 10);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxCENTER);
   {
      S.TieTextBox( _("Length (seconds)"),length, 10 );
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
   ShuttleGui S( this, eIsSettingToDialog );
   PopulateOrExchange( S );
   return true;
}

bool ToneGenDialog::TransferDataFromWindow()
{
   ShuttleGui S( this, eIsGettingFromDialog );
   PopulateOrExchange( S );
   amplitude[0] = TrapDouble(amplitude[0], AMP_MIN, AMP_MAX);
   frequency[0] = TrapDouble(frequency[0], FREQ_MIN, FREQ_MAX);
   amplitude[1] = TrapDouble(amplitude[1], AMP_MIN, AMP_MAX);
   frequency[1] = TrapDouble(frequency[1], FREQ_MIN, FREQ_MAX);
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


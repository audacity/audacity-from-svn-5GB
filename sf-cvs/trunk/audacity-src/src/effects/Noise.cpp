/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectNoise
\brief An Effect for the "Generator" menu to add white noise.

*//*******************************************************************/

#include "../Audacity.h"

#include "Noise.h"

#include "../WaveTrack.h"
#include "../widgets/TimeTextCtrl.h"

#include <wx/defs.h> 
#include <stdlib.h>

class NoiseDialog:public GenerateDialog
{
 public:
   NoiseDialog(wxWindow * parent, const wxString & action):
      GenerateDialog(parent, action)
   {
      Init();
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      S.StartStatic(_("Specify Length"), true);
         mLenCtrl = new TimeTextCtrl(this,
                                    wxID_ANY,
                                    TimeTextCtrl::GetBuiltinFormat(0),
                                    30.0,
                                    44000.0);
         S.AddWindow(mLenCtrl);
      S.EndStatic();
   }

   bool TransferDataToWindow()
   {
      mLenCtrl->SetTimeValue(mLength);
      mLenCtrl->SetFocus();

      return true;
   }

   bool TransferDataFromWindow()
   {
      mLength = mLenCtrl->GetTimeValue();

      return true;
   }

   bool Validate()
   {
      return true;
   }

   double GetLength()
   {
      return mLength;
   }

   void SetLength(double length)
   {
      mLength = length;
   }

 private:

   TimeTextCtrl *mLenCtrl;
   wxString mLenText;
   double mLength;
};

void MakeNoise(float *buffer, sampleCount len)
{
   sampleCount i;
   float div = ((float)RAND_MAX) / 2.0f;

   for(i=0; i<len; i++)
      buffer[i] = (rand() / div) - 1.0;
}

bool EffectNoise::PromptUser()
{
   if (mT1 > mT0)
      length = mT1 - mT0;
   else
	   length = sDefaultGenerateLen;

   NoiseDialog dlog(mParent, _("White Noise Generator"));
   dlog.SetLength(length);
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == 0)
      return false;

   length = dlog.GetLength();
   return true;
}

bool EffectNoise::Process()
{
   if (length <= 0.0)
      length = sDefaultGenerateLen;

   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat());
      tmp->SetRate(track->GetRate());
      longSampleCount numSamples =
         (longSampleCount)(length * track->GetRate() + 0.5);
      longSampleCount i = 0;
      float *data = new float[tmp->GetMaxBlockSize()];
      sampleCount block;

      while(i < numSamples) {
         block = tmp->GetBestBlockSize(i);
         if (block > (numSamples - i))
             block = numSamples - i;
         MakeNoise(data, block);
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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0ca03dc2-c229-44b4-a6eb-1d5d04a3983c


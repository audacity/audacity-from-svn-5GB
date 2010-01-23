/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.cpp

  Lynn Allan (from DM's Normalize)

*******************************************************************//**

\class EffectTruncSilence
\brief An Effect.

  \todo mBlendFrameCount only retrieved from prefs ... not using dialog
        Only way to change (for windows) is thru registry
        The values should be figured dynamically ... too many frames could be invalid

*//****************************************************************//**

\class TruncSilenceDialog
\brief Dialog used with EffectTruncSilence

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/wx.h>
#include <math.h>

#include "../Prefs.h"
#include "../Project.h"
#include "TruncSilence.h"

EffectTruncSilence::EffectTruncSilence()
{
   Init();
}

bool EffectTruncSilence::Init()
{
   mTruncLongestAllowedSilentMs = gPrefs->Read(wxT("/CsPresets/TruncLongestAllowedSilentMs"), 200L);
   if ((mTruncLongestAllowedSilentMs < 0) || (mTruncLongestAllowedSilentMs >= 9999999)) {  // corrupted Prefs?
      mTruncLongestAllowedSilentMs = SKIP_EFFECT_MILLISECOND;
      gPrefs->Write(wxT("/CsPresets/TruncLongestAllowedSilentMs"), mTruncLongestAllowedSilentMs);
   }
   mTruncDbChoiceIndex = gPrefs->Read(wxT("/CsPresets/TruncDbChoiceIndex"), 4L);
   if ((mTruncDbChoiceIndex < 0) || (mTruncDbChoiceIndex >= Enums::NumDbChoices)) {  // corrupted Prefs?
      mTruncDbChoiceIndex = Enums::NumDbChoices - 1;  // Off-Skip
      gPrefs->Write(wxT("/CsPresets/TruncDbChoiceIndex"), mTruncDbChoiceIndex);
      mTruncLongestAllowedSilentMs = SKIP_EFFECT_MILLISECOND;
      gPrefs->Write(wxT("/CsPresets/TruncLongestAllowedSilentMs"), mTruncLongestAllowedSilentMs);
   }
   mBlendFrameCount = gPrefs->Read(wxT("/CsPresets/BlendFrameCount"), 100L);
   if ((mBlendFrameCount < 0) || (mBlendFrameCount >= 5000)) {  // corrupted Prefs?
      mBlendFrameCount = 100;
      gPrefs->Write(wxT("/CsPresets/BlendFrameCount"), 100);
   }
   return true;
}

bool EffectTruncSilence::CheckWhetherSkipEffect()
{
   return ((mTruncDbChoiceIndex >= (Enums::NumDbChoices - 1))
          ||  (mTruncLongestAllowedSilentMs >= SKIP_EFFECT_MILLISECOND));
}

void EffectTruncSilence::End()
{
}

bool EffectTruncSilence::PromptUser()
{
   TruncSilenceDialog dlog(this, mParent);

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   gPrefs->Write(wxT("/CsPresets/TruncLongestAllowedSilentMs"), mTruncLongestAllowedSilentMs);
   gPrefs->Write(wxT("/CsPresets/TruncDbChoiceIndex"), mTruncDbChoiceIndex);
   
   return true;
}

bool EffectTruncSilence::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferEnum(wxT("Db"), mTruncDbChoiceIndex, Enums::NumDbChoices, Enums::GetDbChoices());
   shuttle.TransferInt(wxT("Duration"), mTruncLongestAllowedSilentMs, 200);
   return true;
}

#define QUARTER_SECOND_MS 250
bool EffectTruncSilence::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *t;
   double t0 = mT0;
   double t1 = mT1;
   int tndx; 
   int tcount = 0;

   // Init using first track
   t = (WaveTrack *) iter.First();
   double rate = t->GetRate();
   sampleCount blockLen = t->GetMaxBlockSize();

   // Get the left and right bounds for all tracks
   while (t) {
      // Make sure all tracks have the same sample rate
      if (rate != t->GetRate()) {
         wxMessageBox(_("All tracks must have the same sample rate"), _("Truncate Silence"));
         return false;
      }

      // Count the tracks
      tcount++;

      // Set the current bounds to whichever left marker is
      // greater and whichever right marker is less
      t0 = wxMax(mT0, t->GetStartTime());
      t1 = wxMin(mT1, t->GetEndTime());

      // Use the smallest block size of all the tracks
      blockLen = wxMin(blockLen, t->GetMaxBlockSize());

      // Iterate to the next track
      t = (WaveTrack*) iter.Next();
   }

   // Transform the marker timepoints to samples
   t = (WaveTrack *) iter.First();
   longSampleCount start = t->TimeToLongSamples(t0);
   longSampleCount end = t->TimeToLongSamples(t1);

   // Bigger buffers reduce 'reset'
   blockLen *= 8;

   // Allocate buffers
   float **buffer = new float*[tcount];
   for (tndx = 0; tndx < tcount; tndx++) {
      buffer[tndx] = new float[blockLen];
   }

   // Set thresholds
   // We have a lower bound on the amount of silence we chop out at a time
   // to avoid chopping up low frequency sounds.  We're good down to 10Hz
   // if we use 100ms.
   const float minTruncMs = 1.0f;
   double truncDbSilenceThreshold = Enums::Db2Signal[mTruncDbChoiceIndex];
   int truncLongestAllowedSilentSamples = 
      int((wxMax( mTruncLongestAllowedSilentMs, minTruncMs) * rate) / 1000.0);

   

   // Figure out number of frames for ramping
   int quarterSecondFrames = int((rate * QUARTER_SECOND_MS) / 1000.0);
   int rampInFrames = (truncLongestAllowedSilentSamples / 4);
   if (rampInFrames > quarterSecondFrames) {
      rampInFrames = quarterSecondFrames;
   }

   // Start processing
   this->CopyInputWaveTracks(); // Set up m_pOutputWaveTracks.
   TrackListIterator iterOut(m_pOutputWaveTracks);

   longSampleCount index = start;
   longSampleCount outTrackOffset = start;
   bool cancelled = false;
   while (index < end) {

      // Limit size of current block if we've reached the end
      sampleCount limit = blockLen;
      if ((index + blockLen) > end) {
         limit = end - index; 
      }

      // Fill the buffers
      tndx = 0;
      t = (WaveTrack *) iter.First();
      while (t) {
         t->Get((samplePtr)buffer[tndx++], floatSample, index, blockLen);
         t = (WaveTrack *) iter.Next();
      }

      // Reset
      bool ignoringFrames = false;
      sampleCount consecutiveSilentFrames = 0;
      sampleCount truncIndex = 0;

      // Look for silences in current block
      for (sampleCount i = 0; i < limit; i++) {

         // Is current frame in all tracks below threshold
         bool below = true;
         for (tndx = 0; tndx < tcount; tndx++) {
            if (fabs(buffer[tndx][i]) >= truncDbSilenceThreshold) {
               below = false;
               break;
            }
         }

         // Count frame if it's below threshold
         if (below) {
            consecutiveSilentFrames++;

            // Ignore this frame (equivalent to cutting it)
            // otherwise, keep sample to be part of allowed silence
            if (consecutiveSilentFrames > truncLongestAllowedSilentSamples) {
               ignoringFrames = true;
               continue;
            }
         }
         else {
            if (ignoringFrames == true) {
               sampleCount curOffset = i - rampInFrames;
               truncIndex -= rampInFrames; // backup into ignored frames

               for (tndx = 0; tndx < tcount; tndx++) {
                  sampleCount trunci = truncIndex;
                  for (int fr = 0; fr < rampInFrames; fr++) {
                     buffer[tndx][trunci++] = buffer[tndx][curOffset + fr];
                  }
                  if(((trunci - rampInFrames) - mBlendFrameCount) >= 0) {
                     BlendFrames(buffer[tndx], mBlendFrameCount,
                             ((trunci - rampInFrames) - mBlendFrameCount), 
                             ((i - rampInFrames) - mBlendFrameCount));
                  }
               }
               truncIndex += rampInFrames;
            }
            consecutiveSilentFrames = 0;
            ignoringFrames = false;
         }

         // Can get here either because > dbThreshold
         // or silence duration isn't longer than allowed
         for (tndx = 0; tndx < tcount; tndx++) {
            buffer[tndx][truncIndex] = buffer[tndx][i];
         }
         truncIndex++;
      }

      // Update tracks if any samples were removed
      if (truncIndex < limit) {

         // Put updated sample back into output tracks.
         tndx = 0;
         t = (WaveTrack *) iterOut.First();
         while (t) {
            t->Set((samplePtr)buffer[tndx++], floatSample, outTrackOffset, truncIndex);
            t = (WaveTrack *) iterOut.Next();
         }
      }

      // Maintain output index
      outTrackOffset += truncIndex;

      // Update progress and bail if user cancelled
      cancelled = TrackProgress(0, ((double)index / (double)end));
      if (cancelled) {
         break;
      }

      // Bump to next block
      index += limit;
   }

   // Remove stale data at end of output tracks.
   if (!cancelled && (outTrackOffset < end)) {
      t = (WaveTrack *) iterOut.First();
      while (t) {
         t->Clear(outTrackOffset / rate, t1);
         t = (WaveTrack *) iterOut.Next();
      }
      t1 = outTrackOffset / rate;
   }

   // Free buffers
   for (tndx = 0; tndx < tcount; tndx++) {
      delete [] buffer[tndx];
   }
   delete [] buffer;

   mT0 = t0;
   mT1 = t1;

   this->ReplaceProcessedWaveTracks(!cancelled); 
   return !cancelled;
}

void EffectTruncSilence::BlendFrames(float* buffer, int blendFrameCount, int leftIndex, int rightIndex)
{
   float* bufOutput = &buffer[leftIndex];
   float* bufBefore = &buffer[leftIndex];
   float* bufAfter  = &buffer[rightIndex];
   double beforeFactor = 1.0;
   double afterFactor  = 0.0;
   double adjFactor = 1.0 / (double)blendFrameCount;
   for (int j = 0; j < blendFrameCount; ++j) {
      bufOutput[j] = (float)((bufBefore[j] * beforeFactor) + (bufAfter[j] * afterFactor));
      beforeFactor -= adjFactor;
      afterFactor  += adjFactor;
   }
}

//----------------------------------------------------------------------------
// TruncSilenceDialog
//----------------------------------------------------------------------------

#define ID_LONGEST_SILENCE_TEXT   7000
#define ID_DB_SILENCE_THRESHOLD_CHOICE 7001

BEGIN_EVENT_TABLE(TruncSilenceDialog, EffectDialog)
    EVT_BUTTON(ID_EFFECT_PREVIEW, TruncSilenceDialog::OnPreview)
    EVT_TEXT( ID_LONGEST_SILENCE_TEXT, TruncSilenceDialog::OnDurationChange )
END_EVENT_TABLE()

TruncSilenceDialog::TruncSilenceDialog(EffectTruncSilence * effect,
                                       wxWindow * parent)
:  EffectDialog(parent, _("Truncate Silence"), PROCESS_EFFECT),
   mEffect(effect)
{
   Init();
   effect->SetDialog(this);
}

void TruncSilenceDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("by Lynn Allan"));
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxCENTER, false);
   {
      // Add a little space
   }
   S.EndHorizontalLay();

   S.StartThreeColumn();
   {
      wxArrayString choices(Enums::NumDbChoices, Enums::GetDbChoices());

      S.Id( ID_LONGEST_SILENCE_TEXT ).TieTextBox(_("Max silence duration:"),
                   mEffect->mTruncLongestAllowedSilentMs,
                   10);
      S.AddUnits( _("milliseconds") );
      //S.AddUnits(_("(9999999 or greater is off)"));
      S.TieChoice(_("Threshold for silence:"),
                  mEffect->mTruncDbChoiceIndex,
                  &choices);
   }
   S.EndTwoColumn();
   pWarning = S.AddVariableText( wxT("") );
}

void TruncSilenceDialog::OnPreview(wxCommandEvent & event)
{
   TransferDataFromWindow();
   mEffect->Preview();
}

void TruncSilenceDialog::OnDurationChange(wxCommandEvent & event)
{
   // We may even get called during the constructor.
   // This test saves us from calling unsafe functions.
   if( !IsShown() )
      return;
   TransferDataFromWindow();
   bool bOk =  mEffect->mTruncLongestAllowedSilentMs > 0.9f ;
   pWarning->SetLabel( bOk ? 
      wxT("") : 
      _("   Duration must be at least 1 millisecond")
         );
   wxWindow *pWnd;
   pWnd = FindWindowById( wxID_OK, this );
   pWnd->Enable( bOk );
   pWnd = FindWindowById( ID_EFFECT_PREVIEW, this );
   pWnd->Enable( bOk );

}

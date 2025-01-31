/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.cpp

  Lynn Allan (from DM's Normalize)
  Philip Van Baren (more options and boundary fixes)

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
   mTruncInitialAllowedSilentMs = gPrefs->Read(wxT("/CsPresets/TruncInitialAllowedSilentMs"), 200L);
   if ((mTruncInitialAllowedSilentMs < 0) || (mTruncInitialAllowedSilentMs >= 9999999)) {  // corrupted Prefs?
      mTruncInitialAllowedSilentMs = 200L;
      gPrefs->Write(wxT("/CsPresets/TruncInitialAllowedSilentMs"), mTruncInitialAllowedSilentMs);
   }
   mTruncLongestAllowedSilentMs = gPrefs->Read(wxT("/CsPresets/TruncLongestAllowedSilentMs"), 1000L);
   if ((mTruncLongestAllowedSilentMs < 0) || (mTruncLongestAllowedSilentMs >= 9999999)) {  // corrupted Prefs?
      mTruncLongestAllowedSilentMs = 1000L;
      gPrefs->Write(wxT("/CsPresets/TruncLongestAllowedSilentMs"), mTruncLongestAllowedSilentMs);
   }
   
   if( mTruncLongestAllowedSilentMs < mTruncInitialAllowedSilentMs )
      mTruncInitialAllowedSilentMs = mTruncLongestAllowedSilentMs;

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
   mSilenceCompressRatio = 0.1*gPrefs->Read(wxT("/CsPresets/SilenceCompressRatio"), 40L);
   if ((mSilenceCompressRatio < 1.0) || (mSilenceCompressRatio > 20.0)) {  // corrupted Prefs?
      mSilenceCompressRatio = 4.0;
      gPrefs->Write(wxT("/CsPresets/SilenceCompressRatio"), 40L);
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

   gPrefs->Write(wxT("/CsPresets/TruncInitialAllowedSilentMs"), mTruncInitialAllowedSilentMs);
   gPrefs->Write(wxT("/CsPresets/TruncLongestAllowedSilentMs"), mTruncLongestAllowedSilentMs);
   gPrefs->Write(wxT("/CsPresets/TruncDbChoiceIndex"), mTruncDbChoiceIndex);
   gPrefs->Write(wxT("/CsPresets/SilenceCompressRatio"), (int)floor(10.0*mSilenceCompressRatio+0.5));

   return true;
}

bool EffectTruncSilence::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferEnum(wxT("Db"), mTruncDbChoiceIndex, Enums::NumDbChoices, Enums::GetDbChoices());
   shuttle.TransferInt(wxT("Minimum"), mTruncInitialAllowedSilentMs, 200);
   shuttle.TransferInt(wxT("Duration"), mTruncLongestAllowedSilentMs, 1000);
   shuttle.TransferDouble(wxT("Compress"), mSilenceCompressRatio, 4.0f);
   return true;
}

#define QUARTER_SECOND_MS 250
bool EffectTruncSilence::Process()
{
   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *t;
   double t0 = mT0;
   double t1 = mT1;
   int tndx; 
   int tcount = 0;
   int fr;

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

   // Just a sanity check, really it should be much higher
   if(blockLen < 4*mBlendFrameCount)
      blockLen = 4*mBlendFrameCount;

   // Transform the marker timepoints to samples
   t = (WaveTrack *) iter.First();
   sampleCount start = t->TimeToLongSamples(t0);
   sampleCount end = t->TimeToLongSamples(t1);

   // Bigger buffers reduce 'reset'
   //blockLen *= 8;
   // Stress-test the logic for cutting samples through block endpoints
   //blockLen /= 8;

   // Set thresholds
   // We have a lower bound on the amount of silence we chop out at a time
   // to avoid chopping up low frequency sounds.  We're good down to 10Hz
   // if we use 100ms.
   const float minTruncMs = 1.0f;
   double truncDbSilenceThreshold = Enums::Db2Signal[mTruncDbChoiceIndex];
   int truncInitialAllowedSilentSamples = 
      int((wxMax( mTruncInitialAllowedSilentMs, minTruncMs) * rate) / 1000.0);
   int truncLongestAllowedSilentSamples = 
      int((wxMax( mTruncLongestAllowedSilentMs, minTruncMs) * rate) / 1000.0);

   // Don't allow either value to be less than the cross-fade length
   if(truncInitialAllowedSilentSamples < mBlendFrameCount)
      truncInitialAllowedSilentSamples = mBlendFrameCount;
   if(truncLongestAllowedSilentSamples < mBlendFrameCount)
      truncLongestAllowedSilentSamples = mBlendFrameCount;

   // For sake of efficiency, don't let blockLen be less than double the longest silent samples
   // up until a sane limit of 1Meg samples
   while((blockLen > 0) && (blockLen < truncLongestAllowedSilentSamples*2) && (blockLen < 1048576)) {
      blockLen *= 2;
   }
    // Don't allow either value to be more than half of the block length
   if(truncLongestAllowedSilentSamples > blockLen/2)
      truncLongestAllowedSilentSamples = blockLen/2;
   if(truncInitialAllowedSilentSamples > truncLongestAllowedSilentSamples)
      truncInitialAllowedSilentSamples = truncLongestAllowedSilentSamples;

   // We use the 'longest' variable as additive to the 'initial' variable
   truncLongestAllowedSilentSamples -= truncInitialAllowedSilentSamples;

   // Perform the crossfade half-way through the minimum removed silence duration
   int rampInFrames = (truncInitialAllowedSilentSamples + mBlendFrameCount) / 2;
   if(rampInFrames > truncInitialAllowedSilentSamples)
      rampInFrames = truncInitialAllowedSilentSamples;

   // Allocate buffers
   float **buffer = new float*[tcount];
   for (tndx = 0; tndx < tcount; tndx++) {
      buffer[tndx] = new float[blockLen];
   }

   // Start processing
   //Track::All is needed because this effect has clear functionality
   this->CopyInputTracks(Track::All); // Set up mOutputTracks.
   SelectedTrackListOfKindIterator iterOut(Track::Wave, mOutputTracks);

   sampleCount index = start;
   sampleCount outTrackOffset = start;
   bool cancelled = false;
   // Reset
   bool ignoringFrames = false;
   bool truncToMinimum = true;  // Ignore the initial samples until we get above the noise floor
   sampleCount consecutiveSilentFrames = 0;
   sampleCount truncIndex = 0;
   sampleCount i = 0;
   sampleCount keep;

   while (index < end) {

      // Limit size of current block if we've reached the end
      sampleCount count = blockLen-i;
      if ((index + count) > end) {
         count = end - index; 
      }

      // Fill the buffers
      tndx = 0;
      t = (WaveTrack *) iter.First();
      while (t) {
         t->Get((samplePtr)(buffer[tndx++]+i), floatSample, index, count);
         t = (WaveTrack *) iter.Next();
      }

      // Shift over to account for samples remaining from prior block
      sampleCount limit = count+i;

      // Look for silences in current block
      for ( ; i < limit; i++) {

         // Is current frame in all tracks below threshold
         bool below = true;
         for (tndx = 0; tndx < tcount; tndx++) {
            if (fabs(buffer[tndx][i]) >= truncDbSilenceThreshold) {
               below = false;
               break;
            }
         }
         // Make sure we cross-fade and output the last silence
         // so we get a smooth transition into whatever follows the selected region
         // Also set the 'truncToMinimum' flag so that the last silence is truncated to the minimum amount
         if(below && ((index+i+1) == end)) {
            below = false;
            truncToMinimum = true;
         }

         // Count frame if it's below threshold
         if (below) {
            consecutiveSilentFrames++;

            // Ignore this frame (equivalent to cutting it)
            // otherwise, keep sample to be part of allowed silence
            if (consecutiveSilentFrames > truncInitialAllowedSilentSamples) {
               ignoringFrames = true;
               continue;
            }
         }
         else {
            if (ignoringFrames == true) {
               // Scale the consectiveSilentFrames so we keep a silence duration
               // which is proportional to the original silence up to the limit
               keep = consecutiveSilentFrames - truncInitialAllowedSilentSamples;
               keep /= mSilenceCompressRatio;

               // The first and last samples always get truncated to the minimum amount
               if(truncToMinimum == true)
                  keep = 0;
               if(keep > truncLongestAllowedSilentSamples)
                  keep = truncLongestAllowedSilentSamples;
               if(keep < 0)
                  keep = 0;

               // Compute the location of the cross-fade to be halfway through the silence
               // with restriction to the samples we still have available to use
               rampInFrames = (truncInitialAllowedSilentSamples - keep + mBlendFrameCount) / 2;
               if(rampInFrames > truncInitialAllowedSilentSamples)
                  rampInFrames = truncInitialAllowedSilentSamples;
               if(rampInFrames < mBlendFrameCount)
                  rampInFrames = mBlendFrameCount;

               // Include the cross-fade samples in the count to make the loop logic easier
               keep += rampInFrames;
               truncIndex -= rampInFrames;

               // back up for cross-fade
               sampleCount curOffset = i - keep;

               if(curOffset < 0) {
                  // This should never happen, but just in case...
                  keep += curOffset - rampInFrames;
                  if(keep < mBlendFrameCount)
                     keep = mBlendFrameCount;
                  curOffset = 0;
               }
               if(truncIndex < 0) {
                  // This should never happen, but just in case...
                  truncIndex = 0;
               }

               for (tndx = 0; tndx < tcount; tndx++) {
                  // Cross fade the cut point
                  for (fr = 0; fr < mBlendFrameCount; fr++) {
                     buffer[tndx][truncIndex+fr] = ((mBlendFrameCount-fr)*buffer[tndx][truncIndex+fr] + fr*buffer[tndx][curOffset + fr]) / mBlendFrameCount;
                  }
                  // Append the 'keep' samples, if any
                  for ( ; fr < keep; fr++) {
                     buffer[tndx][truncIndex+fr] = buffer[tndx][curOffset + fr];
                  }
               }
               truncIndex += keep;
            }
            consecutiveSilentFrames = 0;
            ignoringFrames = false;
            truncToMinimum = false;
         }

         // Can get here either because > dbThreshold
         // or silence duration isn't longer than allowed
         for (tndx = 0; tndx < tcount; tndx++) {
            buffer[tndx][truncIndex] = buffer[tndx][i];
         }
         truncIndex++;
      }

      // Update tracks if any samples were removed, now or before
      if (outTrackOffset + truncIndex != index + limit) {
         // Put updated sample back into output tracks.
         tndx = 0;
         t = (WaveTrack *) iterOut.First();
         while (t) {
            t->Set((samplePtr)buffer[tndx++], floatSample, outTrackOffset, truncIndex);
            t = (WaveTrack *) iterOut.Next();
         }
      }

      // If currently in a silent section, retain samples for the next pass
      if(ignoringFrames) {
         keep = consecutiveSilentFrames - truncInitialAllowedSilentSamples;
         if(keep > (truncLongestAllowedSilentSamples+mBlendFrameCount))
            keep = truncLongestAllowedSilentSamples+mBlendFrameCount;
         for (tndx = 0; tndx < tcount; tndx++) {
            for(fr = 0; fr < truncInitialAllowedSilentSamples; fr++) {
               buffer[tndx][fr] = buffer[tndx][truncIndex-truncInitialAllowedSilentSamples+fr];
            }
            for(fr = 0; fr < keep; fr++) {
               buffer[tndx][truncInitialAllowedSilentSamples+fr] = buffer[tndx][i-keep+fr];
            }
         }
         // Update the output index, less what we are retaining for next time
         outTrackOffset += truncIndex - truncInitialAllowedSilentSamples;
         // Append the following buffer to the existing data
         i = consecutiveSilentFrames = truncInitialAllowedSilentSamples + keep;
         truncIndex = truncInitialAllowedSilentSamples;
      } else {
         // Maintain output index
         outTrackOffset += truncIndex;
         // Reset the buffer pointers to the beginning
         i = 0;
         truncIndex = 0;
         consecutiveSilentFrames = 0;
      }

      // Update progress and bail if user cancelled
      cancelled = TrackProgress(0, ((double)index / (double)end));
      if (cancelled) {
         break;
      }

      // Bump to next block
      index += count;
   }

   // Remove stale data at end of output tracks.
   if (!cancelled && (outTrackOffset < end)) {
      t = (WaveTrack *) iterOut.First();
      while (t) {
         t->Clear(outTrackOffset / rate, t1, mOutputTracks);
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

   this->ReplaceProcessedTracks(!cancelled); 
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

#define ID_SHORTEST_SILENCE_TEXT   7000
#define ID_LONGEST_SILENCE_TEXT   7001
#define ID_COMPRESS_FACTOR   7002
#define ID_DB_SILENCE_THRESHOLD_CHOICE 7003

BEGIN_EVENT_TABLE(TruncSilenceDialog, EffectDialog)
    EVT_BUTTON(ID_EFFECT_PREVIEW, TruncSilenceDialog::OnPreview)
    EVT_TEXT( ID_SHORTEST_SILENCE_TEXT, TruncSilenceDialog::OnDurationChange )
    EVT_TEXT( ID_LONGEST_SILENCE_TEXT, TruncSilenceDialog::OnDurationChange )
    EVT_TEXT( ID_COMPRESS_FACTOR, TruncSilenceDialog::OnDurationChange )
END_EVENT_TABLE()

TruncSilenceDialog::TruncSilenceDialog(EffectTruncSilence * effect,
                                       wxWindow * parent)
:  EffectDialog(parent, _("Truncate Silence"), PROCESS_EFFECT),
   mEffect(effect)
{
   Init();
}

void TruncSilenceDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("by Lynn Allan && Philip Van Baren"));
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

      S.Id( ID_SHORTEST_SILENCE_TEXT ).TieTextBox(_("Min silence duration:"),
                   mEffect->mTruncInitialAllowedSilentMs,
                   10);
      S.AddUnits( _("milliseconds") );
      S.Id( ID_LONGEST_SILENCE_TEXT ).TieTextBox(_("Max silence duration:"),
                   mEffect->mTruncLongestAllowedSilentMs,
                   10);
      S.AddUnits( _("milliseconds") );
      S.Id( ID_COMPRESS_FACTOR ).TieTextBox(_("Silence compression:"),
                   mEffect->mSilenceCompressRatio,
                   10);
      S.AddUnits( _(":1") );
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
   bool bOk =  (mEffect->mTruncInitialAllowedSilentMs > 0.9f) 
            && (mEffect->mTruncLongestAllowedSilentMs > 0.9f)
            && (mEffect->mSilenceCompressRatio >= 1.0f);
   pWarning->SetLabel( bOk ? 
      wxT("") : 
   _("   Duration must be at least 1 millisecond\n   Compress ratio must be at least 1:1")
         );
   wxWindow *pWnd;
   pWnd = FindWindowById( wxID_OK, this );
   pWnd->Enable( bOk );
   pWnd = FindWindowById( ID_EFFECT_PREVIEW, this );
   pWnd->Enable( bOk );

}

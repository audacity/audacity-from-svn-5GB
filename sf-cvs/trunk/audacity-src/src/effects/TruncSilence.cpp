/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.cpp

  Lynn Allan (from DM's Normalize)
  Note: Only works on complete mono track for now

**********************************************************************/

#include <wx/wx.h>
#include <math.h>

#include "../Prefs.h"
#include "../Project.h"
#include "TruncSilence.h"

EffectTruncSilence::EffectTruncSilence() : mUserPrompted(false)
{
   Init();
}

bool EffectTruncSilence::Init()
{
   mTruncLongestAllowedSilentMs = gPrefs->Read(wxT("/CsPresets/TruncLongestAllowedSilentMs"), 200L);
   if ((mTruncLongestAllowedSilentMs < 0) || (mTruncLongestAllowedSilentMs >= 9999999)) {  // corrupted Prefs?
      mTruncLongestAllowedSilentMs = SKIP_EFFECT_MILLISECOND;
      gPrefs->Write(wxT("/CsPresets/TruncLongestAllowedSilentMs"), (int)mTruncLongestAllowedSilentMs);
   }
   mTruncDbChoiceIndex = gPrefs->Read(wxT("/CsPresets/TruncDbChoiceIndex"), 4L);
   if ((mTruncDbChoiceIndex < 0) || (mTruncDbChoiceIndex >= gNumDbChoices)) {  // corrupted Prefs?
      mTruncDbChoiceIndex = gNumDbChoices - 1;  // Off-Skip
      gPrefs->Write(wxT("/CsPresets/TruncDbChoiceIndex"), mTruncDbChoiceIndex);
      mTruncLongestAllowedSilentMs = SKIP_EFFECT_MILLISECOND;
      gPrefs->Write(wxT("/CsPresets/TruncLongestAllowedSilentMs"), (int)mTruncLongestAllowedSilentMs);
   }
   return true;
}

bool EffectTruncSilence::CheckWhetherSkipEffect()
{
   bool rc = ((mTruncDbChoiceIndex >= (gNumDbChoices - 1))
          ||  (mTruncLongestAllowedSilentMs >= SKIP_EFFECT_MILLISECOND));
   return rc;
}

void EffectTruncSilence::End()
{
}

bool EffectTruncSilence::PromptUser()
{
   TruncSilenceDialog dlog(mParent, -1, _("Truncate Silence"));
   dlog.mTruncLongestAllowedSilentMs = mTruncLongestAllowedSilentMs;
   dlog.mTruncDbChoiceIndex = mTruncDbChoiceIndex;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode()) {
      return false;
   }
   mUserPrompted = true;
   mTruncLongestAllowedSilentMs = dlog.mTruncLongestAllowedSilentMs;
   gPrefs->Write(wxT("/CsPresets/TruncLongestAllowedSilentMs"), (long)mTruncLongestAllowedSilentMs);

   mTruncDbChoiceIndex = dlog.mTruncDbChoiceIndex;
   gPrefs->Write(wxT("/CsPresets/TruncDbChoiceIndex"), mTruncDbChoiceIndex);
   
   return true;
}

bool EffectTruncSilence::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferEnum(wxT("Db"),mTruncDbChoiceIndex,gNumDbChoices,gDbChoices);
   shuttle.TransferLongLong(wxT("Duration"),mTruncLongestAllowedSilentMs,200);
   return true;
}

bool EffectTruncSilence::Process()
{
   TrackListIterator iter(mWaveTracks);
   mTrack = (WaveTrack *) iter.First();

   ProcessOne();

   mUserPrompted = false;
   return true;
}

#define QUARTER_SECOND_MS 250
bool EffectTruncSilence::ProcessOne()
{
   float  curFrame;
   float  fabsCurFrame;
   int    consecutiveSilentFrames = 0;
   int    samplesCleared = 0;
   int    truncIndex = 0;
   double curRate = mTrack->GetRate();
   int    quarterSecondFrames = (int)((curRate * QUARTER_SECOND_MS)/ 1000.0);
   int    truncLongestAllowedSilentSamples = int((mTruncLongestAllowedSilentMs * curRate) / 1000.0);
   double truncDbSilenceThreshold = gDb2Signal[mTruncDbChoiceIndex];
   bool   ignoringFrames = false;

   double selectionEnd = mT1;
   double trackEnd = mTrack->GetEndTime();
   longSampleCount selectionStartLongSampleCount = mTrack->TimeToLongSamples(mT0);
   longSampleCount selectionEndLongSampleCount = mTrack->TimeToLongSamples(mT1);
   sampleCount selectionStartSampleCount = selectionStartLongSampleCount;
   sampleCount selectionEndSampleCount = selectionEndLongSampleCount;

   sampleCount totalSampleCount = selectionEndSampleCount - selectionStartSampleCount;
//   sampleCount idealBlockLen = mTrack->GetMaxBlockSize() * 16;   // bigger buffer reduces 'reset'
   sampleCount idealBlockLen = mTrack->GetMaxBlockSize() * 8;   // bigger buffer reduces 'reset'
//   sampleCount idealBlockLen = mTrack->GetMaxBlockSize() * 1;
   sampleCount index = selectionStartSampleCount;
   sampleCount indexAtLoopStart = index;
   sampleCount outTrackOffset = selectionStartSampleCount;
   float *buffer = new float[idealBlockLen];
   bool rc;

   int rampInFrames = (truncLongestAllowedSilentSamples / 4);
   if (rampInFrames > quarterSecondFrames) {
      rampInFrames = quarterSecondFrames;
   }
   while (index < selectionEndSampleCount) {
      rc = mTrack->Get((samplePtr)buffer, floatSample, index, idealBlockLen);
      truncIndex = 0;
      indexAtLoopStart = index;
      ignoringFrames = false;       // may be unneccesary to reset, but safer
      consecutiveSilentFrames = 0;  // may be unneccesary to reset, but safer
      sampleCount limit = idealBlockLen;
      if ((index + idealBlockLen) > selectionEndSampleCount) {
         limit = selectionEndSampleCount - index; 
      }
      double seconds = index / curRate;
      for (sampleCount i = 0; i < limit; ++i) {
         index++;
         curFrame = buffer[i];
         fabsCurFrame = (float)fabs(curFrame);

         if (fabsCurFrame < truncDbSilenceThreshold) {
            consecutiveSilentFrames++;
            if (consecutiveSilentFrames > truncLongestAllowedSilentSamples) {
               ignoringFrames = true;
               continue;  // ignore this frame (equivalent to cutting it)
            }             // otherwise, keep sample to be part of allowed silence
         }
         else {
            if (ignoringFrames == true) {
               float* pb = &buffer[truncIndex]; 
               sampleCount curOffset = i - rampInFrames;
               truncIndex -= rampInFrames; // backup into ignored frames
               wxASSERT((truncIndex < idealBlockLen) && (curOffset > 0) && ((curOffset + rampInFrames) < idealBlockLen));
               for (int fr = 0; fr < rampInFrames; ++fr) {
                  buffer[truncIndex] = buffer[curOffset + fr];
                  truncIndex++;
               }
            }
            consecutiveSilentFrames = 0;
            ignoringFrames = false;
         }
         wxASSERT(truncIndex < idealBlockLen);
         buffer[truncIndex] = curFrame;  // Can get here either because > dbThreshold
         truncIndex++;                   // or silence duration isn't longer than allowed
      }
      samplesCleared += (limit - truncIndex);
      rc = mTrack->Set((samplePtr)buffer, floatSample, outTrackOffset, truncIndex);
      outTrackOffset += truncIndex;
      bool cancellingFlag = TrackProgress(0, ((double)index / (double)selectionEndSampleCount));
      if (cancellingFlag == true) {
         delete [] buffer;
         return false;
      }
   }
   double lenToCut = (samplesCleared / curRate);
   double middle =  selectionEnd - lenToCut;
   mTrack->Cut(middle, selectionEnd, (Track**)&mTrack);

   mT0 = 0.0;
   mT1 = trackEnd - lenToCut;
   if (mUserPrompted == true) {
      AudacityProject *proj = GetActiveProject();
      proj->OnZoomFit();
   }
   gPrefs->Write(wxT("/Validate/TruncSamplesCleared"), samplesCleared);
   delete [] buffer;
   return true;
}

//----------------------------------------------------------------------------
// TruncSilenceDialog
//----------------------------------------------------------------------------

#define ID_LONGEST_SILENCE_TEXT   7000
#define ID_DB_SILENCE_THRESHOLD_CHOICE 7001

BEGIN_EVENT_TABLE(TruncSilenceDialog,wxDialog)
   EVT_BUTTON( wxID_OK, TruncSilenceDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, TruncSilenceDialog::OnCancel )
END_EVENT_TABLE()

TruncSilenceDialog::TruncSilenceDialog(wxWindow *parent, wxWindowID id, const wxString &title) :
   wxDialog( parent, id, title )
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxStaticText *statText = new wxStaticText(this, -1,
                           _("Truncate Silence by Lynn Allan"));
   mainSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   statText = new wxStaticText(this, -1, _("Max Silent Duration (milliseconds): \n"
                                           "(99999 or greater is off)"));
   hSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);
   
   wxString truncLongestAllowedSilentMsStr;
   truncLongestAllowedSilentMsStr.Printf(wxT("%d"), mTruncLongestAllowedSilentMs);
   mTruncLongestAllowedSilentMsText = new wxTextCtrl(this, ID_LONGEST_SILENCE_TEXT, 
                     truncLongestAllowedSilentMsStr, wxDefaultPosition,
                     wxSize(60, -1), 0,
                     wxTextValidator(wxFILTER_NUMERIC));
   hSizer->Add(mTruncLongestAllowedSilentMsText, 0, wxALL, 5);
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);
   hSizer = new wxBoxSizer(wxHORIZONTAL);

   statText = new wxStaticText(this, -1, _("Theshold for silence: "));
   hSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);

   mTruncDbSilenceThresholdChoice = new wxChoice(this, ID_DB_SILENCE_THRESHOLD_CHOICE,
                                   wxDefaultPosition, wxSize(64, -1), gNumDbChoices,
                                   gDbChoices);
   hSizer->Add(mTruncDbSilenceThresholdChoice, 0, wxALIGN_CENTER | wxALL, 4);
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);
   hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *cancel = new wxButton(this, wxID_CANCEL, _("Cancel"));
   hSizer->Add(cancel, 0, wxALIGN_CENTRE|wxALL, 5);

   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   ok->SetFocus();
   hSizer->Add(ok, 0, wxALIGN_CENTRE|wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

bool TruncSilenceDialog::TransferDataToWindow()
{
   mTruncLongestAllowedSilentMsText->SetValue(wxString::Format(wxT("%d"), mTruncLongestAllowedSilentMs));
   mTruncDbSilenceThresholdChoice->SetSelection(mTruncDbChoiceIndex);

   return true;
}

bool TruncSilenceDialog::TransferDataFromWindow()
{
   long ms;
   mTruncLongestAllowedSilentMsText->GetValue().ToLong(&ms);
   mTruncLongestAllowedSilentMs = ms;

   mTruncDbChoiceIndex = mTruncDbSilenceThresholdChoice->GetSelection();

   return true;
}

void TruncSilenceDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   EndModal(true);
}

void TruncSilenceDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(false);
}

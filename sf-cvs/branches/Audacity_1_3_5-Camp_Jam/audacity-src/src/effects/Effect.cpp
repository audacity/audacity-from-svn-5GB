/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

*******************************************************************//**

\class Effect
\brief Base class for many of the effects in Audacity.

*//****************************************************************//**

\class EffectDialog
\brief New (Jun-2006) base class for effects dialogs.  Likely to get 
greater use in future.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/string.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/sizer.h>
#include <wx/timer.h>

#include "Effect.h"
#include "../AudioIO.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../WaveTrack.h"

//
// public static methods
//

EffectArray Effect::mEffects;
int Effect::sNumEffects = 0;
double Effect::sDefaultGenerateLen = 30.0;

// Some static variables used for Repeat Last Effect.
int Effect::LastType=0;
int Effect::LastIndex=0;
Effect * Effect::pLastEffect=NULL;

wxString Effect::StripAmpersand(const wxString& str)
{
   wxString strippedStr = str;
   strippedStr.Replace(wxT("&"), wxT(""));
   return strippedStr;
}

void Effect::RegisterEffect(Effect *f, int NewFlags)
{
   f->mID = sNumEffects;
   sNumEffects++;
   if( NewFlags != 0)
      f->SetEffectFlags( NewFlags );

   // Insert the effect into the list in alphabetical order
   // A linear search is good enough as long as there are
   // only a few dozen or even a few hundred effects.
   wxString name = StripAmpersand(f->GetEffectName());
   int len = mEffects.GetCount();
   int i;
   for(i=0; i<len; i++)
      if (name.CmpNoCase(StripAmpersand(mEffects[i]->GetEffectName())) < 0) {
         mEffects.Insert(f, i);
         break;
      }
   if (i==len)
      mEffects.Add(f);
}

void Effect::UnregisterEffects()
{
   for(int i=0; i<sNumEffects; i++)
      delete mEffects[i];

   mEffects.Clear();
}

int Effect::GetNumEffects()
{
   return sNumEffects;
}

Effect *Effect::GetEffect(int ID)
{
   for(int i=0; i<sNumEffects; i++)
      if (mEffects[i]->mID == ID)
         return mEffects[i];
   
   return NULL;
}

Effect* Effect::GetEffectByIdentifier(const wxString strTarget, const int kFlags /*= ALL_EFFECTS*/)
{
   for (unsigned int i = 0; i < mEffects.GetCount(); i++) 
   {
      int nFlags = mEffects[i]->GetEffectFlags();
      if (((nFlags & kFlags) == nFlags) && strTarget.IsSameAs(mEffects[i]->GetEffectIdentifier()))
         return mEffects[i];
   }
   return NULL;
}

EffectArray *Effect::GetEffects(int flags /* = ALL_EFFECTS */)
{
   EffectArray *results = new EffectArray();

   int len = mEffects.GetCount();
   for(int i=0; i<len; i++) {
      int g = mEffects[i]->GetEffectFlags();
      if ((flags & g) == g)
         results->Add(mEffects[i]);
   }

   return results;
}

//
// public methods
//

Effect::Effect()
{
   mWaveTracks = NULL;
   m_pOutputWaveTracks = NULL;

   // Can change effect flags later (this is the new way)
   // OR using the old way, over-ride GetEffectFlags().
   mFlags = BUILTIN_EFFECT | PROCESS_EFFECT | ADVANCED_EFFECT ;
}

bool Effect::DoEffect(wxWindow *parent, int flags,
                      double projectRate,
                      TrackList *list,
                      TrackFactory *factory,
                      double *t0, double *t1)
{
   wxASSERT(*t0 <= *t1);

   if (mWaveTracks) {
      delete mWaveTracks;
      mWaveTracks = NULL;
   }

   if (m_pOutputWaveTracks) {
      delete m_pOutputWaveTracks;
      m_pOutputWaveTracks = NULL;
   }

   mFactory = factory;
   mProjectRate = projectRate;
   mParent = parent;
   mDialog = NULL; // It's up to descendants to set this.
   mTracks = list;
   mT0 = *t0;
   mT1 = *t1;
   CountWaveTracks();

   if (!Init())
      return false;

   // Don't prompt user if we are dealing with a 
   // effect that is already configured, e.g. repeating
   // the last effect on a different selection.
   if( (flags & CONFIGURED_EFFECT) == 0)
   {
      if (!PromptUser())
         return false;
   }

   bool returnVal = true;
   bool skipFlag = CheckWhetherSkipEffect();
   if (skipFlag == false) {
      GetActiveProject()->ProgressShow(StripAmpersand(GetEffectName()),
                                       GetEffectAction());
      returnVal = Process();
      GetActiveProject()->ProgressHide();
   }

   End();

   delete mWaveTracks;
   if (m_pOutputWaveTracks != mWaveTracks) // If processing completed successfully, they should be the same.
      delete m_pOutputWaveTracks;
   mWaveTracks = NULL;
   m_pOutputWaveTracks = NULL;

   if (returnVal) {
      *t0 = mT0;
      *t1 = mT1;
   }
   
   return returnVal;
}

bool Effect::TotalProgress(double frac)
{
   return !GetActiveProject()->ProgressUpdate((int)(frac*1000 + 0.5));
}

bool Effect::TrackProgress(int whichTrack, double frac)
{
   return TotalProgress((whichTrack+frac)/mNumTracks);
}

bool Effect::TrackGroupProgress(int whichGroup, double frac)
{
   return TotalProgress((whichGroup+frac)/mNumGroups);
}

//
// private methods
//
// Use these two methods to copy the input tracks to m_pOutputWaveTracks, if 
// doing the processing on them, and replacing the originals only on success (and not cancel).
void Effect::CopyInputWaveTracks()
{
   if (this->GetNumWaveTracks() <= 0)
      return;

   // Copy the mWaveTracks, to process the copies.
   TrackListIterator iterIn(mWaveTracks);
   WaveTrack* pInWaveTrack = (WaveTrack*)(iterIn.First());
   m_pOutputWaveTracks = new TrackList();
   WaveTrack* pOutWaveTrack = NULL;
   while (pInWaveTrack != NULL)
   {
      pOutWaveTrack = mFactory->DuplicateWaveTrack(*(WaveTrack*)pInWaveTrack);
      m_pOutputWaveTracks->Add(pOutWaveTrack);
      pInWaveTrack = (WaveTrack*)(iterIn.Next());
   }
}


// If bGoodResult, replace mWaveTracks tracks in mTracks with successfully processed 
// m_pOutputWaveTracks copies, get rid of old mWaveTracks, and set mWaveTracks to m_pOutputWaveTracks. 
// Else clear and delete m_pOutputWaveTracks copies.
void Effect::ReplaceProcessedWaveTracks(const bool bGoodResult)
{
   if (bGoodResult)
   {
      // Circular replacement of the input wave tracks with the processed m_pOutputWaveTracks tracks. 
      // But mWaveTracks is temporary, so replace in mTracks. More bookkeeping.
      TrackListIterator iterIn(mWaveTracks);
      WaveTrack* pInWaveTrack = (WaveTrack*)(iterIn.First());

      wxASSERT(m_pOutputWaveTracks != NULL); // Make sure we at least did the CopyInputWaveTracks().
      TrackListIterator iterOut(m_pOutputWaveTracks);
      WaveTrack* pOutWaveTrack = (WaveTrack*)(iterOut.First());

      TrackListIterator iterAllTracks(mTracks);
      Track* pFirstTrack = iterAllTracks.First();
      Track* pTrack = pFirstTrack;
      do
      {
         if (pTrack == pInWaveTrack)
         {
            // Replace pInWaveTrack with processed pOutWaveTrack, at end of list.
            if (pOutWaveTrack != NULL) // Can be NULL as result of Tracks > Stereo to Mono.
            {
               mTracks->Add(pOutWaveTrack);
               if (pTrack == pFirstTrack)
                  pFirstTrack = pOutWaveTrack; // We replaced the first track, so update stop condition.
            }
            delete pInWaveTrack;

            pInWaveTrack = (WaveTrack*)(iterIn.Next());
            pOutWaveTrack = (WaveTrack*)(iterOut.Next());
         }
         else
            mTracks->Add(pTrack); // Add pTrack back to end of list.

         // Remove former pTrack from front of list and set pTrack to next.
         pTrack = iterAllTracks.RemoveCurrent(); 
      } while (pTrack != pFirstTrack);

      // Also need to clean up mWaveTracks, primarily because Preview uses it as the processed tracks. 
      delete mWaveTracks;
      mWaveTracks = m_pOutputWaveTracks;
   }
   else
   {
      // Processing failed or was cancelled so throw away the processed tracks.
      m_pOutputWaveTracks->Clear(true); // true => delete the tracks
   }
}

void Effect::CountWaveTracks()
{
   mNumTracks = 0;
   mNumGroups = 0;
   mWaveTracks = new TrackList();
   
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   
   while(t) {
      if (!t->GetSelected()) {
         t = iter.Next();
         continue;
      }
      
      if (t->GetKind() == Track::Wave) {
         mWaveTracks->Add(t);
         mNumTracks++;
         if (!t->GetLinked())
            mNumGroups++;
      }
      t = iter.Next();
   }
}

float TrapFloat(float x, float min, float max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

double TrapDouble(double x, double min, double max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

long TrapLong(long x, long min, long max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

wxString Effect::GetPreviewName()
{
   return _("Pre&view");
}

void Effect::Preview()
{
   if (gAudioIO->IsBusy())
      return;

   // Mix the first 3 seconds of audio from all of the tracks
   double previewLen = 3.0;
   gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen);
   
   WaveTrack *mixLeft = NULL;
   WaveTrack *mixRight = NULL;
   double rate = mProjectRate;
   double t0 = mT0;
   double t1 = t0 + previewLen;

   if (t1 > mT1)
      t1 = mT1;

   if (t1 <= t0)
      return;

   if (!::MixAndRender(mWaveTracks, mFactory, rate, floatSample, t0, t1,
                       &mixLeft, &mixRight))
   {
      if (mDialog)
      {
         GetActiveProject()->SetEnabledWindow(mDialog);
         mDialog->SetFocus();
      }
      return;
   }

   // Apply effect

   TrackList *saveWaveTracks = mWaveTracks;
   mWaveTracks = new TrackList();
   mWaveTracks->Add(mixLeft);
   if (mixRight)
      mWaveTracks->Add(mixRight);

   t0 = 0.0;
   t1 = mixLeft->GetEndTime();

   double t0save = mT0;
   double t1save = mT1;
   mT0 = t0;
   mT1 = t1;

   // Effect is already inited; we call Process, End, and then Init
   // again, so the state is exactly the way it was before Preview
   // was called.
   GetActiveProject()->ProgressShow(StripAmpersand(GetEffectName()),
                                    _("Preparing preview"));
   bool bSuccess = Process();
   GetActiveProject()->ProgressHide(mDialog);
   End();
   Init();
   if (bSuccess)
   {
      mT0 = t0save;
      mT1 = t1save;

      WaveTrackArray playbackTracks;
      WaveTrackArray recordingTracks;
      // Probably not the same tracks post-processing, so can't rely on previous values of mixLeft & mixRight.
      TrackListIterator iter(mWaveTracks); 
      mixLeft = (WaveTrack*)(iter.First());
      mixRight = (WaveTrack*)(iter.Next());
      playbackTracks.Add(mixLeft);
      if (mixRight)
         playbackTracks.Add(mixRight);

      // Start audio playing

      int token =
         gAudioIO->StartStream(playbackTracks, recordingTracks, NULL,
                               rate, t0, t1, NULL);

      if (token) {
         bool previewing = true;

         GetActiveProject()->ProgressShow(StripAmpersand(GetEffectName()),
                                          _("Previewing"));

         while (gAudioIO->IsStreamActive(token) && previewing) {
            ::wxMilliSleep(100);
            int t = int(1000 * (gAudioIO->GetStreamTime() / t1));
            previewing = GetActiveProject()->ProgressUpdate(t);
         }
         gAudioIO->StopStream();

         while (gAudioIO->IsBusy()) {
            ::wxMilliSleep(100);
         }

         GetActiveProject()->ProgressHide(mDialog);
      }
      else {
         wxMessageBox(_("Error while opening sound device. Please check the output device settings and the project sample rate."),
                      _("Error"), wxOK | wxICON_EXCLAMATION, mParent);
      }
   }

   if (mWaveTracks == m_pOutputWaveTracks) // typical case, but depends on descendant implementation
      m_pOutputWaveTracks = NULL; 
   mWaveTracks->Clear(true); // true => delete the tracks
   delete mWaveTracks;

   mWaveTracks = saveWaveTracks;

   if (mDialog)
      mDialog->SetFocus();
}

EffectDialog::EffectDialog(wxWindow * parent,
                           const wxString & title,
                           int type,
                           int flags)
: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags)
{
   mType = type;
}

void EffectDialog::Init()
{
   ShuttleGui S(this, eIsCreating);
   
   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      PopulateOrExchange(S);

      long buttons = eOkButton;

      if (mType == PROCESS_EFFECT || mType == INSERT_EFFECT)
      {
         buttons |= eCancelButton;
         if (mType == PROCESS_EFFECT)
         {
            buttons |= ePreviewButton;
         }
      }
      S.AddStandardButtons(buttons);
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

/// This is a virtual function which will be overridden to
/// provide the actual parameters that we want for each
/// kind of dialog.
void EffectDialog::PopulateOrExchange(ShuttleGui & S)
{
   return;
}

bool EffectDialog::TransferDataToWindow()
{
   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::Validate()
{
   return true;
}

void EffectDialog::OnPreview(wxCommandEvent & event)
{
   return;
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
// arch-tag: 113bebd2-dbcf-4fc5-b3b4-b52d6ac4efb2


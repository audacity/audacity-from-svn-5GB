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
#include "../widgets/ProgressDialog.h"
#include "../ondemand/ODManager.h"
//
// public static methods
//

double Effect::sDefaultGenerateLen = 30.0;


wxString Effect::StripAmpersand(const wxString& str)
{
   wxString strippedStr = str;
   strippedStr.Replace(wxT("&"), wxT(""));
   return strippedStr;
}


//
// public methods
//

Effect::Effect()
{
   mWaveTracks = NULL;
   mOutputWaveTracks = NULL;

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

   if (mOutputWaveTracks) {
      delete mOutputWaveTracks;
      mOutputWaveTracks = NULL;
   }

   mFactory = factory;
   mProjectRate = projectRate;
   mParent = parent;
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
      mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
                                     GetEffectAction());
      returnVal = Process();
      delete mProgress;
   }

   End();

   delete mWaveTracks;
   if (mOutputWaveTracks != mWaveTracks) // If processing completed successfully, they should be the same.
      delete mOutputWaveTracks;
   mWaveTracks = NULL;
   mOutputWaveTracks = NULL;

   if (returnVal) {
      *t0 = mT0;
      *t1 = mT1;
   }
   
   return returnVal;
}

bool Effect::TotalProgress(double frac)
{
   int updateResult = mProgress->Update(frac);
   if (updateResult == eProgressSuccess)
     return false;
   return true;
}

bool Effect::TrackProgress(int whichTrack, double frac)
{
   int updateResult = mProgress->Update(whichTrack + frac, (double) mNumTracks);
   if (updateResult == eProgressSuccess)
     return false;
   return true;
}

bool Effect::TrackGroupProgress(int whichGroup, double frac)
{
   int updateResult = mProgress->Update(whichGroup + frac, (double) mNumGroups);
   if (updateResult == eProgressSuccess)
     return false;
   return true;
}

//
// private methods
//
// Use these two methods to copy the input tracks to mOutputWaveTracks, if 
// doing the processing on them, and replacing the originals only on success (and not cancel).
void Effect::CopyInputWaveTracks()
{
   if (this->GetNumWaveTracks() <= 0)
      return;

   // Copy the mWaveTracks, to process the copies.
   TrackListIterator iterIn(mWaveTracks);
   WaveTrack* pInWaveTrack = (WaveTrack*)(iterIn.First());
   mOutputWaveTracks = new TrackList();
   WaveTrack* pOutWaveTrack = NULL;
   while (pInWaveTrack != NULL)
   {
      pOutWaveTrack = mFactory->DuplicateWaveTrack(*(WaveTrack*)pInWaveTrack);
      mOutputWaveTracks->Add(pOutWaveTrack);
      pInWaveTrack = (WaveTrack*)(iterIn.Next());
      
   }
}


// If bGoodResult, replace mWaveTracks tracks in mTracks with successfully processed 
// mOutputWaveTracks copies, get rid of old mWaveTracks, and set mWaveTracks to mOutputWaveTracks. 
// Else clear and delete mOutputWaveTracks copies.
void Effect::ReplaceProcessedWaveTracks(const bool bGoodResult)
{
   if (bGoodResult)
   {
      // Circular replacement of the input wave tracks with the processed mOutputWaveTracks tracks. 
      // But mWaveTracks is temporary, so replace in mTracks. More bookkeeping.
      TrackListIterator iterIn(mWaveTracks);
      WaveTrack* pInWaveTrack = (WaveTrack*)(iterIn.First());

      wxASSERT(mOutputWaveTracks != NULL); // Make sure we at least did the CopyInputWaveTracks().
      TrackListIterator iterOut(mOutputWaveTracks);
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
                  
               //swap the wavecache track the ondemand task uses, since now the new one will be kept in the project
//#ifdef EXPERIMENTAL_ONDEMAND      
               if(ODManager::IsInstanceCreated())
                  ODManager::Instance()->ReplaceWaveTrack(pInWaveTrack,pOutWaveTrack);
//#endif
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
      mWaveTracks = mOutputWaveTracks;
   }
   else
   {
      // Processing failed or was cancelled so throw away the processed tracks.
      mOutputWaveTracks->Clear(true); // true => delete the tracks
      
      //TODO:undo the non-gui ODTask transfer
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

void Effect::HandleLinkedTracksOnGenerate(double length, double t0)
{
   AudacityProject *p = (AudacityProject*)mParent;
   if ( !p || !(p->IsSticky()) || (mT1!=mT0) ) return;
   
   TrackListIterator iter(p->GetTracks());
   
   int editGroup = 0;
   bool handleGroup = false;
   Track *t=iter.First();
   Track *n=t;
   
   while (t){//find edit group number
      n=iter.Next();
      if (t->GetSelected()) handleGroup = true;
      if ( (n && n->GetKind()==Track::Wave && t->GetKind()==Track::Label) 
            || (!n) ) {
         if (handleGroup){
            t=iter.First();
            for (int i=0; i<editGroup; i++){//go to first in edit group
               while (t && t->GetKind()==Track::Wave) t=iter.Next();
               while (t && t->GetKind()==Track::Label) t=iter.Next();
            }
            while (t && t->GetKind()==Track::Wave){
               if ( !(t->GetSelected()) ){
                  //printf("t(w)(gen): %x\n", t);
                  TrackFactory *factory = p->GetTrackFactory();
                  WaveTrack *tmp = factory->NewWaveTrack( ((WaveTrack*)t)->GetSampleFormat(), ((WaveTrack*)t)->GetRate());
                  tmp->InsertSilence(0.0, length);
                  tmp->Flush();
                  ((WaveTrack *)t)->HandlePaste(t0, tmp);
               }
               t=iter.Next();
            }
            while (t && t->GetKind()==Track::Label){
               //printf("t(l)(gen): %x\n", t);
               ((LabelTrack *)t)->ShiftLabelsOnInsert(length, t0);
               t=iter.Next();         
            }
         }
         editGroup++;
         handleGroup = false;
      }
      t=n;
   }
}

bool Effect::HandleGroupChangeSpeed(double m_PercentChange, double mCurT0, double mCurT1)
{
   AudacityProject *p = (AudacityProject*)mParent;   
   if (mCurT1 < mCurT0) return false;
   TrackListIterator fullIter(p->GetTracks());
   
   int editGroup=0;   
   bool insertSilence = false;
   
   double length = ( (mCurT1-mCurT0) / ((100 + m_PercentChange)/100) ) - (mCurT1-mCurT0);
   if (length < 0) length = -length;  
   
   Track *t = fullIter.First();
   Track *n = t;
   while (t){//find edit group
      n=fullIter.Next();
      if (n && n->GetKind()==Track::Wave && t->GetKind()==Track::Label) 
         editGroup++;
         
      if (t->GetSelected()){
         t=fullIter.First();
         for (int i=0; i<editGroup; i++){//go to first in edit group
            while (t && t->GetKind()==Track::Wave) t=fullIter.Next();
            while (t && t->GetKind()==Track::Label) t=fullIter.Next();
         }
         insertSilence = false;
         while (t && t->GetKind()==Track::Wave){
            if ( !(t->GetSelected()) ){
               if (m_PercentChange > 0) insertSilence = true;
               if (m_PercentChange < 0){
                  TrackFactory *factory = p->GetTrackFactory();
                  WaveTrack *tmp = factory->NewWaveTrack( ((WaveTrack*)t)->GetSampleFormat(), ((WaveTrack*)t)->GetRate());
                  tmp->InsertSilence(0.0, length);
                  tmp->Flush();
                  if ( !( ((WaveTrack *)t)->HandlePaste(mCurT1, tmp)) ) return false;
               }
            }
            t=fullIter.Next();
         }

         if (insertSilence){
            t=fullIter.First();
            for (int i=0; i<editGroup; i++){//go to first in edit group
               while (t && t->GetKind()==Track::Wave) t=fullIter.Next();
               while (t && t->GetKind()==Track::Label) t=fullIter.Next();
            }
            while ( t && t->GetKind()==Track::Wave ){
               if (t->GetSelected()){
                  TrackFactory *factory = p->GetTrackFactory();
                  WaveTrack *tmp = factory->NewWaveTrack( ((WaveTrack*)t)->GetSampleFormat(), ((WaveTrack*)t)->GetRate());
                  tmp->InsertSilence(0.0, length);
                  tmp->Flush();
                  double change = (100 + m_PercentChange)/100;
                  double insertPt = mCurT1 - (mCurT1 - mCurT0) + ((mCurT1 - mCurT0)/change);
                  if ( !( ((WaveTrack *)t)->HandlePaste(insertPt, tmp)) ) return false;
               }
               t=fullIter.Next();
            }
         }
         
         while (t && t->GetKind()==Track::Label && !insertSilence){
            ((LabelTrack *)t)->ShiftLabelsOnChangeSpeed(mCurT0, mCurT1, m_PercentChange);
            t=fullIter.Next();
         }
         n=t;
         editGroup++;//we've gone through a edit group
      }else
         t=n;
   }
   return true;  
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
   mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
                                  _("Preparing preview"));
   bool bSuccess = Process();
   delete mProgress;
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
         int previewing = eProgressSuccess;

         mProgress = new ProgressDialog(StripAmpersand(GetEffectName()),
                                        _("Previewing"), pdlgHideCancelButton);

         while (gAudioIO->IsStreamActive(token) && previewing == eProgressSuccess) {
            ::wxMilliSleep(100);
            previewing = mProgress->Update(gAudioIO->GetStreamTime(), t1);
         }
         gAudioIO->StopStream();

         while (gAudioIO->IsBusy()) {
            ::wxMilliSleep(100);
         }

         delete mProgress;
      }
      else {
         wxMessageBox(_("Error while opening sound device. Please check the output device settings and the project sample rate."),
                      _("Error"), wxOK | wxICON_EXCLAMATION, mParent);
      }
   }

   if (mWaveTracks == mOutputWaveTracks) // typical case, but depends on descendant implementation
      mOutputWaveTracks = NULL; 
   mWaveTracks->Clear(true); // true => delete the tracks
   delete mWaveTracks;

   mWaveTracks = saveWaveTracks;
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


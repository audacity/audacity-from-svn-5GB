/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/string.h>
#include <wx/progdlg.h>
#include <wx/timer.h>

#include "Effect.h"
#include "../WaveTrack.h"

//
// public static methods
//

EffectArray *Effect::mEffects = new EffectArray();

int Effect::RegisterEffect(Effect * f)
{
   mEffects->Add(f);
   return mEffects->Count();
}

int Effect::GetNumEffects()
{
   return mEffects->Count();
}

Effect *Effect::GetEffect(int i)
{
   return (*mEffects)[i];
}

//
// public methods
//

Effect::Effect()
{
   mWaveTracks = NULL;
   mProgress = NULL;
}

bool Effect::DoEffect(wxWindow *parent, TrackList *list, double t0, double t1)
{
   wxASSERT(t0 <= t1);

   if (mWaveTracks) {
      delete mWaveTracks;
      mWaveTracks = NULL;
   }

   mParent = parent;
   mTracks = list;
   mT0 = t0;
   mT1 = t1;
   CountWaveTracks();

   if (!Init())
      return false;
   
   if (!PromptUser())
      return false;
      
   wxBusyCursor busy;
   wxYield();
   wxStartTimer();

   bool returnVal = Process();

   End();
   
   if (mProgress) {
      delete mProgress;
      mProgress = NULL;
   }
   
   delete mWaveTracks;
   mWaveTracks = NULL;
   
   return returnVal;
}

void Effect::GetSamples(WaveTrack *t, sampleCount *s0, sampleCount *slen)
{
   wxASSERT(s0);
   wxASSERT(slen);

   int ss0 = (int) ((mT0 - t->tOffset) * t->rate);
   int ss1 = (int) ((mT1 - t->tOffset) * t->rate);

   if (ss0 < 0)
      ss0 = 0;
   if (ss1 >= t->numSamples)
      ss1 = t->numSamples;
   
   if (ss1 < ss0)
      ss1 = ss0;
   
   *s0 = (sampleCount)ss0;
   *slen = (sampleCount)(ss1 - ss0);
}

bool Effect::TotalProgress(double frac)
{
   if (!mProgress && wxGetElapsedTime(false) > 500) {
      mProgress =
         new wxProgressDialog(GetEffectName(),
                              GetEffectAction(),
                              1000,
                              mParent,
                              wxPD_CAN_ABORT |
                              wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
   }
   
   bool cancelling = false;

   if (mProgress) {
      cancelling =
         !mProgress->Update((int)(frac*1000 + 0.5));
   }
   
   return cancelling;
}

bool Effect::TrackProgress(int whichTrack, double frac)
{
   return TotalProgress((whichTrack+frac)/mNumTracks);
}

bool Effect::TrackGroupProgress(int whichGroup, double frac)
{
   return TotalProgress((whichGroup+frac)/mNumGroups);
}

void Effect::CountWaveTracks()
{
   mNumTracks = 0;
   mNumGroups = 0;
   mWaveTracks = new TrackList();
   
   TrackListIterator iter(mTracks);
   VTrack *t = iter.First();
   
   while(t) {
      if (!t->selected)
         continue;
      
      if (t->GetKind() == VTrack::Wave) {
         mWaveTracks->Add(t);
         mNumTracks++;
         if (!t->linked)
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

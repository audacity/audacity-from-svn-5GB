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

EffectArray *Effect::mEffects[2] = {new EffectArray(), new EffectArray()};

int Effect::RegisterEffect(Effect * f, bool plugin)
{
   mEffects[plugin?1:0]->Add(f);
   return mEffects[plugin?1:0]->Count();
}

int Effect::GetNumEffects(bool plugin)
{
   return mEffects[plugin?1:0]->Count();
}

Effect *Effect::GetEffect(int i, bool plugin)
{
   return (*mEffects[plugin?1:0])[i];
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

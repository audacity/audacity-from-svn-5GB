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

EffectArray Effect::mEffects;
int Effect::sNumEffects = 0;
double Effect::sDefaultGenerateLen = 30.0;

void Effect::RegisterEffect(Effect *f)
{
   f->mID = sNumEffects;
   sNumEffects++;

   // Insert the effect into the list in alphabetical order
   // A linear search is good enough as long as there are
   // only a few dozen or even a few hundred effects.
   wxString name = f->GetEffectName();
   int len = mEffects.GetCount();
   int i;
   for(i=0; i<len; i++)
      if (name.CmpNoCase(mEffects[i]->GetEffectName()) < 0) {
         mEffects.Insert(f, i);
         break;
      }
   if (i==len)
      mEffects.Add(f);
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
   mProgress = NULL;
}

bool Effect::DoEffect(wxWindow *parent, TrackList *list,
                      TrackFactory *factory,
                      double *t0, double *t1)
{
   wxASSERT(*t0 <= *t1);

   if (mWaveTracks) {
      delete mWaveTracks;
      mWaveTracks = NULL;
   }

   mFactory = factory;
   mParent = parent;
   mTracks = list;
   mT0 = *t0;
   mT1 = *t1;
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

   if (returnVal) {
      *t0 = mT0;
      *t1 = mT1;
   }
   
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

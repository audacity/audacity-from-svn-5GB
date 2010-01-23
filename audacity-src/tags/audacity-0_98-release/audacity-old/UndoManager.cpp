/**********************************************************************

  Audacity: A Digital Audio Editor

  UndoManager.cpp

  Dominic Mazzoni

**********************************************************************/

#include "UndoManager.h"
#include "Track.h"

#include "WaveTrack.h"          // temp

UndoManager::UndoManager()
{
   current = -1;
}

UndoManager::~UndoManager()
{
   ClearStates();
}

void UndoManager::ClearStates()
{
   for (int i = stack.Count() - 1; i >= 0; i--) {
      TrackListIterator iter(stack[i]->tracks);

      VTrack *t = iter.First();
      while (t) {
         delete t;
         t = iter.Next();
      }
      stack.RemoveAt(i);
   }

   current = -1;
}

bool UndoManager::UndoAvailable()
{
   return (current > 0);
}

bool UndoManager::RedoAvailable()
{
   return (current < stack.Count() - 1);
}

void UndoManager::PushState(TrackList * l, double sel0, double sel1)
{
   int i;

   for (i = current + 1; i < stack.Count(); i++) {
      TrackListIterator iter(stack[i]->tracks);
      VTrack *t = iter.First();
      while (t) {
         delete t;
         t = iter.Next();
      }
   }

   i = stack.Count() - 1;
   while (i > current)
      stack.RemoveAt(i--);

   TrackList *tracksCopy = new TrackList();
   TrackListIterator iter(l);
   VTrack *t = iter.First();
   while (t) {
      tracksCopy->Add(t->Duplicate());
      t = iter.Next();
   }

   UndoStackElem *push = new UndoStackElem();
   push->tracks = tracksCopy;
   push->sel0 = sel0;
   push->sel1 = sel1;

   stack.Add(push);
   current++;
}

TrackList *UndoManager::Undo(double *sel0, double *sel1)
{
   wxASSERT(UndoAvailable());

   current--;

   *sel0 = stack[current]->sel0;
   *sel1 = stack[current]->sel1;

   return stack[current]->tracks;
}

TrackList *UndoManager::Redo(double *sel0, double *sel1)
{
   wxASSERT(RedoAvailable());

   current++;

   *sel0 = stack[current]->sel0;
   *sel1 = stack[current]->sel1;

   return stack[current]->tracks;
}

void UndoManager::Debug()
{
   for (int i = 0; i < stack.Count(); i++) {

      TrackListIterator iter(stack[i]->tracks);
      WaveTrack *t = (WaveTrack *) (iter.First());
      printf("*%d* %s %d\n", i, (i == current) ? "-->" : "   ",
             t ? t->numSamples : 0);
   }
}

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


void UndoManager::GetDescription(unsigned int n, wxString *desc, wxString *size)
{
   n -= 1; // 1 based to zero based

   wxASSERT(n < stack.Count());

   *desc = stack[n]->description;
   *size = "X MB";
}

void UndoManager::SetDescription(unsigned int n, wxString desc)
{
   n -= 1;

   wxASSERT(n < stack.Count());

   stack[n]->description = desc;
}

void UndoManager::RemoveStateAt(int n)
{
   TrackListIterator iter(stack[n]->tracks);

   VTrack *t = iter.First();
   while (t) {
      delete t;
      t = iter.Next();
   }
   stack.RemoveAt(n);

   current -= 1;
}


void UndoManager::RemoveStates(int num)
{
   for (int i = 0; i < num; i++)
      RemoveStateAt(0);
}
   
void UndoManager::ClearStates()
{
   RemoveStates(stack.Count());
}

unsigned int UndoManager::GetNumStates()
{
   return stack.Count();
}

unsigned int UndoManager::GetCurrentState()
{
   return current + 1;  // the array is 0 based, the abstraction is 1 based
}

bool UndoManager::UndoAvailable()
{
   return (current > 0);
}

bool UndoManager::RedoAvailable()
{
   return (current < (int)stack.Count() - 1);
}

void UndoManager::PushState(TrackList * l, double sel0, double sel1,
                            wxString desc)
{
   unsigned int i;

   for (i = current + 1; i < stack.Count(); i++) {
      TrackListIterator iter(stack[i]->tracks);
      VTrack *t = iter.First();
      while (t) {
         delete t;
         t = iter.Next();
      }
   }

   i = stack.Count() - 1;
   while (i > (unsigned int)current)
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
   push->description = desc;

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
   for (unsigned int i = 0; i < stack.Count(); i++) {

      TrackListIterator iter(stack[i]->tracks);
      WaveTrack *t = (WaveTrack *) (iter.First());
      printf("*%d* %s %d\n", i, (i == (unsigned int)current) ? "-->" : "   ",
             t ? t->numSamples : 0);
   }
}

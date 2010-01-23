/**********************************************************************

  Audacity: A Digital Audio Editor

  UndoManager.cpp

  Dominic Mazzoni

*******************************************************************//**

\class UndoManager
\brief Works with HistoryWindow to provide the Undo functionality.

*//****************************************************************//**

\class UndoStackElem
\brief Holds one item with description and time range for the 
UndoManager

*//*******************************************************************/


#include "Audacity.h"

#include <wx/textctrl.h>
#include <wx/log.h>

#include "Internat.h"
#include "UndoManager.h"
#include "Track.h"

#include "WaveTrack.h"          // temp

UndoManager::UndoManager()
{
   current = -1;
   saved = -1;
   consolidationCount = 0;
}

UndoManager::~UndoManager()
{
   ClearStates();
}


void UndoManager::GetLongDescription(unsigned int n, wxString *desc,
                                     wxString *size)
{
   n -= 1; // 1 based to zero based

   wxASSERT(n < stack.Count());
   wxLongLong bytes;

   *desc = stack[n]->description;

   if (n == 0)
      bytes = stack[n]->tracks->GetSpaceUsage();
   else {
      bytes = stack[n]->tracks->GetAdditionalSpaceUsage(&stack);
   }

   *size = Internat::FormatSize(bytes);
}

void UndoManager::GetShortDescription(unsigned int n, wxString *desc)
{
   n -= 1; // 1 based to zero based

   wxASSERT(n < stack.Count());

   *desc = stack[n]->shortDescription;
}

void UndoManager::SetLongDescription(unsigned int n, wxString desc)
{
   n -= 1;

   wxASSERT(n < stack.Count());

   stack[n]->description = desc;
}

void UndoManager::RemoveStateAt(int n)
{
   TrackListIterator iter(stack[n]->tracks);
   Track *t = iter.First();
   while(t)
   {
      delete t;
      t = iter.Next();
   }

   delete stack[n]->tracks;

   UndoStackElem *tmpStackElem = stack[n];
   stack.RemoveAt(n);
   delete tmpStackElem;

   current -= 1;
   saved -= 1;
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

void UndoManager::ModifyState(TrackList * l, double sel0, double sel1)
{
   // Delete current

   TrackListIterator iter(stack[current]->tracks);
   Track *t = iter.First();
   while (t) {
      delete t;
      t = iter.Next();
   }

   // Duplicate

   TrackList *tracksCopy = new TrackList();
   TrackListIterator iter2(l);
   t = iter2.First();
   while (t) {
      tracksCopy->Add(t->Duplicate());
      t = iter2.Next();
   }

   // Replace
   delete stack[current]->tracks;
   stack[current]->tracks = tracksCopy;
   stack[current]->sel0 = sel0;
   stack[current]->sel1 = sel1;
}

void UndoManager::PushState(TrackList * l, double sel0, double sel1,
                            wxString longDescription,
                            wxString shortDescription,
                            bool consolidate)
{
   unsigned int i;

   // If consolidate is set to true, group up to 3 identical operations.

   if (consolidate && lastAction == longDescription &&
       consolidationCount < 2) {
      consolidationCount++;
      ModifyState(l, sel0, sel1);
      // MB: If the "saved" state was modified by ModifyState, reset
      //  it so that UnsavedChanges returns true.
      if (current == saved) {
         saved = -1;
      }
      return;
   }

   consolidationCount = 0;

   for (i = current + 1; i < stack.Count(); i++) {
      TrackListIterator iter(stack[i]->tracks);
      Track *t = iter.First();
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
   Track *t = iter.First();
   while (t) {
      tracksCopy->Add(t->Duplicate());
      t = iter.Next();
   }

   UndoStackElem *push = new UndoStackElem();
   push->tracks = tracksCopy;
   push->sel0 = sel0;
   push->sel1 = sel1;
   push->description = longDescription;
   push->shortDescription = shortDescription;

   stack.Add(push);
   current++;

   if (saved >= current)
      saved = -1;

   lastAction = longDescription;
}

TrackList *UndoManager::SetStateTo(unsigned int n, double *sel0, double *sel1)
{
   n -= 1;
   
   wxASSERT(n < stack.Count());

   current = n;

   if (current == int(stack.Count()-1)) {
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
   }
   else {
      current++;
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
      current--;
   }

   lastAction = wxT("");
   consolidationCount = 0;

   return stack[current]->tracks;
}

TrackList *UndoManager::Undo(double *sel0, double *sel1)
{
   wxASSERT(UndoAvailable());

   current--;

   *sel0 = stack[current]->sel0;
   *sel1 = stack[current]->sel1;

   lastAction = wxT("");
   consolidationCount = 0;

   return stack[current]->tracks;
}

TrackList *UndoManager::Redo(double *sel0, double *sel1)
{
   wxASSERT(RedoAvailable());

   current++;
   
   *sel0 = stack[current]->sel0;
   *sel1 = stack[current]->sel1;

   /*
   if (!RedoAvailable()) {
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
   }
   else {
      current++;
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
      current--;
   }
   */

   lastAction = wxT("");
   consolidationCount = 0;

   return stack[current]->tracks;
}

bool UndoManager::UnsavedChanges()
{
   return (saved != current);
}

void UndoManager::StateSaved()
{
   saved = current;
}

void UndoManager::Debug()
{
   for (unsigned int i = 0; i < stack.Count(); i++) {

      TrackListIterator iter(stack[i]->tracks);
      WaveTrack *t = (WaveTrack *) (iter.First());
      wxPrintf(wxT("*%d* %s %f\n"), i, (i == (unsigned int)current) ? wxT("-->") : wxT("   "),
             t ? t->GetEndTime()-t->GetStartTime() : 0);
   }
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
// arch-tag: 726e879c-98e1-4721-9488-bae71a171cc2


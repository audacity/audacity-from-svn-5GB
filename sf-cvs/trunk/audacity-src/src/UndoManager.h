/**********************************************************************

  Audacity: A Digital Audio Editor

  UndoManager.h

  Dominic Mazzoni
  
  After each operation, call UndoManager's PushState, pass it
  the entire track hierarchy.  The UndoManager makes a duplicate
  of every single track using its Duplicate method, which should
  increment reference counts.  If we were not at the top of
  the stack when this is called, delete above first.

  Undo() temporarily moves down one state and returns the track
  hierarchy.  If another PushState is called, the redo information
  is lost.

  Redo() 

  UndoAvailable()
  
  RedoAvailable()

**********************************************************************/

#ifndef __AUDACITY_UNDOMANAGER__
#define __AUDACITY_UNDOMANAGER__

#include <wx/dynarray.h>
#include <wx/string.h>

#include "HistoryWindow.h"

class VTrack;
class TrackList;

struct UndoStackElem {
   TrackList *tracks;
   wxString description;
   double sel0;
   double sel1;
};

WX_DEFINE_ARRAY(UndoStackElem *, UndoStack);

class UndoManager {
 public:
   UndoManager();
   ~UndoManager();

   void PushState(TrackList * l, double sel0, double sel1, wxString desc);
   void ClearStates();
   void RemoveStates(int num);  // removes the 'num' oldest states
   void RemoveStateAt(int n);   // removes the n'th state (1 is oldest) 
   unsigned int GetNumStates();
   unsigned int GetCurrentState();
   void GetDescription(unsigned int n, wxString *desc, wxString *size);
   void SetDescription(unsigned int n, wxString desc);

   TrackList *SetStateTo(unsigned int n, double *sel0, double *sel1);
   TrackList *Undo(double *sel0, double *sel1);
   TrackList *Redo(double *sel0, double *sel1);

   bool UndoAvailable();
   bool RedoAvailable();

   bool UnsavedChanges();
   void StateSaved();

   void Debug();

 private:
   int current;
   int saved;
   UndoStack stack;
   
};

#endif

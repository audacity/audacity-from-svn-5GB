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

class VTrack;
class TrackList;

struct UndoStackElem {
   TrackList *tracks;
   double sel0;
   double sel1;
};

WX_DEFINE_ARRAY(UndoStackElem *, UndoStack);

class UndoManager {
 public:
   UndoManager();
   ~UndoManager();

   void PushState(TrackList * l, double sel0, double sel1);
   void ClearStates();
   void ClearStates(int num);
   unsigned int  GetNumUndoableStates();
   TrackList *Undo(double *sel0, double *sel1);
   TrackList *Redo(double *sel0, double *sel1);

   bool UndoAvailable();
   bool RedoAvailable();

   void Debug();

 private:
   int current;
   UndoStack stack;
};

#endif

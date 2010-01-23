/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  DiskIO.h

  Copyright (c) 2004 Joshua Haberman, Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_DISK_IO
#define __MEZZO_DISK_IO__

#include <vector>

#include "SeqDataFileBlock.h"

namespace Mezzo {

class DiskIO;

// There is only one global DiskIO object
extern DiskIO *gDiskIO;

class DiskIOCallable {
 public:
   virtual void HandleDiskIOTasks() = 0;
};

struct CallableProcess {
   int priority;
   DiskIOCallable *callable;
};

class DiskIO {
 public:
   DiskIO();
   ~DiskIO();

   // The client program should call this method on gDiskIO
   // from a separate thread.  (Starting a new thread is a
   // platform-dependent operation.)
   void Run();

   void RegisterCallable(DiskIOCallable *callable, int priority);
   void UnregisterCallable(DiskIOCallable *callable);

   //
   // Thread-safe methods that can be called by the GUI thread when it
   // creates new block files or modifies priority information about 
   // existing block files.
   //

   // This must be called when a new SeqDataFileBlock is created,
   // so that DiskIO knows to manage it and eventually garbage-collect it.
   void EnqueueNewBlock(SeqDataFileBlock *newBlock);

 private:
   static int msDiskIOCount; // Make sure we're a singleton

   std::vector<CallableProcess> mProcesses;
};

} // Namespace Mezzo

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3



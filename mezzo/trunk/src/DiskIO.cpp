/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  DiskIO.cpp

  Copyright (c) 2004 Joshua Haberman, Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "DiskIO.h"

namespace Mezzo {

// There is only one global DiskIO object
DiskIO *gDiskIO = new DiskIO();
int DiskIO::msDiskIOCount = 0;

DiskIO::DiskIO()
{
   if (msDiskIOCount != 0) {
      fprintf(stderr, "Attempt to create more than one DiskIO object.\n");
      exit(-1);
   }
}

DiskIO::~DiskIO()
{
   if (msDiskIOCount != 1) {
      fprintf(stderr, "Attempt to delete more than one DiskIO object.\n");
      exit(-1);
   }
}

// The client program should call this method on gDiskIO
// from a separate thread.  (Starting a new thread is a
// platform-dependent operation.)
void DiskIO::Run()
{
   struct timespec sleeptime;
   sleeptime.tv_sec = 0;
   sleeptime.tv_nsec = 100000000; /* 100 million nanosecs = 0.1 secs */

   for(;;) {
      unsigned int i;

      // TODO: Handle tasks in priority order

      for(i=0; i<mProcesses.size(); i++) {
         mProcesses[i].callable->HandleDiskIOTasks();
      }

      nanosleep(&sleeptime, NULL);
   }
}

void DiskIO::RegisterCallable(DiskIOCallable *callable, int priority)
{
   CallableProcess process;
   process.priority = priority;
   process.callable = callable;
   mProcesses.push_back(process);
}

void DiskIO::UnregisterCallable(DiskIOCallable *callable)
{
   unsigned int i;

   for(i=0; i<mProcesses.size(); i++)
      if (mProcesses[i].callable == callable) {
         fprintf(stderr, "TODO: unregister callback\n");
         //mProcesses.erase(i);
         return;
      }

   fprintf(stderr, "Couldn't find callable to unregister: %08X\n",
           (int)callable);
}

//
// Thread-safe methods that can be called by the GUI thread when it
// creates new block files or modifies priority information about 
// existing block files.
//

// This must be called when a new SeqDataFileBlock is created,
// so that DiskIO knows to manage it and eventually garbage-collect it.
void DiskIO::EnqueueNewBlock(SeqDataFileBlock *newBlock)
{
   // TODO
}

} // namespace Mezzo

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3



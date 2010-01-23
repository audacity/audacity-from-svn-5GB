/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  ManagedFile.h

  Copyright (c) 2004 Dominic Mazzoni, Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_MANAGEDFILE__
#define __MEZZO_MANAGEDFILE__

#include <string>
#include <vector>

#include "Storable.h"
#include "platform/DiskFunctions.h"

namespace Mezzo {

class CachedDiskData {
 public:
   CachedDiskData():GUIReadRequestCount(0),dataIsLoaded(false),deletePending(false) { }

   virtual void Load() = 0;
   virtual void Unload() = 0;
   virtual ~CachedDiskData() { }

   // These fields can only be modified by the thread that owns the
   // SeqDataFileBlock, and not by the thread that loads/saves data
   // to disk.  When GUIReadRequestCount is nonzero, the disk thread will
   // try to load this block as soon as possible, but only after audio
   // read/write requests are satisfied.  When GUIReadRequestCount is
   // zero, the disk thread may unload the data in order to save memory.
   int GUIReadRequestCount;

   // Use these methods to modify it so that the disk thread can get
   // notified of the change in status immediately
   void IncrementReadRequestCount() { GUIReadRequestCount++; }
   void DecrementReadRequestCount() { GUIReadRequestCount--; }

   // These fields are initialized once by the thread that owns the
   // SeqDataFileBlock on creation, but from then on must only be
   // modified by the thread that loads/saves data to disk.
   // Achtung!  The thread that owns SeqDataFileBlock must check the
   // the "deletePending" flag before checking either "dataIsLoaded" or
   // "data" - if it is true, it must treat this as if dataIsLoaded is
   // false (and try again later).  Read from "data" at your own peril,
   // because it could be deleted out from under you!
   bool   dataIsLoaded;
   bool   deletePending;

   bool IsLoaded()
   {
      return dataIsLoaded && !deletePending;
   }

   // Used only by the disk thread.
   // priority := max(AudioReadRequestID, GUIReadRequestCount)
   int priorityQueueIndex;
};


class ManagedFile : public virtual Storable
{
public:
   ManagedFile(std::string fileName, std::string fullPathName);
   virtual ~ManagedFile();

private:
   // Make a copy that is identical besides using a different disk file
   virtual ManagedFile *Copy(std::string fileName) = 0;

   bool mLocked;  // is this file locked?  (see ManagedFileContext.h for explanation)
   int mRefCount;

protected:
   virtual std::vector<CachedDiskData*> GetCacheObjects() = 0;

   ManagedFile() { }

   std::string        mFileName;   ///< The file name relative to the managed file directory
   Platform::FileName mFullPathName;  ///< The full file name
   std::vector<CachedDiskData*> mCachedData;

   friend class ManagedFileContext;
   friend class TestManagedFileContext;
};

}  // namespace

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3



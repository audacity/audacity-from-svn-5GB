/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  ManagedFileContext.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_MANAGED_FILE_CONTEXT__
#define __MEZZO_MANAGED_FILE_CONTEXT__

#include <string>
#include <set>
#include <vector>

#include "Buffer.h"
#include "SeqBlockContext.h"

namespace Mezzo {

class ManagedFile;
class ManagedFileContextData;

/// A class that creates and tracks ManagedFiles in a particular directory

/// A class that creates and tracks ManagedFiles in a particular directory.
/// The files in this directory are reference-counted by ManagedFileContext
/// and moved, deleted, or copied as needed.
///
/// Classes that are using ManagedFiles need to be careful to release all
/// references to them when they are finished.  When a ManagedFileContext
/// is destroyed by going out of scope or being deleted it will throw
/// an exception if any references to files in that context remain.
class ManagedFileContext : public SeqBlockContext
{
public:
   ManagedFileContext(std::string dir);
   ManagedFileContext(Loader& loader);

   /// Free the context, throwing an exception if references to its files are live
   ~ManagedFileContext();

   /// Get a reference suitable for use in this context
   ManagedFile* GetRef(ManagedFile *file);

   void ReleaseRef(ManagedFile *file);

   /// Move or copy all files to a new directory
   void MoveToNewDirectory(std::string dir);

   /// Lock the given set of files in this context.
   void LockFiles(std::set<ManagedFile*> filesToCommit);

   /// Allow this context to delete itself when all its files are abandoned
   void Release();

   virtual void Store(Storer& storer);
   virtual void Store(Storer& storer, std::string relativePath, std::set<ManagedFile*> filesToStore);

   //
   // These methods come from SeqBlockContext
   //

   // not thread-safe!
   SeqBlock *NewSeqBlock(Buffer buffer);

   /// Get a reference that is suitable for use in this context, whether the
   /// given SeqBlock is from this context or not.  In practice this will add
   /// one to the refcount and return the same block if the given block is from
   /// this context, otherwise it will make a copy in this context.
   /// This method must be called from the disk thread, because it does a lot
   /// of serious disk I/O.
   SeqBlock *GetSeqBlockRef(SeqBlock *block);

   SeqBlock *GetSeqBlockRef(Loader& loader, std::string id);

   /// Free use of a block previously acquired using either NewSeqBlock or GetRef.
   /// In practice this will decrement the reference count by one, and delete
   /// the block if the reference count hits zero.
   void ReleaseSeqBlockRef(SeqBlock *block);

   /// Get the number of SeqBlocks currently allocated in this context.  If the
   /// owner of this context thinks that all blocks have been freed but finds
   /// that this method returns nonzero, the client knows it leaked some blocks.
   int GetNumSeqBlocks();

   void Ref();
   void Deref();

   static void LoadBackgroundRequests();

 private:
   std::string GenerateFileName(std::string extension);
   void AddFileToContext(ManagedFile *file);

   std::set<ManagedFile*> mFiles;
   std::string mDir;
   int mFileIndex;
   int mRefCount;
   bool mReleased;

   static std::vector<ManagedFileContext*> mContexts;
};

}  // namespace

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

#endif


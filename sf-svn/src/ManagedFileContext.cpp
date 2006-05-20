/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  ManagedFileContext.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "ManagedFileContext.h"

#include <algorithm>

#include "platform/DiskFunctions.h"
#include "ManagedFile.h"
#include "SeqBlock.h"
#include "SeqDataFileBlock.h"
#include "Util.h"
#include "Exceptions.h"

// #define DEBUG_REFCOUNTING

namespace Mezzo {

std::vector<ManagedFileContext*> ManagedFileContext::mContexts;

ManagedFileContext::ManagedFileContext(std::string dir):
   mDir(dir),
   mFileIndex(0),
   mRefCount(0),
   mReleased(false)
{
   Platform::CreateDirectory(dir);
   mContexts.push_back(this);
}

ManagedFileContext::~ManagedFileContext()
{
   // Shoot, we're not allowed to throw exceptions from a destructor.  I guess
   // the best we can do here is complain loudly
   if(mFiles.size() != 0)
   {
      printf("FATAL ERROR in ManagedFileContext::~ManagedFileContext():\n"
                      "\tAttempted to delete a ManagedFileContext "
                      "that has live ManagedFiles\n");
#ifdef DEBUG_REFCOUNTING
      std::set<ManagedFile*>::const_iterator i;
      for(i = mFiles.begin(); i != mFiles.end(); i++)
         printf("\t%s\n", (*i)->mFullPathName.GetFullPath().c_str());
#endif
   }

   if(mRefCount != 0)
   {
      printf("FATAL ERROR in ManagedFileContext::~ManagedFileContext():\n"
                      "\tAttempted to delete a ManagedFileContext "
                      "that is still being referenced\n");
#ifdef DEBUG_REFCOUNTING
      printf("RefCount is: %d\n", mRefCount);
#endif
   }
}

void ManagedFileContext::Ref()
{
   mRefCount++;
#ifdef DEBUG_REFCOUNTING
   printf("RefCount of context incremented (%d=>%d)\n", mRefCount-1, mRefCount);
#endif
}

void ManagedFileContext::Deref()
{
#ifdef DEBUG_REFCOUNTING
   printf("RefCount of context decremented (%d=>%d)\n", mRefCount, mRefCount-1);
#endif
   mRefCount--;
   if(mRefCount == 0 && mReleased == true)
      delete this;
}

/// If no clients are currently referencing this context, the context will
/// be deleted.  Otherwise, it will be deleted when no clients reference it.
void ManagedFileContext::Release()
{
   if(mRefCount == 0)
      delete this;
   else
      mReleased = true;
}

/// A client calls this method when it wants to use a ManagedFile it has
/// obtained from somewhere, perhaps the same context but perhaps not.
/// If it is from this context, it will increase the refcount by one
/// and return the same file.  Otherwise it will copy the file into this
/// context and return the copy.
ManagedFile*
ManagedFileContext::GetRef(ManagedFile *file)
{
   if(mFiles.count(file) == 1)
   {
      // This file already exists in this context
      file->mRefCount++;
#ifdef DEBUG_REFCOUNTING
      printf("RefCount of file %s incremented in GetRef() (%d=>%d)\n",
             file->mFullPathName.GetFullPath().c_str(), file->mRefCount-1, file->mRefCount);
#endif
      return file;
   }
   else
   {
      // this file must be copied into this context
      std::string extension;
      //extension = GetExtension(file->mFileName);
      ManagedFile *newFile = file->Copy(GenerateFileName(extension));
      // TODO: will ManagedFile::Copy() actually copy the file, or do we need to
      // do that here?
      mFiles.insert(newFile);
      return newFile;
   }
}

/// A client calls this method when it is no longer interested in a
/// reference to a ManagedFile.  This is all a client should do;
/// it should not delete the memory object nor the disk file.
void ManagedFileContext::ReleaseRef(ManagedFile *file)
{
   file->mRefCount--;
#ifdef DEBUG_REFCOUNTING
   printf("RefCount of file %s DECREMENTED in ReleaseRef() (%d=>%d)\n",
          file->mFullPathName.GetFullPath().c_str(), file->mRefCount+1, file->mRefCount);
#endif

   if(file->mRefCount == 0)
   {
      mFiles.erase(file);
      if(file->mLocked == false)
         Platform::DeleteFile(file->mFullPathName);

      delete file;
   }
}

// Moves or copies all files in the context to a new directory.  Files are
// copied if they were locked in the previous directory, otherwise they
// are moved.  In the new directory, no files are locked.
void ManagedFileContext::MoveToNewDirectory(std::string dir)
{
   Platform::CreateDirectory(dir);

   std::set<ManagedFile*>::const_iterator i;
   for(i = mFiles.begin(); i != mFiles.end(); i++)
   {
      Platform::FileName newLoc(dir + Platform::DirSeparator + (*i)->mFileName);

      if((*i)->mLocked)
         Platform::CopyFile((*i)->mFullPathName, newLoc);
      else
         Platform::MoveFile((*i)->mFullPathName, newLoc);

      (*i)->mLocked = false;
      (*i)->mFullPathName = newLoc;
   }
}

// Lock the given set of files in this context.  Any previously locked files
// are unlocked unless they are in this set.
void ManagedFileContext::LockFiles(std::set<ManagedFile*> filesToLock)
{
   // make sure that the set of files supplied to us contains only files that belong
   // to this context.
   std::set<ManagedFile*> ourFilesToLock = SetIntersection(filesToLock, mFiles);

   std::set<ManagedFile*>::const_iterator i;

   // clear the "locked" flag on all blocks
   for(i = mFiles.begin(); i != mFiles.end(); i++)
      (*i)->mLocked = false;

   // set it on the specified blocks
   for(i = ourFilesToLock.begin(); i != ourFilesToLock.end(); i++)
      (*i)->mLocked = true;
}

void ManagedFileContext::Store(Storer& storer)
{
   Store(storer, "", mFiles);
}

void ManagedFileContext::Store(Storer& storer, std::string relativePath,
                               std::set<ManagedFile*> filesToStore)
{
   LockFiles(filesToStore);

   // <ManagedFileContext id="3" dir="relativePath" fileindex="5">
   //    <!-- individual ManagedFiles save themselves -->
   // </ManagedFileContext>

   AttrDict attrs;
   attrs["id"]        = storer.CreateID(this);
   attrs["dir"]       = relativePath;
   attrs["fileindex"] = fmt("%d", mFileIndex);

   storer.StoreBeginNode("ManagedFileContext", attrs);

   std::set<ManagedFile*>::const_iterator i;
   for(i = filesToStore.begin(); i != filesToStore.end(); i++)
      (*i)->Store(storer);

   storer.StoreEndNode("ManagedFileContext");
}

ManagedFileContext::ManagedFileContext(Loader& loader):
   mRefCount(0),
   mReleased(false)
{
   mContexts.push_back(this);

   Loader::Token tok = loader.GetNextToken();

   ClientAssert(tok.name == "ManagedFileContext" && tok.type == Loader::Token::beginNode,
                "Attempted to load node " + tok.name + " with ManagedFileContext constructor");

   // TODO: paste CWD to "dir" if dir is not absolute
   mDir =       tok.attrs["dir"];
   mFileIndex = strtol(tok.attrs["fileindex"].c_str(), NULL, 0);

   // Clients of this context will need to refer to it by id
   loader.RegisterObj(tok.attrs["id"], this);

   while(1) {
      tok = loader.PeekNextToken();

      if(tok.name == "ManagedFileContext" && tok.type == Loader::Token::endNode)
      {
         // Gobble the closing token </ManagedFileContext>
         tok = loader.GetNextToken();
         break;
      }
      else if(tok.name == "SeqDataFileBlock" && tok.type == Loader::Token::beginNode)
      {
         // <SeqDataFileBlock/>
         SeqDataFileBlock *newBlock = new SeqDataFileBlock(loader, mDir);

         // Loaded blocks should start out locked so that they don't disappear unless
         // unlocked.
         newBlock->mLocked = true;

         // Loaded blocks should start out with a reference count of 0, because the
         // reference count will be incremented once Sequences start referencing it
         newBlock->mRefCount = 0;

         mFiles.insert(newBlock);
      }
      else
      {
         ClientAssert(false, "ManagedFileContext read unrecognized node " + tok.name);
      }
   }
}


//
// Private functions
//

std::string
ManagedFileContext::GenerateFileName(std::string extension)
{
   // we construct a full path to make sure that we are not generating
   // a filename that already exists.  However, we return a filename relative
   // to the given directory

   // for now, just code a single-directory solution like we've used for a while.
   // later we can revise to be a deep directory structure
   char fileName[1000];
   do {
      snprintf(fileName, 1000, "b%05d.%s", mFileIndex++, extension.c_str());
   } while( Platform::FileExists(fileName) );

   return fileName;
}

//
// SeqBlockContext methods
//

SeqBlock*
ManagedFileContext::NewSeqBlock(Buffer buf)
{
   std::string fileName = GenerateFileName(SeqDataFileBlock::GetExtension());
   std::string fullPathName = mDir + Platform::DirSeparator + fileName;
   SeqDataFileBlock *newBlock = new SeqDataFileBlock(fileName, fullPathName, buf);

   AddFileToContext(newBlock);

   return newBlock;
}

SeqBlock *ManagedFileContext::GetSeqBlockRef(SeqBlock *block)
{
   ManagedFile *thisBlock = dynamic_cast<ManagedFile*>(block);

   if(thisBlock == NULL)
   {
      // we are copying from a SeqBlock that isn't a file.

      // For a SeqSilentBlock always make a copy.  There is no disk file and they
      // are incredibly small in memory
      SeqSilentBlock *silentBlock = dynamic_cast<SeqSilentBlock*>(block);
      if(silentBlock)
         return new SeqSilentBlock(*silentBlock);
      else
      {
         Buffer buffer = block->Get();
         std::string fileName = GenerateFileName(SeqDataFileBlock::GetExtension());
         std::string fullPathName = mDir + Platform::DirSeparator + fileName;
         SeqDataFileBlock *newRef = new SeqDataFileBlock(fileName, fullPathName, buffer);
         AddFileToContext(newRef);
         return newRef;
      }
   }
   else
   {
      ManagedFile *newRef = ManagedFileContext::GetRef(thisBlock);
      SeqBlock *newSeqBlockRef = dynamic_cast<SeqBlock*>(newRef);

      // this isn't going to be false unless the derived SeqDataFileBlock::Copy() screws
      // up and constructs the wrong object.
      // assert(newSeqBlockRef)
      return newSeqBlockRef;
   }
}

SeqBlock *ManagedFileContext::GetSeqBlockRef(Loader& loader, std::string id)
{
   Storable *obj = loader.GetObj(id);

   ManagedFile *file = dynamic_cast<ManagedFile*>(obj);
   InternalAssert(file && mFiles.count(file) == 1,
                  "Attempted to load an object by id that isn't part of this context");

   file->mRefCount++;
#ifdef DEBUG_REFCOUNTING
   printf("RefCount of file %s incremented in GetSeqBlockRef()\n", file->mFullPathName.GetFullPath().c_str());
#endif

   SeqBlock* block = dynamic_cast<SeqBlock*>(file);
   InternalAssert(block, "Attempted to load object as a SeqBlock that isn't a SeqBlock");

   return block;
}

void ManagedFileContext::ReleaseSeqBlockRef(SeqBlock *block)
{
   ManagedFile *thisBlock = dynamic_cast<ManagedFile*>(block);

   ClientAssert(thisBlock,
               "Attempted to release a reference to a block that doesn't "
               "belong to this context");

   ManagedFileContext::ReleaseRef(thisBlock);
}

int ManagedFileContext::GetNumSeqBlocks()
{
   return mFiles.size();
}

void ManagedFileContext::AddFileToContext(ManagedFile *file)
{
   mFiles.insert(file);
}

// static
void ManagedFileContext::LoadBackgroundRequests()
{
   for(unsigned int c = 0; c < mContexts.size(); c++)
   {
      std::set<ManagedFile*>::const_iterator file;
      for(file = mContexts[c]->mFiles.begin(); file != mContexts[c]->mFiles.end(); file++)
      {
         std::vector<CachedDiskData*> caches = (*file)->GetCacheObjects();
         for(unsigned int i = 0; i < caches.size(); i++)
         {
            if(caches[i]->GUIReadRequestCount > 0 && !caches[i]->dataIsLoaded)
            {
               caches[i]->Load();
               caches[i]->dataIsLoaded = true;
               printf("loaded block\n");
            }

            if(caches[i]->GUIReadRequestCount == 0 && caches[i]->dataIsLoaded)
            {
               caches[i]->deletePending = true;;

               if(caches[i]->GUIReadRequestCount != 0)
                  continue;

               caches[i]->dataIsLoaded = false;
               caches[i]->Unload();

               caches[i]->deletePending = false;
               printf("UNloaded block\n");
            }
         }
      }
   }
}

}

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


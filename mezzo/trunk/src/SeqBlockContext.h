/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  SeqBlockContext.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_SEQ_BLOCK_CONTEXT__
#define __MEZZO_SEQ_BLOCK_CONTEXT__

#include "Storable.h"
#include "Buffer.h"

namespace Mezzo {

class SeqBlock;

class SeqBlockContext : public virtual Storable
{
   friend class BlockedSequence;

 public:
   virtual ~SeqBlockContext() { }

 private:
   virtual SeqBlock *NewSeqBlock(Buffer buffer) = 0;

   /// Get a reference that is suitable for use in this context, whether the
   /// given SeqBlock is from this context or not.  In practice this will add
   /// one to the refcount and return the same block if the given block is from
   /// this context, otherwise it will make a copy in this context.
   virtual SeqBlock *GetSeqBlockRef(SeqBlock *block) = 0;

   virtual SeqBlock *GetSeqBlockRef(Loader& loader, std::string id) = 0;

   /// Free use of a block previously acquired using either NewSeqBlock or GetRef.
   /// In practice this will decrement the reference count by one, and delete
   /// the block if the reference count hits zero.
   virtual void ReleaseSeqBlockRef(SeqBlock *block) = 0;

   virtual void Ref() = 0;
   virtual void Deref() = 0;

   /// Get the number of SeqBlocks currently allocated in this context.  If the
   /// owner of this context thinks that all blocks have been freed but finds
   /// that this method returns nonzero, the client knows it leaked some blocks.
   virtual int GetNumSeqBlocks() = 0;
};

} // namespace

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


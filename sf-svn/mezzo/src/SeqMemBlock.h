/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  SeqMemBlock.h

  Copyright (c) 2004 Dominic Mazzoni, Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_SEQ_MEM_BLOCK__
#define __MEZZO_SEQ_MEM_BLOCK__

#include <set>

#include "SeqBlock.h"
#include "SeqBlockContext.h"

namespace Mezzo {

class SeqMemBlock : public SeqBlock {
 public:
   /// Construct a new SeqMemBlock given some data
   SeqMemBlock(Buffer buffer);

   ~SeqMemBlock() {}

   /// Retrieves audio data from this SeqMemBlock
   Buffer Get(int start = 0);
   /// Retrieves audio data from this SeqMemBlock
   Buffer Get(int start, int len);

   bool GetIfCached(Buffer &buf, int start = 0) { Get(start); return true; }
   bool GetIfCached(Buffer &buf, int start, int len) { Get(start, len); return true; }

   virtual SeqBlockSummary *GetSummary() { return &mSummary; }
   virtual bool GetSummaryIfCached(SeqBlockSummary **summary) { *summary = &mSummary; return true; }

   float GetMin();
   float GetMax();
   float GetSumSq();

   int GetRAMBytesUsed();
   int GetDiskBytesUsed() { return 0; }

   void Store(Storer& storer);

 protected:
   Buffer          mBuffer;
   SeqBlockSummary mSummary;
   int             mRefCount;

   friend class SeqMemBlockContext;
};

class SeqMemBlockContext : public SeqBlockContext {
public:
   SeqMemBlockContext();

   SeqBlock *NewSeqBlock(Buffer buffer);
   SeqBlock *GetSeqBlockRef(SeqBlock *block);
   void ReleaseSeqBlockRef(SeqBlock *block);
   void Ref();
   void Deref();
   int GetNumSeqBlocks();
   ~SeqMemBlockContext();

private:
   std::set<SeqMemBlock*> mSeqBlocks;
   int mRefCount;
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


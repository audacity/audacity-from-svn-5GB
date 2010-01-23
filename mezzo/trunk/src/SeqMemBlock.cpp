/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  SeqMemBlock.cpp

  Copyright (c) 2004 Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "SeqMemBlock.h"
#include "Util.h"

namespace Mezzo {

SeqMemBlock::SeqMemBlock(Buffer buffer):
   mBuffer(buffer),
   mSummary(buffer),
   mRefCount(1)
{
   mLen = buffer.GetLength();
}

Buffer SeqMemBlock::Get(int start)
{
   return mBuffer.Get(start);
}

Buffer SeqMemBlock::Get(int start, int len)
{
   return mBuffer.Get(start, len);
}

float
SeqMemBlock::GetMin()
{
   return mSummary.mMin;
}

float
SeqMemBlock::GetMax()
{
   return mSummary.mMax;
}

float SeqMemBlock::GetSumSq()
{
   return mSummary.mSumSq;
}

int SeqMemBlock::GetRAMBytesUsed()
{
   return
      //mSummary.GetByteCount() +
      mBuffer.GetBytesPerSample() * mBuffer.GetLength() +
      sizeof(*this);
}

void SeqMemBlock::Store(Storer& storer)
{
   // <SeqMemBlock id="32">
   //    <Buffer>
   //       <Sample value="3.2634413"/>
   //       <Sample value="2.2343813"/>
   //    </Buffer>
   // </SeqMemBlock>

   int i;
   AttrDict attrs;
   attrs["id"] = storer.CreateID(this);
   storer.StoreBeginNode("SeqMemBlock", attrs);

   // <Buffer>

   FloatBuffer saveBuffer = mBuffer.AsFloat();
   storer.StoreBeginNode("Buffer", attrs);
   for(i = 0; i < saveBuffer.GetLength(); i++)
   {
      attrs.clear();
      attrs["value"] = fmt("%f", saveBuffer[i]);
      storer.StoreLeafNode("Sample", attrs);
   }
   storer.StoreEndNode("Buffer");

   storer.StoreEndNode("SeqMemBlock");
}

// -------------------------------------------------------------------------------------
//
// SeqMemBlockContext
//
// -------------------------------------------------------------------------------------

SeqMemBlockContext::SeqMemBlockContext():
   mRefCount(0)
{
}

void SeqMemBlockContext::Ref()
{
   mRefCount++;
}

void SeqMemBlockContext::Deref()
{
   mRefCount--;
}

SeqBlock*
SeqMemBlockContext::NewSeqBlock(Buffer buffer)
{
   // it is constructed with refcount of 1
   SeqMemBlock *newBlock = new SeqMemBlock(buffer);
   mSeqBlocks.insert(newBlock);
   return newBlock;
}

SeqBlock*
SeqMemBlockContext::GetSeqBlockRef(SeqBlock *block)
{
   SeqMemBlock *seqMemBlock = dynamic_cast<SeqMemBlock*>(block);

   // if it's not a SeqMemBlock it can't be from this context, and
   // if it's not in our list, it's not from this context
   if((seqMemBlock == NULL) || mSeqBlocks.count(seqMemBlock) == 0)
   {
      // constructed with refcount of 1
      SeqMemBlock *newBlock = new SeqMemBlock(block->Get());
      mSeqBlocks.insert(newBlock);
      return newBlock;
   }
   else
   {
      seqMemBlock->mRefCount++;
      return seqMemBlock;
   }
}

void SeqMemBlockContext::ReleaseSeqBlockRef(SeqBlock *block)
{
   SeqMemBlock *seqMemBlock = dynamic_cast<SeqMemBlock*>(block);

   InternalAssert(seqMemBlock && mSeqBlocks.count(seqMemBlock) == 1,
                  "Attempted to release a SeqBlock that isn't from this context");

   if(seqMemBlock->mRefCount-- == 0)
   {
      mSeqBlocks.erase(seqMemBlock);
      delete seqMemBlock;
   }
}

int SeqMemBlockContext::GetNumSeqBlocks()
{
   return mSeqBlocks.size();
}

SeqMemBlockContext::~SeqMemBlockContext()
{
   if(mSeqBlocks.size() != 0)
   {
      // we can't throw an exception (it's bad C++, we could be already
      // handling an exception) so this is the best we can do)
      printf("FATAL ERROR in SeqMemBlockContext::~SeqMemBlockContext():\n"
                      "\tAttempted to delete a SeqMemBlockContext "
                      "that has live ManagedFiles\n");
   }

   std::set<SeqMemBlock*>::const_iterator i;
   for(i = mSeqBlocks.begin(); i != mSeqBlocks.end(); i++)
      delete *i;
}

}  // namespace

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Sequence.cpp

  Copyright (c) 2004 Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "Sequence.h"
#include "Exceptions.h"
#include "SeqBlockContext.h"
#include "Util.h"
#include "ManagedFile.h"

#include <float.h>
#include <math.h>

namespace Mezzo {

//
// Sequence
//

Sequence *Sequence::Cut(long_sample_count start, long_sample_count len)
{
   Sequence *result = Copy(start, len);
   Delete(start, len);
   return result;
}

//
// MemorySequence
//

MemorySequence::MemorySequence(Buffer::sampleFormat format) :
   Sequence(format),
   mBuffer(FloatBuffer(0))
{
}

MemorySequence::~MemorySequence()
{
}

Sequence *MemorySequence::Duplicate() const
{
   MemorySequence *newSeq = new MemorySequence();
   newSeq->mBuffer = mBuffer;
   return newSeq;
}

//
// Editing
//

long_sample_count MemorySequence::GetLength() const
{
   return mBuffer.GetLength();
}

Buffer MemorySequence::Get(long_sample_count start, int len) const
{
   ClientAssert(start >= 0 && start <= mBuffer.GetLength() &&
                len >= 0 && start+len <= mBuffer.GetLength(),
                fmt("Attempt to Get from a MemorySequence with start=%d and "
                    "len=%d, when Sequence len is %d",
                    start, len, mBuffer.GetLength()));

   return mBuffer.Get(start, len);
}

void MemorySequence::Set(long_sample_count start, Buffer newData)
{
   ClientAssert(start >= 0 && start <= mBuffer.GetLength() &&
                start+newData.GetLength() <= mBuffer.GetLength(),
                fmt("Attempt to Set in a MemorySequence with start=%d and "
                    "len=%d, when Sequence len is %d",
                    start, newData.GetLength(), mBuffer.GetLength()));

   mBuffer.Set(start, newData);
}

Sequence *MemorySequence::Copy(long_sample_count start, long_sample_count len) const
{
   ClientAssert(start >= 0 && start <= mBuffer.GetLength() &&
                len >= 0 && start+len <= mBuffer.GetLength(),
                fmt("Attempt to Copy from a MemorySequence with start=%d and "
                    "len=%d, when Sequence len is %d",
                    start, len, mBuffer.GetLength()));

   MemorySequence *newSeq = new MemorySequence();
   newSeq->Append(Get(start, len));
   return newSeq;
}

void MemorySequence::Paste(long_sample_count start, const Sequence *src)
{
   ClientAssert(start >= 0 && start <= mBuffer.GetLength(),
                fmt("Attempt to Paste a MemorySequence at start=%d, "
                    "when Sequence len is %d",
                    start, mBuffer.GetLength()));

   Buffer oldBuffer = mBuffer;
   mBuffer = Buffer::Append(oldBuffer.Get(0, start),
                            src->Get(0, src->GetLength()),
                            oldBuffer.Get(start,
                                          oldBuffer.GetLength()-start));
}

void MemorySequence::Append(Buffer data)
{
   mBuffer = Buffer::Append(mBuffer, data);
}

void MemorySequence::Delete(long_sample_count start, long_sample_count len)
{
   ClientAssert(start >= 0 && start <= mBuffer.GetLength() &&
                len >= 0 && start+len <= mBuffer.GetLength(),
                fmt("Attempt to Delete in a MemorySequence with start=%d and "
                    "len=%d, when Sequence len is %d",
                    start, len, mBuffer.GetLength()));

   int origLen = mBuffer.GetLength();
   mBuffer = Buffer::Append(mBuffer.Get(0, start),
                            mBuffer.Get(start+len,
                                        origLen-(start+len)));
}

void MemorySequence::SetSilence(long_sample_count start, long_sample_count len)
{
   ClientAssert(start >= 0 && start <= mBuffer.GetLength() &&
                len >= 0 && start+len <= mBuffer.GetLength(),
                fmt("Attempt to SetSilence in a MemorySequence with "
                    "start=%d and len=%d, when Sequence len is %d",
                    start, len, mBuffer.GetLength()));

   Buffer silence(mFormat, len);
   mBuffer.Set(start, silence);
}

void MemorySequence::InsertSilence(long_sample_count start, long_sample_count len)
{
   ClientAssert(start >= 0 && start <= mBuffer.GetLength() &&
                len >= 0,
                fmt("Attempt to Insert Silence in a MemorySequence "
                    "at start=%d, when Sequence len is %d",
                    start, mBuffer.GetLength()));

   Buffer oldBuffer = mBuffer;
   Buffer silence(mFormat, len);
   mBuffer = Buffer::Append(oldBuffer.Get(0, start),
                            silence,
                            oldBuffer.Get(start,
                                          oldBuffer.GetLength()-start));
}

void MemorySequence::Store(Storer& storer)
{
   // TODO
}

void MemorySequence::GetWaveDisplay(FloatBuffer& outMin, FloatBuffer& outMax, FloatBuffer& outRMS,
                                    Int16Buffer& outFlag,
                                    long_sample_count start, long_sample_count len, int pixelLen,
                                    double samplesPerPixel)
{
   // TODO
}

//
// BlockedSequence
//

BlockedSequence::BlockedSequence(SeqBlockContext* context, Buffer::sampleFormat format) :
   Sequence(format),
   mMinSamples(16384),
   mMaxSamples(32768),
   mNumSamples(0),
   mContext(context),
   mPrevWaveDisplayBlock0(0),
   mPrevWaveDisplayBlock1(0),
   mPrevWaveDisplayDivisor(0)
{
   mContext->Ref();
}

BlockedSequence::~BlockedSequence()
{
   unsigned int i;

   for(i=0; i<mNodes.size(); i++)
      mContext->ReleaseSeqBlockRef(mNodes[i].block);

   mContext->Deref();
}

Sequence *BlockedSequence::Duplicate(SeqBlockContext* context) const
{
   BlockedSequence *newSeq = new BlockedSequence(context);
   unsigned int i;
   // we need to ref the blocks when we add them to the new sequence becuase
   // right now they belong to this sequence
   for(i=0; i<mNodes.size(); i++)
      newSeq->AppendBlock(newSeq->mContext->GetSeqBlockRef(mNodes[i].block));
   return newSeq;
}

Sequence *BlockedSequence::Duplicate() const
{
   return Duplicate(mContext);
}

//
// Editing
//

long_sample_count BlockedSequence::GetLength() const
{
   return mNumSamples;
}

Buffer BlockedSequence::Get(long_sample_count start, int len) const
{
   ClientAssert(start >= 0 && start <= mNumSamples &&
                len >= 0 && start+len <= mNumSamples,
                fmt("Attempt to Get from a BlockedSequence with "
                    "start=%s and len=%d, when Sequence len is %s",
                    LongSampleCountToStr(start).c_str(), len,
                    LongSampleCountToStr(mNumSamples).c_str()));

   if (len==0)
      return Buffer(mFormat, 0);

   int n = FindNode(start);
   Buffer result(mFormat, len);
   int i = 0;

   while(len) {
      int bstart = start - mNodes[n].start;
      int blen = mNodes[n].end() - start;
      if (blen > len)
         blen = len;
      result.Set(i, mNodes[n].Get(bstart, blen));
      start += blen;
      len -= blen;
      i += blen;
      n++;
   }

   return result;
}

void BlockedSequence::Set(long_sample_count start, Buffer newData)
{
   ClientAssert(start >= 0 && start <= mNumSamples &&
                start+newData.GetLength() <= mNumSamples,
                fmt("Attempt to Set in a BlockedSequence with "
                    "start=%s and len=%d, when Sequence len is %s",
                    LongSampleCountToStr(start).c_str(), newData.GetLength(),
                    LongSampleCountToStr(mNumSamples).c_str()));

   long_sample_count len = newData.GetLength();
   int n = FindNode(start);
   int i = 0;

   while(len) {
      int bstart = start - mNodes[n].start;
      int blen = mNodes[n].end() - start;
      if (blen > len)
         blen = len;
      Buffer newBlockData = mNodes[n].Get();
      newBlockData.Set(bstart, newData.Get(i, blen));
      mContext->ReleaseSeqBlockRef(mNodes[n].block);
      mNodes[n].block = mContext->NewSeqBlock(newBlockData);
      start += blen;
      len -= blen;
      i += blen;
      n++;
   }
}

Sequence *BlockedSequence::Copy(long_sample_count start, long_sample_count len) const
{
   ClientAssert(start >= 0 && start <= mNumSamples &&
                len >= 0 && start+len <= mNumSamples,
                fmt("Attempt to Copy from a BlockedSequence with "
                    "start=%d and len=%d, when Sequence len is %s",
                    LongSampleCountToStr(start).c_str(), len,
                    LongSampleCountToStr(mNumSamples).c_str()));

   BlockedSequence *newSeq = new BlockedSequence(mContext);
   int numNodes = mNodes.size();
   int n0 = FindNode(start);
   int n1 = FindNode(start+len);

   // Do the first block
   if (n0 >= 0 && n0 < numNodes && start != mNodes[n0].start) {
      int blockStart = start - mNodes[n0].start;
      int blockLen = mNodes[n0].len() - blockStart;
      if (blockLen > len)
         blockLen = len;
      Buffer nodeData = mNodes[n0].Get(blockStart, blockLen);
      newSeq->AppendBlock(newSeq->mContext->NewSeqBlock(nodeData));
   }

   if (n0 >= 0 && n0 < numNodes && start == mNodes[n0].start)
      n0--;

   // If there are blocks in the middle, copy the blockfiles directly
   for (int n = n0 + 1; n < n1; n++)
      newSeq->AppendBlock(newSeq->mContext->GetSeqBlockRef(mNodes[n].block));

   // Do the last block
   if (n1 > n0 && n1 < numNodes) {
      long_sample_count blen = start + len - mNodes[n1].start;
      Buffer nodeData = mNodes[n1].Get(0, blen);
      newSeq->AppendBlock(newSeq->mContext->NewSeqBlock(nodeData));
   }

   InternalAssert(newSeq->mNumSamples == len,
                  fmt("Copy failed: expected to get %s samples, "
                      "but produced a Sequence with %s samples.",
                      LongSampleCountToStr(len).c_str(),
                      LongSampleCountToStr(newSeq->mNumSamples).c_str()));

   return newSeq;
}

void BlockedSequence::Paste(long_sample_count start, const Sequence *src)
{
   ClientAssert(start >= 0 && start <= mNumSamples,
                fmt("Attempt to Paste into a BlockedSequence with "
                    "start=%s, when Sequence len is %s",
                    LongSampleCountToStr(start).c_str(),
                    LongSampleCountToStr(mNumSamples).c_str()));

   ClientAssert(mNumSamples + (double)src->GetLength() < LONG_SAMPLE_COUNT_MAX,
                fmt("Pasting %s samples in BlockedSequence of length %s "
                    "would overflow.",
                    LongSampleCountToStr(src->GetLength()).c_str(),
                    LongSampleCountToStr(mNumSamples).c_str()));

   if (src->GetLength() == 0)
      return;

   const BlockedSequence *bsrc = dynamic_cast<const BlockedSequence *>(src);
   if (!bsrc) {
      // If it's some other type of Sequence, we convert it to
      // a BlockedSequence, then call Paste again...
      BlockedSequence tempSequence(mContext);
      long_sample_count len = src->GetLength();
      int i = 0;
      while(i < len) {
         int chunk = mMaxSamples < len-i? mMaxSamples: len-i;
         tempSequence.AppendBlock(mContext->NewSeqBlock(src->Get(i, chunk)));
         i += chunk;
      }
      Paste(start, &tempSequence);
      return;
   }

   long_sample_count srcLen = bsrc->mNumSamples;
   int numNodes = mNodes.size();
   SeqBlockNodeVec srcNodes = bsrc->mNodes;
   int srcNumNodes = srcNodes.size();
   int n;

   if (numNodes == 0 ||
       (start == mNumSamples && mNodes[numNodes-1].len() >= mMinSamples)) {
      // Special case: this seq is currently empty, or it's safe to append
      // onto the end because the current last block is longer than the
      // minimum size

      for(n=0; n<srcNumNodes; n++)
         AppendBlock(mContext->GetSeqBlockRef(srcNodes[n].block));

      return;
   }

   int n0 = FindNode(start);

   if (mNodes[n0].len() + srcLen <= mMaxSamples) {
      // Special case: we can fit all of the new samples inside of
      // one block!

      long_sample_count splitPoint = start - mNodes[n0].start;
      Buffer left = mNodes[n0].Get(0, splitPoint);
      Buffer right = mNodes[n0].Get(splitPoint);
      Buffer newBlockData = Buffer::Append(left, bsrc->Get(0, srcLen), right);
      mContext->ReleaseSeqBlockRef(mNodes[n0].block);
      mNodes[n0].block = mContext->NewSeqBlock(newBlockData);
      mNumSamples += srcLen;
      for(n=n0+1; n<numNodes; n++)
         mNodes[n].start += srcLen;
      return;
   }

   // Create a new array of blocks
   SeqBlockNodeVec oldNodes = mNodes;
   mNodes.clear();
   mNodes.reserve(numNodes + srcNumNodes + 2);
   mNumSamples = 0;

   for(n=0; n<n0; n++) {
      mNodes.push_back(oldNodes[n]);
      mNumSamples += oldNodes[n].len();
   }

   long_sample_count splitPoint = start - oldNodes[n0].start;

   if (srcNumNodes <= 4) {
      // If we are inserting four or fewer blocks,
      // it's simplest to just lump all the data together
      // into one big block along with the split block,
      // then resplit it all

      Buffer left = oldNodes[n0].Get(0, splitPoint);
      Buffer right = oldNodes[n0].Get(splitPoint);
      Buffer newBlockData = Buffer::Append(left, bsrc->Get(0, srcLen), right);
      SplitAndAppend(newBlockData);
   }
   else {
      // The final case is that we're inserting at least five blocks.
      // We divide these into three groups: the first two get merged
      // with the first half of the split block, the middle ones get
      // copied in as is, and the last two get merged with the last
      // half of the split block.

      Buffer groupOneData = Buffer::Append(oldNodes[n0].Get(0, splitPoint),
                                           srcNodes[0].Get(),
                                           srcNodes[1].Get());
      SplitAndAppend(groupOneData);

      for(n=2; n<srcNumNodes-2; n++) {
         AppendBlock(mContext->GetSeqBlockRef(srcNodes[n].block));
      }

      Buffer groupThreeData = Buffer::Append(srcNodes[srcNumNodes-2].Get(),
                                             srcNodes[srcNumNodes-1].Get(),
                                             oldNodes[n0].Get(splitPoint));
      SplitAndAppend(groupThreeData);
   }

   mContext->ReleaseSeqBlockRef(oldNodes[n0].block);

   for(n=n0+1; n<numNodes; n++) {
      oldNodes[n].start += srcLen;
      mNodes.push_back(oldNodes[n]);
      mNumSamples += oldNodes[n].len();
   }
}

void BlockedSequence::Append(Buffer data)
{
   ClientAssert(mNumSamples + (double)data.GetLength() < LONG_SAMPLE_COUNT_MAX,
                fmt("Appending %d samples to BlockedSequence of length %s "
                    "would overflow.",
                    data.GetLength(), LongSampleCountToStr(mNumSamples).c_str()));

   int dataLen = data.GetLength();

   // If the last block is not full, we need to add samples to it
   int numNodes = mNodes.size();
   if (numNodes > 0 && mNodes[numNodes-1].len() < mMinSamples) {
      SeqBlockNode *lastNode = &mNodes[numNodes-1];
      int lastLen = lastNode->len();
      int addLen = (lastLen + dataLen <= mMaxSamples)?
         dataLen: mMaxSamples - lastLen;
      Buffer newBlockData = Buffer::Append(lastNode->Get(),
                                           data.Get(0, addLen));
      mContext->ReleaseSeqBlockRef(lastNode->block);
      lastNode->block = mContext->NewSeqBlock(newBlockData);
      mNumSamples += addLen;
      data = data.Get(addLen, dataLen-addLen);
      dataLen -= addLen;
   }
 
   // Append the rest as new blocks
   while(dataLen > 0) {
      int addLen = dataLen <= mMaxSamples? dataLen: mMaxSamples;
      AppendBlock(mContext->NewSeqBlock(data.Get(0, addLen)));

      // JHtoDM: this seems like it's going to be pretty inefficient:
      // why not just increment the starting offset instead of copying
      // all that data to a new buffer?
      data = data.Get(addLen, dataLen-addLen);
      dataLen -= addLen;
   }
}

void BlockedSequence::Delete(long_sample_count start, long_sample_count len)
{
   ClientAssert(start >= 0 && start <= mNumSamples &&
                len >= 0 && start+len <= mNumSamples,
                fmt("Attempt to Delete from a BlockedSequence with "
                    "start=%s and len=%s, when Sequence len is %s",
                    LongSampleCountToStr(start).c_str(),
                    LongSampleCountToStr(len).c_str(),
                    LongSampleCountToStr(mNumSamples).c_str()));

   int numNodes = mNodes.size();
   int n0 = FindNode(start);
   int n1 = FindNode(start+len);
   int n;

   // Special case: if the samples to delete are all within a single
   // block and the resulting length is not too small, perform the
   // deletion within this block:
   if (n0 == n1 && mNodes[n0].len() - len >= mMinSamples) {
      Buffer left = mNodes[n0].Get(0, start-mNodes[n0].start);
      Buffer right = mNodes[n0].Get(start+len-mNodes[n0].start,
                                    mNodes[n0].end()-(start+len));
      mContext->ReleaseSeqBlockRef(mNodes[n0].block);
      mNodes[n0].block = mContext->NewSeqBlock(Buffer::Append(left, right));
      mNumSamples -= len;
      for(n=n0+1; n<numNodes; n++)
         mNodes[n].start -= len;
      return;
   }

   // Create a new array of blocks
   SeqBlockNodeVec oldNodes = mNodes;
   mNodes.clear();
   mNodes.reserve(numNodes - (n1 - n0) + 2);
   mNumSamples = 0;

   // Copy the blocks before the deletion point over to
   // the new array
   for (n = 0; n < n0; n++) {
      mNodes.push_back(oldNodes[n]);
      mNumSamples += oldNodes[n].len();
   }

   // First grab the samples in node n0 before the deletion point
   // into preBuffer.  If this is enough samples for its own block,
   // or if this would be the first block in the array, write it out.
   // Otherwise combine it with the previous block (splitting them
   // 50/50 if necessary).

   int preBufferLen = start - oldNodes[n0].start;
   if (preBufferLen)  // there are samples in block n0 before the delete region
   {
      if (preBufferLen >= mMinSamples || n0 == 0) {
         AppendBlock(mContext->NewSeqBlock(oldNodes[n0].Get(0, preBufferLen)));
      }
      else {
         mNumSamples -= oldNodes[n0-1].len();
         mNodes.pop_back();
         SplitAndAppend(Buffer::Append(oldNodes[n0-1].Get(),
                                       oldNodes[n0].Get(0, preBufferLen)));
         mContext->ReleaseSeqBlockRef(oldNodes[n0-1].block);
      }
   }

   if (n0 != n1)
      // if n0 == n1 then the block will be deleted in a second
      mContext->ReleaseSeqBlockRef(oldNodes[n0].block);

   // Delete blocks strictly between n0 and n1
   for (n = n0 + 1; n < n1; n++)
      mContext->ReleaseSeqBlockRef(oldNodes[n].block);

   // Now, symmetrically, grab the samples in node n1 after the
   // deletion point into postBuffer.  If this is enough samples
   // for its own block, or if this would be the last block in
   // the array, write it out.  Otherwise combine it with the
   // subsequent block (splitting them 50/50 if necessary).

   int postBufferLen = oldNodes[n1].end() - (start + len);
   if (postBufferLen != 0)  // if there are samples in block n1 after the deletion region
   {
      int blockStart = oldNodes[n1].len() - postBufferLen;
      if (postBufferLen >= mMinSamples || n1 == numNodes-1) {
         AppendBlock(mContext->NewSeqBlock(oldNodes[n1].Get(blockStart,
                                           postBufferLen)));
         mContext->ReleaseSeqBlockRef(oldNodes[n1].block);
      }
      else {
         SplitAndAppend(Buffer::Append(oldNodes[n1].Get(blockStart,
                                                        postBufferLen),
                                       oldNodes[n1+1].Get()));
         mContext->ReleaseSeqBlockRef(oldNodes[n1].block);
         mContext->ReleaseSeqBlockRef(oldNodes[n1+1].block);
         n1++;
      }
   }
   else
   {
      mContext->ReleaseSeqBlockRef(oldNodes[n1].block);
   }


   // Copy the remaining blocks over from the old array
   for(n = n1 + 1; n < numNodes; n++) {
      oldNodes[n].start -= len;
      mNodes.push_back(oldNodes[n]);
      mNumSamples += oldNodes[n].len();
   }
}

void BlockedSequence::SetSilence(long_sample_count start, long_sample_count len)
{
   ClientAssert(start >= 0 && start <= mNumSamples &&
                len >= 0 && start+len <= mNumSamples,
                fmt("Attempt to SetSilence in a BlockedSequence with "
                    "start=%s and len=%s, when Sequence len is %s",
                    LongSampleCountToStr(start).c_str(),
                    LongSampleCountToStr(len).c_str(),
                    LongSampleCountToStr(mNumSamples).c_str()));

   int n = FindNode(start);
   int i = 0;

   while(len) {
      int bstart = start - mNodes[n].start;
      int blen = mNodes[n].end() - start;
      if (blen > len)
         blen = len;
      if (bstart == 0 && blen == mNodes[n].len()) {
         mContext->ReleaseSeqBlockRef(mNodes[n].block);
         mNodes[n].block = new SeqSilentBlock(blen);
      }
      else {
         Buffer newBlockData = mNodes[n].Get();
         newBlockData.Set(bstart, FloatBuffer(blen));
         mContext->ReleaseSeqBlockRef(mNodes[n].block);
         mNodes[n].block = mContext->NewSeqBlock(newBlockData);
      }
      start += blen;
      len -= blen;
      i += blen;
      n++;
   }
}

void BlockedSequence::InsertSilence(long_sample_count start, long_sample_count len)
{
   ClientAssert(start >= 0 && start <= mNumSamples && len >= 0,
                fmt("Attempt to Insert Silence into a BlockedSequence with "
                    "start=%s, when Sequence len is %s",
                    LongSampleCountToStr(start).c_str(),
                    LongSampleCountToStr(mNumSamples).c_str()));

   // Create a new track containing as much silence as we
   // need to insert, and then call Paste to do the insertion.
   // We make use of a SeqSilentBlock, which takes up no
   // space on disk.

   BlockedSequence *silence = new BlockedSequence(mContext);
   while(len) {
      int l = (len > mMaxSamples? mMaxSamples: len);
      silence->AppendBlock(new SeqSilentBlock(l));
      len -= l;
   }

   Paste(start, silence);

   delete silence;
}

void BlockedSequence::AppendBlock(SeqBlock *block)
{
   mNodes.push_back(SeqBlockNode(block, mNumSamples));
   mNumSamples += block->GetLength();
}

void BlockedSequence::SplitAndAppend(const Buffer& data)
{
   int len = data.GetLength();
   int num = (len + (mMaxSamples - 1)) / mMaxSamples;
   int i;

   for (i = 0; i < num; i++) {
      int blockStart = i * len / num;
      int blockLen = ((i + 1) * len / num) - blockStart;
      AppendBlock(mContext->NewSeqBlock(data.Get(blockStart, blockLen)));
   }
}

int BlockedSequence::FindNode(long_sample_count pos) const
{
   InternalAssert(pos >= 0 && pos <= mNumSamples,
                  fmt("FindNode: Asked for pos=%s, mNumSamples=%s!",
                      LongSampleCountToStr(pos).c_str(),
                      LongSampleCountToStr(mNumSamples).c_str()));

   int numNodes = mNodes.size();
   if (pos == mNumSamples)
      return numNodes-1;
   return FindNode(pos, 0, numNodes / 2, numNodes);
}

int BlockedSequence::FindNode(long_sample_count pos, long_sample_count lo,
                              long_sample_count guess, long_sample_count hi) const
{
   // Finds the block containing a given sample number (pos) using
   // a binary search.

   if (pos >= mNodes[guess].start &&
       pos < mNodes[guess].start + mNodes[guess].block->GetLength())
      return guess;

   if (pos < mNodes[guess].start)
      return FindNode(pos, lo, (lo + guess) / 2, guess);
   else
      return FindNode(pos, guess + 1, (guess + 1 + hi) / 2, hi);
}

void BlockedSequence::GetWaveDisplay(FloatBuffer& outMin, FloatBuffer& outMax, FloatBuffer& outRMS,
                                     Int16Buffer& outFlag,
                                     long_sample_count start, long_sample_count len, int pixelLen,
                                     double samplesPerPixel)
{
   ClientAssert(start >= 0 && start <= mNumSamples &&
                len >= 0 && start+len <= mNumSamples,
                fmt("Attempt to GetWaveDisplay from a BlockedSequence with "
                    "start=%s and len=%s, when Sequence len is %s",
                    LongSampleCountToStr(start).c_str(),
                    LongSampleCountToStr(len).c_str(),
                    LongSampleCountToStr(mNumSamples).c_str()));

   outMin.Resize(pixelLen);
   outMax.Resize(pixelLen);
   outRMS.Resize(pixelLen);
   outFlag.Resize(pixelLen);

   long_sample_count where[pixelLen+1];
   for(int i = 0; i <= pixelLen; i++)
      where[i] = (long_sample_count)floor(start + (i*samplesPerPixel) + 0.5);

   long_sample_count s0 = start;
   long_sample_count s1 = start + len;

   if (s1 > mNumSamples)
      s1 = mNumSamples;

   unsigned int block0 = FindNode(s0);
   unsigned int block1 = FindNode(s1);

   int divisor;
   if (samplesPerPixel >= 65536)
      divisor = 65536;
   else if (samplesPerPixel >= 256)
      divisor = 256;
   else
      divisor = 1;

   unsigned int block;

   // mark everything this routine will use as used
   for(block = block0; block <= block1; block++)
   {
      switch(divisor) {
         case 1:
            mNodes[block].block->IncrementDataReadRequestCount();
            break;

         default:
            mNodes[block].block->IncrementSummaryReadRequestCount();
            break;
      }
   }

   // mark everything the last run used as unused
   // DANGER WILL ROBINSON -- intolerant to the Sequence changing between calls!
   if(mPrevWaveDisplayDivisor != 0)
   {
      for(block = mPrevWaveDisplayBlock0; block <= mPrevWaveDisplayBlock1; block++)
      {
         switch(mPrevWaveDisplayDivisor) {
            case 1:
               mNodes[block].block->DecrementDataReadRequestCount();
               break;

            default:
               mNodes[block].block->DecrementSummaryReadRequestCount();
               break;

         }
      }
   }

   mPrevWaveDisplayBlock0 = block0;
   mPrevWaveDisplayBlock1 = block1;
   mPrevWaveDisplayDivisor = divisor;

   long_sample_count srcX = s0;

   FloatBuffer temp(0);
   FloatBuffer tempMin(0);
   FloatBuffer tempMax(0);
   FloatBuffer tempSumSq(0);

   int pixel = 0;
   float theMin = 0;
   float theMax = 0;
   float sumsq = float(0.0);
   unsigned int b = block0;
   int jcount = 0;
   bool wasLoaded = false;

   while (srcX < s1) {
      // Get more samples
      long_sample_count num;

      num = ((mNodes[b].block->GetLength() -
              (srcX - mNodes[b].start)) + divisor - 1)
         / divisor;

      if (num > (s1 - srcX + divisor - 1) / divisor)
         num = (s1 - srcX + divisor - 1) / divisor;

      SeqBlockSummary *summary;
      switch (divisor) {
      default:
      case 1:
         if(mNodes[b].block->GetIfCached(temp, srcX - mNodes[b].start, num))
         {
            temp = temp.AsFloat();
            wasLoaded = true;
         }
         else
         {
            wasLoaded = false;
         }
         break;
      case 256:
         if(mNodes[b].block->GetSummaryIfCached(&summary))
         {
            wasLoaded = true;
         }
         else
         {
            wasLoaded = false;
         }
         tempMin = summary->m256Min;
         tempMax = summary->m256Max;
         tempSumSq = summary->m256SumSq;

         break;
      case 65536:
         if(mNodes[b].block->GetSummaryIfCached(&summary))
         {
            wasLoaded = true;
         }
         else
         {
            wasLoaded = false;
         }
         tempMin = summary->m64kMin;
         tempMax = summary->m64kMax;
         tempSumSq = summary->m64kSumSq;

         break;
      }

      // Get min/max/rms of samples for each pixel we can
      int x = 0;

      if (b==block0) {
         if (divisor > 1) {
            theMin = tempMin[0];
            theMax = tempMax[0];
         }
         else {
            theMin = temp[0];
            theMax = temp[0];
         }
         sumsq = float(0.0);
         jcount = 0;
      }

      while (x < num) {

         while (pixel < pixelLen &&
                where[pixel] / divisor == srcX / divisor + x) {
            if (pixel > 0) {
               outMin[pixel - 1] = theMin;
               outMax[pixel - 1] = theMax;
               if (jcount > 0)
                  outRMS[pixel - 1] = (float)sqrt(sumsq / jcount);
               else
                  outRMS[pixel - 1] = 0.0f;

               outFlag[pixel-1] = wasLoaded;
            }
            pixel++;
            if (where[pixel] != where[pixel-1]) {
               theMin = FLT_MAX;
               theMax = -FLT_MAX;
               sumsq = float(0.0);
               jcount = 0;
            }
         }

         long_sample_count stop = (where[pixel] - srcX) / divisor;

         if (stop == x)
            stop++;
         if (stop > num)
            stop = num;

         switch (divisor) {
         default:
         case 1:
            while (x < stop) {
               if (temp[x] < theMin)
                  theMin = temp[x];
               if (temp[x] > theMax)
                  theMax = temp[x];
               sumsq += (temp[x]) * (temp[x]);
               x++;
               jcount++;
            }
            break;
         case 256:
         case 65536:
            while (x < stop) {
               if (tempMin[x] < theMin)
                  theMin = tempMin[x];
               if (tempMax[x] > theMax)
                  theMax = tempMax[x];
               sumsq += tempSumSq[x];
               x++;
               jcount++;
            }

            break;
         }
      }

      b++;

      srcX += num * divisor;

      if (b >= mNodes.size())
         break;

      srcX = mNodes[b].start;

   }

   // Make sure that min[pixel - 1] doesn't segfault
   if (pixel <= 0)
      pixel = 1;

   if (pixel == 0)
      pixel++;

   while (pixel <= pixelLen) {
      outMin[pixel - 1] = theMin;
      outMax[pixel - 1] = theMax;
      if (jcount > 0)
         outRMS[pixel - 1] = (float)sqrt(sumsq / jcount);
      else
         outRMS[pixel - 1] = 0.0f;
      pixel++;
   }
}

bool BlockedSequence::NewWaveDisplayDataAvailable()
{
   return true;
}

//
// Debugging
//

void BlockedSequence::ConsistencyCheck(bool verbose)
{
   long_sample_count len = 0;
   int errs = 0;
   unsigned int i;

   for(i=0; i<mNodes.size(); i++) {
      if (mNodes[i].start != len) {
         printf("Error: node %d thinks it starts at %lld, "
                "but it starts at %lld.\n",
                i, mNodes[i].start, len);
         errs++;
      }
      if (i > 0 && i < mNodes.size()-1 &&
          (mNodes[i].len() < mMinSamples || mNodes[i].len() > mMaxSamples)) {
         printf("Error: invariant violated in node %d: len=%d "
                "(min=%d, max=%d)\n",
                i, mNodes[i].len(), mMinSamples, mMaxSamples);
         errs++;
      }
      len += mNodes[i].len();
   }

   if (len != mNumSamples) {
      printf("Error: mNumSamples is %lld, should be %lld\n",
             mNumSamples, len);
      errs++;
   }

   if (verbose || errs) {
      printf("Total errors: %d\n", errs);

      len = 0;
      for(i=0; i<mNodes.size(); i++) {
         printf("Node %5d: start %9lld len %7d%s\n",
                i, mNodes[i].start, mNodes[i].len(),
                mNodes[i].start == len? "": " ERROR");
         len += mNodes[i].len();
      }
      printf("Total:            %9lld (mNumSamples = %lld)\n",
             len, mNumSamples);
   }

   InternalAssert(errs==0,
                  "Consistency check failed in BlockedSequence.");
}

BlockedSequence::BlockedSequence(Loader& loader):
   Sequence(Buffer::FloatSample)
{
   Loader::Token tok = loader.GetNextToken();
   ClientAssert(tok.name == "BlockedSequence" && tok.type == Loader::Token::beginNode,
                "Attempted to load node " + tok.name + " with BlockedSequence constructor");

   mNumSamples = StrToLongSampleCount(tok.attrs["len"]);
   mMinSamples = strtol(tok.attrs["minsamples"].c_str(), NULL, 0);
   mMaxSamples = strtol(tok.attrs["maxsamples"].c_str(), NULL, 0);
   mFormat     = StrToSampleFormat(tok.attrs["format"]);

   mContext    = dynamic_cast<SeqBlockContext*>(loader.GetObj(tok.attrs["contextid"]));
   mContext->Ref();

   while(1) {
      // Get <SeqBlockNode>
      tok = loader.GetNextToken();

      if(tok.name == "BlockedSequence" && tok.type == Loader::Token::endNode)
      {
         break;
      }
      else if(tok.name == "SeqBlockNode" && tok.type == Loader::Token::beginNode)
      {
         long_sample_count nodeStart = StrToLongSampleCount(tok.attrs["start"]);

         // It would be possible for us to obtain the SeqBlock directly from the loader
         // using Loader::GetObj(), but we need the Context to be in on the transaction
         // so the reference-counting is handled properly

         // Get <SeqBlock>
         tok = loader.GetNextToken();
         ClientAssert(tok.name == "SeqBlock" && tok.type == Loader::Token::beginNode,
                      "SeqBlockNode only expects SeqBlock children, not " + tok.name);

         SeqBlock *block = mContext->GetSeqBlockRef(loader, tok.attrs["id"]);
         mNodes.push_back(SeqBlockNode(block, nodeStart));

         // Get </SeqBlock>
         tok = loader.GetNextToken();
         ClientAssert(tok.name == "SeqBlock" && tok.type == Loader::Token::endNode,
                      "SeqBlock doesn't expect any children, but got " + tok.name);

         // Get </SeqBlockNode>
         tok = loader.GetNextToken();
         ClientAssert(tok.name == "SeqBlockNode" && tok.type == Loader::Token::endNode,
                  "SeqBlockNode expects one SeqBlock child, but got a second child of" + tok.name);
      }
      else
      {
         ClientAssert(false, "BlockedSequence only expects SeqBlockNode children, not " + tok.name);
      }
   }


}

void BlockedSequence::Store(Storer& storer)
{
   // <BlockedSequence format="float" len="53432" minsamples="3423" maxsamples="3233433" contextid="2">
   //    <SeqBlockNode start="333">
   //       <SeqBlock id="3"/>  <!-- this is only a reference; the context has the data -->
   //    </SeqBlockNode>
   // </BlockedSequence>

   AttrDict attrs;
   attrs["len"]        = LongSampleCountToStr(mNumSamples);
   attrs["minsamples"] = fmt("%d", mMinSamples);
   attrs["maxsamples"] = fmt("%d", mMaxSamples);
   attrs["contextid"]  = storer.GetID(mContext);
   attrs["format"]     = SampleFormatToStr(mFormat);

   storer.StoreBeginNode("BlockedSequence", attrs);

   for(unsigned int i = 0; i < mNodes.size(); i++)
   {
      attrs.clear();
      attrs["start"] = LongSampleCountToStr(mNodes[i].start);

      storer.StoreBeginNode("SeqBlockNode", attrs);

      //
      // Store the SeqBlock
      //
      attrs.clear();
      try {
         attrs["id"] = storer.GetID(mNodes[i].block);
         storer.StoreLeafNode("SeqBlock", attrs);
      }
      catch(ClientException& e)
      {
         ClientAssert(false, "Attempted to save a Sequence without saving its context first");
      }

      storer.StoreEndNode("SeqBlockNode");
   }

   storer.StoreEndNode("BlockedSequence");
}

std::set<Storable*>
BlockedSequence::GetPrereqStorables()
{
   std::set<Storable*> storables;
   storables.insert(mContext);

   return storables;
}

std::set<ManagedFile*>
BlockedSequence::GetManagedFilesInUse()
{
   std::set<ManagedFile*> inUse;
   ManagedFile *file;

   for(unsigned int i = 0; i < mNodes.size(); i++)
   {
      file = dynamic_cast<ManagedFile*>(mNodes[i].block);

      if(file)
         inUse.insert(file);
   }

   return inUse;
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


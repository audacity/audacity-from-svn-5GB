/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Sequence.h

  Copyright (c) 2004 Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_SEQUENCE__
#define __MEZZO_SEQUENCE__

#include <vector>

#include "Types.h"
#include "Buffer.h"
#include "SeqBlock.h"

namespace Mezzo {

class SeqBlockContext;

class Sequence : public virtual Storable {
 public:

   virtual ~Sequence() {}
   virtual Sequence *Duplicate() const = 0;

   //
   // Editing
   //

   virtual long_sample_count GetLength() const = 0;

   virtual Buffer Get(long_sample_count start, int len) const = 0;
   virtual void Set(long_sample_count start, Buffer newData) = 0;

   virtual Sequence *Cut(long_sample_count start, long_sample_count len);
   virtual Sequence *Copy(long_sample_count start, long_sample_count len) const = 0;
   virtual void Paste(long_sample_count start, const Sequence *src) = 0;

   virtual void Append(Buffer data) = 0;

   virtual void Delete(long_sample_count start, long_sample_count len) = 0;

   virtual void SetSilence(long_sample_count start, long_sample_count len) = 0;
   virtual void InsertSilence(long_sample_count start, long_sample_count len) = 0;

   virtual void GetWaveDisplay(FloatBuffer& outMin, FloatBuffer& outMax, FloatBuffer& outRMS,
                               Int16Buffer& outFlag,
                               long_sample_count start, long_sample_count len, int pixelLen,
                               double samplesPerPixel) = 0;

 protected:
   Sequence(Buffer::sampleFormat format): mFormat(format) {}
   Buffer::sampleFormat mFormat;
};

class MemorySequence : public Sequence {
 public:

   MemorySequence(Buffer::sampleFormat format = Buffer::FloatSample);
   virtual ~MemorySequence();

   virtual Sequence *Duplicate() const;

   //
   // Editing
   //

   virtual long_sample_count GetLength() const;

   virtual Buffer Get(long_sample_count start, int len) const;
   virtual void Set(long_sample_count start, Buffer newData);

   virtual Sequence *Copy(long_sample_count start, long_sample_count len) const;
   virtual void Paste(long_sample_count start, const Sequence *src);

   virtual void Append(Buffer data);

   virtual void Delete(long_sample_count start, long_sample_count len);

   virtual void SetSilence(long_sample_count start, long_sample_count len);
   virtual void InsertSilence(long_sample_count start, long_sample_count len);

   virtual void GetWaveDisplay(FloatBuffer& outMin, FloatBuffer& outMax, FloatBuffer& outRMS,
                               Int16Buffer& outFlag,
                               long_sample_count start, long_sample_count len, int pixelLen,
                               double samplesPerPixel);

   void Store(Storer& storer);

 protected:
   Buffer mBuffer;
};

#ifndef DOXYGEN_SHOULD_SKIP_THIS

class SeqBlockNode {
 public:
   SeqBlockNode(SeqBlock *_block, long_sample_count _start) {
      block = _block; start = _start;
   }
   int len() const { return block->GetLength(); }
   long_sample_count end() const { return start + len(); }
   Buffer Get() const {
      return block->Get(0, len());
   }
   Buffer Get(long_sample_count start) const {
      return block->Get(start, len()-start);
   }
   Buffer Get(long_sample_count start, int len) const {
      return block->Get(start, len);
   }
   SeqBlock *block;
   long_sample_count start;
};

typedef std::vector<SeqBlockNode> SeqBlockNodeVec;

#endif

class BlockedSequence : public Sequence {
 public:

   BlockedSequence(SeqBlockContext *context,
                   Buffer::sampleFormat format = Buffer::FloatSample);
   BlockedSequence(Loader& loader);

   virtual ~BlockedSequence();

   // duplicate in the same context
   virtual Sequence *Duplicate() const;
   virtual Sequence *Duplicate(SeqBlockContext* context) const;

   //
   // Editing
   //

   virtual long_sample_count GetLength() const;

   virtual Buffer Get(long_sample_count start, int len) const;
   virtual void Set(long_sample_count start, Buffer newData);

   virtual Sequence *Copy(long_sample_count start, long_sample_count len) const;
   virtual void Paste(long_sample_count start, const Sequence *src);

   virtual void Append(Buffer data);

   virtual void Delete(long_sample_count start, long_sample_count len);

   virtual void SetSilence(long_sample_count start, long_sample_count len);
   virtual void InsertSilence(long_sample_count start, long_sample_count len);

   virtual void GetWaveDisplay(FloatBuffer& outMin, FloatBuffer& outMax, FloatBuffer& outRMS,
                               Int16Buffer& outFlag,
                               long_sample_count start, long_sample_count len, int pixelLen,
                               double samplesPerPixel);


   virtual bool NewWaveDisplayDataAvailable();

   //
   // Debugging
   //

   virtual void ConsistencyCheck(bool verbose);

   virtual void Store(Storer& storer);

   virtual std::set<Storable*> GetPrereqStorables();
   virtual std::set<ManagedFile*> GetManagedFilesInUse();

 protected:
   virtual void AppendBlock(SeqBlock *block);
   virtual void SplitAndAppend(const Buffer& data);

   int FindNode(long_sample_count pos) const;
   int FindNode(long_sample_count pos, long_sample_count lo, long_sample_count guess,
                long_sample_count hi) const;

   int                 mMinSamples;
   int                 mMaxSamples;
   long_sample_count   mNumSamples;
   SeqBlockNodeVec  mNodes;
   SeqBlockContext  *mContext;

   long_sample_count mPrevWaveDisplayBlock0;
   long_sample_count mPrevWaveDisplayBlock1;
   int               mPrevWaveDisplayDivisor;
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


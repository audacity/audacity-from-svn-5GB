/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  SeqBlock.h

  Copyright (c) 2004 Dominic Mazzoni, Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_SEQ_BLOCK__
#define __MEZZO_SEQ_BLOCK__

#include <vector>

#include "Buffer.h"
#include "Storable.h"

namespace Mezzo {

class SeqBlockMetadataType {
 public:
   SeqBlockMetadataType(std::string type);
   SeqBlockMetadataType(char type[5]);
   bool operator==(SeqBlockMetadataType& other);
   std::string typeStr;
};

/// Encapsulates the metadata format used by SeqBlocks that are written to disk
class SeqBlockMetadata {
 public:
   SeqBlockMetadata();
   SeqBlockMetadata(char *data, int len);

   ~SeqBlockMetadata();

   // the data will be given back to you in this endianness.  if your data
   // is endian-dependent, you must either store the endianness, or
   // (more advisably) store it in host-endian format and convert when
   // you read if IsNativeEndian() returns false
   void AddSection(SeqBlockMetadataType type, char *data, int len);

   bool HasSection(SeqBlockMetadataType type);

   // if IsNativeEndian() is false, you will probably need to byte-swap
   // any data you read
   char *GetSectionData(SeqBlockMetadataType type);
   int GetSectionLen(SeqBlockMetadataType type);

   int GetPackedLen(); // in bytes
   void GetPackedData(char *outData);

   bool IsNativeEndian() { return mNativeEndian; }

 private:
   struct section {
      section(SeqBlockMetadataType _type, int _len, char *_data):
         type(_type),len(_len),data(_data)
      { }
      SeqBlockMetadataType type;
      int len;
      char *data;
   };
   std::vector<section> mSections;
   bool mNativeEndian;
};

class SeqBlockSummary {
 public:
   /// Create summary data given uncompressed samples.
   SeqBlockSummary(Buffer samples);

   /// Create from packed summary data that had been previously
   /// saved from GetPackedData.  Also needs to know the number
   /// of samples in the original block in order to load correctly.
   SeqBlockSummary(SeqBlockMetadata *metadata);

   void Write(SeqBlockMetadata *metadata);

   static void Get256(SeqBlockMetadata *md, int start, int len,
               FloatBuffer& min, FloatBuffer& max, FloatBuffer& sumSq);
   static void Get64k(SeqBlockMetadata *md, int start, int len,
               FloatBuffer& min, FloatBuffer& max, FloatBuffer& sumSq);

   int         m256Len;
   int         m64kLen;
   FloatBuffer m256Min;
   FloatBuffer m256Max;
   FloatBuffer m256SumSq;
   FloatBuffer m64kMin;
   FloatBuffer m64kMax;
   FloatBuffer m64kSumSq;
   float       mMin;
   float       mMax;
   float       mSumSq;
};

/// Represents an immutable block of samples in a Sequence data structure.
class SeqBlock : public virtual Storable {
 public:
   virtual ~SeqBlock() {}

   int GetLength() { return mLen; }

   /// Retrieves audio data from this SeqBlock
   virtual Buffer Get(int start = 0) = 0;
   /// Retrieves audio data from this SeqBlock
   virtual Buffer Get(int start, int len) = 0;

   virtual bool GetIfCached(Buffer &buf, int start = 0) = 0;
   virtual bool GetIfCached(Buffer &buf, int start, int len) = 0;

   /// Gets extreme values for the whole block (these do not go to disk for the data)
   virtual float GetMin() = 0;
   virtual float GetMax() = 0;
   virtual float GetSumSq() = 0;

   virtual SeqBlockSummary *GetSummary() = 0;
   virtual bool GetSummaryIfCached(SeqBlockSummary **summary) = 0;

   virtual void IncrementDataReadRequestCount() { }
   virtual void DecrementDataReadRequestCount() { }
   virtual void IncrementSummaryReadRequestCount() { }
   virtual void DecrementSummaryReadRequestCount() { }


   virtual int GetRAMBytesUsed() = 0;
   virtual int GetDiskBytesUsed() = 0;

   //virtual SeqBlock* LoadSeqBlock(Loader& loader) const = 0;

 protected:
   int      mLen;
};

class SeqSilentBlock : public SeqBlock {
 public:
   SeqSilentBlock(int len) { mLen = len; }

   ~SeqSilentBlock() {}

   Buffer Get(int start = 0) {
      return FloatBuffer(mLen-start);
   }
   Buffer Get(int start, int len) {
      return FloatBuffer(len);
   }

   float GetMin() { return 0; }
   float GetMax() { return 0; }
   float GetSumSq() { return 0; }

   bool GetIfCached(Buffer &buf, int start = 0) { Get(start); return true; }
   bool GetIfCached(Buffer &buf, int start, int len) { Get(start, len); return true; }

   SeqBlockSummary *GetSummary() { return new SeqBlockSummary(FloatBuffer(mLen)); }  // TODO  memory leak!!
   bool GetSummaryIfCached(SeqBlockSummary **summary) { *summary = new SeqBlockSummary(FloatBuffer(mLen)); return true; }

   int GetRAMBytesUsed() { return sizeof(*this); }
   int GetDiskBytesUsed() { return 0; }

   void Store(Storer& storer);
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


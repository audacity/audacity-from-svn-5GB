/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  SeqDataFileBlock.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include <string>

#include "SeqBlock.h"
#include "ManagedFile.h"

namespace Mezzo {

class SeqDataFileBlock;

class SeqDataFileBlock : public SeqBlock, public ManagedFile
{
 public:
   // Construction is not complete until you give it a buffer to write
   // using one of the following two methods
   SeqDataFileBlock(std::string fullPathName, std::string fileName, Buffer data);

   SeqDataFileBlock(Loader& loader, std::string absDir);

   /// Retrieves audio data from this BlockFile
   Buffer Get(int start = 0);
   /// Retrieves audio data from this BlockFile
   Buffer Get(int start, int len);

   /// Same functions as above, but to be used for GUI drawing when
   /// it's okay to defer and get the data later
   bool GetIfCached(Buffer &buf, int start = 0);
   bool GetIfCached(Buffer &buf, int start, int len);

   float GetMin() { return mMin; }
   float GetMax() { return mMax; }
   float GetSumSq() { return mSumSq; }

   SeqBlockSummary *GetSummary();
   bool GetSummaryIfCached(SeqBlockSummary **summary);

   void IncrementDataReadRequestCount() { mCachedBufferData.IncrementReadRequestCount(); }
   void DecrementDataReadRequestCount() { mCachedBufferData.DecrementReadRequestCount(); }
   void IncrementSummaryReadRequestCount() { mCachedMetaData.IncrementReadRequestCount(); }
   void DecrementSummaryReadRequestCount() { mCachedMetaData.DecrementReadRequestCount(); }

   ManagedFile *Copy(std::string fileName);

   int GetRAMBytesUsed();
   int GetDiskBytesUsed();

   void Store(Storer& storer);

   static std::string GetExtension() { return "au"; }

 private:
   class CachedBufferData : public CachedDiskData
   {
      CachedBufferData(SeqDataFileBlock *b): block(b) { }

      SeqDataFileBlock *block;
      Buffer data;

      void Load() { data = block->Get(); }
      void Unload() { data = FloatBuffer(0); }

      friend class SeqDataFileBlock;
   }mCachedBufferData;

   class CachedMetaData : public CachedDiskData
   {
      CachedMetaData(SeqDataFileBlock *b): block(b) { }

      SeqDataFileBlock *block;
      SeqBlockSummary *summary;

      void Load() { summary = block->GetSummary(); }
      void Unload() { summary = NULL; }

      friend class SeqDataFileBlock;
   }mCachedMetaData;

   std::vector<CachedDiskData*> GetCacheObjects()
   {
      std::vector<CachedDiskData*> objects;
      objects.push_back(&mCachedBufferData);
      objects.push_back(&mCachedMetaData);
      return objects;
   }


   float  mMin;
   float  mMax;
   float  mSumSq;
};


} // namespace

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


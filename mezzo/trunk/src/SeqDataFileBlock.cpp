/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  SeqDataFileBlock.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "SeqDataFileBlock.h"

#include "Exceptions.h"
#include "platform/DiskFunctions.h"
#include "Util.h"

#include <posh/posh.h>

#include <stdio.h>
#include <errno.h>

namespace Mezzo {

#ifndef DOXYGEN_SHOULD_SKIP_THIS

// The AU formats we care about
enum {
   AU_SAMPLE_FORMAT_16 = 3,
   AU_SAMPLE_FORMAT_24 = 4,
   AU_SAMPLE_FORMAT_FLOAT = 6,
};

typedef struct {
   posh_u32_t magic;      // magic number
   posh_u32_t dataOffset; // byte offset to start of audio data
   posh_u32_t dataSize;   // data length, in bytes (optional)
   posh_u32_t encoding;   // data encoding enumeration
   posh_u32_t sampleRate; // samples per second
   posh_u32_t channels;   // number of interleaved channels
} auHeader;

#endif

SeqDataFileBlock::SeqDataFileBlock(std::string fileName, std::string fullPathName, Buffer buffer):
   ManagedFile(fileName, fullPathName),
   mCachedBufferData(this),
   mCachedMetaData(this)
{
   mLen = buffer.GetLength();

   FILE* file = fopen(mFullPathName.GetFullPath().c_str(), "wb");

   UserAssert(file != NULL, fmt("Unable to open file %s, errno = %d", mFullPathName.GetFullPath().c_str(), errno));

   //
   // Write header data
   //

   SeqBlockMetadata md;

   // Create summary
   SeqBlockSummary summary(buffer);
   summary.Write(&md);

   int mdBytes = md.GetPackedLen();
   char *mdBuffer = new char[mdBytes];
   md.GetPackedData(mdBuffer);

   auHeader header;

   // AU files can be either big or little endian.  Which it is is
   // determined implicitly by the byte-order of the magic 0x2e736e64
   // (.snd).  We want it to be native-endian, so we write the magic
   // to memory and then let it write that to a file in native
   // endianness
   header.magic = 0x2e736e64;

   // We store the summary data at the end of the header, so the data
   // offset is the length of the summary data plus the length of the header
   header.dataOffset = sizeof(auHeader) + mdBytes;

   // dataSize is optional, and we opt out
   header.dataSize = 0xffffffff;

   switch(buffer.GetSampleFormat()) {
      case Buffer::Int16Sample:
         header.encoding = AU_SAMPLE_FORMAT_16;
         break;

      case Buffer::Int24Sample:
         header.encoding = AU_SAMPLE_FORMAT_24;
         break;

      case Buffer::FloatSample:
         header.encoding = AU_SAMPLE_FORMAT_FLOAT;
         break;
   }

   // unfortunately we can never give a meaningful value here since
   // Sequence is by nature sample-rate neutral.
   header.sampleRate = 44100;

   // BlockFiles are always mono
   header.channels = 1;

   fwrite(&header, sizeof(header), 1, file);
   fwrite(mdBuffer, mdBytes, 1, file);

   delete[] mdBuffer;

   //
   // Write sample data
   //

   int dataLen = buffer.GetPackedBytesPerSample() * buffer.GetLength();
   char *packedData = new char[dataLen];

   buffer.GetPackedData(packedData);

   fwrite(packedData, dataLen, 1, file);
   fclose(file);


   delete[] packedData;
}

Buffer
SeqDataFileBlock::Get(int start)
{
   return Get(start, mLen - start);
}

Buffer
SeqDataFileBlock::Get(int start, int len)
{
   InternalAssert(start >= 0 && len >= 0 && start <= mLen &&
                  start+len <= mLen,
                  fmt("Attempt to Get from a SeqDataFileBlock with "
                      "start=%d and len=%d, when block len is %d",
                      start, len, mLen));

   // TODO -- support non-native endianness
   FILE* file = fopen(mFullPathName.GetFullPath().c_str(), "rb");

   InternalAssert(file, "Unable to open SeqDataFileBlock file " + mFullPathName.GetFullPath());

   //
   // read header
   //

   auHeader header;

   fread(&header, sizeof(header), 1, file);

   //
   // read data
   //

   fseek(file, header.dataOffset, SEEK_SET);  // seek to beginning of data

   switch(header.encoding)
   {
      case AU_SAMPLE_FORMAT_16:
      {
         fseek(file, start * header.channels * sizeof(short), SEEK_CUR);
         short *int16Samples = new short[len];
         fread(int16Samples, len * sizeof(short), 1, file);
         fclose(file);
         return Int16Buffer(int16Samples, 0, len, Buffer::DisposeWithDelete);
      }

      case AU_SAMPLE_FORMAT_24:
      {
         // TODO: make sure we unpack correctly
         fseek(file, start * header.channels * sizeof(int), SEEK_CUR);
         int *int24Samples = new int[len];
         fread(int24Samples, len * sizeof(int), 1, file);
         fclose(file);
         return Int24Buffer(int24Samples, 0, len, 1, Buffer::DisposeWithDelete);
      }

      case AU_SAMPLE_FORMAT_FLOAT:
      {
         fseek(file, start * header.channels * sizeof(float), SEEK_CUR);
         float *floatSamples = new float[len];
         fread(floatSamples, len * sizeof(float), 1, file);
         fclose(file);
         return FloatBuffer(floatSamples, 0, len, 1, Buffer::DisposeWithDelete);
      }

      default:
         UserAssert(false, fmt("SeqDataFileBlock %s has a bad data format", mFullPathName.GetFullPath().c_str()));
         return FloatBuffer(0);
   }
}

bool SeqDataFileBlock::GetIfCached(Buffer &buf, int start)
{
   return GetIfCached(buf, start, mLen - start);
}

bool SeqDataFileBlock::GetIfCached(Buffer &buf, int start, int len)
{
   if(mCachedBufferData.IsLoaded())
   {
      buf = mCachedBufferData.data;
      return true;
   }
   else
   {
      buf = FloatBuffer(len);
      return false;
   }
}

ManagedFile*
SeqDataFileBlock::Copy(std::string fileName)
{
   return new SeqDataFileBlock(*this);
}

SeqBlockSummary *SeqDataFileBlock::GetSummary()
{
   // TODO -- support non-native endianness
   FILE* file = fopen(mFullPathName.GetFullPath().c_str(), "rb");

   InternalAssert(file, "Unable to open SeqDataFileBlock file " + mFullPathName.GetFullPath());

   //
   // read header
   //

   auHeader header;

   fread(&header, sizeof(header), 1, file);

   int mdSize = header.dataOffset - sizeof(auHeader);

   char metadata[mdSize];

   fread(metadata, mdSize, 1, file);

   SeqBlockMetadata md(metadata, mdSize);

   fclose(file);

   return new SeqBlockSummary(&md);
}

bool SeqDataFileBlock::GetSummaryIfCached(SeqBlockSummary **summary)
{
   if(mCachedMetaData.IsLoaded())
   {
      *summary = mCachedMetaData.summary;
      return true;
   }
   else
   {
      // TODO: memory leak!!!
      *summary = new SeqBlockSummary(FloatBuffer(mLen));
      return false;
   }
}

int
SeqDataFileBlock::GetRAMBytesUsed()
{
   return 0;
}

int
SeqDataFileBlock::GetDiskBytesUsed()
{
   return 0;
}

void SeqDataFileBlock::Store(Storer& storer)
{
   // <SeqDataFileBlock id="37" len="3535" file="foo.au"/>
   AttrDict attrs;
   attrs["id"] = storer.CreateID(this);
   attrs["len"] =  fmt("%d", mLen);
   attrs["file"] = mFileName;

   storer.StoreLeafNode("SeqDataFileBlock", attrs);
}

SeqDataFileBlock::SeqDataFileBlock(Loader& loader, std::string absDir):
   mCachedBufferData(this),
   mCachedMetaData(this)
{
   Loader::Token tok = loader.GetNextToken();
   InternalAssert(tok.name == "SeqDataFileBlock" && tok.type == Loader::Token::beginNode,
                  "Attempted to load node " + tok.name + " with SeqDataFileBlock constructor");

   mFileName     = tok.attrs["file"];
   mFullPathName = absDir + Platform::DirSeparator + mFileName;
   mLen          = strtol(tok.attrs["len"].c_str(), NULL, 0);

   loader.RegisterObj(tok.attrs["id"], this);

   UserAssert(Platform::FileExists(mFullPathName),
              "Attempted to load sound file " + mFullPathName.GetFullPath() + ", which doesn't exist");

   tok = loader.GetNextToken();
   InternalAssert(tok.name == "SeqDataFileBlock" && tok.type == Loader::Token::endNode,
                  "Node SeqDataFileBlock doesn't expect children");
}

} // Namespace

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  SeqBlock.cpp

  Copyright (c) 2004 Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include <stdio.h>

#include "SeqBlock.h"
#include "Util.h"

#include <posh/posh.h>

namespace Mezzo {

//
// SeqBlockMetadata
//
SeqBlockMetadataType::SeqBlockMetadataType(std::string type):
   typeStr(type)
{
   UserAssert(typeStr.size() == 4, "SeqBlockMetadataTypes must be 4 characters long");

   if(type != "256i" &&
      type != "256m" &&
      type != "256M" &&
      type != "256s" &&
      type != "64ki" &&
      type != "64km" &&
      type != "64kM" &&
      type != "smry")
      UserAssert(false, fmt("Summary type %s unknown", type.c_str()));
}

SeqBlockMetadataType::SeqBlockMetadataType(char _type[5]):
   typeStr(_type)
{
   // TODO
}

bool SeqBlockMetadataType::operator==(SeqBlockMetadataType& other)
{
   return typeStr == other.typeStr;
}

SeqBlockMetadata::SeqBlockMetadata():
   mNativeEndian(false)
{
}

SeqBlockMetadata::SeqBlockMetadata(char *data, int len)
{
   posh_u32_t riffID;

   memcpy(&riffID, data, 4);
   data += 4;

   if(riffID == 'RIFF')
      mNativeEndian = true;
   else if(riffID == 'FFIR')
      mNativeEndian = false;
   else
      UserAssert(false, "Metadata is not in RIFF format");

   posh_u32_t riffSize;
   memcpy(&riffSize, data, 4);
   data += 4;

   if(!mNativeEndian)
      riffSize = POSH_SwapU32(riffSize);

   while(riffSize > 0)
   {
      char id[5];
      memcpy(id, data, 4);
      data += 4;
      id[4] = '\0';

      posh_u32_t sectionSize;
      memcpy(&sectionSize, data, 4);
      data += 4;

      if(!mNativeEndian)
         sectionSize = POSH_SwapU32(sectionSize);

      char *sectionData = new char[sectionSize];
      memcpy(sectionData, data, sectionSize);
      data += sectionSize;

      riffSize -= 8 + sectionSize;

      mSections.push_back(section(id, sectionSize, sectionData));
   }
}

SeqBlockMetadata::~SeqBlockMetadata()
{
   for(unsigned int i = 0; i < mSections.size(); i++)
      delete[] mSections[i].data;
}

void SeqBlockMetadata::AddSection(SeqBlockMetadataType type, char *data, int len)
{
   char *dataCopy = new char[len];
   memcpy(dataCopy, data, len);

   section s(type, len, dataCopy);

   mSections.push_back(s);
}

bool SeqBlockMetadata::HasSection(SeqBlockMetadataType type)
{
   for(unsigned int i = 0; i < mSections.size(); i++)
      if(mSections[i].type == type)
         return true;
   return false;
}

char *SeqBlockMetadata::GetSectionData(SeqBlockMetadataType type)
{
   for(unsigned int i = 0; i < mSections.size(); i++)
      if(mSections[i].type == type)
         return mSections[i].data;

   UserAssert(false, "Tried to get metadata for a section that doesn't exist.  "
                     "Use HasSection() to avoid this error");
   return NULL;
}

int SeqBlockMetadata::GetSectionLen(SeqBlockMetadataType type)
{
   for(unsigned int i = 0; i < mSections.size(); i++)
      if(mSections[i].type == type)
         return mSections[i].len;

   UserAssert(false, "Tried to get metadata length for a section that doesn't exist.  "
                     "Use HasSection() to avoid this error");
   return 0;
}

int SeqBlockMetadata::GetPackedLen()
{
   int size = 8;  // 4 for 'RIFF' and 4 for 32-bit size of entire block
   for(unsigned int i = 0; i < mSections.size(); i++)
   {
      size += 8;  // 4 for section type, 4 for 32-bit section length
      size += mSections[i].len;
   }

   return size;
}

void SeqBlockMetadata::GetPackedData(char *outData)
{
   // Write out everything to be host-endian

   posh_u32_t riffID = 'RIFF';  // we intentionally want this to be host-endian
   memcpy(outData, &riffID, 4);
   outData += 4;

   posh_u32_t totalSize = GetPackedLen() - 8; // don't count id and size overhead
   memcpy(outData, &totalSize, 4);
   outData += 4;

   for(unsigned int i = 0; i < mSections.size(); i++)
   {
      memcpy(outData, mSections[i].type.typeStr.c_str(), 4);
      outData += 4;

      memcpy(outData, &mSections[i].len, 4);
      outData += 4;

      memcpy(outData, mSections[i].data, mSections[i].len);
      outData += mSections[i].len;
   }
}

/*
SeqBlockMetadataHeader::SeqBlockMetadataHeader(FILE *file)
{
   // TODO
}

bool SeqBlockMetadataHeader::HasSection(enum SeqBlockMetadata::metadataType type)
{
   for(unsigned int i = 0; i < mSections.size(); i++)
      if(mSections[i].type == type)
         return true;
   return false;
}

int SeqBlockMetadataHeader::GetSectionOffset(enum SeqBlockMetadata::metadataType type)
{
   for(unsigned int i = 0; i < mSections.size(); i++)
      if(mSections[i].type == type)
         return mSections[i].offset;

   UserAssert(false, "Tried to get metadata offset for a section that doesn't exist.  "
                     "Use HasSection() to avoid this error");
   return 0;
}

int SeqBlockMetadataHeader::GetSectionLen(enum SeqBlockMetadata::metadataType type)
{
   for(unsigned int i = 0; i < mSections.size(); i++)
      if(mSections[i].type == type)
         return mSections[i].len;

   UserAssert(false, "Tried to get metadata len for a section that doesn't exist.  "
                     "Use HasSection() to avoid this error");
   return 0;
}
*/

//
// SeqBlockSummary
//

// Create summary data given uncompressed samples
SeqBlockSummary::SeqBlockSummary(Buffer samples):
   // the 256 summary has one summary sample for every 256 samples.  We add
   // 255 to ensure that the division always rounds up, because we will create
   // one full summary sample for however many samples remain.
   m256Len((samples.GetLength()+255)/256),
   // the 64k summary has one summary sample for every 65535 samples, which is
   // the same as one summary sample for every 256 256-summary samples.
   m64kLen((m256Len+255)/256),
   m256Min(m256Len),
   m256Max(m256Len),
   m256SumSq(m256Len),
   m64kMin(m64kLen),
   m64kMax(m64kLen),
   m64kSumSq(m64kLen)
{
   FloatBuffer floatSamples = samples.AsFloat();
   int len = floatSamples.GetLength();

   int i;

   //
   // create the 256 summaries
   //
   for(i=0; i<m256Len; i++) {
      int subLen = i*256 + 256 > len? len - i*256: 256;
      FloatBuffer sub = floatSamples.Get(i*256, subLen);
      m256Min[i] = sub.GetMin();
      m256Max[i] = sub.GetMax();
      m256SumSq[i] = sub.GetSumSq();
   }

   //
   // create the 64k summaries
   //
   for(i=0; i<m64kLen; i++) {
      int subLen = i*256 + 256 > m256Len? m256Len - i*256: 256;
      m64kMin[i] = m256Min.Get(i*256, subLen).GetMin();
      m64kMax[i] = m256Max.Get(i*256, subLen).GetMax();

      // We don't want to use SumSq here because all of the original samples have
      // already been squared, and squaring the samples of the 256 summary would
      // be meaningless. Instead we get a value that is all the samples in the
      // buffer added together
      m64kSumSq[i] = m256SumSq.Get(i*256, subLen).GetSum();
   }

   mMin = m64kMin.GetMin();
   mMax = m64kMax.GetMax();
   mSumSq = m64kSumSq.GetSum();
}

//
// The current summary format is to save all data in this order:
//
// 256Min
// 256Max
// 256SumSq
// 64kMin
// 64kMax
// 64kSumSq
// Min
// Max
// SumSq

SeqBlockSummary::SeqBlockSummary(SeqBlockMetadata *md):
   m256Min(md->GetSectionLen("256m")),
   m256Max(md->GetSectionLen("256M")),
   m256SumSq(md->GetSectionLen("256s")),
   m64kMin(md->GetSectionLen("64km")),
   m64kMax(md->GetSectionLen("64kM")),
   m64kSumSq(md->GetSectionLen("64ks"))
{
   m256Min.Set((float*)md->GetSectionData("256m"), 0, m256Min.GetLength());
   m256Max.Set((float*)md->GetSectionData("256M"), 0, m256Max.GetLength());
   m256SumSq.Set((float*)md->GetSectionData("256s"), 0, m256SumSq.GetLength());

   m64kMin.Set((float*)md->GetSectionData("64km"), 0, m64kMin.GetLength());
   m64kMax.Set((float*)md->GetSectionData("64kM"), 0, m64kMax.GetLength());
   m64kSumSq.Set((float*)md->GetSectionData("64ks"), 0, m64kSumSq.GetLength());

   float *blockSummary = (float*)md->GetSectionData("smry");

   mMin = blockSummary[0];
   mMax = blockSummary[1];
   mSumSq = blockSummary[2];
}

void SeqBlockSummary::Write(SeqBlockMetadata *md)
{
   md->AddSection("256m", (char*)m256Min.GetPtr(), m256Min.GetLength() * sizeof(float));
   md->AddSection("256M", (char*)m256Max.GetPtr(), m256Max.GetLength() * sizeof(float));
   md->AddSection("256s", (char*)m256SumSq.GetPtr(), m256SumSq.GetLength() * sizeof(float));

   md->AddSection("64km", (char*)m64kMin.GetPtr(), m64kMin.GetLength() * sizeof(float));
   md->AddSection("64kM", (char*)m64kMax.GetPtr(), m64kMax.GetLength() * sizeof(float));
   md->AddSection("64ks", (char*)m64kSumSq.GetPtr(), m64kSumSq.GetLength() * sizeof(float));

   FloatBuffer blockSummary(3);
   blockSummary[0] = mMin;
   blockSummary[1] = mMax;
   blockSummary[2] = mSumSq;

   md->AddSection("smry", (char*)blockSummary.GetPtr(), 3*sizeof(float));
}


// static
void SeqBlockSummary::Get256(SeqBlockMetadata *md, int start, int len,
            FloatBuffer& min, FloatBuffer& max, FloatBuffer& sumSq)
{

   if(md->HasSection("256m"))
   {
      min.Resize(md->GetSectionLen("256m"));
      min.Set((float*)md->GetSectionData("256m"), 0, md->GetSectionLen("256m"));
   }
   else
      printf("no 256m section!\n");


   if(md->HasSection("256M"))
   {
      max.Resize(md->GetSectionLen("256M"));
      max.Set((float*)md->GetSectionData("256M"), 0, md->GetSectionLen("256M"));
   }
   else
      printf("no 256M section!\n");

   if(md->HasSection("256s"))
   {
      sumSq.Resize(md->GetSectionLen("256s"));
      sumSq.Set((float*)md->GetSectionData("256s"), 0, md->GetSectionLen("256s"));
   }
   else
      printf("no 256s section!\n");
}

//static

void SeqBlockSummary::Get64k(SeqBlockMetadata *md, int start, int len,
            FloatBuffer& min, FloatBuffer& max, FloatBuffer& sumSq)
{
   min.Resize(md->GetSectionLen("64km"));
   max.Resize(md->GetSectionLen("64kM"));
   sumSq.Resize(md->GetSectionLen("64ks"));

   min.Set((float*)md->GetSectionData("64km"), 0, md->GetSectionLen("64km"));
   max.Set((float*)md->GetSectionData("64kM"), 0, md->GetSectionLen("64kM"));
   sumSq.Set((float*)md->GetSectionData("64ks"), 0, md->GetSectionLen("64ks"));
}

//
// SeqBlock
//

/*
void SeqBlock::GetMinMax(int start, int len,
                         float *outMin, float *outMax, float *outRMS) const
{
   // TODO: raise exception if start or len are out of range

   if ((start == 0) && (len == mLen)) {
      if (outMin)
         *outMin = mMin;
      if (outMax)
         *outMax = mMax;
      if (outRMS)
         *outRMS = mRMS;

      return;
   }

   FloatBuffer subset = Get(start, len).AsFloat();

   if (outMin)
      *outMin = subset.GetMin();
   if (outMax)
      *outMax = subset.GetMax();
   if (outRMS)
      *outRMS = subset.GetRMS();
} */

void SeqSilentBlock::Store(Storer& storer)
{
   // <SeqSilentBlock len="335"/>
   AttrDict attrs;
   attrs["len"] = fmt("%d", mLen);
   storer.StoreLeafNode("SeqSilentBlock", attrs);
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


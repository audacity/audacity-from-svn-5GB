/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include "WaveTrack.h"

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textfile.h>
#include <wx/intl.h>

#include "BlockFile.h"
#include "Envelope.h"
#include "DirManager.h"

const int headerTagLen = 20;
char headerTag[headerTagLen + 1] = "AudacityBlockFile110";

int WaveTrack::sMaxDiskBlockSize = 1048576;

// Static
void WaveTrack::SetMaxDiskBlockSize(int bytes)
{
   sMaxDiskBlockSize = bytes;
}

void WaveTrack::CalcSummaryInfo()
{
   SummaryInfo *s = &mSummary;

   s->bytesPerFrame = 6;

   s->frames64K = (mMaxSamples + 65535) / 65536;
   s->frames256 = s->frames64K * 256;

   s->offset64K = headerTagLen;
   s->offset256 = s->offset64K + (s->frames64K * s->bytesPerFrame);
   s->totalSummaryBytes = s->offset256 + (s->frames256 * s->bytesPerFrame);  
}

// WaveTrack methods
WaveTrack::WaveTrack(DirManager * projDirManager):
VTrack(projDirManager)
{
   mNumSamples = 0;
   mRate = 44100.0;
   SetName(_("Audio Track"));
   mDisplay = WaveformDisplay;

   mSampleFormat = int16Sample;

   mBlock = new BlockArray();
   mEnvelope = new Envelope();

   mMinSamples = sMaxDiskBlockSize / SAMPLE_SIZE(mSampleFormat) / 2;
   mMaxSamples = mMinSamples * 2;

   CalcSummaryInfo();
}

WaveTrack::WaveTrack(const WaveTrack &orig) :
VTrack(orig)
{
   mNumSamples = 0;
   mRate = orig.mRate;
   mDisplay = orig.mDisplay;
   mSampleFormat = orig.mSampleFormat;
   mSummary = orig.mSummary;
   mMaxSamples = orig.mMaxSamples;
   mMinSamples = orig.mMinSamples;

   mBlock = new BlockArray();
   mEnvelope = new Envelope();

   Paste(0.0, &orig);
   mEnvelope->SetOffset(GetOffset());
}

WaveTrack::~WaveTrack()
{
   for (unsigned int i = 0; i < mBlock->Count(); i++) {
      GetDirManager()->Deref(mBlock->Item(i)->f);
      delete mBlock->Item(i);
   }

   delete mBlock;
   delete mEnvelope;
}

int WaveTrack::GetSummaryBytes() const
{
   return mSummary.totalSummaryBytes;
}

sampleCount WaveTrack::GetMaxBlockSize() const
{
   return mMaxSamples;
}

sampleCount WaveTrack::GetIdealBlockSize() const
{
   return mMaxSamples;
}

void WaveTrack::Lock()
{
   for (unsigned int i = 0; i < mBlock->Count(); i++)
      mBlock->Item(i)->f->Lock();
}

void WaveTrack::Unlock()
{
   for (unsigned int i = 0; i < mBlock->Count(); i++)
      mBlock->Item(i)->f->Unlock();
}

double WaveTrack::GetMaxLen() const
{
   return ((double) mNumSamples) / (mRate) + GetOffset();
}

double WaveTrack::GetRate() const
{
   return mRate;
}

void WaveTrack::SetRate(double newRate)
{
   mRate = newRate;
   SetDirty(GetDirty() + 1);                     // forces redraw
}

void WaveTrack::SetOffset(double t)
{
   VTrack::SetOffset(t);
   mEnvelope->SetOffset(t);
}

sampleFormat WaveTrack::GetSampleFormat() const
{
   return mSampleFormat;
}

bool WaveTrack::SetSampleFormat(sampleFormat format)
{
   if (mBlock->Count() > 0 || mNumSamples > 0)
      return false;

   mSampleFormat = format;
   return true;
}

void WaveTrack::ConvertToSampleFormat(sampleFormat format)
{
   if (format == mSampleFormat)
      return;

   if (mBlock->Count() == 0)
      return;

   wxBusyCursor busy;
   wxYield();

   sampleFormat oldFormat = mSampleFormat;
   mSampleFormat = format;

   for (unsigned int i = 0; i < mBlock->Count(); i++) {
      BlockFile *oldBlock = mBlock->Item(i)->f;
      sampleCount len = mBlock->Item(i)->len;

      if (!oldBlock->IsAlias()) {
         BlockFile *newBlock =
            GetDirManager()->NewBlockFile(mSummary.totalSummaryBytes);

         samplePtr buffer1 = NewSamples(len, oldFormat);
         samplePtr buffer2 = NewSamples(len, mSampleFormat);

         oldBlock->ReadData(buffer1, oldFormat, 0, len);
         CopySamples(buffer1, oldFormat,
                     buffer2, mSampleFormat, len);
         newBlock->WriteData(buffer2, mSampleFormat, len);

         mBlock->Item(i)->f = newBlock;
         GetDirManager()->Deref(oldBlock);

         UpdateSummaries(buffer2, mBlock->Item(i), len);

         DeleteSamples(buffer2);
         DeleteSamples(buffer1);
      }
   }
}

void WaveTrack::GetMinMax(sampleCount start, sampleCount len,
                          float * outMin, float * outMax) const
{
   if (len == 0 || mBlock->Count() == 0) {
      *outMin = 0.0;
      *outMax = 0.0;
      return;
   }

   short min = 32767;
   short max = -32768;

   unsigned int block0 = FindBlock(start);
   unsigned int block1 = FindBlock(start + len);
   
   sampleCount s0, l0, maxl0;

   // First calculate the min/max of the blocks in the middle of this region;
   // this is very fast because we have the min/max of every entire block
   // already in memory.
   unsigned int b;
   int i;

   for (b = block0 + 1; b < block1; b++) {
      if (mBlock->Item(b)->min < min)
         min = mBlock->Item(b)->min;
      if (mBlock->Item(b)->max > max)
         max = mBlock->Item(b)->max;
   }

   // Now we take the first and last blocks into account, noting that the
   // selection may only partly overlap these blocks.  If the overall min/max
   // of either of these blocks is within min...max, then we can ignore them.
   // If not, we need read some samples and summaries from disk.
   if (mBlock->Item(block0)->min < min || mBlock->Item(block0)->max > max) {
      s0 = start - mBlock->Item(block0)->start;
      l0 = len;
      maxl0 = mBlock->Item(block0)->start + mBlock->Item(block0)->len - start;
      if (l0 > maxl0)
         l0 = maxl0;

      short *buffer = new short[l0];

      // TODO: optimize this to use Read256 and Read64K
      Read((samplePtr)buffer, int16Sample, mBlock->Item(block0), s0, l0);
      for (i = 0; i < l0; i++) {
         if (buffer[i] < min)
            min = buffer[i];
         if (buffer[i] > max)
            max = buffer[i];
      }

      delete[]buffer;
   }

   if (block1 > block0 &&
       (mBlock->Item(block1)->min < min
        || mBlock->Item(block1)->max > max)) {

      s0 = 0;
      l0 = (start + len) - mBlock->Item(block1)->start;
      short *buffer = new short[l0];

      // TODO: optimize this to use Read256 and Read64K
      Read((samplePtr)buffer, int16Sample, mBlock->Item(block1), s0, l0);
      for (i = 0; i < l0; i++) {
         if (buffer[i] < min)
            min = buffer[i];
         if (buffer[i] > max)
            max = buffer[i];
      }

      delete[]buffer;

   }

   CopySamples((samplePtr)&min, int16Sample,
               (samplePtr)outMin, floatSample, 1);
   CopySamples((samplePtr)&max, int16Sample,
               (samplePtr)outMax, floatSample, 1);
}

void WaveTrack::SetDisplay(int d)
{
   mDisplay = d;
   SetDirty(GetDirty() + 1);                     // forces redraw
}

int WaveTrack::GetDisplay() const
{
   return mDisplay;
}

void WaveTrack::Cut(double t0, double t1, VTrack ** dest)
{
   Copy(t0, t1, dest);
   Clear(t0, t1);
}

void WaveTrack::Copy(double t0, double t1, VTrack ** dest) const
{
   *dest = 0;

   wxASSERT(t0 <= t1);

   sampleCount s0 = (sampleCount) ((t0 - GetOffset()) * mRate + 0.5);
   sampleCount s1 = (sampleCount) ((t1 - GetOffset()) * mRate + 0.5);

   if (t0 < GetOffset())
      s0 = 0;
   if (s1 >= mNumSamples)
      s1 = mNumSamples;

   if (s0 >= s1 || s0 >= mNumSamples || s1 < 0)
      return;

   int numBlocks = mBlock->Count();
   int b0 = FindBlock(s0);
   int b1 = FindBlock(s1);

   if (s1 == mNumSamples)
      b1 = numBlocks;

   *dest = new WaveTrack(GetDirManager());
   ((WaveTrack *) * dest)->mRate = mRate;
   ((WaveTrack *) * dest)->mSampleFormat = mSampleFormat;

   samplePtr buffer = NewSamples(mMaxSamples, mSampleFormat);

   int blocklen;

   // Do the first block

   if (b0 >= 0 && b0 < numBlocks && s0 != mBlock->Item(b0)->start) {

      blocklen = (mBlock->Item(b0)->start + mBlock->Item(b0)->len - s0);
      if (blocklen > (s1 - s0))
         blocklen = s1 - s0;
      Get(buffer, mSampleFormat, s0, blocklen);

      ((WaveTrack *) * dest)->Append(buffer, mSampleFormat, blocklen);
   }

   if (b0 >= 0 && b0 < numBlocks && s0 == mBlock->Item(b0)->start) {
      b0--;
   }
   // If there are blocks in the middle, copy the blockfiles directly
   for (int b = b0 + 1; b < b1; b++)
      ((WaveTrack *) * dest)->AppendBlock(mBlock->Item(b));

   // Do the last block
   if (b1 > b0 && b1 < numBlocks) {
      blocklen = (s1 - mBlock->Item(b1)->start);
      Get(buffer, mSampleFormat, mBlock->Item(b1)->start, blocklen);
      ((WaveTrack *) * dest)->Append(buffer, mSampleFormat, blocklen);
   }

   DeleteSamples(buffer);

   ((WaveTrack *) *dest)->GetEnvelope()->CopyFrom(GetEnvelope(), t0, t1);
}

void WaveTrack::Paste(double t, const VTrack * src)
{
   wxASSERT(src->GetKind() == WaveTrack::Wave);

   mEnvelope->Paste(t, ((WaveTrack *) src)->mEnvelope);

   sampleCount s = (sampleCount) ((t - GetOffset()) * mRate + 0.5);

   if (s < 0)
      s = 0;
   if (s >= mNumSamples)
      s = mNumSamples;

   BlockArray *srcBlock = ((WaveTrack *) src)->GetBlockArray();
   int addedLen = ((WaveTrack *) src)->mNumSamples;
   unsigned int srcNumBlocks = srcBlock->Count();
   int sampleSize = SAMPLE_SIZE(mSampleFormat);

   if (addedLen == 0 || srcNumBlocks == 0)
      return;

   unsigned int b = FindBlock(s);
   unsigned int numBlocks = mBlock->Count();

   if (numBlocks == 0) {
      // Special case: this track is currently empty.

      for (unsigned int i = 0; i < srcNumBlocks; i++)
         AppendBlock(srcBlock->Item(i));

      ConsistencyCheck("Paste branch one");
      return;
   }

   if (b >= 0 && b < numBlocks
       && mBlock->Item(b)->len + addedLen < mMaxSamples) {
      // Special case: we can fit all of the new samples inside of
      // one block!

      samplePtr buffer = NewSamples(mMaxSamples, mSampleFormat);

      int splitPoint = s - mBlock->Item(b)->start;
      Read(buffer, mSampleFormat, mBlock->Item(b), 0, splitPoint);
      ((WaveTrack *) src)->Get(buffer + splitPoint*sampleSize,
                               mSampleFormat, 0, addedLen);
      Read(buffer + (splitPoint + addedLen)*sampleSize,
           mSampleFormat, mBlock->Item(b),
           splitPoint, mBlock->Item(b)->len - splitPoint);

      WaveBlock *largerBlock = new WaveBlock();
      largerBlock->start = mBlock->Item(b)->start;
      largerBlock->len = mBlock->Item(b)->len + addedLen;
      largerBlock->f =
         GetDirManager()->NewBlockFile(mSummary.totalSummaryBytes);

      FirstWrite(buffer, largerBlock, largerBlock->len);

      GetDirManager()->Deref(mBlock->Item(b)->f);
      delete mBlock->Item(b);
      mBlock->Item(b) = largerBlock;

      for (unsigned int i = b + 1; i < numBlocks; i++)
         mBlock->Item(i)->start += addedLen;

      mNumSamples += addedLen;

      DeleteSamples(buffer);

      ConsistencyCheck("Paste branch two");
      return;
   }

   // Case two: if we are inserting four or fewer blocks,
   // it's simplest to just lump all the data together
   // into one big block along with the split block,
   // then resplit it all
   unsigned int i;

   BlockArray *newBlock = new BlockArray();
   newBlock->Alloc(numBlocks + srcNumBlocks + 2);
   int newNumBlocks = 0;

   for (i = 0; i < b; i++) {
      newBlock->Add(mBlock->Item(i));
      newNumBlocks++;
   }

   WaveBlock *splitBlock = mBlock->Item(b);
   sampleCount splitLen = mBlock->Item(b)->len;
   int splitPoint = s - splitBlock->start;

   if (srcNumBlocks <= 4) {

      sampleCount sum = splitLen + addedLen;

      samplePtr sumBuffer = NewSamples(sum, mSampleFormat);
      Read(sumBuffer, mSampleFormat, splitBlock, 0, splitPoint);
      ((WaveTrack *) src)->Get(sumBuffer + splitPoint * sampleSize,
                               mSampleFormat,
                               0, addedLen);
      Read(sumBuffer + (splitPoint + addedLen) * sampleSize, mSampleFormat,
           splitBlock, splitPoint,
           splitBlock->len - splitPoint);

      BlockArray *split = Blockify(sumBuffer, sum);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += splitBlock->start;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;
      DeleteSamples(sumBuffer);
   } else {

      // The final case is that we're inserting at least five blocks.
      // We divide these into three groups: the first two get merged
      // with the first half of the split block, the middle ones get
      // copied in as is, and the last two get merged with the last
      // half of the split block.

      sampleCount srcFirstTwoLen =
          srcBlock->Item(0)->len + srcBlock->Item(1)->len;
      sampleCount leftLen = splitPoint + srcFirstTwoLen;

      samplePtr leftBuffer = NewSamples(leftLen, mSampleFormat);
      Read(leftBuffer, mSampleFormat, splitBlock, 0, splitPoint);
      ((WaveTrack *) src)->Get(leftBuffer + splitPoint*sampleSize,
                               mSampleFormat,
                               0, srcFirstTwoLen);

      BlockArray *split = Blockify(leftBuffer, leftLen);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += splitBlock->start;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;
      DeleteSamples(leftBuffer);

      for (i = 2; i < srcNumBlocks - 2; i++) {
         WaveBlock *insertBlock = new WaveBlock();
         insertBlock->start = srcBlock->Item(i)->start + s;
         insertBlock->len = srcBlock->Item(i)->len;
         insertBlock->min = srcBlock->Item(i)->min;
         insertBlock->max = srcBlock->Item(i)->max;

         insertBlock->f = GetDirManager()->CopyBlockFile(srcBlock->Item(i)->f);
         if (!insertBlock->f) {
            wxMessageBox("Could not paste!  (Out of disk space?)");
         }

         newBlock->Add(insertBlock);
         newNumBlocks++;
      }

      sampleCount srcLastTwoLen =
          srcBlock->Item(srcNumBlocks - 2)->len +
          srcBlock->Item(srcNumBlocks - 1)->len;
      sampleCount rightSplit = splitBlock->len - splitPoint;
      sampleCount rightLen = rightSplit + srcLastTwoLen;

      samplePtr rightBuffer = NewSamples(rightLen, mSampleFormat);
      sampleCount lastStart = srcBlock->Item(srcNumBlocks - 2)->start;
      ((WaveTrack *) src)->Get(rightBuffer, mSampleFormat,
                               lastStart, srcLastTwoLen);
      Read(rightBuffer + srcLastTwoLen * sampleSize, mSampleFormat,
           splitBlock, splitPoint, rightSplit);

      sampleCount pos = s + lastStart;

      split = Blockify(rightBuffer, rightLen);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += pos;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;
      DeleteSamples(rightBuffer);
   }

   GetDirManager()->Deref(splitBlock->f);
   delete splitBlock;

   // Copy remaining blocks to new block array and
   // swap the new block array in for the old
   for (i = b + 1; i < numBlocks; i++) {
      mBlock->Item(i)->start += addedLen;
      newBlock->Add(mBlock->Item(i));
      newNumBlocks++;
   }

   delete mBlock;
   mBlock = newBlock;

   mNumSamples += addedLen;

   ConsistencyCheck("Paste branch three");
}

void WaveTrack::Clear(double t0, double t1)
{
   wxASSERT(t0 <= t1);

   mEnvelope->CollapseRegion(t0, t1);

   sampleCount s0 = (sampleCount) ((t0 - GetOffset()) * mRate + 0.5);
   sampleCount s1 = (sampleCount) ((t1 - GetOffset()) * mRate + 0.5);

   if (t0 < GetOffset())
      s0 = 0;
   if (s1 >= mNumSamples)
      s1 = mNumSamples;

   if (s0 >= s1 || s0 >= mNumSamples || s1 < 0)
      return;

   Delete(s0, (s1 - s0));
}

void WaveTrack::Silence(double t0, double t1)
{
   wxASSERT(t0 <= t1);

   sampleCount s0 = (sampleCount) ((t0 - GetOffset()) * mRate + 0.5);
   sampleCount s1 = (sampleCount) ((t1 - GetOffset()) * mRate + 0.5);

   if (t0 < GetOffset())
      s0 = 0;
   if (s1 >= mNumSamples)
      s1 = mNumSamples;

   if (s0 >= s1 || s0 >= mNumSamples || s1 < 0)
      return;

   Set(NULL, mSampleFormat, s0, s1 - s0);
}

void WaveTrack::InsertSilence(double t, double lenSecs)
{
   // Create a new track containing as much silence as we
   // need to insert, and then call Paste to do the insertion
   sampleCount len = (sampleCount) (lenSecs * mRate + 0.5);

   WaveTrack *sTrack = new WaveTrack(GetDirManager());
   sTrack->mRate = mRate;
   sTrack->mSampleFormat = mSampleFormat;

   sampleCount idealSamples = GetIdealBlockSize();

   // Allocate a zeroed buffer, exploiting the fact that zeroing a
   // buffer creates zero ints and zero floats equally well...
   samplePtr buffer = NewSamples(idealSamples, mSampleFormat);
   ClearSamples(buffer, mSampleFormat, 0, idealSamples);

   sampleCount pos = 0;
   BlockFile *firstBlockFile = NULL;

   while (len) {
      sampleCount l = (len > idealSamples ? idealSamples : len);

      WaveBlock *w = new WaveBlock();
      w->start = pos;
      w->len = l;
      w->min = 0;
      w->max = 0;
      if (pos == 0 || len == l) {
         w->f = GetDirManager()->NewBlockFile(mSummary.totalSummaryBytes);
         firstBlockFile = w->f;
         FirstWrite(buffer, w, l);
      } else {
         w->f = GetDirManager()->CopyBlockFile(firstBlockFile);
         if (!w->f) {
            wxMessageBox("Could not paste!  (Out of disk space?)");
         }
      }

      sTrack->mBlock->Add(w);

      pos += l;
      len -= l;
   }

   sTrack->mNumSamples = pos;

   Paste(t, sTrack);

   delete sTrack;
   DeleteSamples(buffer);

   ConsistencyCheck("InsertSilence");
}

void WaveTrack::AppendAlias(wxString fullPath,
                            sampleCount start,
                            sampleCount len, int channel)
{
   WaveBlock *newBlock = new WaveBlock();
   newBlock->start = mNumSamples;
   newBlock->len = len;
   newBlock->f = GetDirManager()->NewBlockFile(mSummary.totalSummaryBytes);
   newBlock->f->SetAliasedData(fullPath, start, len, channel);

   samplePtr buffer = NewSamples(len, mSampleFormat);
   Read(buffer, mSampleFormat, newBlock, 0, len);

   UpdateSummaries(buffer, newBlock, len);

   DeleteSamples(buffer);

   mBlock->Add(newBlock);
   mNumSamples += newBlock->len;

   mEnvelope->SetTrackLen(mNumSamples / mRate);
}

void WaveTrack::AppendBlock(WaveBlock * b)
{
   WaveBlock *newBlock = new WaveBlock();
   newBlock->start = mNumSamples;
   newBlock->len = b->len;
   newBlock->f = GetDirManager()->CopyBlockFile(b->f);
   if (!newBlock->f) {
      wxMessageBox("Could not paste!  (Out of disk space?)");
   }
   newBlock->min = b->min;
   newBlock->max = b->max;
   GetDirManager()->Ref(newBlock->f);
   mBlock->Add(newBlock);
   mNumSamples += newBlock->len;

   mEnvelope->SetTrackLen(mNumSamples / mRate);

   // Don't do a consistency check here because this
   // function gets called in an inner loop
}

sampleCount WaveTrack::GetBestBlockSize(sampleCount start) const
{
   // This method returns a nice number of samples you should try to grab in
   // one big chunk in order to land on a block boundary, based on the starting
   // sample.  The value returned will always be nonzero and will be no larger
   // than the value of GetMaxBlockSize();
   int b = FindBlock(start);
   int numBlocks = mBlock->Count();
   
   sampleCount result = (mBlock->Item(b)->start + mBlock->Item(b)->len - start);
   
   while(result < mMinSamples && b+1<numBlocks &&
         (mBlock->Item(b)->len+result) <= mMaxSamples) {
      b++;
      result += mBlock->Item(b)->len;
   }
   
   wxASSERT(result > 0 && result <= mMaxSamples);
   
   return result;
}

bool WaveTrack::Load(wxTextFile * in, DirManager * dirManager)
{
   bool result = VTrack::Load(in, dirManager);

   mEnvelope->SetOffset(GetOffset());

   if (result) {
      result = mEnvelope->Load(in, dirManager);
   }

   if (!result) {
      wxMessageBox(_("Error loading a Track.\n"));
      return false;
   }

   int b;
   long longNumSamples;
   long numBlocks;
   long longBlockStart;
   long longBlockLen;
   long longSampleFormat;
   long longMaxSamples;
   WaveBlock *w;

   if (in->GetNextLine() != "maxSamples")
      goto readWaveTrackError;
   if (!(in->GetNextLine().ToLong(&longMaxSamples)))
      goto readWaveTrackError;
   mMaxSamples = longMaxSamples;
   mMinSamples = mMaxSamples / 2;
   CalcSummaryInfo();

   if (in->GetNextLine() != "sampleFormat")
      goto readWaveTrackError;
   if (!(in->GetNextLine().ToLong(&longSampleFormat)))
      goto readWaveTrackError;
   mSampleFormat = (sampleFormat)longSampleFormat;

   if (in->GetNextLine() != "numSamples")
      goto readWaveTrackError;
   if (!(in->GetNextLine().ToLong(&longNumSamples)))
      goto readWaveTrackError;
   mNumSamples = longNumSamples;

   if (in->GetNextLine() != "rate")
      goto readWaveTrackError;
   if (!(in->GetNextLine().ToDouble(&mRate)))
      goto readWaveTrackError;

   if (in->GetNextLine() != "numBlocks")
      goto readWaveTrackError;
   if (!(in->GetNextLine().ToLong(&numBlocks)))
      goto readWaveTrackError;

   mBlock->Alloc(numBlocks);

   for (b = 0; b < numBlocks; b++) {
      w = new WaveBlock();

      if (in->GetNextLine() != "Block start")
         goto readWaveTrackError;
      if (!(in->GetNextLine().ToLong(&longBlockStart)))
         goto readWaveTrackError;
      w->start = longBlockStart;

      if (in->GetNextLine() != "Block len")
         goto readWaveTrackError;
      if (!(in->GetNextLine().ToLong(&longBlockLen)))
         goto readWaveTrackError;
      w->len = longBlockLen;

      if (in->GetNextLine() != "Block info")
         goto readWaveTrackError;

      w->f = dirManager->LoadBlockFile(in, mSampleFormat);

      if (!w->f) {

         mNumSamples = 0;
         mBlock->Clear();

         wxString msg;
         msg.Printf(_("The file named \"%s\" is missing from the project."),
                    (const char *) in->GetCurrentLine());
         wxMessageBox(msg);

         return false;
      }

      mBlock->Add(w);
   }

   return true;

 readWaveTrackError:
   wxMessageBox(wxString::Format(_("Error reading WaveTrack in line %d"),
                                 in->GetCurrentLine()));
   return false;
}

bool WaveTrack::Save(wxTextFile * out, bool overwrite)
{
   VTrack::Save(out, overwrite);

   mEnvelope->Save(out, overwrite);

   unsigned int b;

   out->AddLine("maxSamples");
   out->AddLine(wxString::Format("%d", (int)mMaxSamples));

   out->AddLine("sampleFormat");
   out->AddLine(wxString::Format("%d", (int)mSampleFormat));

   out->AddLine("numSamples");
   out->AddLine(wxString::Format("%d", mNumSamples));

   out->AddLine("rate");
   out->AddLine(wxString::Format("%g", mRate));

   out->AddLine("numBlocks");
   out->AddLine(wxString::Format("%d", mBlock->Count()));

   WaveBlock *bb;

   for (b = 0; b < mBlock->Count(); b++) {
      bb = mBlock->Item(b);

      out->AddLine("Block start");
      out->AddLine(wxString::Format("%d", bb->start));
      out->AddLine("Block len");
      out->AddLine(wxString::Format("%d", bb->len));
      out->AddLine("Block info");
      GetDirManager()->SaveBlockFile(bb->f, out);
   }

   return true;
}

float WaveTrack::Get(sampleCount pos) const
{
   float temp[1];
   Get((samplePtr)temp, floatSample, pos, 1);
   return temp[0];
}

WaveBlock *WaveTrack::NewInitedWaveBlock()
{
   WaveBlock *b = new WaveBlock();
   b->f = GetDirManager()->NewBlockFile(mSummary.totalSummaryBytes);
   return b;
}

int WaveTrack::FindBlock(sampleCount pos, sampleCount lo,
                         sampleCount guess, sampleCount hi) const
{
   wxASSERT(mBlock->Item(guess)->len > 0);
   wxASSERT(lo <= guess && guess <= hi && lo <= hi);

   if (pos >= mBlock->Item(guess)->start &&
       pos < mBlock->Item(guess)->start + mBlock->Item(guess)->len)
      return guess;

   if (pos < mBlock->Item(guess)->start)
      return FindBlock(pos, lo, (lo + guess) / 2, guess);
   else
      return FindBlock(pos, guess + 1, (guess + 1 + hi) / 2, hi);
}

int WaveTrack::FindBlock(sampleCount pos) const
{
   wxASSERT(pos >= 0 && pos <= mNumSamples);

   int numBlocks = mBlock->Count();

   if (pos == 0)
      return 0;

   if (pos == mNumSamples)
      return (numBlocks - 1);

   int rval = FindBlock(pos, 0, numBlocks / 2, numBlocks);

   wxASSERT(rval >= 0 && rval < numBlocks &&
            pos >= mBlock->Item(rval)->start &&
            pos < mBlock->Item(rval)->start + mBlock->Item(rval)->len);

   return rval;
}

void WaveTrack::Read(samplePtr buffer, sampleFormat format,
                     WaveBlock * b, sampleCount start, sampleCount len) const
{
   wxASSERT(b);
   wxASSERT(start >= 0);
   wxASSERT(start + len <= b->len);

   BlockFile *f = b->f;

   int result = f->ReadData(buffer, format, start, len);

   if (result != len) {
      printf(_("Expected to read %d samples, got %d samples.\n"),
             len, result);
   }
}

void WaveTrack::Read256(short *buffer, WaveBlock * b,
                        sampleCount start, sampleCount len) const
{
   wxASSERT(b);
   wxASSERT(start >= 0);
   wxASSERT(start + len <= ((b->len + 255) / 256));

   char *summary = (char *)malloc(GetSummaryBytes());
   if (!b->f->ReadSummary(summary)) {
      // TODO handle missing file
      return;
   }

   memcpy(buffer,
          summary + mSummary.offset256 + (start * mSummary.bytesPerFrame),
          len * mSummary.bytesPerFrame);

   free(summary);
}

void WaveTrack::Read64K(short *buffer, WaveBlock * b,
                        sampleCount start, sampleCount len) const
{
   wxASSERT(b);
   wxASSERT(start >= 0);
   wxASSERT(start + len <= ((b->len + 65535) / 65536));

   char *summary = (char *)malloc(GetSummaryBytes());
   if (!b->f->ReadSummary(summary)) {
      // TODO handle missing file
      return;
   }

   memcpy(buffer,
          summary + mSummary.offset64K + (start * mSummary.bytesPerFrame),
          len * mSummary.bytesPerFrame);

   free(summary);
}

void WaveTrack::FirstWrite(samplePtr buffer, WaveBlock * b,
                           sampleCount len)
{
   wxASSERT(b);
   wxASSERT(b->len <= mMaxSamples);

   SetDirty(GetDirty() + 1);    // forces redraw

   UpdateSummaries(buffer, b, len);
   b->f->WriteData(buffer, mSampleFormat, len);
}

void WaveTrack::CopyWrite(samplePtr buffer, WaveBlock *b,
                          sampleCount start, sampleCount len)
{
   // We don't ever write to an existing block; to support Undo,
   // we copy the old block entirely into memory, dereference it,
   // make the change, and then write the new block to disk.

   wxASSERT(b);
   wxASSERT(b->len <= mMaxSamples);
   wxASSERT(start + len <= b->len);

   SetDirty(GetDirty() + 1);    // forces redraw

   int sampleSize = SAMPLE_SIZE(mSampleFormat);
   samplePtr newBuffer = NewSamples(mMaxSamples, mSampleFormat);
   wxASSERT(newBuffer);

   Read(newBuffer, mSampleFormat, b, 0, b->len);
   memcpy(newBuffer + start*sampleSize, buffer, len*sampleSize);

   BlockFile *oldBlockFile = b->f;
   b->f = GetDirManager()->NewBlockFile(mSummary.totalSummaryBytes);

   GetDirManager()->Deref(oldBlockFile);

   b->f->WriteData(newBuffer, mSampleFormat, b->len);
   UpdateSummaries(newBuffer, b, b->len);

   DeleteSamples(newBuffer);
}

void WaveTrack::UpdateSummaries(samplePtr buffer,
                                WaveBlock * b, sampleCount len)
{
   char *fullSummary = (char *)malloc(mSummary.totalSummaryBytes);

   memcpy(fullSummary, headerTag, headerTagLen);

   short *summary64K = (short *)(fullSummary + mSummary.offset64K);
   short *summary256 = (short *)(fullSummary + mSummary.offset256);

   short *sbuffer = new short[len];
   CopySamples(buffer, mSampleFormat,
               (samplePtr)sbuffer, int16Sample, len);

   sampleCount sumLen;
   sampleCount i, j, jcount;

   short min, max;
   float sumsq;

   // Recalc 256 summaries
   sumLen = (len + 255) / 256;

   for (i = 0; i < sumLen; i++) {
      min = sbuffer[i * 256];
      max = sbuffer[i * 256];
      sumsq = ((float)min) * ((float)min);
      jcount = 256;
      if (i * 256 + jcount > len)
         jcount = len - i * 256;
      for (j = 1; j < jcount; j++) {
         short s1 = sbuffer[i * 256 + j];
         sumsq += ((float)s1) * ((float)s1);
         if (s1 < min)
            min = s1;
         else if (s1 > max)
            max = s1;
      }

      float rms = (float)sqrt(sumsq / jcount);

      summary256[i * 3] = min;
      summary256[i * 3 + 1] = max;
      summary256[i * 3 + 2] = (short)(rms + 0.5);
   }
   for (i = sumLen; i < mSummary.frames256; i++) {
      summary256[i * 3] = 0;
      summary256[i * 3 + 1] = 0;
      summary256[i * 3 + 2] = 0;
   }

   // Recalc 64K summaries
   sumLen = (len + 65535) / 65536;

   for (i = 0; i < sumLen; i++) {
      min = summary256[3 * i * 256];
      max = summary256[3 * i * 256 + 1];
      sumsq = (float)summary256[3 * i * 256 + 2];
      sumsq *= sumsq;

      for (j = 1; j < 256; j++) {
         if (summary256[3 * (i * 256 + j)] < min)
            min = summary256[3 * (i * 256 + j)];
         if (summary256[3 * (i * 256 + j) + 1] > max)
            max = summary256[3 * (i * 256 + j) + 1];
         float r1 = summary256[3 * (i * 256 + j) + 2];
         sumsq += r1*r1;
      }

      float rms = (float)sqrt(sumsq / 256);

      summary64K[i * 3] = min;
      summary64K[i * 3 + 1] = max;
      summary64K[i * 3 + 2] = (short)(rms + 0.5);
   }
   for (i = sumLen; i < mSummary.frames64K; i++) {
      summary256[i * 3] = 0;
      summary256[i * 3 + 1] = 0;
      summary256[i * 3 + 2] = 0;
   }

   // Recalc block-level summary
   min = summary64K[0];
   max = summary64K[1];
   sumsq = (float)summary64K[2];
   sumsq *= sumsq;
   
   for (i = 1; i < sumLen; i++) {
      if (summary64K[3*i] < min)
         min = summary64K[3*i];
      else if (summary64K[3*i+1] > max)
         max = summary64K[3*i+1];
      float r1 = (float)summary64K[3*i+2];
      sumsq += (r1*r1);
   }
   b->min = min;
   b->max = max;
   b->rms = (short)(sqrt(sumsq / sumLen) + 0.5);

   b->f->WriteSummary(fullSummary);

   delete[] sbuffer;
   free(fullSummary);
}

void WaveTrack::Get(float * buffer, sampleCount start,
                    sampleCount len) const
{
   Get((samplePtr)buffer, floatSample, start, len);
}

void WaveTrack::Get(samplePtr buffer, sampleFormat format,
                    sampleCount start, sampleCount len) const
{
   wxASSERT(start < mNumSamples && start + len <= mNumSamples);
   int b = FindBlock(start);

   while (len) {
      sampleCount blen =
          mBlock->Item(b)->start + mBlock->Item(b)->len - start;
      if (blen > len)
         blen = len;
      sampleCount bstart = (start - (mBlock->Item(b)->start));

      Read(buffer, format, mBlock->Item(b), bstart, blen);

      len -= blen;
      buffer += (blen * SAMPLE_SIZE(format));
      b++;
      start += blen;
   }
}

// Pass NULL to set silence
void WaveTrack::Set(float *buffer,
                    sampleCount start, sampleCount len)
{
   Set((samplePtr)buffer, floatSample, start, len);
}

// Pass NULL to set silence
void WaveTrack::Set(samplePtr buffer, sampleFormat format,
                    sampleCount start, sampleCount len)
{
   wxASSERT(start < mNumSamples && start + len <= mNumSamples);

   samplePtr temp = NULL;
   if (format != mSampleFormat) {
      temp = NewSamples(mMaxSamples, mSampleFormat);
      wxASSERT(temp);
   }

   samplePtr silence = NULL;
   if (!buffer) {
      silence = NewSamples(mMaxSamples, format);
      wxASSERT(silence);
      ClearSamples(silence, format, 0, mMaxSamples);
   }

   int b = FindBlock(start);

   while (len) {
      int blen = mBlock->Item(b)->start + mBlock->Item(b)->len - start;
      if (blen > len)
         blen = len;

      if (buffer) {
         if (format == mSampleFormat)
            CopyWrite(buffer, mBlock->Item(b), start - mBlock->Item(b)->start,
                      blen);
         else {
            CopySamples(buffer, format, temp, mSampleFormat, blen);
            CopyWrite(temp, mBlock->Item(b), start - mBlock->Item(b)->start,
                      blen);
         }
         buffer += (blen * SAMPLE_SIZE(format));
      } else
         CopyWrite(silence, mBlock->Item(b), start - mBlock->Item(b)->start,
                   blen);

      len -= blen;
      start += blen;
      b++;
   }

   if (!buffer)
      DeleteSamples(silence);

   if (format != mSampleFormat)
      DeleteSamples(temp);

   ConsistencyCheck("Set");
}

void WaveTrack::Append(samplePtr buffer, sampleFormat format,
                       sampleCount len)
{
   samplePtr temp = NULL;
   if (format != mSampleFormat) {
      temp = NewSamples(mMaxSamples, mSampleFormat);
      wxASSERT(temp);
   }

   // If the last block is not full, we need to add samples to it
   int numBlocks = mBlock->Count();
   if (numBlocks > 0 && mBlock->Item(numBlocks - 1)->len < mMinSamples) {
      WaveBlock *lastBlock = mBlock->Item(numBlocks - 1);
      sampleCount addLen;
      if (lastBlock->len + len < mMaxSamples)
         addLen = len;
      else
         addLen = GetIdealBlockSize() - lastBlock->len;

      WaveBlock *newLastBlock = NewInitedWaveBlock();

      samplePtr buffer2 = NewSamples((lastBlock->len + addLen), mSampleFormat);
      Read(buffer2, mSampleFormat, lastBlock, 0, lastBlock->len);

      if (format == mSampleFormat)
         memcpy(buffer2 + lastBlock->len * SAMPLE_SIZE(format),
                buffer,
                addLen * SAMPLE_SIZE(format));
      else {
         CopySamples(buffer, format, temp, mSampleFormat, addLen);
         memcpy(buffer2 + lastBlock->len * SAMPLE_SIZE(format),
                temp,
                addLen * SAMPLE_SIZE(format));
      }

      newLastBlock->start = lastBlock->start;
      newLastBlock->len = lastBlock->len + addLen;

      FirstWrite(buffer2, newLastBlock, lastBlock->len + addLen);

      DeleteSamples(buffer2);

      GetDirManager()->Deref(lastBlock->f);
      delete lastBlock;
      mBlock->Item(numBlocks - 1) = newLastBlock;

      len -= addLen;
      mNumSamples += addLen;
      buffer += addLen * SAMPLE_SIZE(format);
   }
   // Append the rest as new blocks
   while (len) {
      sampleCount idealSamples = GetIdealBlockSize();
      sampleCount l = (len > idealSamples ? idealSamples : len);
      WaveBlock *w = new WaveBlock();
      w->f = GetDirManager()->NewBlockFile(mSummary.totalSummaryBytes);
      w->start = mNumSamples;
      w->len = l;

      if (format == mSampleFormat)
         FirstWrite(buffer, w, l);
      else {
         CopySamples(buffer, format, temp, mSampleFormat, l);
         FirstWrite(temp, w, l);
      }

      mBlock->Add(w);

      buffer += l * SAMPLE_SIZE(format);
      mNumSamples += l;
      len -= l;
   }

   mEnvelope->SetTrackLen(mNumSamples / mRate);

   if (format != mSampleFormat)
      DeleteSamples(temp);

   ConsistencyCheck("Append");
}

BlockArray *WaveTrack::Blockify(samplePtr buffer, sampleCount len)
{
   BlockArray *list = new BlockArray();
   list->Alloc(10);

   if (len == 0)
      return list;

   int num = (len + (mMaxSamples - 1)) / mMaxSamples;

   for (int i = 0; i < num; i++) {
      WaveBlock *b = NewInitedWaveBlock();

      b->start = i * len / num;
      b->len = ((i + 1) * len / num) - b->start;

      FirstWrite(buffer + (b->start * SAMPLE_SIZE(mSampleFormat)),
                 b, b->len);

      list->Add(b);
   }

   return list;
}

void WaveTrack::Delete(sampleCount start, sampleCount len)
{
   if (len == 0)
      return;

   unsigned int numBlocks = mBlock->Count();
   unsigned int newNumBlocks = 0;

   unsigned int b0 = FindBlock(start);
   unsigned int b1 = FindBlock(start + len - 1);

   int sampleSize = SAMPLE_SIZE(mSampleFormat);

   // Special case: if the samples to delete are all within a single
   // block and the resulting length is not too small, perform the
   // deletion within this block:
   if (b0 == b1 && mBlock->Item(b0)->len - len >= mMinSamples) {
      WaveBlock *b = mBlock->Item(b0);
      sampleCount pos = start - b->start;
      sampleCount newLen = b->len - len;

      samplePtr buffer = NewSamples(newLen, mSampleFormat);

      Read(buffer, mSampleFormat, b, 0, pos);
      Read(buffer + (pos * sampleSize), mSampleFormat,
           b, pos + len, newLen - pos);

      WaveBlock *newBlock = NewInitedWaveBlock();
      newBlock->start = b->start;
      newBlock->len = newLen;
      FirstWrite(buffer, newBlock, newLen);

      mBlock->Item(b0) = newBlock;

      for (unsigned int j = b0 + 1; j < numBlocks; j++)
         mBlock->Item(j)->start -= len;

      DeleteSamples(buffer);

      GetDirManager()->Deref(b->f);
      delete b;

      mNumSamples -= len;
      mEnvelope->SetTrackLen(mNumSamples / mRate);
      ConsistencyCheck("Delete - branch one");

      return;
   }

   // Create a new array of blocks
   BlockArray *newBlock = new BlockArray();
   newBlock->Alloc(numBlocks - (b1 - b0) + 2);

   // Copy the blocks before the deletion point over to
   // the new array
   unsigned int i;
   for (i = 0; i < b0; i++) {
      newBlock->Add(mBlock->Item(i));
      newNumBlocks++;
   }

   // First grab the samples in block b0 before the deletion point
   // into preBuffer.  If this is enough samples for its own block,
   // or if this would be the first block in the array, write it out.
   // Otherwise combine it with the previous block (splitting them
   // 50/50 if necessary).
   WaveBlock *preBlock = mBlock->Item(b0);
   sampleCount preBufferLen = start - preBlock->start;
   if (preBufferLen) {
      if (preBufferLen >= mMinSamples || b0 == 0) {
         WaveBlock *insBlock = NewInitedWaveBlock();

         insBlock->len = preBufferLen;
         insBlock->start = preBlock->start;

         samplePtr preBuffer = NewSamples(preBufferLen, mSampleFormat);
         Read(preBuffer, mSampleFormat, preBlock, 0, preBufferLen);
         FirstWrite(preBuffer, insBlock, preBufferLen);
         DeleteSamples(preBuffer);

         newBlock->Add(insBlock);
         newNumBlocks++;

         if (b0 != b1) {
            GetDirManager()->Deref(preBlock->f);
            delete preBlock;
         }
      } else {
         WaveBlock *prepreBlock = mBlock->Item(b0 - 1);
         sampleCount prepreLen = prepreBlock->len;
         sampleCount sum = prepreLen + preBufferLen;

         samplePtr sumBuffer = NewSamples(sum, mSampleFormat);

         Read(sumBuffer, mSampleFormat, prepreBlock, 0, prepreLen);
         Read(sumBuffer + prepreLen*sampleSize, mSampleFormat,
              preBlock, 0, preBufferLen);

         BlockArray *split = Blockify(sumBuffer, sum);
         split->Item(0)->start += prepreBlock->start;
         newBlock->Item(b0 - 1) = split->Item(0);
         for (i = 1; i < split->Count(); i++) {
            split->Item(i)->start += prepreBlock->start;
            newBlock->Add(split->Item(i));
            newNumBlocks++;
         }
         delete split;

         DeleteSamples(sumBuffer);

         GetDirManager()->Deref(prepreBlock->f);
         delete prepreBlock;

         if (b0 != b1) {
            GetDirManager()->Deref(preBlock->f);
            delete preBlock;
         }
      }
   } else {
      // The sample where we begin deletion happens to fall
      // right on the beginning of a block.
      if (b0 != b1) {
         GetDirManager()->Deref(mBlock->Item(b0)->f);
         delete mBlock->Item(b0);
      }
   }

   // Next, delete blocks strictly between b0 and b1
   for (i = b0 + 1; i < b1; i++) {
      GetDirManager()->Deref(mBlock->Item(i)->f);
      delete mBlock->Item(i);
   }

   // Now, symmetrically, grab the samples in block b1 after the
   // deletion point into postBuffer.  If this is enough samples
   // for its own block, or if this would be the last block in
   // the array, write it out.  Otherwise combine it with the
   // subsequent block (splitting them 50/50 if necessary).
   WaveBlock *postBlock = mBlock->Item(b1);
   sampleCount postBufferLen =
       (postBlock->start + postBlock->len) - (start + len);
   if (postBufferLen) {
      if (postBufferLen >= mMinSamples || b1 == numBlocks - 1) {
         WaveBlock *insBlock = NewInitedWaveBlock();

         insBlock->len = postBufferLen;
         insBlock->start = start;

         samplePtr postBuffer = NewSamples(postBufferLen, mSampleFormat);
         sampleCount pos = (start + len) - postBlock->start;
         Read(postBuffer, mSampleFormat, postBlock, pos, postBufferLen);
         FirstWrite(postBuffer, insBlock, postBufferLen);
         DeleteSamples(postBuffer);

         newBlock->Add(insBlock);
         newNumBlocks++;

         GetDirManager()->Deref(postBlock->f);
         delete postBlock;
      } else {
         WaveBlock *postpostBlock = mBlock->Item(b1 + 1);
         sampleCount postpostLen = postpostBlock->len;
         sampleCount sum = postpostLen + postBufferLen;

         samplePtr sumBuffer = NewSamples(sum, mSampleFormat);
         sampleCount pos = (start + len) - postBlock->start;
         Read(sumBuffer, mSampleFormat, postBlock, pos, postBufferLen);
         Read(sumBuffer + (postBufferLen * sampleSize), mSampleFormat,
              postpostBlock, 0, postpostLen);

         BlockArray *split = Blockify(sumBuffer, sum);
         for (i = 0; i < split->Count(); i++) {
            split->Item(i)->start += start;
            newBlock->Add(split->Item(i));
            newNumBlocks++;
         }
         delete split;
         b1++;

         DeleteSamples(sumBuffer);

         GetDirManager()->Deref(postpostBlock->f);
         delete postpostBlock;
         GetDirManager()->Deref(postBlock->f);
         delete postBlock;
      }
   } else {
      // The sample where we begin deletion happens to fall
      // right on the end of a block.
      if (b0 != b1) {
         GetDirManager()->Deref(mBlock->Item(b1)->f);
         delete mBlock->Item(b1);
      }
   }

   // Copy the remaining blocks over from the old array
   for (i = b1 + 1; i < numBlocks; i++) {
      mBlock->Item(i)->start -= len;
      newBlock->Add(mBlock->Item(i));
      newNumBlocks++;
   }

   // Substitute our new array for the old one
   delete mBlock;
   mBlock = newBlock;

   // Update total number of samples, update the envelope,
   // and do a consistency check.
   mNumSamples -= len;
   mEnvelope->SetTrackLen(mNumSamples / mRate);
   ConsistencyCheck("Delete - branch two");
}

void WaveTrack::ConsistencyCheck(const char *whereStr)
{
   unsigned int i;
   int pos = 0;
   unsigned int numBlocks = mBlock->Count();
   bool error = false;

   for (i = 0; i < numBlocks; i++) {
      if (pos != mBlock->Item(i)->start)
         error = true;
      pos += mBlock->Item(i)->len;
   }
   if (pos != mNumSamples)
      error = true;

   if (error) {
#ifdef __WXDEBUG__
      printf("*** Consistency check failed after %s ***\n", whereStr);
      Debug();
      wxASSERT(0);
#else
      wxMessageBox(_("Internal error (WaveTrack)"));
      exit(0);
#endif
   }
}

void WaveTrack::DebugPrintf(wxString *dest)
{
   unsigned int i;
   int pos = 0;

   for (i = 0; i < mBlock->Count(); i++) {
      *dest += wxString::Format
         ("Block %3d: start %8d len %8d  %s",
          i,
          mBlock->Item(i)->start,
          mBlock->Item(i)->len,
          (const char *) (mBlock->Item(i)->f->GetName()));
      if (pos != mBlock->Item(i)->start)
         *dest += "  ERROR\n";
      else
         *dest += "\n";
      pos += mBlock->Item(i)->len;
   }
   if (pos != mNumSamples)
      *dest += wxString::Format
         ("ERROR mNumSamples = %d\n", mNumSamples);
}

void WaveTrack::Debug()
{
   wxString s;
   DebugPrintf(&s);
   printf((const char *)s);
}

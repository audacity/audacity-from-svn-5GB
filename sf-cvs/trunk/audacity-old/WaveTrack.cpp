/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>
#include <wx/file.h>
#include <wx/msgdlg.h>
#include <wx/textfile.h>

#include "WaveTrack.h"
#include "DirManager.h"
#include "Prefs.h"

// Max file size of 16-bit samples is 1MB
// (About 12 seconds of audio)

const int headerTagLen = 20;
char headerTag[headerTagLen + 1] = "AudacityBlockFile095";

// Static variables for block size

sampleCount WaveTrack::summary64KLen = 8 * 2;
sampleCount WaveTrack::summary256Len = summary64KLen * 256;
sampleCount WaveTrack::totalHeaderLen =
    headerTagLen + (summary64KLen + summary256Len) * sizeof(sampleType);

sampleCount WaveTrack::maxSamples = summary256Len * 256 / 2;
sampleCount WaveTrack::minSamples = maxSamples / 2;

// Static methods

sampleCount WaveTrack::GetMaxBlockSize()
{
   return maxSamples;
}

sampleCount WaveTrack::GetIdealBlockSize()
{
   return maxSamples;
   
   // Originally we split the difference between the minimum and maximum
   // size.  Now I believe that always making blocks as big as possible
   // is actually optimal more often.  -dmazzoni
   //
   //return (minSamples + maxSamples) / 2;
}

void WaveTrack::SetMaxDiskBlockSize(int bytes)
{
   maxSamples = bytes / sizeof(sampleType);
   minSamples = maxSamples / 2;

   summary256Len = (maxSamples + 255) / 256 * 2;
   summary64KLen = (summary256Len + 255) / 256 * 2;
   totalHeaderLen =
       headerTagLen + (summary64KLen + summary256Len) * sizeof(sampleType);
}

// WaveTrack methods

WaveTrack::WaveTrack(DirManager * projDirManager):
VTrack(projDirManager)
{
   numSamples = 0;
   rate = 44100.0;

   name = "Audio Track";

   display = 0;

   block = new BlockArray();

#if wxUSE_THREADS
   blockMutex = new wxMutex();
#endif
}

WaveTrack::~WaveTrack()
{
   for (int i = 0; i < block->Count(); i++) {
      dirManager->Deref(block->Item(i)->f);
      delete block->Item(i);
   }

   delete block;

#if wxUSE_THREADS
   delete blockMutex;
#endif
}

void WaveTrack::DeleteButDontDereference()
{
   for (int i = 0; i < block->Count(); i++) {
      delete block->Item(i);
   }
   block->Clear();

   delete this;
}

double WaveTrack::GetMaxLen()
{
   return ((double) numSamples) / (rate) + tOffset;
}

double WaveTrack::GetRate()
{
   return rate;
}

void WaveTrack::SetRate(double newRate)
{
   rate = newRate;
   dirty++;                     // forces redraw
}

void WaveTrack::Offset(double t)
{
   VTrack::Offset(t);

   envelope.SetOffset(tOffset);
}

VTrack *WaveTrack::Duplicate()
{
   WaveTrack *copy = new WaveTrack(dirManager);

   copy->tOffset = tOffset;
   copy->rate = rate;
   copy->name = name;
   copy->linked = linked;
   copy->Paste(0.0, this);
   copy->collapsedHeight = collapsedHeight;
   copy->expandedHeight = expandedHeight;
   copy->channel = channel;
   copy->selected = selected;
   copy->envelope.CopyFrom(&envelope);
   copy->display = display;

   return (VTrack *) copy;
}

void WaveTrack::GetMinMax(sampleCount start, sampleCount len,
                          sampleType * outMin, sampleType * outMax)
{
   sampleType min = 32727;
   sampleType max = -32768;

   int block0 = FindBlock(start);
   int block1 = FindBlock(start + len);

   // First calculate the min/max of the blocks in the middle of this region;
   // this is very fast because we have the min/max of every entire block already
   // in memory.

   int b, i;

   for (b = block0 + 1; b < block1; b++) {
      if (block->Item(b)->min < min)
         min = block->Item(b)->min;
      if (block->Item(b)->max > max)
         max = block->Item(b)->max;
   }

   // Now we take the first and last blocks into account, noting that the selection
   // may only partly overlap these blocks.  If the overall min/max of either of
   // these blocks is within min...max, then we can ignore them.  If not,
   // we need read some samples and summaries from disk.

   if (block->Item(block0)->min < min || block->Item(block0)->max > max) {
      int s0 = start - block->Item(block0)->start;
      int l0 = len;
      int maxl0 = block->Item(block0)->len - start;
      if (l0 > maxl0)
         l0 = maxl0;
      sampleType *buffer = new sampleType[l0];

      // TODO: optimize this to use Read256 and Read64K

      Read(buffer, block->Item(block0), s0, l0);
      for (i = 0; i < l0; i++) {
         if (buffer[i] < min)
            min = buffer[i];
         if (buffer[i] > max)
            max = buffer[i];
      }

      delete[]buffer;
   }

   if (block1 > block0 &&
       (block->Item(block1)->min < min
        || block->Item(block1)->max > max)) {

      int s0 = 0;
      int l0 = (start + len) - block->Item(block1)->start;
      sampleType *buffer = new sampleType[l0];

      // TODO: optimize this to use Read256 and Read64K

      Read(buffer, block->Item(block1), s0, l0);
      for (i = 0; i < l0; i++) {
         if (buffer[i] < min)
            min = buffer[i];
         if (buffer[i] > max)
            max = buffer[i];
      }

      delete[]buffer;

   }
   *outMin = min;
   *outMax = max;
}

void WaveTrack::SetDisplay(int d)
{
   display = d;
   dirty++;                     // forces redraw
}

int WaveTrack::GetDisplay()
{
   return display;
}

void WaveTrack::Cut(double t0, double t1, VTrack ** dest)
{
   Copy(t0, t1, dest);
   Clear(t0, t1);
}

void WaveTrack::Copy(double t0, double t1, VTrack ** dest)
{
   *dest = 0;

   wxASSERT(t0 <= t1);

#if wxUSE_THREADS
   wxMutexLocker lock(*blockMutex);
#endif

   sampleCount s0 = (sampleCount) ((t0 - tOffset) * rate + 0.5);
   sampleCount s1 = (sampleCount) ((t1 - tOffset) * rate + 0.5);

   if (s0 < 0)
      s0 = 0;
   if (s1 >= numSamples)
      s1 = numSamples;

   if (s0 >= s1 || s0 >= numSamples || s1 < 0)
      return;

   int numBlocks = block->Count();
   int b0 = FindBlock(s0);
   int b1 = FindBlock(s1);

   if (s1 == numSamples)
      b1 = numBlocks;

   *dest = new WaveTrack(dirManager);
   ((WaveTrack *) * dest)->rate = rate;

   sampleType *buffer = new sampleType[maxSamples];

   int blocklen;

   // Do the first block

   if (b0 >= 0 && b0 < numBlocks && s0 != block->Item(b0)->start) {

      blocklen = (block->Item(b0)->start + block->Item(b0)->len - s0);
      if (blocklen > (s1 - s0))
         blocklen = s1 - s0;
      Get(buffer, s0, blocklen);

      ((WaveTrack *) * dest)->Append(buffer, blocklen);
   }

   if (b0 >= 0 && b0 < numBlocks && s0 == block->Item(b0)->start) {
      b0--;
   }
   // If there are blocks in the middle, copy the blockfiles directly

   for (int b = b0 + 1; b < b1; b++)
      ((WaveTrack *) * dest)->AppendBlock(block->Item(b));

   // Do the last block

   if (b1 > b0 && b1 < numBlocks) {
      blocklen = (s1 - block->Item(b1)->start);
      Get(buffer, block->Item(b1)->start, blocklen);
      ((WaveTrack *) * dest)->Append(buffer, blocklen);
   }

   delete[]buffer;
}

void WaveTrack::Paste(double t, VTrack * src)
{
   wxASSERT(src->GetKind() == WaveTrack::Wave);

   envelope.ExpandRegion(t, src->GetMaxLen());

#if wxUSE_THREADS
   wxMutexLocker lock(*blockMutex);
#endif

   sampleCount s = (sampleCount) ((t - tOffset) * rate + 0.5);

   if (s < 0)
      s = 0;
   if (s >= numSamples)
      s = numSamples;

   BlockArray *srcBlock = ((WaveTrack *) src)->GetBlockArray();
   int addedLen = ((WaveTrack *) src)->numSamples;
   int srcNumBlocks = srcBlock->Count();

   if (addedLen == 0 || srcNumBlocks == 0)
      return;

   int b = FindBlock(s);
   int numBlocks = block->Count();

   if (numBlocks == 0) {
      // Special case: this track is currently empty.

      for (int i = 0; i < srcNumBlocks; i++)
         AppendBlock(srcBlock->Item(i));

      envelope.SetTrackLen(numSamples / rate);

      ConsistencyCheck("Paste branch one");

      return;
   }

   if (b >= 0 && b < numBlocks
       && block->Item(b)->len + addedLen < maxSamples) {
      // Special case: we can fit all of the new samples inside of
      // one block!

      sampleType *buffer = new sampleType[maxSamples];

      int splitPoint = s - block->Item(b)->start;
      Read(buffer, block->Item(b), 0, splitPoint);
      ((WaveTrack *) src)->Get(&buffer[splitPoint], 0, addedLen);
      Read(&buffer[splitPoint + addedLen], block->Item(b),
           splitPoint, block->Item(b)->len - splitPoint);

      WaveBlock *largerBlock = new WaveBlock();
      largerBlock->start = block->Item(b)->start;
      largerBlock->len = block->Item(b)->len + addedLen;
      largerBlock->f = dirManager->NewBlockFile();
      bool inited = InitBlock(largerBlock);
      wxASSERT(inited);

      FirstWrite(buffer, largerBlock, largerBlock->len);

      dirManager->Deref(block->Item(b)->f);
      delete block->Item(b);
      block->Item(b) = largerBlock;

      for (int i = b + 1; i < numBlocks; i++)
         block->Item(i)->start += addedLen;

      numSamples += addedLen;

      delete[]buffer;

      envelope.SetTrackLen(numSamples / rate);

      ConsistencyCheck("Paste branch two");

      return;
   }
   // Case two: if we are inserting four or fewer blocks,
   // it's simplest to just lump all the data together
   // into one big block along with the split block,
   // then resplit it all

   int i;

   BlockArray *newBlock = new BlockArray();
   newBlock->Alloc(numBlocks + srcNumBlocks + 2);
   int newNumBlocks = 0;

   for (i = 0; i < b; i++) {
      newBlock->Add(block->Item(i));
      newNumBlocks++;
   }

   WaveBlock *splitBlock = block->Item(b);
   sampleCount splitLen = block->Item(b)->len;
   int splitPoint = s - splitBlock->start;

   if (srcNumBlocks <= 4) {

      sampleCount sum = splitLen + addedLen;

      sampleType *sumBuffer = new sampleType[sum];

      Read(sumBuffer, splitBlock, 0, splitPoint);
      ((WaveTrack *) src)->Get(&sumBuffer[splitPoint], 0, addedLen);
      Read(&sumBuffer[splitPoint + addedLen], splitBlock,
           splitPoint, splitBlock->len - splitPoint);

      BlockArray *split = Blockify(sumBuffer, sum);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += splitBlock->start;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;

      delete[]sumBuffer;
   } else {

      // The final case is that we're inserting at least five blocks.
      // We divide these into three groups: the first two get merged
      // with the first half of the split block, the middle ones get
      // copied in as is, and the last two get merged with the last
      // half of the split block.

      sampleCount srcFirstTwoLen =
          srcBlock->Item(0)->len + srcBlock->Item(1)->len;
      sampleCount leftLen = splitPoint + srcFirstTwoLen;

      sampleType *leftBuffer = new sampleType[leftLen];

      Read(leftBuffer, splitBlock, 0, splitPoint);
      ((WaveTrack *) src)->Get(&leftBuffer[splitPoint], 0, srcFirstTwoLen);

      BlockArray *split = Blockify(leftBuffer, leftLen);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += splitBlock->start;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;
      delete[]leftBuffer;

      for (i = 2; i < srcNumBlocks - 2; i++) {
         WaveBlock *insertBlock = new WaveBlock();
         insertBlock->start = srcBlock->Item(i)->start + s;
         insertBlock->len = srcBlock->Item(i)->len;
         insertBlock->f = srcBlock->Item(i)->f;
         insertBlock->min = srcBlock->Item(i)->min;
         insertBlock->max = srcBlock->Item(i)->max;
         dirManager->Ref(insertBlock->f);

         newBlock->Add(insertBlock);
         newNumBlocks++;
      }

      sampleCount srcLastTwoLen =
          srcBlock->Item(srcNumBlocks - 2)->len +
          srcBlock->Item(srcNumBlocks - 1)->len;
      sampleCount rightSplit = splitBlock->len - splitPoint;
      sampleCount rightLen = rightSplit + srcLastTwoLen;

      sampleType *rightBuffer = new sampleType[rightLen];

      sampleCount lastStart = srcBlock->Item(srcNumBlocks - 2)->start;
      ((WaveTrack *) src)->Get(rightBuffer, lastStart, srcLastTwoLen);
      Read(&rightBuffer[srcLastTwoLen], splitBlock, splitPoint,
           rightSplit);

      sampleCount pos = s + lastStart;

      split = Blockify(rightBuffer, rightLen);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += pos;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;
      delete[]rightBuffer;
   }

   dirManager->Deref(splitBlock->f);
   delete splitBlock;

   // Copy remaining blocks to new block array and
   // swap the new block array in for the old

   for (i = b + 1; i < numBlocks; i++) {
      block->Item(i)->start += addedLen;
      newBlock->Add(block->Item(i));
      newNumBlocks++;
   }

   delete block;
   block = newBlock;

   numSamples += addedLen;

   envelope.SetTrackLen(numSamples / rate);

   ConsistencyCheck("Paste branch three");
}

// old paste

/*

void WaveTrack::Paste(double t, VTrack *src)
{
  wxASSERT(src->GetKind() == WaveTrack::Wave);

  envelope.ExpandRegion(t, src->GetMaxLen());

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  sampleCount s = (sampleCount)((t - tOffset) * rate + 0.5);

  if (s < 0)
    s = 0;
  if (s >= numSamples)
    s = numSamples;

  sampleType *buffer = new sampleType[maxSamples];

  BlockArray *srcblock = ((WaveTrack *)src)->GetBlockArray();
  int addedLen = ((WaveTrack *)src)->numSamples;
  int numBlocks = block->Count();
  int srcNumBlocks = srcblock->Count();
  int b = FindBlock(s);

  if (b >= 0 && b < numBlocks && block->Item(b)->len + addedLen < maxSamples) {
    // Special case: we can fit all of the new samples inside of
    // one block!

    int splitPoint = s - block->Item(b)->start;
    Read(buffer, block->Item(b), 0, splitPoint);
    ((WaveTrack *)src)->Get(&buffer[splitPoint], 0, addedLen);
    Read(&buffer[splitPoint+addedLen], block->Item(b),
		 splitPoint, block->Item(b)->len - splitPoint);

    WaveBlock *largerBlock = new WaveBlock();
    largerBlock->start = block->Item(b)->start;
    largerBlock->len = block->Item(b)->len + addedLen;
    largerBlock->f = dirManager->NewBlockFile();
    bool inited = InitBlock(largerBlock);
	wxASSERT(inited);
    
    FirstWrite(buffer, largerBlock, largerBlock->len);

    dirManager->Deref(block->Item(b)->f);
    block->Item(b) = largerBlock;

    for(int i=b+1; i<numBlocks; i++)
      block->Item(i)->start += addedLen;
    
    numSamples += addedLen;

    delete[] buffer;

    ConsistencyCheck("Paste branch one");
    
    return;
  }

  // Otherwise, we will split block b and insert all of the
  // new blocks in between.

  int i;
  
  BlockArray *newblock = new BlockArray();
  newblock->Alloc(numBlocks + srcNumBlocks + 2);
  int newb=0;

  for(i=0; i<b; i++) {
    newblock->Add(block->Item(i)); newb++;
  }

  WaveBlock *splitLeft = 0;
  WaveBlock *splitRight = 0;

  if (b>=0 && b < numBlocks && s != block->Item(b)->start) {
    // We need to split the block where we insert

    int splitPoint = s - block->Item(b)->start;

    splitLeft = new WaveBlock();
    splitLeft->start = block->Item(b)->start;
    splitLeft->len = splitPoint;
    splitLeft->f = dirManager->NewBlockFile();
	bool initedLeft = InitBlock(splitLeft);
	wxASSERT(initedLeft);

    Read(buffer, block->Item(b), 0, splitPoint);
    FirstWrite(buffer, splitLeft, splitPoint);

    splitRight = new WaveBlock();
    splitRight->start = s + addedLen;
    splitRight->len = block->Item(b)->len - splitPoint;
    splitRight->f = dirManager->NewBlockFile();
	bool initedRight = InitBlock(splitRight);
	wxASSERT(initedRight);

    Read(buffer, block->Item(b), splitPoint, splitRight->len);
    FirstWrite(buffer, splitRight, splitRight->len);

    dirManager->Deref(block->Item(b)->f);

    newblock->Add(splitLeft); newb++;

    b++;
  }

  // Insert the new blocks

  for(int bi=0; bi<srcNumBlocks; bi++) {
    WaveBlock *insertBlock = new WaveBlock();
    insertBlock->start = srcblock->Item(bi)->start + s;
    insertBlock->len = srcblock->Item(bi)->len;
    insertBlock->f = srcblock->Item(bi)->f;
    dirManager->Ref(insertBlock->f);

    newblock->Add(insertBlock); newb++;
    numSamples += insertBlock->len;
  }

  if (splitRight)
    newblock->Add(splitRight); newb++;

  // Copy remaining blocks to new block array and
  // swap the new block array in for the old

  if (b >= 0) {
    for(i=b; i<numBlocks; i++) {
      block->Item(i)->start += addedLen;
      newblock->Add(block->Item(i)); newb++;
    }
  }

  delete block;
  block = newblock;

  delete[] buffer;

  ConsistencyCheck("Paste branch two");
}
// end old paste

*/

void WaveTrack::Clear(double t0, double t1)
{
   wxASSERT(t0 <= t1);

   envelope.CollapseRegion(t0, t1);

   sampleCount s0 = (sampleCount) ((t0 - tOffset) * rate + 0.5);
   sampleCount s1 = (sampleCount) ((t1 - tOffset) * rate + 0.5);

   if (s0 < 0)
      s0 = 0;
   if (s1 >= numSamples)
      s1 = numSamples;

   if (s0 >= s1 || s0 >= numSamples || s1 < 0)
      return;

   Delete(s0, (s1 - s0));
}

void WaveTrack::Silence(double t0, double t1)
{
   wxASSERT(t0 <= t1);

   sampleCount s0 = (sampleCount) ((t0 - tOffset) * rate + 0.5);
   sampleCount s1 = (sampleCount) ((t1 - tOffset) * rate + 0.5);

   if (s0 < 0)
      s0 = 0;
   if (s1 >= numSamples)
      s1 = numSamples;

   if (s0 >= s1 || s0 >= numSamples || s1 < 0)
      return;

   Set(NULL, s0, s1 - s0);
}

void WaveTrack::InsertSilence(double t, double lenSecs)
{
   // Create a new track containing as much silence as we
   // need to insert, and then call Paste to do the insertion

   sampleCount len = (sampleCount) (lenSecs * rate + 0.5);

   WaveTrack *sTrack = new WaveTrack(dirManager);
   sTrack->rate = rate;

   sampleCount idealSamples = GetIdealBlockSize();
   sampleType *buffer = new sampleType[idealSamples];
   sampleCount i;
   for (i = 0; i < idealSamples; i++)
      buffer[i] = 0;

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
         w->f = dirManager->NewBlockFile();
         firstBlockFile = w->f;
         bool inited = InitBlock(w);
         wxASSERT(inited);
         FirstWrite(buffer, w, l);
      } else {
         w->f = firstBlockFile;
         dirManager->Ref(w->f);
      }

      sTrack->block->Add(w);

      pos += l;
      len -= l;
   }

   sTrack->numSamples = pos;

   Paste(t, sTrack);

   delete sTrack;

   ConsistencyCheck("InsertSilence");
}

void WaveTrack::AppendAlias(wxString fullPath,
                            sampleCount start,
                            sampleCount len, int channel)
{
   WaveBlock *newBlock = new WaveBlock();
   newBlock->start = numSamples;
   newBlock->len = len;
   newBlock->f =
       dirManager->NewAliasBlockFile(totalHeaderLen,
                                     fullPath, start, len, channel);

   InitBlock(newBlock);

   BlockFile *f = newBlock->f;

   sampleType *buffer = new sampleType[len];
   Read(buffer, newBlock, 0, len);

   wxASSERT(f);
   bool opened = f->OpenWriting();
   wxASSERT(opened);
   UpdateSummaries(buffer, newBlock, len);
   f->Close();

   delete[]buffer;

   block->Add(newBlock);
   numSamples += newBlock->len;

   envelope.SetTrackLen(numSamples / rate);
}

void WaveTrack::AppendBlock(WaveBlock * b)
{
   WaveBlock *newBlock = new WaveBlock();
   newBlock->start = numSamples;
   newBlock->len = b->len;
   newBlock->f = b->f;
   newBlock->min = b->min;
   newBlock->max = b->max;
   dirManager->Ref(newBlock->f);
   block->Add(newBlock);
   numSamples += newBlock->len;

   envelope.SetTrackLen(numSamples / rate);

   // Don't do a consistency check here because this
   // function gets called in an inner loop
}

sampleCount WaveTrack::GetBestBlockSize(sampleCount start)
{
   // This method returns a nice number of samples you should try to grab in
   // one big chunk in order to land on a block boundary, based on the starting
   // sample.  The value returned will always be nonzero and will be no larger
   // than the value of GetMaxBlockSize();

   int b = FindBlock(start);
   int numBlocks = block->Count();
   
   sampleCount result = (block->Item(b)->start + block->Item(b)->len - start);
   
   while(result < minSamples && b+1<numBlocks &&
         (block->Item(b)->len+result) <= maxSamples) {
      b++;
      result += block->Item(b)->len;
   }
   
   wxASSERT(result > 0 && result <= maxSamples);
   
   return result;
}

bool WaveTrack::Load(wxTextFile * in, DirManager * dirManager)
{
   bool result = VTrack::Load(in, dirManager);

   envelope.SetOffset(tOffset);

   if (result) {
      result = envelope.Load(in, dirManager);
   }

   if (!result) {
      wxMessageBox("Error loading a Track.\n");
      return false;
   }
#if wxUSE_THREADS
   blockMutex->Lock();
#endif

   int b;
   long longNumSamples;
   long numBlocks;
   long longBlockStart;
   long longBlockLen;
   WaveBlock *w;

   if (in->GetNextLine() != "numSamples")
      goto readWaveTrackError;
   if (!(in->GetNextLine().ToLong(&longNumSamples)))
      goto readWaveTrackError;
   numSamples = longNumSamples;

   if (in->GetNextLine() != "rate")
      goto readWaveTrackError;
   if (!(in->GetNextLine().ToDouble(&rate)))
      goto readWaveTrackError;

   if (in->GetNextLine() != "numBlocks")
      goto readWaveTrackError;
   if (!(in->GetNextLine().ToLong(&numBlocks)))
      goto readWaveTrackError;

   block->Alloc(numBlocks);

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

      w->f = dirManager->LoadBlockFile(in);

      if (!w->f) {

         numSamples = 0;
         block->Clear();

         blockMutex->Unlock();

         wxString msg;
         msg.Printf("The file named \"%s\" is missing from the project.",
                    (const char *) in->GetCurrentLine());
         wxMessageBox(msg);

         return false;
      }

      block->Add(w);
   }

   blockMutex->Unlock();

   return true;

 readWaveTrackError:
   wxMessageBox(wxString::Format("Error reading WaveTrack in line %d",
                                 in->GetCurrentLine()));
   return false;
}

bool WaveTrack::Save(wxTextFile * out, bool overwrite)
{
   VTrack::Save(out, overwrite);

   envelope.Save(out, overwrite);

   int i, b;

   out->AddLine("numSamples");
   out->AddLine(wxString::Format("%d", numSamples));

   out->AddLine("rate");
   out->AddLine(wxString::Format("%g", rate));

   out->AddLine("numBlocks");
   out->AddLine(wxString::Format("%d", block->Count()));

   WaveBlock *bb;

   for (b = 0; b < block->Count(); b++) {
      bb = block->Item(b);

      dirManager->MakePartOfProject(bb->f);

      out->AddLine("Block start");
      out->AddLine(wxString::Format("%d", bb->start));
      out->AddLine("Block len");
      out->AddLine(wxString::Format("%d", bb->len));
      out->AddLine("Block info");
      dirManager->SaveBlockFile(bb->f, out);
   }

   return true;
}

sampleType WaveTrack::Get(sampleCount pos)
{
   sampleType temp[1];
   Get(temp, pos, 1);
   return temp[0];
}

WaveBlock *WaveTrack::NewInitedWaveBlock()
{
   WaveBlock *b = new WaveBlock();
   b->f = dirManager->NewBlockFile();
   bool inited = InitBlock(b);
   wxASSERT(inited);
   return b;
}

bool WaveTrack::InitBlock(WaveBlock * b)
{
   wxASSERT(b);

   BlockFile *f = b->f;

   if (!f->OpenWriting())
      return false;

   f->Write(headerTag, headerTagLen);

   /*
    * This code shouldn't be needed because UpdateSummaries
    * always writes exactly what's needed.

    sampleCount slen = summary64KLen + summary256Len;
    sampleType *tempSamples = new sampleType[slen];
    for(int i=0; i<slen; i++)
    tempSamples[i] = 0;
    f->Write((void *)tempSamples, sizeof(sampleType) * slen);
    delete[] tempSamples;
    */

   f->Close();

   return true;
}

int WaveTrack::FindBlock(sampleCount pos, sampleCount lo,
                         sampleCount guess, sampleCount hi)
{
   wxASSERT(block->Item(guess)->len > 0);
   wxASSERT(lo <= guess && guess <= hi && lo <= hi);

   if (pos >= block->Item(guess)->start &&
       pos < block->Item(guess)->start + block->Item(guess)->len)
      return guess;

   if (pos < block->Item(guess)->start)
      return FindBlock(pos, lo, (lo + guess) / 2, guess);
   else
      return FindBlock(pos, guess + 1, (guess + 1 + hi) / 2, hi);
}

int WaveTrack::FindBlock(sampleCount pos)
{
   wxASSERT(pos >= 0 && pos <= numSamples);

   int numBlocks = block->Count();

   if (pos == 0)
      return 0;

   if (pos == numSamples)
      return (numBlocks - 1);

   int rval = FindBlock(pos, 0, numBlocks / 2, numBlocks);

   wxASSERT(rval >= 0 && rval < numBlocks &&
            pos >= block->Item(rval)->start &&
            pos < block->Item(rval)->start + block->Item(rval)->len);

   return rval;
}

void WaveTrack::Read(sampleType * buffer, WaveBlock * b,
                     sampleCount start, sampleCount len)
{
   wxASSERT(b);
   wxASSERT(start >= 0);
   wxASSERT(start + len <= b->len);

   BlockFile *f = b->f;
   bool opened = f->OpenReadData();
   wxASSERT(opened);

   f->SeekTo(totalHeaderLen + (start * sizeof(sampleType)));

   int result = f->Read((void *) buffer, (int) (len * sizeof(sampleType)));

   if (result != (int) (len * sizeof(sampleType))) {
      printf("Expected to read %d bytes, got %d bytes.\n",
             len * sizeof(sampleType), result);
   }

   wxASSERT(result == (int) (len * sizeof(sampleType)));

   f->Close();
}

void WaveTrack::Read256(sampleType * buffer, WaveBlock * b,
                        sampleCount start, sampleCount len)
{
   wxASSERT(b);
   wxASSERT(start >= 0);
   wxASSERT(start + len <= ((b->len + 255) / 256));
   start *= 2;
   len *= 2;

   BlockFile *f = b->f;
   bool opened = f->OpenReadHeader();
   wxASSERT(opened);

   f->SeekTo(headerTagLen + summary64KLen * sizeof(sampleType)
             + start * sizeof(sampleType));

   int result = f->Read((void *) buffer, (int) (len * sizeof(sampleType)));
   wxASSERT(result == (int) (len * sizeof(sampleType)));

   f->Close();
}

void WaveTrack::Read64K(sampleType * buffer, WaveBlock * b,
                        sampleCount start, sampleCount len)
{
   wxASSERT(b);
   wxASSERT(start >= 0);
   wxASSERT(start + len <= ((b->len + 65535) / 65536));
   start *= 2;
   len *= 2;

   BlockFile *f = b->f;
   bool opened = f->OpenReadHeader();
   wxASSERT(opened);

   f->SeekTo(headerTagLen + start * sizeof(sampleType));

   int result = f->Read((void *) buffer, (int) (len * sizeof(sampleType)));
   wxASSERT(result == (int) (len * sizeof(sampleType)));

   f->Close();
}

void WaveTrack::CopyWrite(sampleType * buffer, WaveBlock * b,
                          sampleCount start, sampleCount len)
{
   // Usually we don't write to an existing block; to support Undo,
   // we copy the old block entirely into memory, dereference it,
   // make the change, and then write the new block to disk.

   wxASSERT(b);
   wxASSERT(b->len <= maxSamples);
   wxASSERT(start + len <= b->len);

   dirty++;                     // forces redraw

   sampleType *newBuffer = 0;

   newBuffer = new sampleType[maxSamples];
   wxASSERT(newBuffer);

   Read(newBuffer, b, 0, b->len);

   for (int i = 0; i < len; i++)
      newBuffer[start + i] = buffer[i];

   BlockFile *oldBlockFile = b->f;
   b->f = dirManager->NewBlockFile();
   bool inited = InitBlock(b);
   wxASSERT(inited);

   buffer = newBuffer;
   start = 0;
   len = b->len;

   dirManager->Deref(oldBlockFile);

   // Write the block

   BlockFile *f = b->f;

   wxASSERT(f);
   bool opened = f->OpenWriting();
   wxASSERT(opened);

   f->SeekTo(totalHeaderLen + (start * sizeof(sampleType)));

   f->Write((void *) buffer, len * sizeof(sampleType));

   UpdateSummaries(buffer, b, len);

   if (newBuffer)
      delete[]newBuffer;

   f->Close();
}

void WaveTrack::FirstWrite(sampleType * buffer, WaveBlock * b,
                           sampleCount len)
{
   wxASSERT(b);
   wxASSERT(b->len <= maxSamples);

   dirty++;                     // forces redraw

   BlockFile *f = b->f;

   wxASSERT(f);
   bool opened = f->OpenWriting();
   wxASSERT(opened);

   f->SeekTo(totalHeaderLen);
   f->Write((void *) buffer, len * sizeof(sampleType));

   UpdateSummaries(buffer, b, len);

   f->Close();
}

void WaveTrack::UpdateSummaries(sampleType * buffer,
                                WaveBlock * b, sampleCount len)
{
   // This method assumes that b->f is already opened

   BlockFile *f = b->f;

   sampleType *summary64K = new sampleType[summary64KLen];
   sampleType *summary256 = new sampleType[summary256Len];

   sampleCount sumLen;
   sampleCount i, j, jcount;

   sampleType min, max;

   // Recalc 256 summaries

   sumLen = (len + 255) / 256;

   for (i = 0; i < sumLen; i++) {
      min = buffer[i * 256];
      max = buffer[i * 256];
      jcount = 256;
      if (i * 256 + jcount > len)
         jcount = len - i * 256;
      for (j = 1; j < jcount; j++) {
         if (buffer[i * 256 + j] < min)
            min = buffer[i * 256 + j];
         else if (buffer[i * 256 + j] > max)
            max = buffer[i * 256 + j];
      }
      summary256[i * 2] = min;
      summary256[i * 2 + 1] = max;
   }
   for (i = sumLen; i < summary256Len / 2; i++) {
      summary256[i * 2] = 0;
      summary256[i * 2 + 1] = 0;
   }

   // Recalc 64K summaries

   sumLen = (len + 65535) / 65536;

   for (i = 0; i < sumLen; i++) {
      min = summary256[i * 256];
      max = summary256[i * 256];
      if (i * 256 + jcount > ((len + 255) / 256))
         jcount = ((len + 255) / 256) - i * 256;

      for (j = 1; j < 256; j++) {
         if (summary256[i * 256 + j] < min)
            min = summary256[i * 256 + j];
         else if (summary256[i * 256 + j] > max)
            max = summary256[i * 256 + j];
      }
      summary64K[i * 2] = min;
      summary64K[i * 2 + 1] = max;
   }
   for (i = sumLen; i < summary64KLen / 2; i++) {
      summary256[i * 2] = 0;
      summary256[i * 2 + 1] = 0;
   }

   // Recalc block-level summary

   min = summary64K[0];
   max = summary64K[0];
   for (i = 1; i < sumLen; i++) {
      if (summary64K[i] < min)
         min = summary64K[i];
      else if (summary64K[i] > max)
         max = summary64K[i];
   }
   b->min = min;
   b->max = max;

   // Write 256 and 64K summaries to disk
   f->SeekTo(headerTagLen);
   f->Write((void *) summary64K, summary64KLen * sizeof(sampleType));
   f->Write((void *) summary256, summary256Len * sizeof(sampleType));

   delete[]summary64K;
   delete[]summary256;
}

void WaveTrack::Get(sampleType * buffer, sampleCount start,
                    sampleCount len)
{
   wxASSERT(start < numSamples && start + len <= numSamples);
   int b = FindBlock(start);

   while (len) {
      sampleCount blen =
          block->Item(b)->start + block->Item(b)->len - start;
      if (blen > len)
         blen = len;
      sampleCount bstart = (start - (block->Item(b)->start));
      Read(buffer, block->Item(b), bstart, blen);
      len -= blen;
      buffer += blen;
      b++;
      start += blen;
   }
}

// Pass NULL to set silence
void WaveTrack::Set(sampleType * buffer, sampleCount start,
                    sampleCount len)
{
   wxASSERT(start < numSamples && start + len <= numSamples);

#if wxUSE_THREADS
   wxMutexLocker lock(*blockMutex);
#endif

   int b = FindBlock(start);

   sampleType *silence;
   if (!buffer) {
      silence = new sampleType[maxSamples];
      for (int i = 0; i < maxSamples; i++)
         silence[i] = 0;
   }

   while (len) {
      int blen = block->Item(b)->start + block->Item(b)->len - start;
      if (blen > len)
         blen = len;

      if (buffer) {
         CopyWrite(buffer, block->Item(b), start - block->Item(b)->start,
                   blen);
         buffer += blen;
      } else
         CopyWrite(silence, block->Item(b), start - block->Item(b)->start,
                   blen);

      len -= blen;
      start += blen;
      b++;
   }

   if (!buffer)
      delete[]silence;

   ConsistencyCheck("Set");
}

void WaveTrack::Append(sampleType * buffer, sampleCount len)
{
#if wxUSE_THREADS
   wxMutexLocker lock(*blockMutex);
#endif

   // If the last block is not full, we need to add samples to it
   int numBlocks = block->Count();
   if (numBlocks > 0 && block->Item(numBlocks - 1)->len < minSamples) {
      WaveBlock *lastBlock = block->Item(numBlocks - 1);
      sampleCount addLen;
      if (lastBlock->len + len < maxSamples)
         addLen = len;
      else
         addLen = GetIdealBlockSize() - lastBlock->len;
      sampleCount pos = lastBlock->len;

      WaveBlock *newLastBlock = NewInitedWaveBlock();

      sampleType *buffer2 = new sampleType[lastBlock->len + addLen];
      Read(buffer2, lastBlock, 0, lastBlock->len);

      for (int j = 0; j < addLen; j++)
         buffer2[lastBlock->len + j] = buffer[j];

      newLastBlock->start = lastBlock->start;
      newLastBlock->len = lastBlock->len + addLen;

      FirstWrite(buffer2, newLastBlock, lastBlock->len + addLen);

      delete[]buffer2;

      dirManager->Deref(lastBlock->f);
      delete lastBlock;
      block->Item(numBlocks - 1) = newLastBlock;

      len -= addLen;
      numSamples += addLen;
      buffer += addLen;
   }
   // Append the rest as new blocks
   while (len) {
      sampleCount idealSamples = GetIdealBlockSize();
      sampleCount l = (len > idealSamples ? idealSamples : len);
      WaveBlock *w = new WaveBlock();
      w->f = dirManager->NewBlockFile();
      w->start = numSamples;
      w->len = l;
      bool inited = InitBlock(w);
      wxASSERT(inited);
      FirstWrite(buffer, w, l);
      block->Add(w);

      buffer += l;
      numSamples += l;
      len -= l;
   }

   envelope.SetTrackLen(numSamples / rate);

   ConsistencyCheck("Append");
}

BlockArray *WaveTrack::Blockify(sampleType * buffer, sampleCount len)
{
   BlockArray *list = new BlockArray();
   list->Alloc(10);

   if (len == 0)
      return list;

   int num = (len + (maxSamples - 1)) / maxSamples;

   for (int i = 0; i < num; i++) {
      WaveBlock *b = NewInitedWaveBlock();

      b->start = i * len / num;
      b->len = ((i + 1) * len / num) - b->start;

      FirstWrite(&buffer[b->start], b, b->len);

      list->Add(b);
   }

   return list;
}

void WaveTrack::Delete(sampleCount start, sampleCount len)
{
   if (len == 0)
      return;

   int numBlocks = block->Count();
   int newNumBlocks = 0;

#if wxUSE_THREADS
   wxMutexLocker lock(*blockMutex);
#endif

   int b0 = FindBlock(start);
   int b1 = FindBlock(start + len - 1);

   // Special case: if the samples to delete are all within a single
   // block and the resulting length is not too small, perform the
   // deletion within this block:

   if (b0 == b1 && block->Item(b0)->len - len >= minSamples) {
      WaveBlock *b = block->Item(b0);
      sampleCount pos = start - b->start;
      sampleCount newLen = b->len - len;
      sampleType *buffer = new sampleType[newLen];

      Read(buffer, b, 0, pos);
      Read(&buffer[pos], b, pos + len, newLen - pos);

      WaveBlock *newBlock = NewInitedWaveBlock();
      newBlock->start = b->start;
      newBlock->len = newLen;
      FirstWrite(buffer, newBlock, newLen);

      block->Item(b0) = newBlock;

      for (int j = b0 + 1; j < numBlocks; j++)
         block->Item(j)->start -= len;

      delete[]buffer;

      dirManager->Deref(b->f);
      delete b;

      numSamples -= len;
      envelope.SetTrackLen(numSamples / rate);
      ConsistencyCheck("Delete - branch one");

      return;
   }
   // Create a new array of blocks

   BlockArray *newBlock = new BlockArray();
   newBlock->Alloc(numBlocks - (b1 - b0) + 2);

   // Copy the blocks before the deletion point over to
   // the new array

   int i;
   for (i = 0; i < b0; i++) {
      newBlock->Add(block->Item(i));
      newNumBlocks++;
   }

   // First grab the samples in block b0 before the deletion point
   // into preBuffer.  If this is enough samples for its own block,
   // or if this would be the first block in the array, write it out.
   // Otherwise combine it with the previous block (splitting them
   // 50/50 if necessary).

   WaveBlock *preBlock = block->Item(b0);
   sampleCount preBufferLen = start - preBlock->start;
   if (preBufferLen) {
      if (preBufferLen >= minSamples || b0 == 0) {
         WaveBlock *insBlock = NewInitedWaveBlock();

         insBlock->len = preBufferLen;
         insBlock->start = preBlock->start;

         sampleType *preBuffer = new sampleType[preBufferLen];
         Read(preBuffer, preBlock, 0, preBufferLen);
         FirstWrite(preBuffer, insBlock, preBufferLen);
         delete[]preBuffer;

         newBlock->Add(insBlock);
         newNumBlocks++;

         if (b0 != b1) {
            dirManager->Deref(preBlock->f);
            delete preBlock;
         }
      } else {
         WaveBlock *prepreBlock = block->Item(b0 - 1);
         sampleCount prepreLen = prepreBlock->len;
         sampleCount sum = prepreLen + preBufferLen;

         sampleType *sumBuffer = new sampleType[sum];
         Read(sumBuffer, prepreBlock, 0, prepreLen);
         Read(&sumBuffer[prepreLen], preBlock, 0, preBufferLen);

         BlockArray *split = Blockify(sumBuffer, sum);
         split->Item(0)->start += prepreBlock->start;
         newBlock->Item(b0 - 1) = split->Item(0);
         for (i = 1; i < split->Count(); i++) {
            split->Item(i)->start += prepreBlock->start;
            newBlock->Add(split->Item(i));
            newNumBlocks++;
         }
         delete split;

         delete[]sumBuffer;

         dirManager->Deref(prepreBlock->f);
         delete prepreBlock;

         if (b0 != b1) {
            dirManager->Deref(preBlock->f);
            delete preBlock;
         }
      }
   } else {
      // The sample where we begin deletion happens to fall
      // right on the beginning of a block.

      if (b0 != b1) {
         dirManager->Deref(block->Item(b0)->f);
         delete block->Item(b0);
      }
   }

   // Next, delete blocks strictly between b0 and b1

   for (i = b0 + 1; i < b1; i++) {
      dirManager->Deref(block->Item(i)->f);
      delete block->Item(i);
   }

   // Now, symmetrically, grab the samples in block b1 after the
   // deletion point into postBuffer.  If this is enough samples
   // for its own block, or if this would be the last block in
   // the array, write it out.  Otherwise combine it with the
   // subsequent block (splitting them 50/50 if necessary).

   WaveBlock *postBlock = block->Item(b1);
   sampleCount postBufferLen =
       (postBlock->start + postBlock->len) - (start + len);
   if (postBufferLen) {
      if (postBufferLen >= minSamples || b1 == numBlocks - 1) {
         WaveBlock *insBlock = NewInitedWaveBlock();

         insBlock->len = postBufferLen;
         insBlock->start = start;

         sampleType *postBuffer = new sampleType[postBufferLen];
         sampleCount pos = (start + len) - postBlock->start;
         Read(postBuffer, postBlock, pos, postBufferLen);
         FirstWrite(postBuffer, insBlock, postBufferLen);
         delete[]postBuffer;

         newBlock->Add(insBlock);
         newNumBlocks++;

         dirManager->Deref(postBlock->f);
         delete postBlock;
      } else {
         WaveBlock *postpostBlock = block->Item(b1 + 1);
         sampleCount postpostLen = postpostBlock->len;
         sampleCount sum = postpostLen + postBufferLen;

         sampleType *sumBuffer = new sampleType[sum];
         sampleCount pos = (start + len) - postBlock->start;
         Read(sumBuffer, postBlock, pos, postBufferLen);
         Read(&sumBuffer[postBufferLen], postpostBlock, 0, postpostLen);

         BlockArray *split = Blockify(sumBuffer, sum);
         for (i = 0; i < split->Count(); i++) {
            split->Item(i)->start += start;
            newBlock->Add(split->Item(i));
            newNumBlocks++;
         }
         delete split;
         b1++;

         delete[]sumBuffer;

         dirManager->Deref(postpostBlock->f);
         delete postpostBlock;
         dirManager->Deref(postBlock->f);
         delete postBlock;
      }
   } else {
      // The sample where we begin deletion happens to fall
      // right on the end of a block.

      if (b0 != b1) {
         dirManager->Deref(block->Item(b1)->f);
         delete block->Item(b1);
      }
   }

   // Copy the remaining blocks over from the old array

   for (i = b1 + 1; i < numBlocks; i++) {
      block->Item(i)->start -= len;
      newBlock->Add(block->Item(i));
      newNumBlocks++;
   }

   // Substitute our new array for the old one

   delete block;
   block = newBlock;

   // Update total number of samples, update the envelope,
   // and do a consistency check.

   numSamples -= len;
   envelope.SetTrackLen(numSamples / rate);
   ConsistencyCheck("Delete - branch two");
}

void WaveTrack::Reblockify()
{
   // TODO
}

void WaveTrack::ConsistencyCheck(const char *whereStr)
{
   int i;
   int pos = 0;
   int numBlocks = block->Count();
   bool error = false;

   for (i = 0; i < numBlocks; i++) {
      if (pos != block->Item(i)->start)
         error = true;
      pos += block->Item(i)->len;
   }
   if (pos != numSamples)
      error = true;

   if (error) {
#ifdef __WXDEBUG__
      printf("*** Consistency check failed after %s ***\n", whereStr);
      Debug();
      wxASSERT(0);
#else
      wxMessageBox("Internal error (WaveTrack)");
      exit(0);
#endif
   }
}

void WaveTrack::Debug()
{
   int i;
   int pos = 0;

   for (i = 0; i < block->Count(); i++) {
      printf("Block %3d: start %8d len %8d  %s",
             i,
             block->Item(i)->start,
             block->Item(i)->len,
             (const char *) (block->Item(i)->f->GetName()));
      if (pos != block->Item(i)->start)
         printf("  ERROR\n");
      else
         printf("\n");
      pos += block->Item(i)->len;
   }
   if (pos != numSamples)
      printf("ERROR numSamples = %d\n", numSamples);
}

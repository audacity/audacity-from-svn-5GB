/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/colour.h>
#include <wx/dc.h>
#include <wx/file.h>
#include <wx/image.h>
#include <wx/msgdlg.h>
#include <wx/textfile.h>

#include "Spectrum.h"
#include "WaveTrack.h"
#include "DirManager.h"

#if wxUSE_APPLE_IEEE
extern "C" void ConvertToIeeeExtended(double num, unsigned char *bytes);
extern "C" double ConvertFromIeeeExtended(const unsigned char *bytes);
#else
#error Requires Apple IEEE Extended conversion routines
#endif

// Max file size of 16-bit samples is 1MB
// (About 12 seconds of audio)

const int headerTagLen = 8;

const sampleCount summary64KLen = 8*2;
const sampleCount summary256Len = summary64KLen * 256;
const sampleCount totalHeaderLen = headerTagLen +
(summary64KLen + summary256Len)*sizeof(sampleType);

const sampleCount maxSamples = summary256Len * 256 / 2;
const sampleCount minSamples = maxSamples / 2;

/*
const sampleCount summary64KLen = 0;
const sampleCount summary256Len = summary64KLen * 256;
const sampleCount totalHeaderLen = headerTagLen +
          (summary64KLen + summary256Len)*sizeof(sampleType);

const sampleCount maxSamples = 16;
const sampleCount minSamples = maxSamples / 2;
*/
// Static methods


sampleCount WaveTrack::GetIdealBlockSize()
{
  return minSamples;
}

// WaveTrack methods

WaveTrack::WaveTrack(DirManager *projDirManager):
  VTrack(projDirManager)
{
  numSamples = 0;
  rate = 44100.0;

  display = 0;
  
  block = new BlockArray();

  #if wxUSE_THREADS
  blockMutex = new wxMutex();
  #endif

  blankBrush.SetColour(214,214,214);
  unselectedBrush.SetColour(192,192,192);
  selectedBrush.SetColour(148,148,170);
  sampleBrush.SetColour(50,50,200);
  selsampleBrush.SetColour(50,50,200);
  blankPen.SetColour(214,214,214);
  unselectedPen.SetColour(192,192,192);
  selectedPen.SetColour(148,148,170);
  samplePen.SetColour(50,50,200);
  selsamplePen.SetColour(50,50,200);
  shadowPen.SetColour(148, 148, 148);
  envelopePen.SetColour(0, 220, 0);

  cache.dirty = true;
  cache.len = 0;
  cache.start = 0.0;
  cache.pps = 1.0;
  cache.min = NULL;
  cache.max = NULL;
  cache.where = NULL;
  cache.freq = NULL;
}

WaveTrack::~WaveTrack()
{
  for(int i=0; i<block->Count(); i++)
    dirManager->Deref(block->Item(i)->f);

  delete block;

  #if wxUSE_THREADS
  delete blockMutex;
  #endif
}

double WaveTrack::GetMaxLen()
{
  return ((double)numSamples)/(rate) + tOffset;
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
  copy->Paste(0.0, this);
  copy->collapsedHeight = collapsedHeight;
  copy->expandedHeight = expandedHeight;
  copy->selected = selected;
  copy->envelope.CopyFrom(&envelope);

  return (VTrack *)copy;
}

void WaveTrack::PrepareCache(double start, double pps, int screenWidth)
{
  wxASSERT(start>=0.0);
  wxASSERT(pps>0.0);
  wxASSERT(screenWidth>0);

  if (!cache.dirty &&
      pps == cache.pps &&
      start == cache.start &&
      screenWidth <= cache.len)
    return;

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  cache.pps = pps;
  cache.start = start;
  cache.len = screenWidth;
  cache.min = new sampleType[cache.len];
  wxASSERT(cache.min);
  cache.max = new sampleType[cache.len];
  wxASSERT(cache.max);
  cache.where = new sampleCount[cache.len+1];
  wxASSERT(cache.where);
  
  sampleCount x;

  for(x=0; x<cache.len+1; x++)
	cache.where[x] = (sampleCount)(start*rate + x*rate/pps);
  
  int divisor;
  if (rate/pps >= 65536) // samp/sec / pixels/sec = samp / pixel
    divisor = 65536;
  else if (rate/pps >= 256)
    divisor = 256;
  else
    divisor = 1;
  
  sampleCount s0 = (sampleCount)(start * rate);
  sampleCount s1 = (sampleCount)((start + screenWidth/pps)*rate);
  if (s1 > numSamples)
    s1 = numSamples;
    
  sampleCount srcX = s0;

  int block0 = FindBlock(s0);

  sampleType *temp = new sampleType[maxSamples];
  
  sampleCount dstX = 0;
  int pixel = 0;
  
  sampleType theMin;
  sampleType theMax;
  int b = block0;

  while(srcX < s1) {
    // Get more samples
    
    sampleCount num;

    num = ((block->Item(b)->len - (srcX - block->Item(b)->start)) + divisor-1) / divisor;

    if (num > (s1 - srcX + divisor-1)/divisor) {
      num = (s1 - srcX+divisor-1)/divisor;
	}

    switch(divisor) {
    case 1:
      Read(temp, block->Item(b), srcX - block->Item(b)->start, num);
      break;
    case 256:
      Read256(temp, block->Item(b), (srcX - block->Item(b)->start)/divisor, num);
      break;
    case 65536:
      Read64K(temp, block->Item(b), (srcX - block->Item(b)->start)/divisor, num);
      break;
    default:
      wxASSERT(0);
      break;
    }
    
    // Get min/max of samples for each pixel we can
    
    x = 0;

    while(x < num) {

      while (pixel < screenWidth &&
			 cache.where[pixel]/divisor == srcX/divisor + x) {
        if (pixel>0) {
          cache.min[pixel-1] = theMin;
          cache.max[pixel-1] = theMax;
        }
        pixel++;
        if (cache.where[pixel] != cache.where[pixel-1]) {
		  theMin = 32767;
		  theMax = -32768;
		}
      }

      sampleCount stop = (cache.where[pixel] - srcX) / divisor;
      if (stop == x)
        stop++;
      if (stop > num)
        stop = num;

      switch(divisor) {
	  case 1:
		while(x < stop) {
		  if (temp[x] < theMin)
			theMin = temp[x];
		  if (temp[x] > theMax)
			theMax = temp[x];
		  x++;
		}
		break;
	  case 256:
	  case 65536:
		// In summaries, mins and maxes are stored in alternating
		// samples
		while(x < stop) {
		  if (temp[2*x] < theMin)
			theMin = temp[2*x];
		  if (temp[2*x+1] > theMax)
			theMax = temp[2*x+1];
		  x++;
		}

		break;
	  default:
		wxASSERT(0);
		break;
	  }
    }

    b++;

	srcX += num*divisor;

    if (b >= block->Count())
      break;

    srcX = block->Item(b)->start;
    
  }

  do {
	cache.min[pixel-1] = theMin;
	cache.max[pixel-1] = theMax;
	pixel++;
  } while (pixel < cache.len);

  cache.dirty = false;

  delete[] temp;
}

void WaveTrack::DrawMinmax(wxDC &dc, wxRect &r, double h, double pps,
						   double sel0, double sel1,
						   bool drawEnvelope)
{
  double tpre = h - tOffset;
  double tstep = 1.0/pps;
  double tpost = tpre+(r.width*tstep);
  
  double t0 = (tpre >= 0.0? tpre: 0.0);
  double t1 = (tpost < (numSamples / rate)? tpost : (numSamples / rate));
  
  if (t1 < 0.0) {
    t1 = 0.0;
  }
  
  if (t0 > t1)
    t0 = t1;
  
  sampleCount s0 = (sampleCount)(t0 * rate);
  sampleCount s1 = (sampleCount)(t1 * rate);

  sampleCount slen = (sampleCount)(s1-s0);

  int ssel0 = (int)((sel0 - tOffset) * rate);
  int ssel1 = (int)((sel1 - tOffset) * rate);

  if (sel1 < tOffset) {
	ssel0 = 0;
	ssel1 = 0;
  }

  if (ssel0 != ssel1) {
    if (ssel0 < 0)
      ssel0 = 0;
    if (ssel1 > numSamples)
      ssel1 = numSamples;
  }

  dc.SetBrush(blankBrush);
  dc.SetPen(blankPen);
  
  wxRect mid = r;

  if (t0 > tpre) {
    wxRect pre = r;
    pre.width = (int)((t0 - tpre)*pps);
    mid.x += pre.width;
    mid.width -= pre.width;
    dc.DrawRectangle(pre);	    
  }
  
  if (tpost > t1) {
    wxRect post = r;
    post.x += (int)((t1-tpre)*pps);
    post.width = r.width - (post.x - r.x);
    mid.width -= post.width;
	if (post.x < r.x) {
	  post.width -= (r.x - post.x);
	  post.x = r.x;
	}
	if (post.width > 0)
	  dc.DrawRectangle(post);
  }
  
  mid.height -= 2;
  int ctr = r.y + (r.height/2);
  
  if (mid.width > 0) {
    dc.SetPen(*wxRED_PEN);	    
    dc.DrawLine(mid.x, ctr, mid.x + mid.width, ctr);

    PrepareCache(t0, pps, mid.width);
  }

  int *heights = new int[mid.width];
  double t = t0;
  int x;
  for(x=0; x<mid.width; x++) {
	heights[x] = int((mid.height/2.0) *
					 envelope.GetValue(t+tOffset));
	t += 1/pps;
  }

  // Draw shadow

  dc.SetPen(shadowPen);
  for(x=0; x<mid.width; x++) {
	if (x+2<r.width)
	  dc.DrawLine(mid.x+x+2, ctr-heights[x]+2, mid.x+x+2, ctr+heights[x]+2);
  }

  // Draw track area
  bool usingSelPen = false;
  dc.SetPen(unselectedPen);

  for(x=0; x<mid.width; x++) {
	
	bool sel = false;
	if (ssel0 <= cache.where[x] && cache.where[x+1] < ssel1) 
	  sel = true;

	if (sel && !usingSelPen)
	  dc.SetPen(selectedPen);
	else 
	  if (!sel && usingSelPen)
		dc.SetPen(unselectedPen);
	usingSelPen = sel;

	dc.DrawLine(mid.x+x, ctr-heights[x], mid.x+x, ctr+heights[x]);
  }

  // Draw samples

  dc.SetPen(samplePen);

  for(x=0; x<mid.width; x++) {
	
    int h1 = ctr+(cache.min[x] * heights[x]) / 32767;
    int h2 = ctr+(cache.max[x] * heights[x]) / 32767;
	
    dc.DrawLine(mid.x+x,h1,mid.x+x,h2+1);
  }

  if (drawEnvelope) {
	dc.SetPen(envelopePen);

	for(x=0; x<mid.width; x++) {
	  
	  int z1 = ctr-heights[x]+3 > ctr? ctr: ctr-heights[x]+3;
	  int z2 = ctr+heights[x]-3 < ctr? ctr: ctr+heights[x]-3;  
	  dc.DrawLine(mid.x+x, ctr-heights[x], mid.x+x, z1);
	  dc.DrawLine(mid.x+x, ctr+heights[x], mid.x+x, z2);

	}
  }

  delete[] heights;

  // Draw arrows on the left side if the track extends to the left of the
  // beginning of time.  :)
  
  if (h == 0.0 && tOffset < 0.0) {
    dc.SetPen(*wxWHITE_PEN);
    dc.DrawLine(r.x + 2, r.y + 6, r.x + 8, r.y + 6);
    dc.DrawLine(r.x + 2, r.y + 6, r.x + 6, r.y + 2);
    dc.DrawLine(r.x + 2, r.y + 6, r.x + 6, r.y + 10);

    dc.DrawLine(r.x + 2, r.y + r.height - 8, r.x + 8, r.y + r.height - 8);
    dc.DrawLine(r.x + 2, r.y + r.height - 8, r.x + 6, r.y + r.height - 4);
    dc.DrawLine(r.x + 2, r.y + r.height - 8, r.x + 6, r.y + r.height - 12);
  }

  if (drawEnvelope) {
	wxRect envRect = r;
	envRect.height-=2;
	envelope.Draw(dc, envRect, h, pps);
  }
}

void WaveTrack::DrawSpectrum(wxDC &dc, wxRect &r, double h, double pps,
							 double sel0, double sel1,
							 bool drawEnvelope)
{
  int x=0;
  double tpre = h - tOffset;
  double tstep = 1.0/pps;

  if (tpre*rate >= numSamples)
    return;

  // We draw directly to a bit image in memory,
  // and then paint this directly to our offscreen
  // bitmap.  Note that this could be optimized even
  // more, but for now this is not bad.  -dmazzoni
	
  wxImage *image = new wxImage((int)r.width,(int)r.height);
  wxASSERT(image);
  unsigned char *data = image->GetData();

  float *spec = new float[r.height];

  while(x<r.width) {
    sampleCount w0 = (sampleCount)((tpre+x*tstep)*rate);

    if (w0 >= numSamples)
      break;

    sampleCount wsize;
    int k=0;
    do {
      k++;
      wsize = (sampleCount)(tstep*k*rate);
    } while (k<30 && !ComputeSpectrum(0, (int)wsize, r.height, rate, 0));

    if (wsize > (numSamples - w0))
      wsize = numSamples - w0;

    sampleType *buffer = new sampleType[wsize];
    Get(buffer, w0, wsize);

    if (ComputeSpectrum(buffer, (int)wsize, r.height, rate, spec)) {

      for(int yy=0; yy<r.height; yy++) {
	unsigned char value = (unsigned char)((1.0-spec[r.height-1-yy])*255);
	for(int xx=0; xx<k; xx++) {
	  data[(yy*r.width + x+xx)*3] = value;
	  data[(yy*r.width + x+xx)*3+1] = value;
	  data[(yy*r.width + x+xx)*3+2] = value;
	}
      }      

    }
    
    delete[] buffer;
    
    //    x += k;
    x += 2;
  }

  wxBitmap converted = image->ConvertToBitmap();

  dc.DrawBitmap(converted, r.x, r.y);

  delete image;  

  delete[] spec;
}

void WaveTrack::Draw(wxDC &dc, wxRect &r, double h, double pps,
                     double sel0, double sel1, bool drawEnvelope)
{
  if (GetDisplay() == WaveTrack::SpectrumDisplay) {
	DrawSpectrum(dc, r, h, pps, sel0, sel1, drawEnvelope);
  }
  else {
	DrawMinmax(dc, r, h, pps, sel0, sel1, drawEnvelope);
  }
}

void WaveTrack::SetDisplay(int d)
{
  display = d;
}

int WaveTrack::GetDisplay()
{
  return display;
}

void WaveTrack::Cut(double t0, double t1, VTrack **dest)
{
  Copy(t0, t1, dest);
  Clear(t0, t1);
}

void WaveTrack::Copy(double t0, double t1, VTrack **dest)
{
  *dest = 0;

  wxASSERT(t0 < t1);

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  sampleCount s0 = (sampleCount)((t0 - tOffset) * rate);
  sampleCount s1 = (sampleCount)((t1 - tOffset) * rate);

  if (s0 < 0)
    s0 = 0;
  if (s1 >= numSamples)
    s1 = numSamples;
	
  if (s0 >= s1 || s0>=numSamples || s1<0)
    return;

  int numBlocks = block->Count();
  int b0 = FindBlock(s0);
  int b1 = FindBlock(s1);

  if (s1 == numSamples)
    b1 = numBlocks;

  *dest = new WaveTrack(dirManager);	
  ((WaveTrack *)*dest)->rate = rate;
	
  sampleType *buffer = new sampleType[maxSamples];

  int blocklen;

  // Do the first block

  if (b0 >= 0 && b0<numBlocks && s0!=block->Item(b0)->start) {
  
    blocklen = (block->Item(b0)->start + block->Item(b0)->len - s0);
    if (blocklen > (s1 - s0))
      blocklen = s1 - s0;
    Get(buffer, s0, blocklen);

    ((WaveTrack *)*dest)->Append(buffer,blocklen);
  }

  if (b0 >= 0 && b0<numBlocks && s0==block->Item(b0)->start) {
    b0--;
  }

  // If there are blocks in the middle, copy the blockfiles directly

  for(int b=b0+1; b<b1; b++)
    ((WaveTrack *)*dest)->AppendBlock(block->Item(b));

  // Do the last block

  if (b1 > b0 && b1<numBlocks) {
    blocklen = (s1 - block->Item(b1)->start);
    Get(buffer, block->Item(b1)->start, blocklen);
    ((WaveTrack *)*dest)->Append(buffer, blocklen);
  }

  delete[] buffer;
}

void WaveTrack::Paste(double t, VTrack *src)
{
  wxASSERT(src->GetKind() == WaveTrack::Wave);

  envelope.ExpandRegion(t, src->GetMaxLen());

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  sampleCount s = (sampleCount)((t - tOffset) * rate);

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
    
    Write(buffer, largerBlock, 0, largerBlock->len, false);

    dirManager->Deref(block->Item(b)->f);
    block->Item(b) = largerBlock;

    for(int i=b+1; i<numBlocks; i++)
      block->Item(i)->start += addedLen;
    
    numSamples += addedLen;

    delete[] buffer;

	envelope.SetTrackLen(numSamples / rate);

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
    Write(buffer, splitLeft, 0, splitPoint, false);

    splitRight = new WaveBlock();
    splitRight->start = s + addedLen;
    splitRight->len = block->Item(b)->len - splitPoint;
    splitRight->f = dirManager->NewBlockFile();
	bool initedRight = InitBlock(splitRight);
	wxASSERT(initedRight);

    Read(buffer, block->Item(b), splitPoint, splitRight->len);
    Write(buffer, splitRight, 0, splitRight->len, false);

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

  envelope.SetTrackLen(numSamples / rate);

  ConsistencyCheck("Paste branch two");
}

void WaveTrack::Clear(double t0, double t1)
{
  wxASSERT(t0 < t1);

  envelope.CollapseRegion(t0, t1);

  sampleCount s0 = (sampleCount)((t0 - tOffset) * rate);
  sampleCount s1 = (sampleCount)((t1 - tOffset) * rate);

  if (s0 < 0)
    s0 = 0;
  if (s1 >= numSamples)
    s1 = numSamples;
  
  if (s0 >= s1 || s0>=numSamples || s1<0)
    return;
	
  Delete(s0, (s1-s0));
}

void WaveTrack::AppendBlock(WaveBlock *b)
{
  WaveBlock *newBlock = new WaveBlock();
  newBlock->start = numSamples;
  newBlock->len = b->len;
  newBlock->f = b->f;
  dirManager->Ref(newBlock->f);
  block->Add(newBlock);
  numSamples += newBlock->len;

  envelope.SetTrackLen(numSamples / rate);

  ConsistencyCheck("AppendBlock");
}

bool WaveTrack::Load(wxTextFile *in, DirManager *dirManager)
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

  if (in->GetNextLine() != "numSamples") goto readWaveTrackError;
  if (!(in->GetNextLine().ToLong(&longNumSamples))) goto readWaveTrackError;
  numSamples = longNumSamples;

  if (in->GetNextLine() != "rate") goto readWaveTrackError;
  if (!(in->GetNextLine().ToDouble(&rate))) goto readWaveTrackError;

  if (in->GetNextLine() != "numBlocks") goto readWaveTrackError;
  if (!(in->GetNextLine().ToLong(&numBlocks))) goto readWaveTrackError;

  block->Alloc(numBlocks);

  for(b=0; b<numBlocks; b++) {
	w = new WaveBlock();

	if (in->GetNextLine() != "Block start") goto readWaveTrackError;
	if (!(in->GetNextLine().ToLong(&longBlockStart))) goto readWaveTrackError;
	w->start = longBlockStart;
	
	if (in->GetNextLine() != "Block len") goto readWaveTrackError;
	if (!(in->GetNextLine().ToLong(&longBlockLen))) goto readWaveTrackError;
	w->len = longBlockLen;
	
	if (in->GetNextLine() != "Block name") goto readWaveTrackError;
	wxString name = in->GetNextLine();
	
	w->f = dirManager->GetBlockFile(name);
	
	if (!w->f) {
	  
	  numSamples = 0;
	  block->Clear();

	  blockMutex->Unlock();	  

	  wxString msg;
	  msg.Printf("The file named \"%s\" is missing from the project.",
				 (const char *)name);
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

bool WaveTrack::Save(wxTextFile *out, bool overwrite)
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

  for(b=0; b<block->Count(); b++) {
    bb = block->Item(b);

    dirManager->MakePartOfProject(bb->f);

	out->AddLine("Block start");
	out->AddLine(wxString::Format("%d", bb->start));
	out->AddLine("Block len");
	out->AddLine(wxString::Format("%d", bb->len));
	out->AddLine("Block name");
	out->AddLine(bb->f->name);
  }

  return true;
}

sampleType WaveTrack::Get(sampleCount pos)
{
  sampleType temp[1];
  Get(temp, pos, 1);
  return temp[0];
}

bool WaveTrack::InitBlock(WaveBlock *b)
{
  wxASSERT(b);

  BlockFile *f = b->f;

  if (!f->Open(true))
    return false;

  char header[headerTagLen+1] = "CMUTRAK1";
  f->Write(header,headerTagLen);

  sampleCount slen = summary64KLen + summary256Len;
  sampleType *tempSamples = new sampleType[slen];
  for(int i=0; i<slen; i++)
    tempSamples[i] = 0;
  f->Write((void *)tempSamples, sizeof(sampleType) * slen);
  delete[] tempSamples;
  f->Close();

  return true;
}

int WaveTrack::FindBlock(sampleCount pos, sampleCount lo, sampleCount guess, sampleCount hi)
{
  wxASSERT(block->Item(guess)->len > 0);
  wxASSERT(lo <= guess && guess <= hi && lo <= hi);

  if (pos >= block->Item(guess)->start &&
      pos < block->Item(guess)->start + block->Item(guess)->len)
    return guess;
  
  if (pos < block->Item(guess)->start)
    return FindBlock(pos, lo, (lo+guess)/2, guess);
  else
    return FindBlock(pos, guess+1, (guess+1+hi)/2, hi);
}

int WaveTrack::FindBlock(sampleCount pos)
{
  wxASSERT(pos>=0 && pos<=numSamples);

  int numBlocks = block->Count();

  if (pos == numSamples)
    return (numBlocks-1);
  
  int rval = FindBlock(pos, 0, numBlocks/2, numBlocks);
  
  wxASSERT(rval>=0 && rval<numBlocks &&
	   pos >= block->Item(rval)->start &&
	   pos < block->Item(rval)->start + block->Item(rval)->len);
  
  return rval;
}

void WaveTrack::Read(sampleType *buffer, WaveBlock *b,
		     sampleCount start, sampleCount len)
{
  wxASSERT(b);
  wxASSERT(start >= 0);
  wxASSERT(start + len <= b->len);

  BlockFile *f = b->f;
  bool opened = f->Open(false);
  wxASSERT(opened);

  f->Seek(totalHeaderLen + (start * sizeof(sampleType)),
	  wxFromStart);

  int result = f->Read((void *)buffer, (int)(len * sizeof(sampleType)));

  if (result != (int)(len * sizeof(sampleType))) {
    printf("Expected to read %d bytes, got %d bytes.\n",
	   len * sizeof(sampleType),
	   result);
  }

  wxASSERT(result == (int)(len * sizeof(sampleType)));

  f->Close();
}

void WaveTrack::Read256(sampleType *buffer, WaveBlock *b,
			sampleCount start, sampleCount len)
{
  wxASSERT(b);
  wxASSERT(start >= 0);
  wxASSERT(start + len <= ((b->len + 255) / 256));
  start*=2;
  len*=2;

  BlockFile *f = b->f;
  bool opened = f->Open(false);
  wxASSERT(opened);
  
  f->Seek(headerTagLen + summary64KLen*sizeof(sampleType)
	  + start*sizeof(sampleType), wxFromStart);

  int result = f->Read((void *)buffer, (int)(len * sizeof(sampleType)));
  wxASSERT(result == (int)(len * sizeof(sampleType)));

  f->Close();
}

void WaveTrack::Read64K(sampleType *buffer, WaveBlock *b,
			sampleCount start, sampleCount len)
{
  wxASSERT(b);
  wxASSERT(start >= 0);
  wxASSERT(start + len <= ((b->len + 65535)/ 65536));
  start*=2;
  len*=2;

  BlockFile *f = b->f;
  bool opened = f->Open(false);
  wxASSERT(opened);
  
  f->Seek(headerTagLen + start*sizeof(sampleType), wxFromStart);

  int result = f->Read((void *)buffer, (int)(len * sizeof(sampleType)));
  wxASSERT(result == (int)(len * sizeof(sampleType)));

  f->Close();
}

void WaveTrack::Write(sampleType *buffer, WaveBlock *b,
		      sampleCount start, sampleCount len,
		      bool makeCopy /* = true */)
{
  wxASSERT(b);
  wxASSERT(b->len <= maxSamples);
  wxASSERT(start + len <= b->len);

  cache.dirty = true;

  sampleType *newBuffer = 0;

  // Usually we don't write to an existing block; to support Undo,
  // we copy the old block entirely into memory, dereference it,
  // make the change, and then write the new block to disk.

  if (makeCopy) {
    newBuffer = new sampleType[maxSamples];
    wxASSERT(newBuffer);
    
    Read(newBuffer,b,0,b->len);
    
    for(int i=0; i<len; i++)
      newBuffer[start+i] = buffer[i];

	BlockFile *oldBlockFile = b->f;
    b->f = dirManager->NewBlockFile();
	bool inited = InitBlock(b);
	wxASSERT(inited);

    buffer = newBuffer;
    start = 0;
    len = b->len;
    
    dirManager->Deref(oldBlockFile);
  }
  
  // Write the block

  BlockFile *f = b->f;

  wxASSERT(f);
  bool opened = f->Open(false);
  wxASSERT(opened);

  f->Seek(totalHeaderLen + (start * sizeof(sampleType)),
		    wxFromStart);

  f->Write((void *)buffer, len * sizeof(sampleType));

  // Get summaries from disk

  sampleType *tempSummary64K = new sampleType[summary64KLen];
  sampleType *tempSummary256 = new sampleType[summary256Len];
  sampleType *writeSamples = new sampleType[maxSamples];

  f->Seek(headerTagLen, wxFromStart);
  f->Read((void *)tempSummary64K, summary64KLen * sizeof(sampleType));
  f->Seek(headerTagLen + summary64KLen*sizeof(sampleType), wxFromStart);  
  f->Read((void *)tempSummary256, summary256Len * sizeof(sampleType));

  sampleCount sumStart;
  sampleCount sumLen;
  sampleCount i,j,jcount;
  
  sampleType min,max;

  // Recalc 256 summaries

  sumStart = start/256;
  sumLen = (start + len)/256 + 1 - sumStart;
  
  f->Seek(totalHeaderLen + (sumStart * 256 * sizeof(sampleType)),
	 wxFromStart);
  int read256Len = (b->len - sumStart*256)*sizeof(sampleType);
  if (read256Len > (sumLen * 256 * sizeof(sampleType)))
	read256Len = sumLen * 256 * sizeof(sampleType);

  f->Read((void *)writeSamples, read256Len);
  
  for(i=0; i<sumLen; i++) {
    min = writeSamples[i*256];
    max = writeSamples[i*256];
    jcount = 256;
    if (i*256+jcount > b->len)
      jcount = b->len - i*256;
    for(j=1; j<jcount; j++) {
      if (writeSamples[i*256+j]<min)
        min = writeSamples[i*256+j];
      else if (writeSamples[i*256+j]>max)
        max = writeSamples[i*256+j];
    }
    tempSummary256[(sumStart+i) * 2    ] = min;
    tempSummary256[(sumStart+i) * 2 + 1] = max;
  }

  // Recalc 64K summaries

  sumStart = start/65536;
  sumLen = (start + len)/65536 + 1 - sumStart;
  
  for(i=0; i<sumLen; i++) {
    min = tempSummary256[i*256];
    max = tempSummary256[i*256];
    if (i*256+jcount > ((b->len+255)/256))
      jcount = ((b->len+255)/256) - i*256;

    for(j=1; j<256; j++) {
      if (tempSummary256[i*256+j]<min)
        min = tempSummary256[i*256+j];
      else if (tempSummary256[i*256+j]>max)
        max = tempSummary256[i*256+j];
    }
    tempSummary64K[(sumStart+i) * 2    ] = min;
    tempSummary64K[(sumStart+i) * 2 + 1] = max;
  }
  
  // Write summaries back
  f->Seek(headerTagLen, wxFromStart);
  f->Write((void *)tempSummary64K, summary64KLen * sizeof(sampleType));
  f->Seek(headerTagLen + summary64KLen*sizeof(sampleType), wxFromStart);  
  f->Write((void *)tempSummary256, summary256Len * sizeof(sampleType));

  delete[] tempSummary64K;
  delete[] tempSummary256;
  delete[] writeSamples;

  // TODO: Check that this code is actually calculating summaries
  // correctly.  Two tests: that it's actually updating the summary
  // indices needed (and no more), and that the total summaries match
  // the total data

  f->Close();

  if (newBuffer)
    delete[] newBuffer;
}

void WaveTrack::Get(sampleType *buffer, sampleCount start, sampleCount len)
{
  wxASSERT(start < numSamples && start+len <= numSamples);
  int b = FindBlock(start);

  while(len) {
    sampleCount blen = block->Item(b)->start + block->Item(b)->len - start;
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

void WaveTrack::Set(sampleType *buffer, sampleCount start, sampleCount len)
{
  wxASSERT(start < numSamples && start+len <= numSamples);

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  int b = FindBlock(start);

  while(len) {
    int blen = block->Item(b)->start + block->Item(b)->len - start;
    if (blen > len)
      blen = len;
    Write(buffer, block->Item(b), start - block->Item(b)->start, blen);
    len -= blen;
    buffer += blen;
    start += blen;
    b++;
  }

  ConsistencyCheck("Set");
}

void WaveTrack::Append(sampleType *buffer, sampleCount len)
{
  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  while(len) {
    int l = (len > minSamples? minSamples: len);
    WaveBlock *w = new WaveBlock();
    w->f = dirManager->NewBlockFile();
    w->start = numSamples;
    w->len = l;
	bool inited = InitBlock(w);
	wxASSERT(inited);
    Write(buffer, w, 0, l, false);
    block->Add(w);
    
    buffer += l;
    numSamples += l;
    len -= l;
  }

  envelope.SetTrackLen(numSamples / rate);
	
  ConsistencyCheck("Append");
}

void WaveTrack::Delete(sampleCount start, sampleCount len)
{
  int numBlocks = block->Count();
  int newNumBlocks = 0;

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  int b0 = FindBlock(start);
  int b1;
  if (start+len==numSamples)
    b1 = numBlocks;
  else
    b1 = FindBlock(start+len);

  int i;

  // Create a new array of blocks

  BlockArray *newBlock = new BlockArray();
  newBlock->Alloc(numBlocks-(b1-b0)+2);

  for(i=0; i<b0; i++) {
    newBlock->Add(block->Item(i)); newNumBlocks++;
  }

  // If the first sample to delete is in the middle of a block,
  // insert a new block containing the first part of the "split"
  // block

  if (block->Item(b0)->start != start) {
    sampleType *tempSamples = new sampleType[maxSamples];
    
    WaveBlock *preBlock = new WaveBlock();
    preBlock->f = dirManager->NewBlockFile();
	bool inited = InitBlock(preBlock);
    wxASSERT(inited);
    
    int prelen = start - block->Item(b0)->start;
    preBlock->len = prelen;
    preBlock->start = block->Item(b0)->start;
    
    Read(tempSamples,block->Item(b0),0,prelen);
    Write(tempSamples,preBlock,0,prelen, false);
    
    delete[] tempSamples;
    
    newBlock->Add(preBlock); newNumBlocks++;
  }

  // Now delete blocks from b0 until one before b1
  
  for(i=b0; i<b1; i++)
    dirManager->Deref(block->Item(i)->f);

  // If the last sample to delete is in the middle of a block,
  // insert a new block containing the last part of the split
  // block
  
  if (b1 != numBlocks) {
    if (block->Item(b1)->start != start+len) {
      WaveBlock *postBlock;

      sampleType *tempSamples = new sampleType[maxSamples];
      
      postBlock = new WaveBlock();
      postBlock->f = dirManager->NewBlockFile();
	  bool inited = InitBlock(postBlock);
      wxASSERT(inited);

      int poststart = (start + len) - block->Item(b1)->start;
      int postlen = block->Item(b1)->start + block->Item(b1)->len - (start + len);
      
      Read(tempSamples,block->Item(b1),poststart,postlen);
      
      postBlock->start = start;
      postBlock->len = postlen;
      
      Write(tempSamples,postBlock,0,postlen, false);
      
      delete[] tempSamples;
      
      newBlock->Add(postBlock); newNumBlocks++;
    }
    
    dirManager->Deref(block->Item(b1)->f);

    for(i=b1+1; i<numBlocks; i++) {
      block->Item(i)->start -= len;
      newBlock->Add(block->Item(i)); newNumBlocks++;
    }
  }

  delete block;
  block = newBlock;

  numSamples -= len;

  envelope.SetTrackLen(numSamples / rate);

  ConsistencyCheck("Delete");
}

void WaveTrack::Reblockify()
{
  // TODO
}

void WaveTrack::ConsistencyCheck(char *whereStr)
{
  int i;
  int pos=0;
  bool error=false;
  
  for(i=0; i<block->Count(); i++) {
    if (pos != block->Item(i)->start)
      error = true;
    pos += block->Item(i)->len;
  }
  if (pos != numSamples)
    error = true;

  if (error) {
    #ifdef __WXDEBUG__
      printf("*** Consistency check failed after %s ***\n");
      Debug();
    #else
      wxMessageBox("Internal error (WaveTrack)");
      exit(0);
    #endif
  }
}

void WaveTrack::Debug()
{
  int i;
  int pos=0;
  
  for(i=0; i<block->Count(); i++) {
	printf("Block %3d: start %8d len %8d  %s",
		   i,
		   block->Item(i)->start,
		   block->Item(i)->len,
		   (const char *)(block->Item(i)->f->name));
	if (pos != block->Item(i)->start)
	  printf("  ERROR\n");
	else
	  printf("\n");
	pos += block->Item(i)->len;
  }
  if (pos != numSamples)
	printf("ERROR numSamples = %d\n",numSamples);
}



/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/colour.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/file.h>
#include <wx/image.h>
#include <wx/msgdlg.h>
#include <wx/textfile.h>

#include "AColor.h"
#include "Spectrum.h"
#include "WaveTrack.h"
#include "DirManager.h"
#include "Prefs.h"

// Max file size of 16-bit samples is 1MB
// (About 12 seconds of audio)

const int headerTagLen = 8;

// Static variables for block size

sampleCount WaveTrack::summary64KLen = 8*2;
sampleCount WaveTrack::summary256Len = summary64KLen * 256;
sampleCount WaveTrack::totalHeaderLen =
  headerTagLen + (summary64KLen + summary256Len)*sizeof(sampleType);

sampleCount WaveTrack::maxSamples = summary256Len * 256 / 2;
sampleCount WaveTrack::minSamples = maxSamples / 2;

// Static methods


sampleCount WaveTrack::GetIdealBlockSize()
{
  return (minSamples+maxSamples)/2;
}

void WaveTrack::SetMaxDiskBlockSize(int bytes)
{
  maxSamples = bytes / sizeof(sampleType);
  minSamples = maxSamples / 2;

  summary256Len = (maxSamples + 255) / 256 * 2;
  summary64KLen = (summary256Len + 255) / 256 * 2;
  totalHeaderLen = 
	headerTagLen +
	(summary64KLen + summary256Len)*sizeof(sampleType);
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
  for(int i=0; i<block->Count(); i++) {
    dirManager->Deref(block->Item(i)->f);
	delete block->Item(i);
  }

  delete block;

  if (cache.min) delete[] cache.min;
  if (cache.max) delete[] cache.max;
  if (cache.where) delete[] cache.where;
  if (cache.freq) delete[] cache.freq;

  #if wxUSE_THREADS
  delete blockMutex;
  #endif
}

void WaveTrack::DeleteButDontDereference()
{
  for(int i=0; i<block->Count(); i++) {
	delete block->Item(i);
  }
  block->Clear();

  delete this;
}

double WaveTrack::GetMaxLen()
{
  return ((double)numSamples)/(rate) + tOffset;
}

double WaveTrack::GetRate()
{
  return rate;
}

void WaveTrack::SetRate(double newRate)
{
  rate = newRate;
  cache.dirty = true;
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
  copy->channel = channel;
  copy->selected = selected;
  copy->envelope.CopyFrom(&envelope);

  return (VTrack *)copy;
}

void WaveTrack::GetMinMax(sampleCount start, sampleCount len, sampleType *outMin, sampleType *outMax)
{
  sampleType min = 32727;
  sampleType max = -32768;
  
  int block0 = FindBlock(start);
  int block1 = FindBlock(start + len);
  
  // First calculate the min/max of the blocks in the middle of this region;
  // this is very fast because we have the min/max of every entire block already
  // in memory.
  
  int b, i;
  
  for(b=block0+1; b<block1; b++) {
    if (block->Item(b)->min < min)
      min = block->Item(b)->min;
    if (block->Item(b)->max > max);
      max = block->Item(b)->max;
  }
  
  // Now we take the first and last blocks into account, noting that the selection
  // may only partly overlap these blocks.  If the overall min/max of either of
  // these blocks is within min...max, then we can ignore them.  If not,
  // we need read some samples and summaries from disk.

  if (block->Item(block0)->min < min || block->Item(block0)->max > max) {
    int s0 = start - block->Item(block0)->start;
    int l0 = len;
    int max = block->Item(block0)->len - start;
    if (l0 > max)
      l0 = max;
    sampleType *buffer = new sampleType[l0];
    
    // TODO: optimize this to use Read256 and Read64K
    
    Read(buffer, block->Item(block0), s0, l0);
    for(i=0; i<l0; i++) {
      if (buffer[i] < min)
        min = buffer[i];
      if (buffer[i] > max)
        max = buffer[i];
    }

    delete[] buffer;
  }
  
  if (block1 > block0 &&
      (block->Item(block1)->min < min || block->Item(block1)->max > max)) {

    int s0 = 0;
    int l0 = (start+len) - block->Item(block1)->start;
    sampleType *buffer = new sampleType[l0];
    
    // TODO: optimize this to use Read256 and Read64K
    
    Read(buffer, block->Item(block1), s0, l0);
    for(i=0; i<l0; i++) {
      if (buffer[i] < min)
        min = buffer[i];
      if (buffer[i] > max)
        max = buffer[i];
    }

    delete[] buffer;

  }
  *outMin = min;
  *outMax = max;
}

void WaveTrack::PrepareCacheMinMax(double start, double pps, int screenWidth)
{
  wxASSERT(start>=0.0);
  wxASSERT(pps>0.0);
  wxASSERT(screenWidth>0);

  if (!cache.spectrum &&
	  !cache.dirty &&
      pps == cache.pps &&
      start == cache.start &&
      screenWidth <= cache.len)
    return;

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  DisplayCache oldcache = cache;

  cache.freq = NULL;

  cache.spectrum = false;
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
	cache.where[x] = (sampleCount)(start*rate + x*rate/pps + 0.5);

  sampleCount s0 = cache.where[0];
  sampleCount s1 = cache.where[cache.len];
  int p0 = 0;
  int p1 = cache.len;

  // Optimization: if the old cache is good and overlaps
  // with the current one, re-use as much of the cache as
  // possible

  if (!oldcache.dirty &&
	  !oldcache.spectrum &&
	  oldcache.pps == pps &&
	  oldcache.where[0] < cache.where[cache.len] &&
	  oldcache.where[oldcache.len] > cache.where[0]) {

	s0 = cache.where[cache.len];
	s1 = cache.where[0];
	p0 = cache.len;
	p1 = 0;

	for(x=0; x<cache.len; x++)

	  if (cache.where[x] >= oldcache.where[0] &&
		  cache.where[x] <= oldcache.where[oldcache.len-1]) {

		int ox =
		  int((double(oldcache.len) * (cache.where[x] - oldcache.where[0])) /
			  (oldcache.where[oldcache.len] - oldcache.where[0])+0.5);

	  cache.min[x] = oldcache.min[ox];
	  cache.max[x] = oldcache.max[ox];

		// Unfortunately we can't make this check due to
		// floating-point roundoff errors
		//
		// Maybe if this happens we should recalculate all???
		//
		//if (!(ox >= 0 && ox <= oldcache.len &&
		//	  cache.where[x] == oldcache.where[ox]))
		//  wxASSERT(0);

	  }
	  else {
		if (cache.where[x] < s0) {
		  s0 = cache.where[x];
		  p0 = x;
		}
		if (cache.where[x+1] > s1) {
		  s1 = cache.where[x+1];
		  p1 = x+1;
		}
	  }
		
  }

  int divisor;
  if (rate/pps >= 65536) // samp/sec / pixels/sec = samp / pixel
    divisor = 65536;
  else if (rate/pps >= 256)
    divisor = 256;
  else
    divisor = 1;
  
  if (s1 > numSamples)
    s1 = numSamples;
    
  sampleCount srcX = s0;

  int block0 = FindBlock(s0);

  sampleType *temp = new sampleType[maxSamples];
  
  sampleCount dstX = 0;
  int pixel = p0;
  
  sampleType theMin;
  sampleType theMax;
  int b = block0;

  while(srcX < s1) {
    // Get more samples
    
    sampleCount num;

    num = ((block->Item(b)->len -
			(srcX - block->Item(b)->start)) + divisor-1)
	  / divisor;

    if (num > (s1 - srcX + divisor-1)/divisor) {
      num = (s1 - srcX+divisor-1)/divisor;
	}

    switch(divisor) {
    case 1:
      Read(temp, block->Item(b),
		   srcX - block->Item(b)->start,
		   num);
      break;
    case 256:
      Read256(temp, block->Item(b),
			  (srcX - block->Item(b)->start)/divisor,
			  num);
      break;
    case 65536:
      Read64K(temp, block->Item(b),
			  (srcX - block->Item(b)->start)/divisor,
			  num);
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
        if (pixel>p0) {
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
  } while (pixel <= p1);

  cache.dirty = false;

  if (oldcache.min) delete[] oldcache.min;
  if (oldcache.max) delete[] oldcache.max;
  if (oldcache.where) delete[] oldcache.where;
  if (oldcache.freq) delete[] oldcache.freq;

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
  
  sampleCount s0 = (sampleCount)(t0 * rate + 0.5);
  sampleCount s1 = (sampleCount)(t1 * rate + 0.5);

  sampleCount slen = (sampleCount)(s1-s0);

  int ssel0 = (int)((sel0 - tOffset) * rate + 0.5);
  int ssel1 = (int)((sel1 - tOffset) * rate + 0.5);

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

  dc.DrawRectangle(r);
  
  wxRect mid = r;

  if (t0 > tpre) {
    wxRect pre = r;
    pre.width = (int)((t0 - tpre)*pps);
    mid.x += pre.width;
    mid.width -= pre.width;
    //dc.DrawRectangle(pre);	    
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
	//if (post.width > 0)
	  //dc.DrawRectangle(post);
  }
  
  mid.height -= 2;
  int ctr = r.y + (r.height/2);
  
  int *heights = NULL;

  if (mid.width > 0) {
    dc.SetPen(*wxRED_PEN);	    
    dc.DrawLine(mid.x, ctr, mid.x + mid.width, ctr);

    PrepareCacheMinMax(t0, pps, mid.width);
    
    heights = new int[mid.width];
  }

  double t = t0;
  int x;
  for(x=0; x<mid.width; x++) {
    heights[x] = int(0.5 + (mid.height/2.0) *
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
	
    int h1 = ctr-(cache.min[x] * heights[x]) / 32767;
    int h2 = ctr-(cache.max[x] * heights[x]) / 32767;
	
    dc.DrawLine(mid.x+x,h2,mid.x+x,h1+1);
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

  if (heights)
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

void WaveTrack::PrepareCacheSpectrum(double start, double pps, 
									 int screenWidth, int screenHeight)
{
  wxASSERT(start>=0.0);
  wxASSERT(pps>0.0);
  wxASSERT(screenWidth>0);
  wxASSERT(screenHeight>0);

  if (cache.spectrum &&
	  !cache.dirty &&
      pps == cache.pps &&
      start == cache.start &&
      screenWidth <= cache.len &&
	  cache.fheight == screenHeight)
    return;

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  DisplayCache oldcache = cache;

  cache.spectrum = true;
  cache.pps = pps;
  cache.start = start;
  cache.len = screenWidth;
  cache.fheight = screenHeight;
  cache.freq = new float[cache.len * screenHeight];
  wxASSERT(cache.freq);
  cache.where = new sampleCount[cache.len+1];
  wxASSERT(cache.where);
  cache.min = NULL;
  cache.max = NULL;
  
  sampleCount x;

  bool *recalc = new bool[cache.len+1];
  
  for(x=0; x<cache.len+1; x++) {
	recalc[x] = true;
	cache.where[x] = (sampleCount)(start*rate + x*rate/pps + 0.5);
  }

  // Optimization: if the old cache is good and overlaps
  // with the current one, re-use as much of the cache as
  // possible

  if (!oldcache.dirty &&
	  oldcache.spectrum &&
	  oldcache.pps == pps &&
	  oldcache.fheight == screenHeight &&
	  oldcache.where[0] < cache.where[cache.len] &&
	  oldcache.where[oldcache.len] > cache.where[0]) {

	for(x=0; x<cache.len; x++)

	  if (cache.where[x] >= oldcache.where[0] &&
		  cache.where[x] <= oldcache.where[oldcache.len-1]) {

		int ox =
		  int((double(oldcache.len) * (cache.where[x] - oldcache.where[0])) /
			  (oldcache.where[oldcache.len] - oldcache.where[0])+0.5);

		if (ox >= 0 && ox <= oldcache.len &&
			cache.where[x] == oldcache.where[ox]) {

		  for(sampleCount i=0; i<screenHeight; i++)
			cache.freq[screenHeight*x + i] =
			  oldcache.freq[screenHeight*ox + i];
		  
		  recalc[x] = false;
		}

	  }
		
  }

  int windowSize = GetSpectrumWindowSize();
  sampleType *buffer = new sampleType[windowSize];

  for(x=0; x<cache.len; x++)
	if (recalc[x]) {

	  sampleCount start = cache.where[x];
	  sampleCount len = windowSize;
	  
	  sampleCount i;
	  
	  if (start >= numSamples) {
		for(i=0; i<screenHeight; i++)
		  cache.freq[screenHeight*x + i] = 0;

	  }
	  else {

		if (start + len > numSamples) {
		  len = numSamples - start;
		  for(i=len; i<windowSize; i++)
			buffer[i] = 0;
		}
	  
		Get(buffer, start, len);
	  
		ComputeSpectrum(buffer, windowSize, screenHeight, rate,
						&cache.freq[screenHeight*x]);
	  }
	}

  delete[] buffer;
  delete[] recalc;

  if (oldcache.min) delete[] oldcache.min;
  if (oldcache.max) delete[] oldcache.max;
  if (oldcache.where) delete[] oldcache.where;
  if (oldcache.freq) delete[] oldcache.freq;
}

void WaveTrack::DrawSpectrum(wxDC &dc, wxRect &r, double h, double pps,
							 double sel0, double sel1,
							 bool drawEnvelope)
{
  int x=0;
  double tpre = h - tOffset;
  double tstep = 1.0/pps;

  double t0 = (tpre >= 0.0? tpre: 0.0);

  int ssel0 = (int)((sel0 - tOffset) * rate + 0.5);
  int ssel1 = (int)((sel1 - tOffset) * rate + 0.5);

  if ((sampleCount)(tpre*rate + 0.5) >= numSamples)
    return;

  // We draw directly to a bit image in memory,
  // and then paint this directly to our offscreen
  // bitmap.  Note that this could be optimized even
  // more, but for now this is not bad.  -dmazzoni
	
  wxImage *image = new wxImage((int)r.width,(int)r.height);
  wxASSERT(image);
  unsigned char *data = image->GetData();

  PrepareCacheSpectrum(t0, pps, r.width, r.height);

  bool isGrayscale = false;
  gPrefs->Read("/Spectrum/Grayscale", &isGrayscale, false);

  int i=0;
  while(x<r.width) {
    sampleCount w0 = (sampleCount)((tpre+x*tstep)*rate + 0.5);

    if (w0 < 0 || w0 >= numSamples) {
      for(int yy=0; yy<r.height; yy++) {
	    data[(yy*r.width + x)*3] = 214;
	    data[(yy*r.width + x)*3+1] = 214;
	    data[(yy*r.width + x)*3+2] = 214;
      }
      x++;
      continue;
    }

	float *spec = &cache.freq[r.height * i];
	
	for(int yy=0; yy<r.height; yy++) {
	  
	  bool selflag = (ssel0 <= w0 && w0 < ssel1);

	  unsigned char rv, gv, bv;

	  if (isGrayscale)
		rv = gv = bv = char(214-214*spec[r.height-1-yy]);
	  else
		GetColorGradient(spec[r.height-1-yy], selflag, &rv, &gv, &bv);
	  
	  data[(yy*r.width + x)*3] = rv;
	  data[(yy*r.width + x)*3+1] = gv;
	  data[(yy*r.width + x)*3+2] = bv;	  
	}
	
	i++;
	x++;
  }

  wxBitmap converted = image->ConvertToBitmap();

  //wxBitmap converted;
  //converted.Create(r.width, r.height);

  wxMemoryDC memDC;  

  memDC.SelectObject(converted);

  dc.Blit(r.x, r.y, r.width, r.height,
		  &memDC, 0, 0, wxCOPY, FALSE);

  //dc.DrawBitmap(converted, r.x, r.y);

  delete image;

  if (drawEnvelope) {
    wxRect envRect = r;
    envRect.height-=2;
    envelope.Draw(dc, envRect, h, pps);
  }
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

  wxASSERT(t0 <= t1);

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  sampleCount s0 = (sampleCount)((t0 - tOffset) * rate + 0.5);
  sampleCount s1 = (sampleCount)((t1 - tOffset) * rate + 0.5);

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

  sampleCount s = (sampleCount)((t - tOffset) * rate + 0.5);

  if (s < 0)
    s = 0;
  if (s >= numSamples)
    s = numSamples;

  BlockArray *srcBlock = ((WaveTrack *)src)->GetBlockArray();
  int addedLen = ((WaveTrack *)src)->numSamples;
  int srcNumBlocks = srcBlock->Count();

  if (addedLen == 0 || srcNumBlocks==0)
	return;

  int b = FindBlock(s);
  int numBlocks = block->Count();

  if (numBlocks == 0) {
    // Special case: this track is currently empty.

    for(int i=0; i<srcNumBlocks; i++)
      AppendBlock(srcBlock->Item(i));

    envelope.SetTrackLen(numSamples / rate);

    ConsistencyCheck("Paste branch one");
    
    return;	
  }

  if (b >= 0 && b < numBlocks && block->Item(b)->len + addedLen < maxSamples) {
    // Special case: we can fit all of the new samples inside of
    // one block!

    sampleType *buffer = new sampleType[maxSamples];

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
    delete block->Item(b);
    block->Item(b) = largerBlock;

    for(int i=b+1; i<numBlocks; i++)
      block->Item(i)->start += addedLen;
    
    numSamples += addedLen;

    delete[] buffer;

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
  int newNumBlocks=0;

  for(i=0; i<b; i++) {
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
	((WaveTrack *)src)->Get(&sumBuffer[splitPoint], 0, addedLen);
	Read(&sumBuffer[splitPoint+addedLen], splitBlock,
		 splitPoint, splitBlock->len - splitPoint);

	BlockArray *split = Blockify(sumBuffer, sum);
	for(i=0; i<split->Count(); i++) {
	  split->Item(i)->start += splitBlock->start;
	  newBlock->Add(split->Item(i));
	  newNumBlocks++;
	}
	delete split;

	delete[] sumBuffer;
  }
  else {

	// The final case is that we're inserting at least five blocks.
	// We divide these into three groups: the first two get merged
	// with the first half of the split block, the middle ones get
	// copied in as is, and the last two get merged with the last
	// half of the split block.

	sampleCount srcFirstTwoLen =
	  srcBlock->Item(0)->len +
	  srcBlock->Item(1)->len;
	sampleCount leftLen = splitPoint + srcFirstTwoLen;

	sampleType *leftBuffer = new sampleType[leftLen];

	Read(leftBuffer, splitBlock, 0, splitPoint);
	((WaveTrack *)src)->Get(&leftBuffer[splitPoint], 0, srcFirstTwoLen);

	BlockArray *split = Blockify(leftBuffer, leftLen);
	for(i=0; i<split->Count(); i++) {
	  split->Item(i)->start += splitBlock->start;
	  newBlock->Add(split->Item(i));
	  newNumBlocks++;
	}
	delete split;
	delete[] leftBuffer;

	for(i=2; i<srcNumBlocks-2; i++) {
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
	  srcBlock->Item(srcNumBlocks-2)->len +
	  srcBlock->Item(srcNumBlocks-1)->len;
	sampleCount rightSplit = splitBlock->len - splitPoint;
	sampleCount rightLen = rightSplit + srcLastTwoLen;

	sampleType *rightBuffer = new sampleType[rightLen];

	sampleCount lastStart = srcBlock->Item(srcNumBlocks-2)->start;
	((WaveTrack *)src)->Get(rightBuffer, lastStart, srcLastTwoLen);
	Read(&rightBuffer[srcLastTwoLen], splitBlock, splitPoint, rightSplit);

	sampleCount pos = s + lastStart;

	split = Blockify(rightBuffer, rightLen);
	for(i=0; i<split->Count(); i++) {
	  split->Item(i)->start += pos;
	  newBlock->Add(split->Item(i));
	  newNumBlocks++;
	}
	delete split;
	delete[] rightBuffer;
  }

  dirManager->Deref(splitBlock->f);
  delete splitBlock;

  // Copy remaining blocks to new block array and
  // swap the new block array in for the old

  for(i=b+1; i<numBlocks; i++) {
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
    
    Write(buffer, largerBlock, 0, largerBlock->len, false);

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

  ConsistencyCheck("Paste branch two");
}
// end old paste

*/

void WaveTrack::Clear(double t0, double t1)
{
  wxASSERT(t0 <= t1);

  envelope.CollapseRegion(t0, t1);

  sampleCount s0 = (sampleCount)((t0 - tOffset) * rate + 0.5);
  sampleCount s1 = (sampleCount)((t1 - tOffset) * rate + 0.5);

  if (s0 < 0)
    s0 = 0;
  if (s1 >= numSamples)
    s1 = numSamples;
  
  if (s0 >= s1 || s0>=numSamples || s1<0)
    return;
	
  Delete(s0, (s1-s0));
}

void WaveTrack::Silence(double t0, double t1)
{
  wxASSERT(t0 <= t1);

  sampleCount s0 = (sampleCount)((t0 - tOffset) * rate + 0.5);
  sampleCount s1 = (sampleCount)((t1 - tOffset) * rate + 0.5);

  if (s0 < 0)
    s0 = 0;
  if (s1 >= numSamples)
    s1 = numSamples;
  
  if (s0 >= s1 || s0>=numSamples || s1<0)
    return;
	
  Set(NULL, s0, s1-s0);
}

void WaveTrack::InsertSilence(double t, double lenSecs)
{
  // Create a new track containing as much silence as we
  // need to insert, and then call Paste to do the insertion

  sampleCount len = (sampleCount)(lenSecs * rate + 0.5);

  WaveTrack *sTrack = new WaveTrack(dirManager);	
  sTrack->rate = rate;

  sampleCount idealSamples = GetIdealBlockSize();
  sampleType *buffer = new sampleType[idealSamples];
  sampleCount i;  
  for(i=0; i<idealSamples; i++)
    buffer[i] = 0;
  
  sampleCount pos = 0;
  BlockFile *firstBlockFile = NULL;
  
  while(len) {
    sampleCount l = (len > idealSamples? idealSamples: len);

    WaveBlock *w = new WaveBlock();
    w->start = pos;
    w->len = l;
    w->min = 0;
    w->max = 0;
    if (pos == 0 || len==l) {
      w->f = dirManager->NewBlockFile();
      firstBlockFile = w->f;
      bool inited = InitBlock(w);
      wxASSERT(inited);
      Write(buffer, w, 0, l, false);
    }
    else {
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

void WaveTrack::AppendBlock(WaveBlock *b)
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
  	
  	if (in->GetNextLine() != "Block info") goto readWaveTrackError;
  	
  	w->f = dirManager->LoadBlockFile(in);
  	
  	if (!w->f) {
  	  
  	  numSamples = 0;
  	  block->Clear();

  	  blockMutex->Unlock();	  

  	  wxString msg;
  	  msg.Printf("The file named \"%s\" is missing from the project.",
  				 (const char *)in->GetCurrentLine());
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

  if (pos == 0)
	return 0;

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
  
  // Recalc block-level summary
  
  sumLen = (maxSamples + 65535) / 65536;
  
  min = tempSummary64K[0];
  max = tempSummary64K[0];  
  
  for(i=1; i<sumLen; i++) {
    if (tempSummary64K[i]<min)
      min = tempSummary64K[i];
    else if (tempSummary64K[i]>max)
      max = tempSummary64K[i];
  }
  b->min = min;
  b->max = max;
  
  // Write 256 and 64K summaries to disk
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

// Pass NULL to set silence
void WaveTrack::Set(sampleType *buffer, sampleCount start, sampleCount len)
{
  wxASSERT(start < numSamples && start+len <= numSamples);

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  int b = FindBlock(start);

  sampleType *silence;
  if (!buffer) {
    silence = new sampleType[maxSamples];
    for(int i=0; i<maxSamples; i++)
      silence[i] = 0;
  }

  while(len) {
    int blen = block->Item(b)->start + block->Item(b)->len - start;
    if (blen > len)
      blen = len;

    if (buffer) {
      Write(buffer, block->Item(b), start - block->Item(b)->start, blen);
      buffer += blen;
    }
    else
      Write(silence, block->Item(b), start - block->Item(b)->start, blen);

    len -= blen;
    start += blen;
    b++;
  }

  if (!buffer)
    delete[] silence;

  ConsistencyCheck("Set");
}

void WaveTrack::Append(sampleType *buffer, sampleCount len)
{
  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  // If the last block is not full, we need to add samples to it
  int numBlocks = block->Count();
  if (numBlocks>0 && block->Item(numBlocks-1)->len < minSamples) {
	WaveBlock *lastBlock = block->Item(numBlocks-1);
	sampleCount addLen;
	if (lastBlock->len + len < maxSamples)
	  addLen = len;
	else
	  addLen = GetIdealBlockSize() - lastBlock->len;
	sampleCount pos = lastBlock->len;

	WaveBlock *newLastBlock = NewInitedWaveBlock();
	
	sampleType *buffer2 = new sampleType[lastBlock->len + addLen];
	Read(buffer2, lastBlock, 0, lastBlock->len);

	for(int j=0; j<addLen; j++)
	  buffer2[lastBlock->len + j] = buffer[j];
	
	newLastBlock->start = lastBlock->start;
	newLastBlock->len = lastBlock->len + addLen;

	Write(buffer2, newLastBlock, 0, lastBlock->len + addLen, false);

	delete[] buffer2;

	dirManager->Deref(lastBlock->f);
	delete lastBlock;
	block->Item(numBlocks-1) = newLastBlock;

	len -= addLen;
	numSamples += addLen;
	buffer += addLen;
  }

  // Append the rest as new blocks
  while(len) {
    sampleCount idealSamples = GetIdealBlockSize();
    sampleCount l = (len > idealSamples? idealSamples: len);
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

BlockArray *WaveTrack::Blockify(sampleType *buffer, sampleCount len)
{
  BlockArray *list = new BlockArray();
  list->Alloc(10);

  if (len == 0)
	return list;

  int num = (len + (maxSamples-1))/maxSamples;

  for(int i=0; i<num; i++) {
  	WaveBlock *b = NewInitedWaveBlock();

  	b->start = i*len/num;
  	b->len = ((i+1)*len/num) - b->start;
  	
  	Write(&buffer[b->start], b, 0, b->len, false);
  	
  	list->Add(b);
  }

  return list;
}

void WaveTrack::Delete(sampleCount start, sampleCount len)
{
  if (len==0)
	return;

  int numBlocks = block->Count();
  int newNumBlocks = 0;

  #if wxUSE_THREADS
  wxMutexLocker lock(*blockMutex);
  #endif

  int b0 = FindBlock(start);
  int b1 = FindBlock(start+len-1);

  // Special case: if the samples to delete are all within a single
  // block and the resulting length is not too small, perform the
  // deletion within this block:

  if (b0 == b1 && block->Item(b0)->len - len >= minSamples) {
	WaveBlock *b = block->Item(b0);
	sampleCount pos = start - b->start;
	sampleCount newLen = b->len - len;
	sampleType *buffer = new sampleType[newLen];

	Read(buffer, b, 0, pos);
	Read(&buffer[pos], b, pos+len, newLen-pos);

	WaveBlock *newBlock = NewInitedWaveBlock();
	newBlock->start = b->start;
	newBlock->len = newLen;
	Write(buffer, newBlock, 0, newLen, false);

	block->Item(b0) = newBlock;

	for(int j=b0+1; j<numBlocks; j++)
	  block->Item(j)->start -= len;

	delete[] buffer;

    dirManager->Deref(b->f);
	delete b;

	numSamples -= len;
	envelope.SetTrackLen(numSamples / rate);
	ConsistencyCheck("Delete - branch one");	

	return;
  }

  // Create a new array of blocks

  BlockArray *newBlock = new BlockArray();
  newBlock->Alloc(numBlocks-(b1-b0)+2);

  // Copy the blocks before the deletion point over to
  // the new array

  int i;
  for(i=0; i<b0; i++) {
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
	if (preBufferLen >= minSamples || b0==0) {
	  WaveBlock *insBlock = NewInitedWaveBlock();
    
	  insBlock->len = preBufferLen;
	  insBlock->start = preBlock->start;

	  sampleType *preBuffer = new sampleType[preBufferLen];
	  Read(preBuffer, preBlock, 0, preBufferLen);
	  Write(preBuffer, insBlock, 0, preBufferLen, false);
	  delete[] preBuffer;
    
	  newBlock->Add(insBlock);
	  newNumBlocks++;

	  if (b0 != b1) {
		dirManager->Deref(preBlock->f);
		delete preBlock;
	  }
	}
	else {
	  WaveBlock *prepreBlock = block->Item(b0-1);
	  sampleCount prepreLen = prepreBlock->len;
	  sampleCount sum = prepreLen + preBufferLen;

	  sampleType *sumBuffer = new sampleType[sum];
	  Read(sumBuffer, prepreBlock, 0, prepreLen);
	  Read(&sumBuffer[prepreLen], preBlock, 0, preBufferLen);

	  BlockArray *split = Blockify(sumBuffer, sum);
	  split->Item(0)->start += prepreBlock->start;
	  newBlock->Item(b0-1) = split->Item(0);
	  for(i=1; i<split->Count(); i++) {
	  split->Item(i)->start += prepreBlock->start;
		newBlock->Add(split->Item(i));
		newNumBlocks++;
	  }
	  delete split;

	  delete[] sumBuffer;

	  dirManager->Deref(prepreBlock->f);
	  delete prepreBlock;

	  if (b0 != b1) {
		dirManager->Deref(preBlock->f);
		delete preBlock;
	  }
	}
  }
  else {
	// The sample where we begin deletion happens to fall
	// right on the beginning of a block.

	if (b0 != b1) {
	  dirManager->Deref(block->Item(b0)->f);
	  delete block->Item(b0);
	}
  }

  // Next, delete blocks strictly between b0 and b1
  
  for(i=b0+1; i<b1; i++) {
    dirManager->Deref(block->Item(i)->f);
	delete block->Item(i);
  }

  // Now, symmetrically, grab the samples in block b1 after the
  // deletion point into postBuffer.  If this is enough samples
  // for its own block, or if this would be the last block in
  // the array, write it out.  Otherwise combine it with the
  // subsequent block (splitting them 50/50 if necessary).

  WaveBlock *postBlock = block->Item(b1);
  sampleCount postBufferLen = (postBlock->start+postBlock->len) - (start+len);
  if (postBufferLen) {
	if (postBufferLen >= minSamples || b1==numBlocks-1) {
	  WaveBlock *insBlock = NewInitedWaveBlock();
    
	  insBlock->len = postBufferLen;
	  insBlock->start = start;

	  sampleType *postBuffer = new sampleType[postBufferLen];
	  sampleCount pos = (start+len) - postBlock->start;
	  Read(postBuffer, postBlock, pos, postBufferLen);
	  Write(postBuffer, insBlock, 0, postBufferLen, false);
	  delete[] postBuffer;
    
	  newBlock->Add(insBlock);
	  newNumBlocks++;

	  dirManager->Deref(postBlock->f);
	  delete postBlock;
	}
	else {
	  WaveBlock *postpostBlock = block->Item(b1+1);
	  sampleCount postpostLen = postpostBlock->len;
	  sampleCount sum = postpostLen + postBufferLen;

	  sampleType *sumBuffer = new sampleType[sum];
	  sampleCount pos = (start+len) - postBlock->start;
	  Read(sumBuffer, postBlock, pos, postBufferLen);
	  Read(&sumBuffer[postBufferLen], postpostBlock, 0, postpostLen);

	  BlockArray *split = Blockify(sumBuffer, sum);
	  for(i=0; i<split->Count(); i++) {
	  split->Item(i)->start += start;
		newBlock->Add(split->Item(i));
		newNumBlocks++;
	  }
	  delete split;
	  b1++;

	  delete[] sumBuffer;
	  
	  dirManager->Deref(postpostBlock->f);
	  delete postpostBlock;
	  dirManager->Deref(postBlock->f);
	  delete postBlock;	  
	}
  }
  else {
	// The sample where we begin deletion happens to fall
	// right on the end of a block.

	if (b0 != b1) {
	  dirManager->Deref(block->Item(b1)->f);
	  delete block->Item(b1);
	}
  }

  // Copy the remaining blocks over from the old array

  for(i=b1+1; i<numBlocks; i++) {
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
  int pos=0;
  int numBlocks = block->Count();
  bool error=false;
  
  for(i=0; i<numBlocks; i++) {
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
  int pos=0;
  
  for(i=0; i<block->Count(); i++) {
	printf("Block %3d: start %8d len %8d  %s",
		   i,
		   block->Item(i)->start,
		   block->Item(i)->len,
		   (const char *)(block->Item(i)->f->GetName()));
	if (pos != block->Item(i)->start)
	  printf("  ERROR\n");
	else
	  printf("\n");
	pos += block->Item(i)->len;
  }
  if (pos != numSamples)
	printf("ERROR numSamples = %d\n",numSamples);
}



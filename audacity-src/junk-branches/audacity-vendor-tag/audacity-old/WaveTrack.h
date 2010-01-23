/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WAVETRACK__
#define __AUDACITY_WAVETRACK__

#include <wx/brush.h>
#include <wx/dynarray.h>
#include <wx/pen.h>
#include <wx/string.h>
#include <wx/thread.h>

#include "Track.h"

typedef signed short sampleType;
typedef unsigned int sampleCount;

class DirManager;
class GenericStream;
class BlockFile;

class WaveBlock
{
public:
  BlockFile     *f;
  
  sampleCount   start;
  sampleCount   len;
};

WX_DEFINE_ARRAY(WaveBlock *, BlockArray);

class DisplayCache
{
public:
  bool          dirty;  
  bool          spectrum;

  sampleCount   len;
  double	start;
  double        pps;
  sampleCount   *where;

  // minmax only
  sampleType    *min;
  sampleType    *max;

  // spectrum only
  float         *freq;
};

class WaveTrack: public VTrack
{
public:
  enum {WaveDisplay,
		SpectrumDisplay};

  sampleCount numSamples;
  double rate;

  WaveTrack(DirManager *projDirManager);
  virtual ~WaveTrack();

  virtual VTrack *Duplicate();

  virtual void Cut(double t0, double t1, VTrack **dest);
  virtual void Copy(double t0, double t1, VTrack **dest);
  virtual void Paste(double t, VTrack *src);
  virtual void Clear(double t0, double t1);
  
  virtual void Draw(wxDC &dc, wxRect &r, double h, double pps,
					double sel0, double sel1);

  void DrawMinmax(wxDC &dc, wxRect &r, double h, double pps,
					double sel0, double sel1);

  void DrawSpectrum(wxDC &dc, wxRect &r, double h, double pps,
					double sel0, double sel1);

  virtual void SetDisplay(int d);
  virtual int GetDisplay();

  virtual bool Load(GenericStream *in, DirManager *dirManager);
  virtual bool Save(GenericStream *out, bool overwrite);

  virtual int GetKind() {return Wave;}

  virtual double GetMaxLen();

  sampleType Get(sampleCount pos);
  void Get(sampleType *buffer, sampleCount start, sampleCount len);

  void Set(sampleType *buffer, sampleCount start, sampleCount len);
  void Append(sampleType *buffer, sampleCount len);
  void Delete(sampleCount start, sampleCount len);
  void AppendBlock(WaveBlock *b);

  static sampleCount GetIdealBlockSize();

  BlockArray *GetBlockArray() {return block;}

private:
  int display; // wave/spectrum

  BlockArray *block;

  #if wxUSE_THREADS
  wxMutex *blockMutex;
  #endif
  
  DisplayCache cache;

  wxBrush blankBrush;
  wxBrush unselectedBrush;
  wxBrush selectedBrush;
  wxBrush sampleBrush;
  wxBrush selsampleBrush;
  wxPen blankPen;
  wxPen unselectedPen;
  wxPen selectedPen;
  wxPen samplePen;
  wxPen selsamplePen;

  void Reblockify();
  int FindBlock(sampleCount pos);
  int FindBlock(sampleCount pos, sampleCount lo,
				sampleCount guess, sampleCount hi);
  bool InitBlock(WaveBlock *b);
  void Read(sampleType *buffer, WaveBlock *b,
	    sampleCount start, sampleCount len);
  void Read256(sampleType *buffer, WaveBlock *b,
	       sampleCount start, sampleCount len);
  void Read64K(sampleType *buffer, WaveBlock *b,
	       sampleCount start, sampleCount len);
  void Write(sampleType *buffer, WaveBlock *b,
	     sampleCount start, sampleCount len,
	     bool makeCopy = true);

  void PrepareCache(double start, double pps, int screenWidth);

public:

  // This function makes sure that the track isn't messed up
  // because of inconsistent block starts & lengths
  void ConsistencyCheck(char *whereStr);

  // This function prints information to stdout about the blocks in the
  // tracks and indicates if there are inconsistencies.
  void Debug();

};

#endif




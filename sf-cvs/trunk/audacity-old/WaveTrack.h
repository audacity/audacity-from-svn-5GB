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

#include "Envelope.h"
#include "Track.h"

typedef signed short sampleType;
typedef unsigned int sampleCount;

class DirManager;
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
  static void SetMaxDiskBlockSize(int bytes);

  enum {WaveDisplay,
		SpectrumDisplay};

  sampleCount numSamples;
  double rate;

  WaveTrack(DirManager *projDirManager);
  virtual ~WaveTrack();

  virtual void DeleteButDontDereference();

  virtual VTrack *Duplicate();

  virtual void Cut(double t0, double t1, VTrack **dest);
  virtual void Copy(double t0, double t1, VTrack **dest);
  virtual void Paste(double t, VTrack *src);
  virtual void Clear(double t0, double t1);
  
  virtual void Draw(wxDC &dc, wxRect &r, double h, double pps,
					double sel0, double sel1,
					bool drawEnvelope);

  void DrawMinmax(wxDC &dc, wxRect &r, double h, double pps,
					double sel0, double sel1,
					bool drawEnvelope);

  void DrawSpectrum(wxDC &dc, wxRect &r, double h, double pps,
					double sel0, double sel1,
					bool drawEnvelope);

  virtual void SetDisplay(int d);
  virtual int GetDisplay();

  virtual bool Load(wxTextFile *in, DirManager *dirManager);
  virtual bool Save(wxTextFile *out, bool overwrite);

  virtual int GetKind() {return Wave;}

  virtual void Offset(double t);

  virtual double GetMaxLen();

  double GetRate();
  void SetRate(double newRate);

  Envelope *GetEnvelope() {return &envelope;}

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

  Envelope envelope;
  
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
  wxPen shadowPen;
  wxPen envelopePen;

  void Reblockify();
  int FindBlock(sampleCount pos);
  int FindBlock(sampleCount pos, sampleCount lo,
				sampleCount guess, sampleCount hi);
  WaveBlock *NewInitedWaveBlock();
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

  BlockArray *Blockify(sampleType *buffer, sampleCount len);

  void PrepareCache(double start, double pps, int screenWidth);

  static sampleCount summary64KLen;
  static sampleCount summary256Len;
  static sampleCount totalHeaderLen;
  static sampleCount maxSamples;
  static sampleCount minSamples;

public:

  // This function makes sure that the track isn't messed up
  // because of inconsistent block starts & lengths
  void ConsistencyCheck(char *whereStr);

  // This function prints information to stdout about the blocks in the
  // tracks and indicates if there are inconsistencies.
  void Debug();

};

#endif




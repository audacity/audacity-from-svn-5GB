/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WAVETRACK__
#define __AUDACITY_WAVETRACK__

#include <wx/dynarray.h>
#include <wx/string.h>
#include <wx/thread.h>

#include "Envelope.h"
#include "Track.h"

typedef signed short sampleType;
typedef int sampleCount;

class DirManager;
class BlockFile;

class WaveBlock {
 public:
   BlockFile * f;

   sampleCount start;
   sampleCount len;
   sampleType min;
   sampleType max;
};

WX_DEFINE_ARRAY(WaveBlock *, BlockArray);

class WaveTrack:public VTrack {
 public:
   friend class TrackArtist;

   static void SetMaxDiskBlockSize(int bytes);
   static int  GetMaxDiskBlockSize();

   enum { WaveDisplay,
      SpectrumDisplay
   };

   WaveTrack(DirManager * projDirManager);
   WaveTrack(const WaveTrack &orig);
   virtual ~ WaveTrack();

   // Locks all of this track's BlockFiles, keeping them
   // from being moved.  See BlockFile.h for details.
   virtual void Lock();
   virtual void Unlock();

   virtual VTrack *Duplicate() const { return new WaveTrack(*this); }

   virtual void Cut  (double t0, double t1, VTrack ** dest);
   virtual void Copy (double t0, double t1, VTrack ** dest) const;
   virtual void Clear(double t0, double t1);
   virtual void Paste(double t, const VTrack * src);

   virtual void Silence(double t0, double t1);
   virtual void InsertSilence(double t, double len);

   virtual void SetDisplay(int d);
   virtual int GetDisplay() const;

   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);

   virtual int GetKind() const { return Wave; } 
   virtual void Offset(double t);

   virtual double GetMaxLen() const;

   void GetMinMax(sampleCount start, sampleCount len, sampleType * min,
                  sampleType * max) const;

   double GetRate() const;
   void SetRate(double newRate);

   sampleCount GetNumSamples() const { return numSamples; }
   void SetNumSamples(sampleCount sc) { numSamples = sc; }

   Envelope *GetEnvelope() { return &envelope; }
   const Envelope *GetEnvelope() const { return &envelope; }

   sampleType Get(sampleCount pos) const;
   void Get(sampleType * buffer, sampleCount start, sampleCount len) const;

   void Set(sampleType * buffer, sampleCount start, sampleCount len);
   void Append(sampleType * buffer, sampleCount len);
   void Delete(sampleCount start, sampleCount len);
   void AppendBlock(WaveBlock * b);

   void AppendAlias(wxString fullPath,
                    sampleCount start, sampleCount len, int channel);

   static sampleCount GetMaxBlockSize();
   static sampleCount GetIdealBlockSize();
   static int GetHeaderLen();
   sampleCount GetBestBlockSize(sampleCount start) const;

   BlockArray *GetBlockArray() const { return block; }

 private:
   int display;                 // wave/spectrum

   BlockArray *block;

   sampleCount numSamples;
   double rate;

#if wxUSE_THREADS
   wxMutex *blockMutex;
#endif

   Envelope envelope;

   void Reblockify();
   int FindBlock(sampleCount pos) const;
   int FindBlock(sampleCount pos, sampleCount lo,
                 sampleCount guess, sampleCount hi) const;
   WaveBlock *NewInitedWaveBlock();
   bool InitBlock(WaveBlock * b);

   void Read(sampleType * buffer, WaveBlock * b,
             sampleCount start, sampleCount len) const;
   void Read256(sampleType * buffer, WaveBlock * b,
                sampleCount start, sampleCount len) const;
   void Read64K(sampleType * buffer, WaveBlock * b,
                sampleCount start, sampleCount len) const;

   // These are the two ways to write data to a block
   void FirstWrite(sampleType * buffer, WaveBlock * b, sampleCount len);
   void CopyWrite(sampleType * buffer, WaveBlock * b,
                  sampleCount start, sampleCount len);

   // Both block-writing methods and AppendAlias call this
   // method to write the summary data
   void UpdateSummaries(sampleType * buffer, WaveBlock * b,
                        sampleCount len);

   BlockArray *Blockify(sampleType * buffer, sampleCount len);

   static sampleCount summary64KLen;
   static sampleCount summary256Len;
   static sampleCount totalHeaderLen;
   static sampleCount maxSamples;
   static sampleCount minSamples;

 public:

   // This function makes sure that the track isn't messed up
   // because of inconsistent block starts & lengths
   void ConsistencyCheck(const char *whereStr);

   // This function prints information to stdout about the blocks in the
   // tracks and indicates if there are inconsistencies.
   void Debug();
   void DebugPrintf(wxString *dest);

};

#endif

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
typedef unsigned int sampleCount;

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

   enum { WaveDisplay,
      SpectrumDisplay
   };

   sampleCount numSamples;
   double rate;

   WaveTrack(DirManager * projDirManager);
   virtual ~ WaveTrack();

   virtual void DeleteButDontDereference();

   virtual VTrack *Duplicate();

   virtual void Cut(double t0, double t1, VTrack ** dest);
   virtual void Copy(double t0, double t1, VTrack ** dest);
   virtual void Paste(double t, VTrack * src);
   virtual void Clear(double t0, double t1);

   virtual void Silence(double t0, double t1);
   virtual void InsertSilence(double t, double len);

   virtual void SetDisplay(int d);
   virtual int GetDisplay();

   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);

   virtual int GetKind() {
      return Wave;
   } virtual void Offset(double t);

   virtual double GetMaxLen();

   void GetMinMax(sampleCount start, sampleCount len, sampleType * min,
                  sampleType * max);

   double GetRate();
   void SetRate(double newRate);

   Envelope *GetEnvelope() {
      return &envelope;
   }

   sampleType Get(sampleCount pos);
   void Get(sampleType * buffer, sampleCount start, sampleCount len);

   void Set(sampleType * buffer, sampleCount start, sampleCount len);
   void Append(sampleType * buffer, sampleCount len);
   void Delete(sampleCount start, sampleCount len);
   void AppendBlock(WaveBlock * b);

   void AppendAlias(wxString fullPath,
                    sampleCount start, sampleCount len, int channel);

   static sampleCount GetMaxBlockSize();
   static sampleCount GetIdealBlockSize();
   static int GetHeaderLen();
   sampleCount GetBestBlockSize(sampleCount start);

   BlockArray *GetBlockArray() {
      return block;
   }

 private:
   int display;                 // wave/spectrum

   BlockArray *block;

#if wxUSE_THREADS
   wxMutex *blockMutex;
#endif

   Envelope envelope;

   void Reblockify();
   int FindBlock(sampleCount pos);
   int FindBlock(sampleCount pos, sampleCount lo,
                 sampleCount guess, sampleCount hi);
   WaveBlock *NewInitedWaveBlock();
   bool InitBlock(WaveBlock * b);
   void Read(sampleType * buffer, WaveBlock * b,
             sampleCount start, sampleCount len);
   void Read256(sampleType * buffer, WaveBlock * b,
                sampleCount start, sampleCount len);
   void Read64K(sampleType * buffer, WaveBlock * b,
                sampleCount start, sampleCount len);

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

};

#endif

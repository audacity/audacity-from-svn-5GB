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

#include "SampleFormat.h"
#include "Track.h"

typedef int sampleCount;

class DirManager;
class BlockFile;
class Envelope;

class WaveBlock {
 public:
   BlockFile * f;

   sampleCount start;
   sampleCount len;
   short       min;
   short       max;
   short       rms;
};

class SummaryInfo {
 public:
   int            bytesPerFrame;
   sampleCount    frames64K;
   int            offset64K;
   sampleCount    frames256;
   int            offset256;
   int            totalSummaryBytes;
};

WX_DEFINE_ARRAY(WaveBlock *, BlockArray);

class WaveTrack:public VTrack {
 public:
   friend class TrackArtist;

   static void SetMaxDiskBlockSize(int bytes);
   int  GetSummaryBytes() const;

   sampleCount GetMaxBlockSize() const;
   sampleCount GetIdealBlockSize() const;

   enum {
      WaveformDisplay,
      WaveformDBDisplay,
      SpectrumDisplay,
      PitchDisplay
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

   // XMLTagHandler callback methods

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

#if LEGACY_PROJECT_FILE_SUPPORT
   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);
#endif

   virtual int GetKind() const { return Wave; } 
   virtual void SetOffset(double t);   

   virtual double GetMaxLen() const;

   sampleFormat GetSampleFormat() const;
   bool SetSampleFormat(sampleFormat format);
   void ConvertToSampleFormat(sampleFormat format);

   void GetMinMax(sampleCount start, sampleCount len,
                  float * min, float * max) const;

   double GetRate() const;
   void SetRate(double newRate);

   sampleCount GetNumSamples() const { return mNumSamples; }
   void SetNumSamples(sampleCount sc) { mNumSamples = sc; }

   Envelope *GetEnvelope() { return mEnvelope; }
   const Envelope *GetEnvelope() const { return mEnvelope; }

   float Get(sampleCount pos) const;
   void  Get(float * buffer, sampleCount start, sampleCount len) const;
   void  Get(samplePtr buffer, sampleFormat format,
             sampleCount start, sampleCount len) const;

   // Pass NULL to set silence
   void Set(float * buffer,
            sampleCount start, sampleCount len);
   void Set(samplePtr buffer, sampleFormat format,
            sampleCount start, sampleCount len);

   void Append(samplePtr buffer, sampleFormat format, sampleCount len);
   void Delete(sampleCount start, sampleCount len);
   void AppendBlock(WaveBlock * b);

   void AppendAlias(wxString fullPath,
                    sampleCount start, sampleCount len, int channel);

   sampleCount GetBestBlockSize(sampleCount start) const;

   BlockArray *GetBlockArray() const { return mBlock; }

 private:
   static int    sMaxDiskBlockSize;

   BlockArray   *mBlock;
   sampleFormat  mSampleFormat;
   sampleCount   mNumSamples;
   double        mRate;
   Envelope     *mEnvelope;
   SummaryInfo   mSummary;

   sampleCount   mMinSamples;
   sampleCount   mMaxSamples;

   // On-screen display info
   int mDisplay; // wave/spectrum

 private:
   void CalcSummaryInfo();

   int FindBlock(sampleCount pos) const;
   int FindBlock(sampleCount pos, sampleCount lo,
                 sampleCount guess, sampleCount hi) const;
   WaveBlock *NewInitedWaveBlock();

   void Read(samplePtr buffer, sampleFormat format,
             WaveBlock * b,
             sampleCount start, sampleCount len) const;
   void Read256(short * buffer, WaveBlock * b,
                sampleCount start, sampleCount len) const;
   void Read64K(short * buffer, WaveBlock * b,
                sampleCount start, sampleCount len) const;

   // These are the two ways to write data to a block
   void FirstWrite(samplePtr buffer, WaveBlock * b, sampleCount len);
   void CopyWrite(samplePtr buffer, WaveBlock * b,
                  sampleCount start, sampleCount len);

   // Both block-writing methods and AppendAlias call this
   // method to write the summary data
   void UpdateSummaries(samplePtr buffer, WaveBlock * b,
                        sampleCount len);

   BlockArray *Blockify(samplePtr buffer, sampleCount len);

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

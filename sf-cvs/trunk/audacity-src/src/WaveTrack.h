/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WAVETRACK__
#define __AUDACITY_WAVETRACK__

#include "Track.h"
#include "SampleFormat.h"
#include "Sequence.h"

#include <wx/longlong.h>

typedef wxLongLong_t longSampleCount; /* 64-bit int */

class Envelope;
class WaveCache;
class SpecCache;

class WaveTrack: public Track {

 public:

   //
   // Constructor / Destructor / Duplicator
   //

   WaveTrack(DirManager * projDirManager, sampleFormat format = floatSample);
   WaveTrack(WaveTrack &orig);
   virtual ~WaveTrack();

   virtual Track *Duplicate();

   //
   // Identifying the type of track
   //

   virtual int GetKind() const { return Wave; } 

   //
   // WaveTrack parameters
   //

   double GetRate() const;
   void SetRate(double newRate);

   virtual double GetOffset() const;
   virtual void SetOffset(double t);

   virtual double GetStartTime();
   virtual double GetEndTime();

   sampleFormat GetSampleFormat() {return mSequence->GetSampleFormat();}
   bool ConvertToSampleFormat(sampleFormat format);

   // GET RID OF THESE
   enum {
      WaveformDisplay,
      WaveformDBDisplay,
      SpectrumDisplay,
      PitchDisplay
   } WaveTrackDisplay;
   void SetDisplay(int display) {mDisplay = display;}
   int GetDisplay() {return mDisplay;}

   //
   // High-level editing
   //

   virtual bool Cut  (double t0, double t1, Track **dest);
   virtual bool Copy (double t0, double t1, Track **dest);
   virtual bool Clear(double t0, double t1);
   virtual bool Paste(double t, const Track *src);

   virtual bool Silence(double t0, double t1);
   virtual bool InsertSilence(double t, double len);

   //
   // Getting high-level data from the track for screen display and
   // clipping calculations
   //

   bool GetWaveDisplay(float *min, float *max, float *rms, sampleCount *where,
                       int numPixels, double t0, double pixelsPerSecond);
   bool GetSpectrogram(float *buffer, sampleCount *where,
                       int numPixels, int height,
                       double t0, double pixelsPerSecond,
                       bool autocorrelation);
   bool GetMinMax(float *min, float *max, double t0, double t1);

   //
   // Getting/setting samples.  The sample counts here are
   // expressed relative to t=0.0 at the track's sample rate.
   //

   bool Get(samplePtr buffer, sampleFormat format,
            longSampleCount start, sampleCount len);
   bool Set(samplePtr buffer, sampleFormat format,
            longSampleCount start, sampleCount len);

   bool Append(samplePtr buffer, sampleFormat format, sampleCount len);
   bool AppendAlias(wxString fName, sampleCount start,
                    sampleCount len, int channel);

   //
   // Getting information about the track's internal block sizes
   // for efficiency
   //

   sampleCount GetBestBlockSize(longSampleCount t);
   sampleCount GetMaxBlockSize() const;
   sampleCount GetIdealBlockSize() const;

   //
   // XMLTagHandler callback methods for loading and saving
   //

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

   //
   // Lock and unlock the track: you must lock the track before
   // doing a copy and paste between projects.
   //

   bool Lock();
   bool Unlock();

   //
   // Access the track's amplitude envelope
   //

   Envelope *GetEnvelope() { return mEnvelope; }

   // Utility function to convert between times in seconds
   // and sample positions

   longSampleCount TimeToLongSamples(double t0);

   //
   // Temporary - to be removed after TrackArtist is deleted:
   //

   Sequence *GetSequence() { return mSequence; }

 protected:

   //
   // Protected variables
   //

   Sequence     *mSequence;
   double        mRate;
   Envelope     *mEnvelope;
   samplePtr     mAppendBuffer;
   int           mAppendBufferLen;

   WaveCache    *mWaveCache;
   SpecCache    *mSpecCache;

   // GET RID OF THIS
   int           mDisplay;

   //
   // Protected methods
   //

   bool TimeToSamples(double t0, sampleCount *s0);
   void TimeToSamplesClip(double t0, sampleCount *s0);

   bool Flush();

};

#endif // __AUDACITY_WAVETRACK__

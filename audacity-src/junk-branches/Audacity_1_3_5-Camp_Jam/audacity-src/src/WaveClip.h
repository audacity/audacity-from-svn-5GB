/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveClip.h

  ?? Dominic Mazzoni
  ?? Markus Meyer

*******************************************************************/

#ifndef __AUDACITY_WAVECLIP__
#define __AUDACITY_WAVECLIP__

#include "Audacity.h"
#include "SampleFormat.h"
#include "Sequence.h"
#include "xml/XMLTagHandler.h"

#include <wx/gdicmn.h>
#include <wx/longlong.h>
#include <wx/list.h>
#include <wx/msgdlg.h>

typedef wxLongLong_t longSampleCount; /* 64-bit int */

class Envelope;
class WaveCache;
class SpecCache;
class AUDACITY_DLL_API WaveClip;
class AUDACITY_DLL_API WaveClipList;

class SpecPxCache {
public:
   SpecPxCache(int cacheLen)
   {
      len = cacheLen;
      values = new float[len];
      valid = false;
   }

   ~SpecPxCache()
   {
      delete[] values;
   }

   sampleCount  len;
   float       *values;
   bool         valid;
};

WX_DECLARE_LIST(WaveClip, WaveClipList);
WX_DEFINE_ARRAY_PTR(WaveClip*, WaveClipArray);

class AUDACITY_DLL_API WaveClip: public XMLTagHandler
{
private:
   WaveClip(WaveClip& orig)
   {
      wxMessageBox(wxT("Fatal error - please report to audacity-devel@lists.sourceforge.net\n"));
   }

public:
   // typical constructor
   WaveClip(DirManager *projDirManager, sampleFormat format, int rate);

   // essentially a copy constructor - but you must pass in the
   // current project's DirManager, because we might be copying
   // from one project to another
   WaveClip(WaveClip& orig, DirManager *projDirManager);

   virtual ~WaveClip();

   void ConvertToSampleFormat(sampleFormat format);

   void TimeToSamplesClip(double t0, longSampleCount *s0) const;
   int GetRate() const { return mRate; }
   
   // Set rate without resampling. This will change the length of the clip
   void SetRate(int rate) { mRate = rate; MarkChanged(); }
   
   // Resample clip. This also will set the rate, but without changing
   // the length of the clip
   bool Resample(int rate, bool progress = false);
   
   void SetOffset(double offset);
   double GetOffset() const { return mOffset; }
   void Offset(double delta) { SetOffset(GetOffset() + delta); }
   double GetStartTime() const;
   double GetEndTime() const;
   longSampleCount GetStartSample() const;
   longSampleCount GetEndSample() const;
   int GetNumSamples() const { return mSequence->GetNumSamples(); }

   bool GetSamples(samplePtr buffer, sampleFormat format,
                   longSampleCount start, sampleCount len) const;
   bool SetSamples(samplePtr buffer, sampleFormat format,
                   longSampleCount start, sampleCount len);
 
   Envelope* GetEnvelope() { return mEnvelope; }
   BlockArray* GetSequenceBlockArray() { return mSequence->GetBlockArray(); }

   // Get low-level access to the sequence. Whenever possible, don't use this,
   // but use more high-level functions inside WaveClip (or add them if you
   // think they are useful for general use)
   Sequence* GetSequence() { return mSequence; }

   // WaveTrack calls this whenever data in the wave clip changes
   // It is called automatically when WaveClip has a chance to know that
   // something has changed, like when member functions SetSamples() etc.
   // are called.
   void MarkChanged() { mDirty++; }

   // Create clip from copy, discarding previous information in the clip
   bool CreateFromCopy(double t0, double t1, WaveClip* other);

   //
   // Getting high-level data from the for screen display and
   // clipping calculations
   //
   bool GetWaveDisplay(float *min, float *max, float *rms, sampleCount *where,
                       int numPixels, double t0, double pixelsPerSecond);
   bool GetSpectrogram(float *buffer, sampleCount *where,
                       int numPixels,
                       double t0, double pixelsPerSecond,
                       bool autocorrelation);
   bool GetMinMax(float *min, float *max, double t0, double t1);

   // Set/clear/get rectangle that this WaveClip fills on screen. This is
   // called by TrackArtist while actually drawing the tracks and clips.
   void ClearDisplayRect();
   void SetDisplayRect(const wxRect& r);
   void GetDisplayRect(wxRect* r);

   // Whenever you do an operation to the sequence that will change the number
   // of samples (that is, the length of the clip), you will want to call
   // this function to tell the envelope about it.
   void UpdateEnvelopeTrackLen();

   /// You must call Flush after the last Append
   bool Append(samplePtr buffer, sampleFormat format,
               sampleCount len, unsigned int stride=1,
               XMLWriter* blockFileLog = NULL);
   /// Flush must be called after last Append
   bool Flush();

   bool AppendAlias(wxString fName, sampleCount start,
                    sampleCount len, int channel);

   /// This name is consistent with WaveTrack::Clear. It performs a "Cut"
   /// operation (but without putting the cutted audio to the clipboard)
   bool Clear(double t0, double t1);

   // Clear, and add cut line that starts at t0 and contains everything until t1.
   bool ClearAndAddCutLine(double t0, double t1);

   // Paste data from other clip, resampling it if not equal rate
   bool Paste(double t0, WaveClip* other);

   // Insert silence - note that this is an efficient operation for
   // large amounts of silence
   bool InsertSilence(double t, double len);

   // Get access to cut lines list
   WaveClipList* GetCutLines() { return &mCutLines; }
   
   // Find cut line at (approximately) this position
   // Returns true and fills in cutLineStart and cutLineEnd (if specified)
   // if a cut line at this position could be found. Return false otherwise.
   bool FindCutLine(double cutLinePosition,
                    double* cutLineStart = NULL,
                    double *cutLineEnd = NULL);

   // Expand cut line (that is, re-insert audio, then delete audio saved in cut line)
   // Returns true if a cut line could be found and sucessfully expanded,
   // false otherwise
   bool ExpandCutLine(double cutLinePosition);

   // Remove cut line, without expanding the audio in it
   bool RemoveCutLine(double cutLinePosition);
   void RemoveAllCutLines();
   
   // Offset cutlines right to time 't0' by time amount 'len'
   void OffsetCutLines(double t0, double len);
   
   // Lock all blockfiles
   void Lock();
   
   // Unlock all blockfiles
   void Unlock();

   //
   // XMLTagHandler callback methods for loading and saving
   //

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual void HandleXMLEndTag(const wxChar *tag);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

   // Cache of values to colour pixels of Spectrogram - used by TrackArtist
   SpecPxCache    *mSpecPxCache;

protected:
   wxRect mDisplayRect;

   double mOffset;
   int mRate;
   int mDirty;
   bool mIsCutLine;
   Sequence *mSequence;
   Envelope *mEnvelope;

   WaveCache    *mWaveCache;
   SpecCache    *mSpecCache;

   samplePtr     mAppendBuffer;
   int           mAppendBufferLen;

   // Cut Lines are nothing more than ordinary wave clips, with the
   // offset relative to the start of the clip.
   WaveClipList mCutLines;
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/intl.h>

#include <math.h>

#include "WaveTrack.h"

#include "Envelope.h"
#include "Sequence.h"
#include "Spectrum.h"

#include "Prefs.h"
#include "Internat.h"

#include "AudioIO.h"

WaveTrack *TrackFactory::NewWaveTrack(sampleFormat format, double rate)
{
   if (format == (sampleFormat)0) 
   {
      format = (sampleFormat) gPrefs->
         Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), int16Sample);
   }
   if (rate == 0) 
   {
      rate = (double) gPrefs->
         Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate());
   }

   return new WaveTrack(mDirManager, format, rate);
}

WaveTrack::WaveTrack(DirManager *projDirManager, sampleFormat format, double rate):
   Track(projDirManager)
{
   if (format == (sampleFormat)0) 
   {
      format = (sampleFormat) gPrefs->
         Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), int16Sample);
   }
   if (rate == 0) 
   {
      rate = (double) gPrefs->
         Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate());
   }

   mDisplay = 0; // Move to GUIWaveTrack

   mFormat = format;
   mRate = rate;
   mGain = 1.0;
   mPan = 0.0;
   SetName(_("Audio Track"));
   mDisplayMin = -1.0;
   mDisplayMax = 1.0;
   mDisplayNumCutLines = 0;
   mDisplayCutLines = NULL;
   mDisplayNumCutLinesAllocated = 0;
}

WaveTrack::WaveTrack(WaveTrack &orig):
   Track(orig)
{
   mDisplay = 0; // Move to GUIWaveTrack

   Init(orig);

   for (WaveClipList::Node *node = orig.mClips.GetFirst(); node; node = node->GetNext())
      mClips.Append(new WaveClip(*node->GetData()));
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack &orig)
{
   Track::Init(orig);
   mFormat = orig.mFormat;
   mRate = orig.mRate;
   mGain = orig.mGain;
   mPan = orig.mPan;
   SetName(orig.GetName());
   mDisplay = orig.mDisplay;
   mDisplayMin = orig.mDisplayMin;
   mDisplayMax = orig.mDisplayMax;
   mDisplayNumCutLines = 0;
   mDisplayCutLines = NULL;
   mDisplayNumCutLinesAllocated = 0;
}

void WaveTrack::Merge(const Track &orig)
{
   if (orig.GetKind() == Wave)
      mDisplay = ((WaveTrack &)orig).mDisplay;
   Track::Merge(orig);
}

WaveTrack::~WaveTrack()
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      delete it->GetData();
   mClips.Clear();
   if (mDisplayCutLines)
      delete mDisplayCutLines;
}

double WaveTrack::GetOffset()
{
   return GetStartTime();
}

void WaveTrack::SetOffset(double o)
{
   double delta = o - GetOffset();

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      clip->SetOffset(clip->GetOffset() + delta);
   }

   mOffset = o;
}

void WaveTrack::GetDisplayBounds(float *min, float *max)
{
   *min = mDisplayMin;
   *max = mDisplayMax;
}

void WaveTrack::SetDisplayBounds(float min, float max)
{
   mDisplayMin = min;
   mDisplayMax = max;
}

Track *WaveTrack::Duplicate()
{
   return new WaveTrack(*this);
}

double WaveTrack::GetRate() const
{
   return mRate;
}

void WaveTrack::SetRate(double newRate)
{
   mRate = newRate;
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->SetRate(newRate);
}

float WaveTrack::GetGain() const
{
   return mGain;
}

void WaveTrack::SetGain(float newGain)
{
   mGain = newGain;
}

float WaveTrack::GetPan() const
{
   return mPan;
}

void WaveTrack::SetPan(float newPan)
{
   if (newPan > 1.0)
      mPan = 1.0;
   else if (newPan < -1.0)
      mPan = -1.0;
   else
      mPan = newPan;
}

float WaveTrack::GetChannelGain(int channel)
{
   float left = 1.0;
   float right = 1.0;

   if (mPan < 0)
      right = (mPan + 1.0);
   else if (mPan > 0)
      left = 1.0 - mPan;

   if ((channel%2) == 0)
      return left*mGain;
   else
      return right*mGain;
}

bool WaveTrack::ConvertToSampleFormat(sampleFormat format)
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->ConvertToSampleFormat(format);
   mFormat = format;

   return true;
}

bool WaveTrack::Cut(double t0, double t1, Track **dest)
{
   if (t1 < t0)
      return false;

   // Cut is the same as 'Copy', then 'Delete'
   if (!Copy(t0, t1, dest))
      return false;
   return Clear(t0, t1);
}

bool WaveTrack::CutAndAddCutLine(double t0, double t1, Track **dest)
{
   if (t1 < t0)
      return false;

   // Cut is the same as 'Copy', then 'Delete'
   if (!Copy(t0, t1, dest))
      return false;
   return ClearAndAddCutLine(t0, t1);
}

bool WaveTrack::Copy(double t0, double t1, Track **dest)
{
   *dest = NULL;

   if (t1 <= t0)
      return false;

   WaveTrack *newTrack = new WaveTrack(mDirManager);

   newTrack->Init(*this);

   WaveClipList::Node* it;
   
   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      if (t0 <= clip->GetStartTime() && t1 >= clip->GetEndTime())
      {
         // Whole clip is in copy region
         //printf("copy: clip %i is in copy region\n", (int)clip);
         
         WaveClip *newClip = new WaveClip(*clip);
         newClip->RemoveAllCutLines();
         newClip->Offset(-t0);
         newTrack->mClips.Append(newClip);
      } else
      if (t1 > clip->GetStartTime() && t0 < clip->GetEndTime())
      {
         // Clip is affected by command
         //printf("copy: clip %i is affected by command\n", (int)clip);
         
         WaveClip *newClip = new WaveClip(*clip);
         newClip->RemoveAllCutLines();
         double clip_t0 = t0;
         double clip_t1 = t1;
         if (clip_t0 < clip->GetStartTime())
            clip_t0 = clip->GetStartTime();
         if (clip_t1 > clip->GetEndTime())
            clip_t1 = clip->GetEndTime();

         //printf("copy: clip_t0=%f, clip_t1=%f\n", clip_t0, clip_t1);

         newClip->Offset(-t0);
         if (newClip->GetOffset() < 0)
            newClip->SetOffset(0);

         //printf("copy: clip offset is now %f\n", newClip->GetOffset());
            
         if (!newClip->CreateFromCopy(clip_t0, clip_t1, clip))
         {
            //printf("paste: CreateFromCopy(%f, %f, %i) returns false, quitting\n",
            //   clip_t0, clip_t1, (int)clip);

            return false;
         }

         newTrack->mClips.Append(newClip);
      }
   }

   *dest = newTrack;

   return true;
}

bool WaveTrack::Paste(double t0, Track *src)
{
   //printf("paste: entering WaveTrack::Paste\n");
   
   if (src->GetKind() != Track::Wave)
      return false;

   //printf("paste: we have a wave track\n");

   WaveTrack* other = (WaveTrack*)src;

   //
   // Pasting is a bit complicated, because with the existence of multiclip mode,
   // we must guess the behaviour the user wants.
   //
   // Currently, two modes are implemented:
   //
   // - If a single clip should be pasted, and it should be pasted inside another
   //   clip, no new clips are generated. The audio is simply inserted.
   //   This resembles the old (pre-multiclip support) behaviour. However, if
   //   the clip is pasted outside of any clip, a new clip is generated. This is
   //   the only behaviour which is different to what was done before, but it
   //   shouldn't confuse users too much.
   //
   // - If multiple clips should be pasted, these are always pasted as single
   //   clips, and the current clip is splitted, when necessary. This may seem
   //   strange at first, but it probably is better than trying to auto-merge
   //   anything. The user can still merge the clips by hand (which should be
   //   a simple command reachable by a hotkey or single mouse click).
   //

   if (other->GetNumClips() == 0)
      return false;

   //printf("paste: we have at least one clip\n");

   // Make room for the pasted data
   WaveClipList::Node* it;

   double insertDuration = other->GetEndTime();

   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();

      //printf("paste: ofsetting already existing clip %i by %f seconds\n", (int)clip, insertDuration);

      if (clip->GetStartTime() > t0)
         clip->Offset(insertDuration);
   }

   if (other->GetNumClips() == 1)
   {
      // Single clip mode
      // printf("paste: checking for single clip mode!\n");
      
      WaveClip *insideClip = NULL;

      for (it=GetClipIterator(); it; it=it->GetNext())
      {
         WaveClip *clip = it->GetData();

         if (t0+src->GetEndTime() >= clip->GetStartTime() && t0 <= clip->GetEndTime())
         {
            insideClip = clip;
            break;
         }
      }

      if (insideClip)
      {
         // Exhibit traditional behaviour
         // printf("paste: traditional behaviour\n");
         return insideClip->Paste(t0, other->GetClipByIndex(0));
      }

      // Just fall through and exhibit new behaviour
   }

   // Insert new clips
   // printf("paste: multi clip mode!\n");
   
   for (it=other->GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();

      WaveClip* newClip = new WaveClip(*clip);
      newClip->Offset(t0);
      newClip->MarkChanged();
      mClips.Append(newClip);
   }
   return true;
}

bool WaveTrack::Clear(double t0, double t1)
{
   return ClearWithCutLines(t0, t1, false);
}

bool WaveTrack::ClearAndAddCutLine(double t0, double t1)
{
   return ClearWithCutLines(t0, t1, true);
}

bool WaveTrack::ClearWithCutLines(double t0, double t1, bool addCutLines)
{
   if (t1 < t0)
      return false;

   WaveClipList::Node* it;
   WaveClipList clipsToDelete;

   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      if (t0 <= clip->GetStartTime() && t1 >= clip->GetEndTime())
      {
         // Whole clip must be deleted - remember this
         clipsToDelete.Append(clip);
      } else
      if (t1 > clip->GetStartTime() && t0 < clip->GetEndTime())
      {
         // Clip data is affected by command
         if (addCutLines)
         {
            if (!clip->ClearAndAddCutLine(t0,t1))
               return false;
         } else
         {
            if (!clip->Clear(t0,t1))
               return false;
         }
      } else
      if (clip->GetStartTime() >= t1)
      {
         // Clip is "behind" the region -- offset it
         clip->Offset(-(t1-t0));
      }
   }

   for (it=clipsToDelete.GetFirst(); it; it=it->GetNext())
   {
      mClips.DeleteObject(it->GetData());
      delete it->GetData();
   }

   return true;
}

bool WaveTrack::Silence(double t0, double t1)
{
   if (t1 < t0)
      return false;

   longSampleCount start = (longSampleCount)floor(t0 * mRate + 0.5);
   longSampleCount len = (longSampleCount)floor(t1 * mRate + 0.5) - start;
   bool result = true;

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      longSampleCount clipStart = clip->GetStartSample();
      longSampleCount clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         sampleCount samplesToCopy = start+len - clipStart;
         if (samplesToCopy > clip->GetNumSamples())
            samplesToCopy = clip->GetNumSamples();
         longSampleCount inclipDelta = 0;
         longSampleCount startDelta = clipStart - start;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            startDelta = 0;
         }

         if (!clip->GetSequence()->SetSilence(inclipDelta, samplesToCopy))
         {
            wxASSERT(false); // should always work
            return false;
         }
         clip->MarkChanged();
      }
   }

   return result;
}

bool WaveTrack::InsertSilence(double t, double len)
{
   if (len <= 0)
      return false;

   if (mClips.IsEmpty())
   {
      // Special case if there is no clip yet
      WaveClip* clip = CreateClip();
      bool result = clip->GetSequence()->InsertSilence(0, (sampleCount)floor(len * mRate + 0.5));
      if (result)
      {
         clip->GetEnvelope()->InsertSpace(0, len);
         clip->MarkChanged();
      }
      return result;
   }

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();
      if (clip->GetStartTime() > t)
         clip->Offset(len);
      else if (clip->GetEndTime() > t)
      {
         longSampleCount s0;
         clip->TimeToSamplesClip(t, &s0);
         sampleCount slen = (sampleCount)floor(len * mRate + 0.5);
         
         if (!clip->GetSequence()->InsertSilence(s0, slen))
         {
            wxASSERT(false);
            return false;
         }
         clip->GetEnvelope()->InsertSpace(t, len);
         clip->MarkChanged();
      }
   }

   return true;
}

bool WaveTrack::Append(samplePtr buffer, sampleFormat format,
                       sampleCount len, unsigned int stride /* = 1 */)
{
   return GetLastOrCreateClip()->Append(buffer, format, len, stride);
}

bool WaveTrack::AppendAlias(wxString fName, sampleCount start,
                            sampleCount len, int channel)
{
   return GetLastOrCreateClip()->AppendAlias(fName, start, len, channel);
}

sampleCount WaveTrack::GetBestBlockSize(longSampleCount s)
{
   sampleCount bestBlockSize = GetMaxBlockSize();

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      longSampleCount startSample = (longSampleCount)floor(clip->GetStartTime()*mRate + 0.5);
      longSampleCount endSample = startSample + clip->GetNumSamples();
      if (s >= startSample && s < endSample)
      {
         bestBlockSize = clip->GetSequence()->GetMaxBlockSize();
         break;
      }
   }

   return bestBlockSize;
}

sampleCount WaveTrack::GetMaxBlockSize()
{
   int maxblocksize = 0;
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();
      if (clip->GetSequence()->GetMaxBlockSize() > maxblocksize)
         maxblocksize = clip->GetSequence()->GetMaxBlockSize();
   }

   if (maxblocksize == 0)
   {
      // We really need the maximum block size, so create a
      // temporary sequence to get it.
      Sequence *tempseq = new Sequence(mDirManager, mFormat);
      maxblocksize = tempseq->GetMaxBlockSize();
      delete tempseq;
   }

   wxASSERT(maxblocksize > 0);

   return maxblocksize;
}

sampleCount WaveTrack::GetIdealBlockSize()
{
   wxASSERT(GetClipIterator()); // must have sequence
   return GetClipIterator()->GetData()->GetSequence()->GetIdealBlockSize();
}

bool WaveTrack::Flush()
{
   return GetLastOrCreateClip()->Flush();
}

bool WaveTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("wavetrack"))) {
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value)
            break;
         
         if (!wxStrcmp(attr, wxT("rate")))
            mRate = wxAtoi(value);
         else if (!wxStrcmp(attr, wxT("offset"))) {
            double ofs = 0.0;
            Internat::CompatibleToDouble(wxString(value), &ofs);
            GetLastOrCreateClip()->SetOffset(ofs);
         }
         else if (!wxStrcmp(attr, wxT("gain"))) {
            double d;
            Internat::CompatibleToDouble(wxString(value), &d);
            mGain = d;
         }
         else if (!wxStrcmp(attr, wxT("pan"))) {
            double d;
            Internat::CompatibleToDouble(wxString(value), &d);
            if (d >= -1.0 && d <= 1.0)
               mPan = d;
         }
         else if (!wxStrcmp(attr, wxT("name")))
            mName = value;
         else if (!wxStrcmp(attr, wxT("channel")))
            mChannel = wxAtoi(value);
         else if (!wxStrcmp(attr, wxT("linked")))
            mLinked = wxAtoi(value) != 0;
         
      } // while
      return true;
   }

   return false;
}

void WaveTrack::HandleXMLEndTag(const wxChar *tag)
{
   // In case we opened a pre-multiclip project, we need to
   // simulate closing the waveclip tag.
   GetLastOrCreateClip()->HandleXMLEndTag( wxT("waveclip") );
}

XMLTagHandler *WaveTrack::HandleXMLChild(const wxChar *tag)
{
   //
   // This is legacy code (1.2 and previous) and is not called for new projects!
   //
   if (!wxStrcmp(tag, wxT("sequence")))
      return GetLastOrCreateClip()->GetSequence();
   else if (!wxStrcmp(tag, wxT("envelope")))
      return GetLastOrCreateClip()->GetEnvelope();
   
   //
   // This is for the new file format (post-1.2)
   //
   if (!wxStrcmp(tag, wxT("waveclip")))
      return CreateClip();
   else
      return NULL;
}

void WaveTrack::WriteXML(int depth, FILE *fp)
{
   int i;

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<wavetrack ");
   fprintf(fp, "name=\"%s\" ", (const char *)XMLEsc(mName).mb_str());
   fprintf(fp, "channel=\"%d\" ", mChannel);
   fprintf(fp, "linked=\"%d\" ", mLinked);
   fprintf(fp, "offset=\"%s\" ", (const char *)Internat::ToString(mOffset, 8).mb_str());
   fprintf(fp, "rate=\"%s\" ", (const char *)Internat::ToString(mRate).mb_str());
   fprintf(fp, "gain=\"%s\" ", (const char *)Internat::ToString((double)mGain).mb_str());
   fprintf(fp, "pan=\"%s\" ", (const char *)Internat::ToString((double)mPan).mb_str());
   fprintf(fp, ">\n");
   
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      it->GetData()->WriteXML(depth+1, fp);
   }

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "</wavetrack>\n");
}

bool WaveTrack::GetErrorOpening()
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      if (it->GetData()->GetSequence()->GetErrorOpening())
         return true;

   return false;
}

bool WaveTrack::Lock()
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->GetSequence()->Lock();

   return true;
}

bool WaveTrack::Unlock()
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      it->GetData()->GetSequence()->Unlock();

   return true;
}

longSampleCount WaveTrack::TimeToLongSamples(double t0)
{
   return (longSampleCount)floor(t0 * mRate + 0.5);
}

double WaveTrack::GetStartTime()
{
   bool found = false;
   double best;

   if (mClips.IsEmpty())
      return 0;

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      if (!found)
      {
         found = true;
         best = it->GetData()->GetStartTime();
      } else if (it->GetData()->GetStartTime() < best)
         best = it->GetData()->GetStartTime();

   return best;
}

double WaveTrack::GetEndTime()
{
   bool found = false;
   double best;

   if (mClips.IsEmpty())
      return 0;

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      if (!found)
      {
         found = true;
         best = it->GetData()->GetEndTime();
      } else if (it->GetData()->GetEndTime() > best)
         best = it->GetData()->GetEndTime();

   return best;
}

//
// Getting/setting samples.  The sample counts here are
// expressed relative to t=0.0 at the track's sample rate.
//

bool WaveTrack::GetMinMax(float *min, float *max,
                          double t0, double t1)
{
   *min = float(0.0);
   *max = float(0.0);

   if (t0 > t1)
      return false;

   if (t0 == t1)
      return true;

   bool result = true;

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* clip = it->GetData();

      if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      {
         float clipmin, clipmax;
         if (it->GetData()->GetMinMax(&clipmin, &clipmax, t0, t1-t0))
         {
            if (clipmin < *min)
               *min = clipmin;
            if (clipmax > *max)
               *max = clipmax;
         } else
         {
            result = false;
         }
      }
   }

   return result;
}

bool WaveTrack::Get(samplePtr buffer, sampleFormat format,
                    longSampleCount start, sampleCount len)
{
   // Simple optimization: When this buffer is completely contained within one clip,
   // don't clear anything (because we never won't have to). Otherwise, just clear
   // everything to be on the safe side.
   WaveClipList::Node* it;
   
   bool doClear = true;
   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();
      if (start >= clip->GetStartSample() && start+len <= clip->GetEndSample())
      {
         doClear = false;
         break;
      }
   }
   if (doClear)
      ClearSamples(buffer, format, 0, len);

   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      longSampleCount clipStart = clip->GetStartSample();
      longSampleCount clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         sampleCount samplesToCopy = start+len - clipStart;
         if (samplesToCopy > clip->GetNumSamples())
            samplesToCopy = clip->GetNumSamples();
         longSampleCount inclipDelta = 0;
         longSampleCount startDelta = clipStart - start;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            startDelta = 0;
         }

         if (!clip->GetSamples((samplePtr)(((char*)buffer)+startDelta*SAMPLE_SIZE(format)),
                               format, inclipDelta, samplesToCopy))
         {
            wxASSERT(false); // should always work
            return false;
         }
      }
   }

   return true;
}

bool WaveTrack::Set(samplePtr buffer, sampleFormat format,
                    longSampleCount start, sampleCount len)
{
   bool result = true;

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();

      longSampleCount clipStart = clip->GetStartSample();
      longSampleCount clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         sampleCount samplesToCopy = start+len - clipStart;
         if (samplesToCopy > clip->GetNumSamples())
            samplesToCopy = clip->GetNumSamples();
         longSampleCount inclipDelta = 0;
         longSampleCount startDelta = clipStart - start;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            startDelta = 0;
         }

         if (!clip->SetSamples((samplePtr)(((char*)buffer)+startDelta*SAMPLE_SIZE(format)),
                               format, inclipDelta, samplesToCopy))
         {
            wxASSERT(false); // should always work
            return false;
         }
         clip->MarkChanged();
      }
   }

   return result;
}

void WaveTrack::GetEnvelopeValues(double *buffer, int bufferLen,
                         double t0, double tstep)
{
   memset(buffer, 0, sizeof(double)*bufferLen);

   double startTime = t0;
   double endTime = t0+tstep*bufferLen;

   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip = it->GetData();
      
      if (clip->GetStartTime() < endTime && clip->GetEndTime() > startTime)
      {
         double* rbuf = buffer;
         int rlen = bufferLen;
         double rt0 = t0;

         if (rt0 < clip->GetStartTime())
         {
            int dx = floor((clip->GetStartTime() - rt0) / tstep + 0.5);
            rbuf += dx;
            rlen -= dx;
            rt0 = clip->GetStartTime();
         }

         if (rt0+rlen*tstep > clip->GetEndTime())
         {
            rlen = (clip->GetEndTime()-rt0) / tstep;
         }

         clip->GetEnvelope()->GetValues(rbuf, rlen, rt0, tstep);
      }
   }
}

WaveClip* WaveTrack::GetClipAtX(int xcoord)
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      wxRect r;
      it->GetData()->GetDisplayRect(&r);
      if (xcoord >= r.x && xcoord < r.x+r.width)
         return it->GetData();
   }

   return NULL;
}

Envelope* WaveTrack::GetEnvelopeAtX(int xcoord)
{
   WaveClip* clip = GetClipAtX(xcoord);
   if (clip)
      return clip->GetEnvelope();
   else
      return NULL;
}

Sequence* WaveTrack::GetSequenceAtX(int xcoord)
{
   WaveClip* clip = GetClipAtX(xcoord);
   if (clip)
      return clip->GetSequence();
   else
      return NULL;
}

WaveClip* WaveTrack::CreateClip()
{
   WaveClip* clip = new WaveClip(mDirManager, mFormat, mRate);
   mClips.Append(clip);
   return clip;
}

WaveClip* WaveTrack::GetLastOrCreateClip()
{
   if (mClips.IsEmpty())
      return CreateClip();
   else
      return mClips.GetLast()->GetData();
}

int WaveTrack::GetClipIndex(WaveClip* clip)
{
   return mClips.IndexOf(clip);
}

WaveClip* WaveTrack::GetClipByIndex(int index)
{
   return mClips.Item(index)->GetData();
}

int WaveTrack::GetNumClips() const
{
   return mClips.GetCount();
}

void WaveTrack::MoveClipToTrack(int clipIndex, WaveTrack* dest)
{
   WaveClipList::Node* node = mClips.Item(clipIndex);
   WaveClip* clip = node->GetData();
   mClips.DeleteNode(node);
   dest->mClips.Append(clip);
}

bool WaveTrack::CanOffsetClip(WaveClip* clip, double amount, double *allowedAmount /* = NULL */)
{
   if (allowedAmount)
      *allowedAmount = amount;
   
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* c = it->GetData();
      if (c != clip && c->GetStartTime() < clip->GetEndTime()+amount &&
                       c->GetEndTime() > clip->GetStartTime()+amount)
      {
         if (!allowedAmount)
            return false; // clips overlap

         if (amount > 0)
         {
            if (c->GetStartTime()-clip->GetEndTime() < *allowedAmount)
               *allowedAmount = c->GetStartTime()-clip->GetEndTime();
            if (*allowedAmount < 0)
               *allowedAmount = 0;
         } else
         {
            if (c->GetEndTime()-clip->GetStartTime() > *allowedAmount)
               *allowedAmount = c->GetEndTime()-clip->GetStartTime();
            if (*allowedAmount > 0)
               *allowedAmount = 0;
         }
      }
   }

   if (allowedAmount)
   {
      if (*allowedAmount == amount)
         return true;

      // Check if the new calculated amount would not violate any other constraint
      if (!CanOffsetClip(clip, *allowedAmount, NULL))
         *allowedAmount = 0; // play safe and don't allow anything
      return false;
   } else
      return true;
}

bool WaveTrack::CanInsertClip(WaveClip* clip)
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* c = it->GetData();
      if (c->GetStartTime() < clip->GetEndTime() && c->GetEndTime() > clip->GetStartTime())
         return false; // clips overlap
   }

   return true;
}

bool WaveTrack::SplitAt(double t)
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip* c = it->GetData();
      if (t > c->GetStartTime() && t < c->GetEndTime())
      {
         WaveClip* newClip = new WaveClip(*c);
         if (!c->Clear(t, c->GetEndTime()))
         {
            delete newClip;
            return false;
         }
         if (!newClip->Clear(c->GetStartTime(), t))
         {
            delete newClip;
            return false;
         }
         newClip->Offset(t - c->GetStartTime());
         mClips.Append(newClip);
         return true;
      }
   }

   return true;
}

void WaveTrack::UpdateCutLinesCache()
{
   WaveClipList::Node* it;
   mDisplayNumCutLines = 0;

   for (it=GetClipIterator(); it; it=it->GetNext())
      mDisplayNumCutLines += it->GetData()->GetCutLines()->GetCount();

   if (mDisplayNumCutLines == 0)
      return;

   if (mDisplayNumCutLines > mDisplayNumCutLinesAllocated)
   {
      // Only realloc, if we need more space than before. Otherwise
      // just use block from before.
      if (mDisplayCutLines)
         delete mDisplayCutLines;
      mDisplayCutLines = new double[mDisplayNumCutLines];
      mDisplayNumCutLinesAllocated = mDisplayNumCutLines;
   }

   int curpos = 0;

   for (it=GetClipIterator(); it; it=it->GetNext())
   {
      WaveClipList* cutlines = it->GetData()->GetCutLines();
      for (WaveClipList::Node* jt = cutlines->GetFirst(); jt; jt=jt->GetNext())
      {
         mDisplayCutLines[curpos] = jt->GetData()->GetOffset() + it->GetData()->GetOffset();
         curpos++;
      }
   }
}

// Expand cut line (that is, re-insert audio, then delete audio saved in cut line)
bool WaveTrack::ExpandCutLine(double cutLinePosition)
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      if (it->GetData()->ExpandCutLine(cutLinePosition))
         return true;

   return false;
}

bool WaveTrack::RemoveCutLine(double cutLinePosition)
{
   for (WaveClipList::Node* it=GetClipIterator(); it; it=it->GetNext())
      if (it->GetData()->RemoveCutLine(cutLinePosition))
         return true;

   return false;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 8cf4eb04-e9b7-4ca5-acd1-aecf564c11d2


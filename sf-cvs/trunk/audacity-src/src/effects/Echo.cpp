/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/generic/textdlgg.h>
#include <wx/intl.h>
#include <math.h>

#include "Echo.h"
#include "../WaveTrack.h"

EffectEcho::EffectEcho()
{
   delay = float(1.0);
   decay = float(0.5);
}

wxString EffectEcho::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   return wxString::Format(_("Applied effect: %s delay = %f seconds, decay factor = %f"), 
                           this->GetEffectName(), delay, decay); 
} 

bool EffectEcho::PromptUser()
{
   wxString temp;
   wxString title = _("Echo");
   wxString caption = _("Delay time (seconds): ");
   wxString default_value = wxString::Format("%f", delay);

   temp = wxGetTextFromUser(caption, title,
                            default_value, mParent, -1, -1, TRUE);
   if (temp == "")
      return false;
   while (sscanf((const char *) temp, "%f", &delay) < 0) {
      caption = _("Please enter a positive number for the delay time: ");
      temp = wxGetTextFromUser(caption, title,
                               default_value, mParent, -1, -1, TRUE);
      if (temp == "")
         return false;
   }

   caption = _("Enter the decay factor: ");
   default_value = wxString::Format("%f", decay);
   temp = wxGetTextFromUser(caption, title,
                            default_value, mParent, -1, -1, TRUE);
   if (temp == "")
      return false;
   while (sscanf((const char *) temp, "%f", &decay) < 0) {
      caption = _("Please enter a positive number for the decay factor: ");
      temp = wxGetTextFromUser(caption, title,
                               default_value, mParent, -1, -1, TRUE);
      if (temp == "")
         return false;
   }

   return true;
}

bool EffectEcho::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         longSampleCount start = track->TimeToLongSamples(t0);
         longSampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if (!ProcessOne(count, track, start, len))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}

bool EffectEcho::ProcessOne(int count, WaveTrack * track,
                            longSampleCount start, sampleCount len)
{
   sampleCount s = 0;
   sampleCount blockSize = (sampleCount) (track->GetRate() * delay);
   
   //do nothing if the delay is less than 1 sample or greater than
   //the length of the selection
   if (blockSize < 1 || blockSize > len)
      return true;

   float *buffer0 = new float[blockSize];
   float *buffer1 = new float[blockSize];

   float *ptr0 = buffer0;
   float *ptr1 = buffer1;

   bool first = true;

   while (s < len) {
      sampleCount block = blockSize;
      if (s + block > len)
         block = len - s;

      track->Get((samplePtr)ptr0, floatSample, start + s, block);
      if (!first) {
         for (sampleCount i = 0; i < block; i++)
            ptr0[i] += ptr1[i] * decay;
         track->Set((samplePtr)ptr0, floatSample, start + s, block);
      }

      float *ptrtemp = ptr0;
      ptr0 = ptr1;
      ptr1 = ptrtemp;

      first = false;

      s += block;
      
      TrackProgress(count, s / (double) len);
   }

   delete[]buffer0;
   delete[]buffer1;

   return true;
}

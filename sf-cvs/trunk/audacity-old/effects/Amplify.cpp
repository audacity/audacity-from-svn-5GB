/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Robert Leidle
  Dominic Mazzoni

**********************************************************************/

#include <wx/generic/textdlgg.h>

#include "Amplify.h"
#include "../WaveTrack.h"

//
// EffectAmplify
//

EffectAmplify::EffectAmplify()
{
   ratio = 1.0;
}

bool EffectAmplify::PromptUser()
{
   wxString temp;
   wxString caption = "Amplification factor: ";
   wxString title = "Amplify";
   wxString default_value = "1.0";

   temp = wxGetTextFromUser(caption,
                            title, default_value, mParent, -1, -1, TRUE);

   if (temp == "")
      return false;

   while (sscanf((const char *) temp, "%f", &ratio) < 0) {
      caption = "Please enter a value greater than zero: ";
      temp = wxGetTextFromUser("Amplify current selection by:",
                               caption, default_value, mParent, -1, -1,
                               TRUE);
      if (temp == "")
         return false;
   }

   return true;
}

bool EffectAmplify::Process()
{
   TrackListIterator iter(mWaveTracks);
   VTrack *t = iter.First();
   int count = 0;
   while(t) {
      sampleCount start, len;
      GetSamples((WaveTrack *)t, &start, &len);
      bool success = ProcessOne(count, (WaveTrack *)t, start, len);
      
      if (!success)
         return false;
   
      t = iter.Next();
      count++;
   }
   
   return true;
}

bool EffectAmplify::ProcessOne(int count, WaveTrack *t,
                               sampleCount start, sampleCount len)
{
   sampleCount s = start;
   sampleCount originalLen = len;
   sampleCount blockSize = t->GetMaxBlockSize();

   sampleType *buffer = new sampleType[blockSize];

   while (len) {
      int block = t->GetBestBlockSize(s);
      if (block > len)
         block = len;

      t->Get(buffer, s, block);
      for (int i = 0; i < block; i++)
         buffer[i] = (sampleType) (buffer[i] * ratio);
      t->Set(buffer, s, block);

      len -= block;
      s += block;
      
      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[] buffer;

   return true;
}

//
// EffectMaxAmplify
//

EffectMaxAmplify::EffectMaxAmplify()
{
}

bool EffectMaxAmplify::Process()
{
   TrackListIterator iter(mWaveTracks);
   VTrack *t = iter.First();
   int count = 0;
   while(t) {
      sampleCount start, len;
      GetSamples((WaveTrack *)t, &start, &len);
      bool success = ProcessOne(count, (WaveTrack *)t, start, len);
      
      if (!success)
         return false;
   
      t = iter.Next();
      count++;
   }
   
   return true;
}

bool EffectMaxAmplify::ProcessOne(int count, WaveTrack *t,
                                  sampleCount start, sampleCount len)
{
   sampleType min, max;

   t->GetMinMax(start, len, &min, &max);

   float ratio = 32767.0 / (abs(min) > abs(max) ? abs(min) : abs(max));

   if (ratio <= 1.0)
      return true;

   sampleCount s = start;
   sampleCount originalLen = len;
   sampleCount blockSize = t->GetMaxBlockSize();

   sampleType *buffer = new sampleType[blockSize];

   while (len) {
      int block = blockSize;
      if (block > len)
         block = len;

      t->Get(buffer, s, block);
      for (int i = 0; i < block; i++)
         buffer[i] = (sampleType) (buffer[i] * ratio);
      t->Set(buffer, s, block);

      len -= block;
      s += block;
      
      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[]buffer;

   return true;
}

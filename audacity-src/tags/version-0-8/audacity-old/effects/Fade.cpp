/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.cpp

  Robert Leidle

**********************************************************************/

#include <wx/generic/textdlgg.h>

#include "Fade.h"
#include "../WaveTrack.h"

bool EffectFadeIn::DoIt(WaveTrack *t,
			sampleCount start,
			sampleCount len)
{
  sampleCount s = start;
  sampleCount blockSize = t->GetIdealBlockSize();
  
  sampleType *buffer = new sampleType[blockSize];
    
  while((s-start)<len) {
    sampleCount block = blockSize;
    if (block > (len-s))
      block = (len-s);
    
    t->Get(buffer, s, block);
    for(sampleCount i=0; i<block; i++)
      buffer[i] = (sampleType)(buffer[i]
			       * (float)(s+i-start)
			       / (float)(len));
    t->Set(buffer, s, block);
    
    s += block;
  }

  delete[] buffer;

  return true;
}

bool EffectFadeOut::DoIt(WaveTrack *t,
			 sampleCount start,
			 sampleCount len)
{
  sampleCount s = start;
  sampleCount blockSize = t->GetIdealBlockSize();
  
  sampleType *buffer = new sampleType[blockSize];

  while((s-start)<len) {
    sampleCount block = blockSize;
    if (block > (len-s))
      block = (len-s);
    
    t->Get(buffer, s, block);
    for(sampleCount i=0; i<block; i++)
      buffer[i] = (sampleType)(buffer[i]
			       * (float)(len-1-(s+i-start))
			       / (float)(len));
    t->Set(buffer, s, block);
    
    s += block;
  }

  delete[] buffer;

  return true;
}





/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/generic/textdlgg.h>

#include "Echo.h"
#include "../WaveTrack.h"

bool EffectEcho::DoIt(WaveTrack *t,
			 sampleCount start,
			 sampleCount len)
{
  wxString temp;
  wxWindow *parent = NULL;
  wxString title = "Echo";
  wxString caption = "Delay time (seconds): ";
  wxString default_value = "1.0";
  float delay;
  float decay;

  temp = wxGetTextFromUser(caption, title,
			   default_value, parent, -1, -1, TRUE);  
  if (temp == "")
    return false;
  while (sscanf((const char *)temp, "%f", &delay) < 0)
  {
    caption = "Please enter a positive number for the delay time: ";
    temp = wxGetTextFromUser(caption, title,
			     default_value, parent, -1, -1, TRUE);
    if (temp == "") return false;
  }

  caption = "Enter the decay factor: ";
  default_value = "0.5";
  temp = wxGetTextFromUser(caption, title,
			   default_value, parent, -1, -1, TRUE);  
  if (temp == "")
    return false;
  while (sscanf((const char *)temp, "%f", &decay) < 0)
  {
    caption = "Please enter a positive number for the decay factor: ";
    temp = wxGetTextFromUser(caption, title,
			     default_value, parent, -1, -1, TRUE);
    if (temp == "") return false;
  }

  sampleCount s = start;
  sampleCount blockSize = (sampleCount)(t->rate * delay);

  if (blockSize < 1 || blockSize > len)
    return true;
  
  sampleType *buffer0 = new sampleType[blockSize];
  sampleType *buffer1 = new sampleType[blockSize];

  sampleType *ptr0 = buffer0;
  sampleType *ptr1 = buffer1;

  bool first = true;
    
  while(len) {
    sampleCount block = blockSize;
    if (block > len)
      block = len;
    
    t->Get(ptr0, s, block);
    if (!first) {
      for(sampleCount i=0; i<block; i++)
	ptr0[i] += (sampleType)(ptr1[i]*decay);
      t->Set(ptr0, s, block);
    }

    sampleType *ptrtemp = ptr0;
    ptr0 = ptr1;
    ptr1 = ptrtemp;

    first = false;
    
    len -= block;
    s += block;
  }

  delete[] buffer0;
  delete[] buffer1;

  return true;
}






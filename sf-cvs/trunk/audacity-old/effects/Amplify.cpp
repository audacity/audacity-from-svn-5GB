/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Robert Leidle

**********************************************************************/

#include <wx/generic/textdlgg.h>

#include "Amplify.h"
#include "../WaveTrack.h"

EffectAmplify::EffectAmplify()
{
  ratio = 1.0;
}

bool EffectAmplify::Begin(wxWindow *parent)
{
  wxString temp;
  wxString caption = "Amplification factor: ";
  wxString title = "Amplify";
  wxString default_value = "1.0";

  temp = wxGetTextFromUser(caption,
			   title,
			   default_value, parent, -1, -1, TRUE);
  
  if (temp == "")
    return false;
  
  while (sscanf((const char *)temp, "%f", &ratio) < 0)
  {
    caption = "Please enter a value greater than zero: ";
    temp = wxGetTextFromUser("Amplify current selection by:",
			     caption, default_value, parent, -1, -1, TRUE);
    if (temp == "") return false;
  }

  return true;
}

bool EffectAmplify::DoIt(WaveTrack *t,
			 sampleCount start,
			 sampleCount len)
{
  sampleCount s = start;
  sampleCount blockSize = t->GetIdealBlockSize();
  
  sampleType *buffer = new sampleType[blockSize];
    
  while(len) {
    int block = blockSize;
    if (block > len)
      block = len;
    
    t->Get(buffer, s, block);
    for(int i=0; i<block; i++)
      buffer[i] = (sampleType)(buffer[i] * ratio);
    t->Set(buffer, s, block);
    
    len -= block;
    s += block;
  }

  delete[] buffer;

  return true;
}




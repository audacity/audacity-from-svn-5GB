/**********************************************************************

  Audacity: A Digital Audio Editor

  Filter.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/timer.h>

#include "Mix.h"

#include "WaveTrack.h"
#include "DirManager.h"

WaveTrack *QuickMix(int count, WaveTrack **tracks, DirManager *dirManager)
{
  if (count<2) return 0;

  WaveTrack *mix = new WaveTrack(dirManager);
  mix->rate = tracks[0]->rate;

  int idealLen = mix->GetIdealBlockSize();
  sampleType *buffer = new sampleType[idealLen];
  sampleType *temp = new sampleType[idealLen];

  int pos=0;

  wxStartTimer();

  int participating;
  do {
    participating=0;
    int maxLen = 0;
    for(int i=0; i<count; i++) {
      if (pos < tracks[i]->numSamples) {
	participating++;
	int len = idealLen;
	if (pos+len >= tracks[i]->numSamples)
	  len = tracks[i]->numSamples - pos;

	if (participating==1)
	  tracks[i]->Get(buffer, pos, len);
	else {
	  tracks[i]->Get(temp, pos, len);
	  
	  int split = len;
	  if (maxLen < len)
	    split = maxLen;

	  int x;
	  for(x=0; x<split; x++)
	    buffer[x] += temp[x];
	  for(x=split; x<len; x++)
	    buffer[x] = temp[x];
	}
	if (len > maxLen)
	  maxLen = len;
      }	
    }
    if (maxLen > 0) {
      mix->Append(buffer, maxLen);
    }
    pos += maxLen;
  } while (participating > 0);

  int elapsedMS = wxGetElapsedTime();
  double elapsedTime = elapsedMS * 0.001;
  double dataTime = pos / mix->rate;
  double maxTracks = dataTime / (elapsedTime/count);

  printf("      Tracks: %d\n",count);
  printf("  Mix length: %lf sec\n",dataTime);
  printf("Elapsed time: %lf sec\n",elapsedTime);
  printf("Max number of tracks to mix in real time: %lf\n",maxTracks);

  delete[] buffer;
  delete[] temp;

  return mix;
}





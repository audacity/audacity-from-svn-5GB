/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni
  
  An effect for the "Generator" menu to add silence.

**********************************************************************/

#include <wx/defs.h> 

#include "Silence.h"
#include "../WaveTrack.h"

bool EffectSilence::Process()
{
   double length = mT1 - mT0;

   if (length <= 0.0)
      length = sDefaultGenerateLen;

   //Iterate over each track
   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      if (track->GetSelected())
         track->InsertSilence(mT0, length);
      
      //Iterate to the next track
      track = iter.Next();
   }

   return true;
}

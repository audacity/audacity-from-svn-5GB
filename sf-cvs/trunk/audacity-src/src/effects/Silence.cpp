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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 78c8d521-815a-4fdb-830a-f9655cd4f529


/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectSilence
\brief An Effect for the "Generator" menu to add silence.

*//*******************************************************************/


#include "../Audacity.h"

#include <wx/defs.h> 

#include <wx/button.h> 
#include <wx/sizer.h> 
#include <wx/stattext.h> 
#include <wx/textctrl.h> 

#include "Silence.h"
#include "../WaveTrack.h"
#include "../TimeDialog.h"

bool EffectSilence::PromptUser()
{
   TimeDialog dlog(mParent, wxID_ANY, _("Silence Generator"));

   dlog.SetSampleRate(mProjectRate);

   if (mT1 > mT0) {
      // there is a selection: let's fit in there...
      length = mT1 - mT0;
      dlog.SetFormatString(wxT("hh:mm:ss + samples"));

   } else {
      // retrieve last used values
      dlog.SetFormatString(wxT("seconds"));
   }
   dlog.SetTimeValue(length);

   if (dlog.ShowModal() == wxID_CANCEL)
      return false;

   length = dlog.GetTimeValue();

   return true;
}

bool EffectSilence::Process()
{
   if (length <= 0.0)
      return false;
      
   HandleLinkedTracksOnGenerate(length, mT0);

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat(), track->GetRate());
      tmp->InsertSilence(0.0, length);
      tmp->Flush();
      track->ClearAndPaste(mT0, mT1, tmp);
      delete tmp;
      
      //Iterate to the next track
      track = (WaveTrack *)iter.Next();
   }

	mT1 = mT0 + length; // Update selection.
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

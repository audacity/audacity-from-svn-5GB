/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectSilence
\brief An Effect for the "Generator" menu to add silence.

*//****************************************************************//**

\class GenerateDialog
\brief Dialog used with EffectSilence and others..

*//*******************************************************************/


#include "../Audacity.h"

#include <wx/defs.h> 

#include <wx/button.h> 
#include <wx/sizer.h> 
#include <wx/stattext.h> 
#include <wx/textctrl.h> 

#include "Silence.h"
#include "../WaveTrack.h"
#include "../widgets/TimeTextCtrl.h"

bool EffectSilence::PromptUser()
{
   if (mT1 > mT0)
      length = mT1 - mT0;

   TimeDialog dlog(mParent, _("Silence Generator"));
   dlog.SetLength(length);
   dlog.Init();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   length = dlog.GetLength();
   return true;
}

bool EffectSilence::Process()
{
   if (length <= 0.0)
      length = sDefaultGenerateLen;

   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat());
      tmp->SetRate(track->GetRate());
      tmp->InsertSilence(0.0, length);
      tmp->Flush();
      track->Clear(mT0, mT1);
      track->Paste(mT0, tmp);
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

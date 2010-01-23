/**********************************************************************

  Audacity: A Digital Audio Editor

  Repair.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectRepair
\brief Use the interpolation code to fill in damaged audio.
Damage can include pops, clicks, or clipping.  As long as the
damaged section is short and surrounded by lots of good audio,
it is usually quite successful.

This was formerly the PopClickRemoval effect, but it was
renamed and focused on the smaller subproblem of repairing
the audio, rather than actually finding the clicks.

*//*******************************************************************/


#include "../Audacity.h"

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/intl.h>

#include "Repair.h"
#include "../WaveTrack.h"
#include "../InterpolateAudio.h"

EffectRepair::EffectRepair()
{
}

EffectRepair::~EffectRepair()
{
}

bool EffectRepair::PromptUser()
{
   return true;
}

bool EffectRepair::TransferParameters( Shuttle & shuttle )
{ 
   //TODO: pop-click values.
//   shuttle.TransferInt("",,0);
   return true;
}

bool EffectRepair::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double repair_t0 = mT0;
      double repair_t1 = mT1;
      repair_t0 = (repair_t0 < trackStart? trackStart: repair_t0);
      repair_t1 = (repair_t1 > trackEnd? trackEnd: repair_t1);
      double rate = track->GetRate();
      double repair_deltat = repair_t1 - repair_t0;

      double spacing = repair_deltat * 2;

      if (spacing < 128. / rate)
         spacing = 128. / rate;

      double t0 = repair_t0 - spacing;
      double t1 = repair_t1 + spacing;
      
      t0 = t0 < trackStart? trackStart: t0;
      t1 = t1 > trackEnd? trackEnd: t1;

      repair_t0 = (repair_t0 < t0? t0: repair_t0);
      repair_t1 = (repair_t1 > t1? t1: repair_t1);

      longSampleCount s0 = track->TimeToLongSamples(t0);
      longSampleCount repair0 = track->TimeToLongSamples(repair_t0);
      longSampleCount repair1 = track->TimeToLongSamples(repair_t1);
      longSampleCount s1 = track->TimeToLongSamples(t1);

      sampleCount repairStart = (sampleCount)(repair0 - s0);
      sampleCount repairLen = (sampleCount)(repair1 - repair0);
      sampleCount len = (sampleCount)(s1 - s0);

      if (repairLen > 128) {
         ::wxMessageBox(_("The Repair effect is intended to be used on very short sections of damaged audio (up to 128 samples).\n\nZoom in and select a tiny fraction of a second to repair."));
         return false;
      }
      
      if (!ProcessOne(count, track,
                      s0, len, repairStart, repairLen)) {
         return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}

bool EffectRepair::ProcessOne(int count, WaveTrack * track,
                              longSampleCount start,
                              sampleCount len,
                              sampleCount repairStart, sampleCount repairLen)
{
   float *buffer = new float[len];
   track->Get((samplePtr) buffer, floatSample, start, len);
   InterpolateAudio(buffer, len, repairStart, repairLen);
   track->Set((samplePtr)&buffer[repairStart], floatSample,
              start + repairStart, repairLen);
   delete[] buffer;
   return true;
}

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3



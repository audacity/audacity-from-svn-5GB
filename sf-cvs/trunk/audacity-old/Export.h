/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

class WaveTrack;
class DirManager;

bool Export(wxWindow * parent,
            TrackList * tracks, bool selectionOnly, double t0, double t1);

#endif

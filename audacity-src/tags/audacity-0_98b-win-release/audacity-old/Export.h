/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

class AudacityProject;
class DirManager;
class WaveTrack;

bool Export(AudacityProject *project,
            wxString format,
            bool selectionOnly, double t0, double t1);

#endif

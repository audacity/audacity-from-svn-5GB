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
            bool selectionOnly, double t0, double t1);

bool ExportLossy(AudacityProject *project,
                 bool selectionOnly, double t0, double t1);

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: b8dfb802-198d-4917-86dd-f53f29421419


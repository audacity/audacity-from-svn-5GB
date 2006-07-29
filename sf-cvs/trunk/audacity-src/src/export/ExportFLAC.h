/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportFLAC.h

  Frederik M.J.V

**********************************************************************/

#ifndef __AUDACITY_EXPORTFLAC__
#define __AUDACITY_EXPORTFLAC__

class wxString;

class AudacityProject;
class DirManager;
class WaveTrack;

bool ExportFLAC(AudacityProject *project,
               bool stereo, wxString fName,
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


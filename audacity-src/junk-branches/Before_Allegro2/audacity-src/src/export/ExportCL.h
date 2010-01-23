/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportCL.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_EXPORTCL__
#define __AUDACITY_EXPORTCL__

#include <wx/string.h>

class MixerSpec;

bool ExportCL(AudacityProject *project, bool stereo, wxString fName,
              bool selectionOnly, double t0, double t1, 
              MixerSpec *mixerSpec = NULL);

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
// arch-tag: 67f53175-a9e1-49bd-93e8-fde76820ec90


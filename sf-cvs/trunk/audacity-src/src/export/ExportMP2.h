/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP2.h

  Dominic Mazzoni
  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP2__
#define __AUDACITY_EXPORTMP2__

#include <wx/string.h>
#include <wx/dynlib.h>

class AudacityProject;
class MixerSpec;

bool ExportMP2(AudacityProject *project,
               int Channels, wxString fName,
               bool selectionOnly, double t0, double t1, 
               MixerSpec *mixerSpec = NULL);

bool ExportMP2Options(AudacityProject *project);

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
// arch-tag: 697b9941-3e7e-44c1-929e-19d34ed70151


/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOGG.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_EXPORTOGG__
#define __AUDACITY_EXPORTOGG__

class AudacityProject;

#include <wx/string.h>

bool ExportOGG(
   AudacityProject *project,
   bool stereo,
   wxString fName,
   bool selectionOnly,
   double t0, double t1);

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
// arch-tag: 8fe98761-9ab9-4e14-af9a-3ed642cb429e


/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.h

  Dominic Mazzoni

  Audacity uses wxWindows' wxConfig class to handle preferences.
  What it actually does depends on the platform - on Unix it's
  a configuration file in the ".audacity" directory, on the
  Mac it's an "Audacity Preferences" file in their System Folder,
  and on Windows it uses the Windows Registry.

  Every time we read a preference, we need to specify the default
  value for that preference, to be used if the preference hasn't
  been set before.

  So, to avoid code duplication, we provide functions in this file
  to read and write preferences which have a nonobvious default
  value, so that if we later want to change this value, we only
  have to change it in one place.

  See Prefs.cpp for a (complete?) list of preferences we keep
  track of...

**********************************************************************/
#ifndef __AUDACITY_PREFS__
#define __AUDACITY_PREFS__

#include <wx/config.h>

void InitPreferences();
void FinishPreferences();

extern wxConfig *gPrefs;

extern int gMenusDirty;

int ReadExportFormatPref();
void WriteExportFormatPref(int format);


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
// arch-tag: 1550a015-ed6f-47ce-a942-9211cfad50c8


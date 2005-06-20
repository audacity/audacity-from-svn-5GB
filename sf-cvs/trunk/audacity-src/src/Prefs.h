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

const wxString gDbChoices[] = {wxT("-20db"), wxT("-25db"), wxT("-30db"), 
                               wxT("-35db"), wxT("-40db"), wxT("-45db"), 
                               wxT("-50db"), wxT("-55db"), wxT("-60db"),
                               wxT("-65db"), wxT("-70db"), wxT("-75db"), 
                               wxT("-80db"), wxT("Off-Skip")};
//                              -20db    -25db    -30db    -35db    -40db    -45db    -50db    -55db    -60db    -65db     -70db     -75db     -80db    Off
const double gDb2Signal[] = { 0.10000, 0.05620, 0.03160, 0.01780, 0.01000, 0.00562, 0.00316, 0.00178, 0.00100, 0.000562, 0.000316, 0.000178, 0.0001000, 0.0 };
const int    gNumDbChoices = 14;
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


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

**********************************************************************/

#include <wx/config.h>

void InitPreferences();
void FinishPreferences();

extern wxConfig *gPrefs;

extern int gMenusDirty;

int ReadFileFormatPref();
void WriteFileFormatPref(int format);

int ReadFileFormatBitsPref();
void WriteFileFormatBitsPref(int bits);

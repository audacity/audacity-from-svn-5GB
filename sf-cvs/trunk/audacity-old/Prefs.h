/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/config.h>

void InitPreferences();
void FinishPreferences();

extern wxConfig* gPrefs;


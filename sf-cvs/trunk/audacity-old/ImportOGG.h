/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportOGG.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_IMPORT_OGG__
#define __AUDACITY_IMPORT_OGG__

#include <wx/string.h>

#include "DirManager.h"
#include "WaveTrack.h"

bool ImportOGG(wxWindow *parent,
			   wxString Filename, WaveTrack ***channels, int *numChannels,
			   DirManager *dirManager);


#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_MP3__
#define __AUDACITY_IMPORT_MP3__

#include <wx/string.h>

#include "DirManager.h"
#include "WaveTrack.h"

bool ImportMP3(wxWindow *parent,
			   wxString Filename, WaveTrack **dest1, WaveTrack **dest2, 
			   DirManager *dirManager);


#endif

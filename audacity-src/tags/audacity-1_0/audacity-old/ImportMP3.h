/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_MP3__
#define __AUDACITY_IMPORT_MP3__

#include <wx/string.h>

class AudacityProject;
class DirManager;
class WaveTrack;

bool ImportMP3(AudacityProject * project,
               wxString fName, WaveTrack ** left, WaveTrack ** right);


#endif

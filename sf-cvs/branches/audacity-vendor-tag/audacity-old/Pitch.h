/**********************************************************************

  Audacity: A Digital Audio Editor

  Pitch.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_PITCH__
#define __AUDACITY_PITCH__

#include <wx/string.h>

class WaveTrack;
class NoteTrack;
class DirManager;

NoteTrack *PitchExtract(WaveTrack *t, DirManager *dirManager);

#endif





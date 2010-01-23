/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include <wx/string.h>

class WaveTrack;
class DirManager;

WaveTrack *QuickMix(int count, WaveTrack **tracks, DirManager *dirManager);

#endif




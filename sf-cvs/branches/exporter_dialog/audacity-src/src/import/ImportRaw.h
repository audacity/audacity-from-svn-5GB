/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportRaw.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_RAW__
#define __AUDACITY_IMPORT_RAW__

class WaveTrack;
class DirManager;

#include "Import.h"

int ImportRaw(wxWindow *parent, wxString fileName,
              TrackFactory *trackFactory, Track ***outTracks,
              progress_callback_t progressCallback, void *userData);

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportOGG.h

  Joshua Haberman

**********************************************************************/

#ifdef USE_LIBVORBIS

#ifndef __AUDACITY_IMPORT_OGG__
#define __AUDACITY_IMPORT_OGG__

#include <wx/string.h>

class DirManager;
class WaveTrack;

bool ImportOGG(wxWindow * parent,
               wxString Filename, WaveTrack *** channels, int *numChannels,
               DirManager * dirManager);


#endif
#endif

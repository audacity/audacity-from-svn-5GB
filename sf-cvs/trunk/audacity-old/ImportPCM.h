/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _IMPORT_PCM_
#define _IMPORT_PCM_

class WaveTrack;
class DirManager;

wxString TrackNameFromFileName(wxString fName);

bool IsPCM(wxString fName);

bool ImportPCM(wxWindow *parent,
			   wxString fName, WaveTrack **dest1, WaveTrack **dest2,
               DirManager *dirManager);

#endif

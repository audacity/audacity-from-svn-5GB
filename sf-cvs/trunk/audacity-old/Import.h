/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _IMPORT_
#define _IMPORT_

class WaveTrack;
class DirManager;

wxString TrackNameFromFileName(wxString fName);

bool ImportWAV(wxWindow *parent,
			   wxString fName, WaveTrack **dest1, WaveTrack **dest2,
               DirManager *dirManager);

#endif

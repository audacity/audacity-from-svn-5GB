/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTPCM__
#define __AUDACITY_EXPORTPCM__

class WaveTrack;
class DirManager;
class wxString;

bool ExportPCM(wxString format, bool stereo, double rate, wxString fName, 
        wxWindow *parent, TrackList *tracks, bool selectionOnly, double t0, double t1);


#endif

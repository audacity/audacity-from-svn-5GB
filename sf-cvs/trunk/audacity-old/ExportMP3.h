/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP3__
#define __AUDACITY_EXPORTMP3__

bool ExportMP3(bool stereo, double rate, wxString fName, wxWindow *parent, 
        TrackList *tracks, bool selectionOnly, double t0, double t1);

#endif

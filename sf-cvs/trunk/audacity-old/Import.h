/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.h

  Dominic Mazzoni

  This file contains a general function which will import almost
  any type of sampled audio file (i.e. anything except MIDI)
  and return the tracks that were imported.  This function just
  figures out which one to call; the actual importers are in
  ImportPCM, ImportMP3, ImportOGG, and ImportRawData.

**********************************************************************/

#ifndef _IMPORT_
#define _IMPORT_

class WaveTrack;
class DirManager;

wxString TrackNameFromFileName(wxString fName);

// returns number of tracks imported
int Import(wxWindow * parent,
           wxString fName, WaveTrack *** tracks, DirManager * dirManager);

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _IMPORT_PCM_
#define _IMPORT_PCM_

class WaveTrack;
class DirManager;

bool IsPCM(wxString fName);

bool ImportPCM(wxWindow * parent,
               wxString fName,
               WaveTrack ** channels[],
               int *numChannels,
               DirManager * dirManager);

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTPCM__
#define __AUDACITY_EXPORTPCM__

class wxString;

class AudacityProject;
class DirManager;
class WaveTrack;
class MixerSpec;

bool ExportPCM(AudacityProject *project,
               int numChannels, wxString fName,
               bool selectionOnly, double t0, double t1, 
               MixerSpec *mixerSpec = NULL);


#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: daa509db-cfa8-4327-9fd4-b572b3dd814a


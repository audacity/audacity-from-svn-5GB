/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_PREFS__
#define __AUDACITY_AUDIO_IO_PREFS__

#include <wx/string.h>

#include "PrefsPanel.h"

class wxWindow;
class wxCheckBox;
class wxChoice;

class AudioIOPrefs:public PrefsPanel {

 public:
   AudioIOPrefs(wxWindow * parent);
   ~AudioIOPrefs();
   bool Apply();

 private:
   wxString  mRecDevice;
   wxString  mPlayDevice;
   
   wxCheckBox *mDuplex;

   wxCheckBox *mPlaythrough; // Currently Mac OS X only

   wxChoice *mRecChoice;
   wxChoice *mPlayChoice;

   wxChoice *mChannelsChoice;

 public:
    DECLARE_EVENT_TABLE()

};

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
// arch-tag: df22b108-e989-4ec4-a8b6-dddbcc7be6a7


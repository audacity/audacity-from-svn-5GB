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

   wxChoice *mRecChoice;
   wxChoice *mPlayChoice;

   wxChoice *mChannelsChoice;

 public:
    DECLARE_EVENT_TABLE()

};

#endif

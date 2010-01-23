/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_PREFS__
#define __AUDACITY_AUDIO_IO_PREFS__

#include <wx/window.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/statbox.h>
#include <wx/event.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>

#include "PrefsPanel.h"

class AudioIOPrefs:public PrefsPanel {

 public:
   AudioIOPrefs(wxWindow * parent);
   ~AudioIOPrefs();
   bool Apply();

 private:
   wxString  mRecDevice;
   wxString  mPlayDevice;
   
   wxCheckBox *mRecordStereo;
   wxCheckBox *mDuplex;

   wxChoice *mRecChoice;
   wxChoice *mPlayChoice;

 public:
    DECLARE_EVENT_TABLE()

};

#endif

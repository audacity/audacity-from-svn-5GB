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

   void TestPlaybackDevice(wxCommandEvent & event);
   void SetPlaybackDeviceDefault(wxCommandEvent & event);
   void TestRecordingDevice(wxCommandEvent & event);
   void SetRecordingDeviceDefault(wxCommandEvent & event);

   void OnRecordingDeviceChoice(wxCommandEvent & event);

 private:
   wxString  mPlayDevice;
   wxString  mRecDevice;
   
   wxCheckBox *mRecordStereo;
   wxCheckBox *mDuplex;

   #ifdef __WXGTK__   
   wxTextCtrl *mPlaybackDeviceCtrl;
   wxButton *mPlaybackDeviceTest;
   wxButton *mPlaybackDeviceDefault;

   wxTextCtrl *mRecordingDeviceCtrl;
   wxButton *mRecordingDeviceTest;
   wxButton *mRecordingDeviceDefault;
   #endif
   
   #ifdef __WXMSW__   
   wxChoice *mPlaybackDeviceCtrl;
   wxButton *mPlaybackDeviceTest;
   wxButton *mPlaybackDeviceVol;

   wxChoice *mRecordingDeviceCtrl;
   wxButton *mRecordingDeviceTest;
   wxButton *mRecordingDeviceVol;
   #endif

   #ifdef __WXMAC__
   wxChoice *mRecordingDeviceChoice;
   wxChoice *mRecordingInputChoice;
   wxChoice *mPlaybackDeviceChoice;

   void GetRecordingDevices(int *theCount, int *theSelected, wxString **theNames);
   void GetRecordingInputs(int deviceNum, int *theCount, int *theSelected, wxString **theNames);
   void GetPlaybackDevices(int *theCount, int *theSelected, wxString **theNames);
   #endif // __WXMAC__

 public:
    DECLARE_EVENT_TABLE()

};

#endif

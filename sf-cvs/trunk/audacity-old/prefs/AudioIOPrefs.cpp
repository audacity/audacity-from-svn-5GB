/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/window.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/gdicmn.h>
#include <wx/statbox.h>
#include <wx/msgdlg.h>
#include <wx/button.h>
#include <wx/utils.h>
#include <wx/sizer.h>

#include "../Prefs.h"
#include "AudioIOPrefs.h"

enum {
   PlaybackTestID = 1000,
   PlaybackDefaultID,
   RecordingTestID,
   RecordingDefaultID
};

BEGIN_EVENT_TABLE(AudioIOPrefs, wxPanel)
   EVT_BUTTON(PlaybackTestID, AudioIOPrefs::TestPlaybackDevice)
   EVT_BUTTON(PlaybackDefaultID, AudioIOPrefs::SetPlaybackDeviceDefault)
   EVT_BUTTON(RecordingTestID, AudioIOPrefs::TestRecordingDevice)
   EVT_BUTTON(RecordingDefaultID, AudioIOPrefs::SetRecordingDeviceDefault)
END_EVENT_TABLE()

AudioIOPrefs::AudioIOPrefs(wxWindow * parent):
PrefsPanel(parent)
{

   /* read prefs all at once, then set up the dialog */
   gPrefs->SetPath("/AudioIO");
   wxString playDevice = gPrefs->Read("PlaybackDevice", "/dev/dsp");
   wxString recDevice = gPrefs->Read("RecordingDevice", "/dev/dsp");
   bool recordStereo;
   gPrefs->Read("RecordStereo", &recordStereo, false);
   bool duplex;
   gPrefs->Read("Duplex", &duplex, false);
   gPrefs->SetPath("/");


   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this,
                      -1,
                     "Audio I/O Settings"),
      wxVERTICAL);

#ifdef __WXGTK__


   {
      wxStaticBoxSizer *playbackSizer =
          new wxStaticBoxSizer(
            new wxStaticBox(this, -1, "Playback Device"),
            wxVERTICAL);

      {
         wxBoxSizer *pFileSizer = new wxBoxSizer(wxHORIZONTAL);

         mPlaybackDeviceCtrl = new wxTextCtrl(this, -1, playDevice);

         pFileSizer->Add(
            new wxStaticText(this, -1, "Device:"), 0, 
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

         pFileSizer->Add(mPlaybackDeviceCtrl, 1, 
            wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

         playbackSizer->Add(pFileSizer, 0,
            wxGROW|wxALL, GENERIC_CONTROL_BORDER);
      }


      {
         wxBoxSizer *pButtonsSizer = new wxBoxSizer(wxHORIZONTAL);

         mPlaybackDeviceTest = new wxButton(this, PlaybackTestID, "Test");
         mPlaybackDeviceDefault = new wxButton(this,
                                               PlaybackDefaultID,
                                               "Default");

         pButtonsSizer->Add(
            mPlaybackDeviceTest, 0,
            wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,
            GENERIC_CONTROL_BORDER);

         pButtonsSizer->Add(
            mPlaybackDeviceDefault, 0,
            wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,
            GENERIC_CONTROL_BORDER);

         playbackSizer->Add(pButtonsSizer, 0,
            wxALIGN_RIGHT|wxLEFT|wxRIGHT, GENERIC_CONTROL_BORDER);
      }

      topSizer->Add(playbackSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);
   }



   {
      wxStaticBoxSizer *recordingSizer =
          new wxStaticBoxSizer(
            new wxStaticBox(this, -1, "Recording Device"),
            wxVERTICAL);

      {

         wxBoxSizer *rFileSizer = new wxBoxSizer(wxHORIZONTAL);

         mRecordingDeviceCtrl = new wxTextCtrl(this, -1, recDevice);

         rFileSizer->Add(
            new wxStaticText(this, -1, "Device:"), 0,
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

         rFileSizer->Add(mRecordingDeviceCtrl, 1,
            wxGROW|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

         recordingSizer->Add(rFileSizer, 0,
            wxGROW|wxALL, GENERIC_CONTROL_BORDER);
      }

      {
         wxBoxSizer *rButtonsSizer = new wxBoxSizer(wxHORIZONTAL);

         mRecordingDeviceTest = new wxButton(this,
                                             RecordingTestID, "Test");
         mRecordingDeviceDefault = new wxButton(this,
                                                RecordingDefaultID,
                                                "Default");

         rButtonsSizer->Add(
            mRecordingDeviceTest, 0,
            wxGROW|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,
            GENERIC_CONTROL_BORDER);

         rButtonsSizer->Add(mRecordingDeviceDefault, 0,
            wxGROW|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,
            GENERIC_CONTROL_BORDER);

         recordingSizer->Add(rButtonsSizer, 0,
            wxALIGN_RIGHT|wxLEFT|wxRIGHT, GENERIC_CONTROL_BORDER);
      }

      topSizer->Add(recordingSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);
   }

#endif                          // __WXGTK__


   mRecordStereo = new wxCheckBox(this, -1, "Record in Stereo");
   mRecordStereo->SetValue(recordStereo);
   topSizer->Add(mRecordStereo, 0, wxGROW);



   mDuplex = new wxCheckBox(this, -1, "Play While Recording");
   mDuplex->SetValue(duplex);
   topSizer->Add(mDuplex, 0, wxGROW);

   SetAutoLayout(true);
   topSizer->Fit(this);
   topSizer->SetSizeHints(this);
   SetSizer(topSizer);

}


bool AudioIOPrefs::Apply()
{
   /* Step 1: Validate input */
#ifdef __WXGTK__
   wxString playDevice = mPlaybackDeviceCtrl->GetValue();
   if (!wxFileExists(playDevice) || wxDirExists(playDevice)) {
      wxMessageBox("Invalid playback device.", "Error",
                   wxOK | wxCENTRE | wxICON_EXCLAMATION);
      return false;
   }

   wxString recDevice = mRecordingDeviceCtrl->GetValue();
   if (!wxFileExists(recDevice) || wxDirExists(recDevice)) {
      wxMessageBox("Invalid recording device.", "Error",
                   wxOK | wxCENTRE | wxICON_EXCLAMATION);
      return false;
   }
#endif                          // __WXGTK__

   bool recordStereo = mRecordStereo->GetValue();
   bool duplex = mDuplex->GetValue();

   /* Step 2: Write to gPrefs */
   gPrefs->SetPath("/AudioIO");
#ifdef __WXGTK__
   gPrefs->Write("PlaybackDevice", playDevice);
   gPrefs->Write("RecordingDevice", recDevice);
#endif                          // __WXGTK__
   gPrefs->Write("RecordStereo", recordStereo);
   gPrefs->Write("Duplex", duplex);
   gPrefs->SetPath("/");

   /* Step 3: Make audio sub-system re-read preferences */

   return true;
}


void AudioIOPrefs::TestPlaybackDevice(wxCommandEvent & event)
{
}

void AudioIOPrefs::SetPlaybackDeviceDefault(wxCommandEvent & event)
{
   /* TODO: attempt autodetection? */
#ifdef __WXGTK__
   mPlaybackDeviceCtrl->SetValue("/dev/dsp");
#endif                          // __WXGTK__
}

void AudioIOPrefs::TestRecordingDevice(wxCommandEvent & event)
{
}

void AudioIOPrefs::SetRecordingDeviceDefault(wxCommandEvent & event)
{
#ifdef __WXGTK__
   mRecordingDeviceCtrl->SetValue("/dev/dsp");
#endif                          // __WXGTK__
}

AudioIOPrefs::~AudioIOPrefs()
{
#ifdef __WXGTK__
   delete mPlaybackDeviceCtrl;
   delete mPlaybackDeviceTest;
   delete mPlaybackDeviceDefault;
   delete mRecordingDeviceCtrl;
   delete mRecordingDeviceTest;
   delete mRecordingDeviceDefault;
#endif                          // __WXGTK__

   delete mRecordStereo;
   delete mDuplex;
}

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

#include "../Prefs.h"
#include "AudioIOPrefs.h"

enum {
	PlaybackTestID    = 1000,
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

AudioIOPrefs::AudioIOPrefs(wxWindow *parent):
	PrefsPanel(parent)
{ 

	mEnclosingBox = new wxStaticBox(this,
									-1, 
									"Audio I/O Settings",
									wxPoint(0, 0),
									GetSize());

	gPrefs->SetPath("/AudioIO");
	wxString playDevice = gPrefs->Read("PlaybackDevice", "/dev/dsp");
	wxString recDevice  = gPrefs->Read("RecordingDevice", "/dev/dsp");
	bool recordStereo;
	gPrefs->Read("RecordStereo", &recordStereo, false);
	bool duplex;
	gPrefs->Read("Duplex", &duplex, false);
	gPrefs->SetPath("/");

	
	mPlaybackDeviceLabel  = new wxStaticText(this, 
											-1, 
											"Playback Device:",
											wxPoint(PREFS_SIDE_MARGINS,
													PREFS_TOP_MARGIN + 3));

	mPlaybackDeviceCtrl   = new wxTextCtrl  (this,
											-1,
											playDevice,
											wxPoint(100, PREFS_TOP_MARGIN),
											wxSize(80, 20));

	mPlaybackDeviceTest   = new wxButton    (this,
											PlaybackTestID,
											"Test",
											wxPoint(190, PREFS_TOP_MARGIN),
											wxSize (60, 20));
	
	mPlaybackDeviceDefault= new wxButton    (this,
											PlaybackDefaultID,
											"Default",
											wxPoint(255,
											PREFS_TOP_MARGIN),
											wxSize (60, 20));

#define LINE_TWO_TOP 40 
	mRecordingDeviceLabel = new wxStaticText(this,
											-1,
											"Recording Device:",
											wxPoint(PREFS_SIDE_MARGINS,
													LINE_TWO_TOP + 3));
	
	mRecordingDeviceCtrl  = new wxTextCtrl  (this,
											-1,
											recDevice,
											wxPoint(100,
											LINE_TWO_TOP),
											wxSize(80, 20));
	
	mRecordingDeviceTest  = new wxButton    (this,
											RecordingTestID,
											"Test",
											wxPoint(190, LINE_TWO_TOP),
											wxSize (60, 20));
	
	mRecordingDeviceDefault= new wxButton   (this,
											RecordingDefaultID,
											"Default",
											wxPoint(255, LINE_TWO_TOP),
											wxSize (60, 20));

	mRecordStereo = new wxCheckBox(this, -1,
				       "Record in Stereo",
				       wxPoint(PREFS_SIDE_MARGINS,
					       PREFS_TOP_MARGIN + 65),
				       wxSize(GetSize().GetWidth() -
					      PREFS_SIDE_MARGINS * 2,
					      15));

	mRecordStereo->SetValue(recordStereo);

	mDuplex = new wxCheckBox(this, -1,
				 "Play While Recording",
				 wxPoint(PREFS_SIDE_MARGINS,
					 PREFS_TOP_MARGIN + 85),
				 wxSize(GetSize().GetWidth() -
					PREFS_SIDE_MARGINS * 2,
					15));
	mDuplex->SetValue(duplex);
}


bool AudioIOPrefs::Apply()
{
	/* Step 1: Validate input */
	wxString playDevice = mPlaybackDeviceCtrl->GetValue();
	if(!wxFileExists(playDevice) || wxDirExists(playDevice)) {
		wxMessageBox("Invalid playback device.", "Error", 
			wxOK | wxCENTRE | wxICON_EXCLAMATION);
		return false;
	}

	wxString recDevice = mRecordingDeviceCtrl->GetValue();
	if(!wxFileExists(recDevice) || wxDirExists(recDevice)) {
		wxMessageBox("Invalid recording device.", "Error", 
			wxOK | wxCENTRE | wxICON_EXCLAMATION);
		return false;
	}

	bool recordStereo = mRecordStereo->GetValue();
	bool duplex = mDuplex->GetValue();

	/* Step 2: Write to gPrefs */
	gPrefs->SetPath("/AudioIO");
	gPrefs->Write("PlaybackDevice", playDevice);
	gPrefs->Write("RecordingDevice", recDevice);
	gPrefs->Write("RecordStereo", recordStereo);
	gPrefs->Write("Duplex", duplex);
	gPrefs->SetPath("/");
	
	/* Step 3: Make audio sub-system re-read preferences */

	return true;
}


void AudioIOPrefs::TestPlaybackDevice(wxCommandEvent& event)
{
}

void AudioIOPrefs::SetPlaybackDeviceDefault(wxCommandEvent& event)
{
	/* TODO: attempt autodetection? */
	mPlaybackDeviceCtrl->SetValue("/dev/dsp");
}

void AudioIOPrefs::TestRecordingDevice(wxCommandEvent& event)
{
}

void AudioIOPrefs::SetRecordingDeviceDefault(wxCommandEvent& event)
{
	mRecordingDeviceCtrl->SetValue("/dev/dsp");
}

AudioIOPrefs::~AudioIOPrefs()
{
	delete mEnclosingBox;
	delete mPlaybackDeviceLabel;
	delete mPlaybackDeviceCtrl;
	delete mPlaybackDeviceTest;
	delete mPlaybackDeviceDefault;
	delete mRecordingDeviceLabel;
	delete mRecordingDeviceCtrl;
	delete mRecordingDeviceTest;
	delete mRecordingDeviceDefault;

	delete mRecordStereo;
	delete mDuplex;
}

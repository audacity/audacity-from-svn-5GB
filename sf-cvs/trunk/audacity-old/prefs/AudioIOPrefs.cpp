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
   RecordingDefaultID,
   RecordingDeviceChoiceID,
   RecordingInputChoiceID,
   PlaybackDeviceChoiceID,
};

BEGIN_EVENT_TABLE(AudioIOPrefs, wxPanel)
   EVT_BUTTON(PlaybackTestID, AudioIOPrefs::TestPlaybackDevice)
   EVT_BUTTON(PlaybackDefaultID, AudioIOPrefs::SetPlaybackDeviceDefault)
   EVT_BUTTON(RecordingTestID, AudioIOPrefs::TestRecordingDevice)
   EVT_BUTTON(RecordingDefaultID, AudioIOPrefs::SetRecordingDeviceDefault)
   EVT_CHOICE(RecordingDeviceChoiceID, AudioIOPrefs::OnRecordingDeviceChoice)   
END_EVENT_TABLE()

AudioIOPrefs::AudioIOPrefs(wxWindow * parent):
PrefsPanel(parent)
{
#ifdef __WXGTK__
   wxString defaultPlaybackDevice = "/dev/dsp";
   wxString defaultRecordingDevice = "/dev/dsp";
#endif // __WXGTK__

#ifdef __WXMSW__
   wxString defaultPlaybackDevice = "/dev/dsp";
   wxString defaultRecordingDevice = "/dev/dsp";
#endif // __WXMSW__

#ifdef __WXMAC__
   wxString defaultPlaybackDevice = "Built-in";
   Handle iconH;
   Str255 name;
   SPBGetIndexedDevice(0, name, &iconH);
   wxString defaultRecordingDevice = p2cstr(name);
   defaultRecordingDevice += "\n";
   defaultRecordingDevice += "Built-in Mic";
#endif // __WXMAC__

   /* read prefs all at once, then set up the dialog */
   gPrefs->SetPath("/AudioIO");
   mPlayDevice = gPrefs->Read("PlaybackDevice", defaultPlaybackDevice);
   mRecDevice = gPrefs->Read("RecordingDevice", defaultRecordingDevice);
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

         mPlaybackDeviceCtrl = new wxTextCtrl(this, -1, mPlayDevice);

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

         mRecordingDeviceCtrl = new wxTextCtrl(this, -1, mRecDevice);

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

#ifdef __WXMAC__

   {
      int count, selected;
      wxString *names;
      wxBoxSizer *choiceSizer;
   
      //
      // Recording Sizer
      //
   
      wxStaticBoxSizer *recordingSizer =
          new wxStaticBoxSizer(
            new wxStaticBox(this, -1, "Recording"),
            wxVERTICAL);

      // Recording Device (i.e. built-in, CD)

      choiceSizer = new wxBoxSizer(wxHORIZONTAL);
      
      choiceSizer->Add(
            new wxStaticText(this, -1, "Device: "), 0, 
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxLEFT | wxTOP | wxRIGHT, GENERIC_CONTROL_BORDER);

      GetRecordingDevices(&count, &selected, &names);
      
      mRecordingDeviceChoice = new wxChoice(this, RecordingDeviceChoiceID,
                                            wxDefaultPosition, wxDefaultSize,
                                            count, names);
      mRecordingDeviceChoice->SetSelection(selected);
      
      choiceSizer->Add(mRecordingDeviceChoice, 1,
                       wxGROW | wxLEFT | wxTOP | wxRIGHT,
                       GENERIC_CONTROL_BORDER);
      
      recordingSizer->Add(choiceSizer, 1,
                          wxGROW | wxALL, GENERIC_CONTROL_BORDER);

      // Recording Input Source (i.e. Mic, Line In, Modem)

      choiceSizer = new wxBoxSizer(wxHORIZONTAL);
      
      choiceSizer->Add(
            new wxStaticText(this, -1, "Input: "), 0, 
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxLEFT | wxBOTTOM | wxRIGHT, GENERIC_CONTROL_BORDER);

      GetRecordingInputs(mRecordingDeviceChoice->GetSelection(), &count, &selected, &names);
      
      mRecordingInputChoice = new wxChoice(this, RecordingInputChoiceID,
                                           wxDefaultPosition, wxDefaultSize,
                                           count, names);
      mRecordingInputChoice->SetSelection(selected);
      
      choiceSizer->Add(mRecordingInputChoice, 1,
                       wxGROW | wxLEFT | wxBOTTOM | wxRIGHT,
                       GENERIC_CONTROL_BORDER);
      
      recordingSizer->Add(choiceSizer, 1,
                          wxGROW | wxALL, GENERIC_CONTROL_BORDER);
                          
      topSizer->Add(recordingSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);

      //
      // Playback sizer
      //

      wxStaticBoxSizer *playbackSizer =
          new wxStaticBoxSizer(
            new wxStaticBox(this, -1, "Playback"),
            wxVERTICAL);
      
      // Playback device (i.e. built-in)

      choiceSizer = new wxBoxSizer(wxHORIZONTAL);
      
      choiceSizer->Add(
            new wxStaticText(this, -1, "Device: "), 0, 
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);
            
      GetPlaybackDevices(&count, &selected, &names);
            
      mPlaybackDeviceChoice = new wxChoice(this, PlaybackDeviceChoiceID,
                                            wxDefaultPosition, wxDefaultSize,
                                            count, names);
      mPlaybackDeviceChoice->SetSelection(selected);
      
      choiceSizer->Add(mPlaybackDeviceChoice, 1, wxGROW | wxALL, GENERIC_CONTROL_BORDER);
      
      playbackSizer->Add(choiceSizer, 1,
                         wxGROW | wxALL, GENERIC_CONTROL_BORDER);
                          
      topSizer->Add(playbackSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);
      
      // Cleanup
      
      delete[] names;
   }

#endif                          // __WXMAC__


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
   mPlayDevice = mPlaybackDeviceCtrl->GetValue();
   if (!wxFileExists(mPlayDevice) || wxDirExists(mPlayDevice)) {
      wxMessageBox("Invalid playback device.", "Error",
                   wxOK | wxCENTRE | wxICON_EXCLAMATION);
      return false;
   }

   mRecDevice = mRecordingDeviceCtrl->GetValue();
   if (!wxFileExists(mRecDevice) || wxDirExists(mRecDevice)) {
      wxMessageBox("Invalid recording device.", "Error",
                   wxOK | wxCENTRE | wxICON_EXCLAMATION);
      return false;
   }
#endif                          // __WXGTK__

#ifdef __WXMAC__
   mPlayDevice = mPlaybackDeviceChoice->GetStringSelection();
   mRecDevice = mRecordingDeviceChoice->GetStringSelection() + "\n" +
                mRecordingInputChoice->GetStringSelection();
#endif                          // __WXMAC__

#ifdef __WXMSW__
   mPlayDevice = "";
   mPecDevice = "";   
#endif                          // __WXMSW__

   bool recordStereo = mRecordStereo->GetValue();
   bool duplex = mDuplex->GetValue();

   /* Step 2: Write to gPrefs */
   gPrefs->SetPath("/AudioIO");

   gPrefs->Write("PlaybackDevice", mPlayDevice);
   gPrefs->Write("RecordingDevice", mRecDevice);

   gPrefs->Write("RecordStereo", recordStereo);
   gPrefs->Write("Duplex", duplex);
   gPrefs->SetPath("/");

   /* Step 3: Make audio sub-system re-read preferences */

   return true;
}

void AudioIOPrefs::OnRecordingDeviceChoice(wxCommandEvent & event)
{
#ifdef __WXMAC__
   wxString *inputNames;
   int count;
   int x;
   
   GetRecordingInputs(mRecordingDeviceChoice->GetSelection(), &count, &x, &inputNames);
   mRecordingInputChoice->Clear();
   for(int y=0; y<count; y++)
      mRecordingInputChoice->Append(inputNames[y]);
   mRecordingInputChoice->SetSelection(x);
   delete[] inputNames;
#endif
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
/*
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
*/
}

#ifdef __WXMAC__

//
// Mac-specific methods to get lists of the available recording devices,
// recording input sources, and playback devices.
//

void AudioIOPrefs::GetRecordingDevices(int *theCount, int *theSelected, wxString **theNames)
{
   int count=0, x, selected=0;
   Handle iconH;
   Str255 name;
   while(SPBGetIndexedDevice(count+1, name, &iconH) == 0)
      count++;
      
   wxString selectedDevice = mRecDevice.BeforeFirst('\n');

   wxString *deviceNames = new wxString[count];
   for(x=0; x<count; x++) {
      SPBGetIndexedDevice(x+1, name, &iconH);
      deviceNames[x] = p2cstr(name);
      
      if (deviceNames[x] == selectedDevice)
         selected = x;
   }
   
   *theCount = count;
   *theSelected = selected;
   *theNames = deviceNames;
}

void AudioIOPrefs::GetRecordingInputs(int deviceNum, int *theCount, int *theSelected, wxString **theNames)
{
   OSErr err;
   Handle h;
   Str255 deviceName;
   long refnum;
   SoundInfoList infoList;
   int count;
   short selected;
   wxString *names;
   Handle iconH;

   wxString selectedInput = mRecDevice.AfterLast('\n');

   SPBGetIndexedDevice(deviceNum+1, deviceName, &iconH);

   err = SPBOpenDevice(deviceName, siReadPermission, &refnum);

   if (err != 0) {
      *theCount = 0;
      *theSelected = 0;
      *theNames = NULL;
   }

   SPBGetDeviceInfo (refnum, siInputAvailable, &infoList);
   count = infoList.count;

   // Get default from OS in case the one in the prefs was not found.
   SPBGetDeviceInfo (refnum, siInputSource, &selected);
   selected--;

   names = new wxString[count];

   char **buffer;
   SPBGetDeviceInfo (refnum, siInputSourceNames, &buffer);
   char *data = buffer[0] + 2;
   
   for(int x=0; x<count; x++) {
      int y = *data++;
      while(y) {
         names[x] += *data++;
         y--;
      }
      if (names[x] == selectedInput)
         selected = x;
   }

   SPBCloseDevice(refnum);
   
   *theCount = count;
   *theNames = names;
   *theSelected = selected;
}

void AudioIOPrefs::GetPlaybackDevices(int *theCount, int *theSelected, wxString **theNames)
{
   int count, x;
   int selected;

   ComponentDescription description;
   description.componentType = kSoundOutputDeviceType;
   description.componentSubType = kAnyComponentSubType;
   description.componentManufacturer = kAnyComponentManufacturer;
   description.componentFlags = 0;
   description.componentFlagsMask = 0;

   count = CountComponents(&description);
   *theSelected = 0;

   wxString *outputNames = new wxString[count];
   
   Component playbackDevice = NULL;      
   for(x=0; x<count; x++) {
      playbackDevice = FindNextComponent(playbackDevice, &description);

      Handle name = NewHandle(256);
      GetComponentInfo(playbackDevice, NULL, name, NULL, NULL);
      HLock(name);
      outputNames[x] = p2cstr((unsigned char *)*name);
      HUnlock(name);
      DisposeHandle(name);
      
      if (outputNames[x] == mPlayDevice)
         *theSelected = x;
   }
   
   *theCount = count;
   *theNames = outputNames;
}

#endif  // __WXMAC__




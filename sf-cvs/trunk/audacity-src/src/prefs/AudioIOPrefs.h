/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_PREFS__
#define __AUDACITY_AUDIO_IO_PREFS__

#include <wx/defs.h>

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/string.h>
#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class DevicePrefs:public PrefsPanel
{
 public:
   DevicePrefs(wxWindow * parent);
   virtual ~DevicePrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void GetNamesAndLabels();

   void OnHost(wxCommandEvent & e);

   wxArrayString mHostNames;
   wxArrayString mHostLabels;
   wxArrayString mChannelNames;
   wxArrayInt    mChannelLabels;

   wxString mPlayDevice;
   wxString mRecordDevice;

   wxChoice *mHost;
   wxChoice *mPlay;
   wxChoice *mRecord;

   DECLARE_EVENT_TABLE();
};

class PlaybackPrefs:public PrefsPanel
{
 public:
   PlaybackPrefs(wxWindow * parent);
   virtual ~PlaybackPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void GetNamesAndLabels();

   wxArrayString mPlayNames;
   wxArrayString mPlayLabels;

   wxChoice *mPlay;
};

class RecordingPrefs:public PrefsPanel
{
 public:
   RecordingPrefs(wxWindow * parent);
   virtual ~RecordingPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void GetNamesAndLabels();

   wxArrayString mRecordNames;
   wxArrayString mRecordLabels;
   wxArrayString mChannelNames;
   wxArrayInt    mChannelLabels;

   wxChoice *mRec;
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


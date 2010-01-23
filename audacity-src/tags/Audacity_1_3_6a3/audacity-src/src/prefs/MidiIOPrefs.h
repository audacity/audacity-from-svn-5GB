#include "../Experimental.h"

#ifdef EXPERIMENTAL_NOTE_TRACK
/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MIDI_IO_PREFS__
#define __AUDACITY_MIDI_IO_PREFS__

#include <wx/string.h>

#include "PrefsPanel.h"

class wxWindow;
class wxCheckBox;
class wxChoice;
#include <wx/defs.h>
#include <wx/string.h>

#include "PrefsPanel.h"

class wxWindow;
class ShuttleGui;

class MidiIOPrefs:public PrefsPanel 
{
public:
   MidiIOPrefs(wxWindow * parent);
   ~MidiIOPrefs();
   virtual bool Apply();

private:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   void GetNamesAndLabels();

   wxArrayString mmPlayNames;
   wxArrayString mmPlayLabels;
   wxArrayString mmRecordNames;
   wxArrayString mmRecordLabels;
   wxArrayString mmChannelNames;
   wxArrayInt    mmChannelLabels;
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

#endif

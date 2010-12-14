/**********************************************************************

  Audacity: A Digital Audio Editor

  SmartRecordPrefs.cpp

  Martyn Shaw

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

********************************************************************//**

\class SmartRecord
\brief A PrefsPanel that configures record-pause on silence.

Provides:
 - Button to to turn the mode on and off.
 - Slider to set the 'silence' level on the meter.

*//********************************************************************/

#include "../Audacity.h"

#include <wx/wxprec.h>
#include "../Prefs.h"
#include "../Theme.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "SmartRecordPrefs.h"
#include "../AColor.h"

/*enum eThemePrefsIds {
   idLoadThemeCache=7000,
   idSaveThemeCache,
   idLoadThemeComponents,
   idSaveThemeComponents,
   idReadThemeInternal,
   idSaveThemeAsCode
};*/

/*BEGIN_EVENT_TABLE(ThemePrefs, wxPanel)
   EVT_BUTTON( idLoadThemeCache,      ThemePrefs::OnLoadThemeCache)
   EVT_BUTTON( idSaveThemeCache,      ThemePrefs::OnSaveThemeCache)
   EVT_BUTTON( idLoadThemeComponents, ThemePrefs::OnLoadThemeComponents)
   EVT_BUTTON( idSaveThemeComponents, ThemePrefs::OnSaveThemeComponents)
   EVT_BUTTON( idReadThemeInternal,   ThemePrefs::OnReadThemeInternal)
   EVT_BUTTON( idSaveThemeAsCode,     ThemePrefs::OnSaveThemeAsCode)
END_EVENT_TABLE()*/

SmartRecordPrefs::SmartRecordPrefs(wxWindow * parent) :
   PrefsPanel(parent)
{
   SetLabel(_("Smart Recording"));         // Provide visual label
   SetName(_("Smart Recording"));          // Provide audible label
  
   Populate();
}

SmartRecordPrefs::~SmartRecordPrefs(void)
{
}

/// Creates the dialog and its contents.
void SmartRecordPrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Create the dialog contents, or exchange data with it.
void SmartRecordPrefs::PopulateOrExchange( ShuttleGui & S)
{
   S.StartStatic( _("Pause Recording on Silence") );
   {
      S.TieCheckBox( _("Pause Recording on Silence"), wxT("/AudioIO/PauseRecOnSilence"),false);
      S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(1);
         S.TieSlider(_("Silence level (dB):"), wxT("/AudioIO/SilenceLevel"), -50, 0, -60);
      S.EndMultiColumn();
   }
   S.EndStatic();
}

/// Update the preferences stored on disk.
bool SmartRecordPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   return true;
}

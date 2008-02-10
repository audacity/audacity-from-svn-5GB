/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class FileFormatPrefs
\brief A PrefsPanel used to select file format preferences and to 
locate the MP3 encoding library.  Now called 'Import/Export' in the
preferences.  Later we will rename this panel and source files.

*//*******************************************************************/

 

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/stattext.h>
#include <wx/button.h>

#include "../export/ExportMP3.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../widgets/LinkingHtmlWindow.h"

#include "FileFormatPrefs.h"

#define ID_MP3_FIND_BUTTON          7001
#define ID_MP3_DOWN_BUTTON          7002

BEGIN_EVENT_TABLE(FileFormatPrefs, wxPanel)
   EVT_BUTTON(ID_MP3_FIND_BUTTON, FileFormatPrefs::OnMP3FindButton)
   EVT_BUTTON(ID_MP3_DOWN_BUTTON, FileFormatPrefs::OnMP3DownButton)
END_EVENT_TABLE()

FileFormatPrefs::FileFormatPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   SetLabel(_("Import / Export"));         // Provide visual label
   SetName(_("Import / Export"));          // Provide audible label
   Populate( );
}

/// Creates the dialog and its contents.
void FileFormatPrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
   // Set the MP3 Version string.
   SetMP3VersionText();
}

/// This PopulateOrExchange function is a good example of mixing the fully 
/// automatic style of reading/writing from GUI to prefs with the partial form.
/// 
/// You'll notice that some of the Tie functions have Prefs identifiers in them
/// and others don't.  
void FileFormatPrefs::PopulateOrExchange( ShuttleGui & S )
{
   S.SetBorder( 2 );
   S.StartStatic( _("MP3 Export Library"));
   {
      S.StartTwoColumn();
         S.AddVariableText( _("MP3 Library Version:"),
            true,
            wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL );
         mMP3Version = S.AddVariableText( wxT("9.99"),
            true,
            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL );
         S.AddVariableText( _("MP3 Library:"),
            true,
            wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL );
         S.Id( ID_MP3_FIND_BUTTON ).AddButton( _("&Find Library"), 
            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL );
         S.AddVariableText( _("LAME MP3 Library:"),
            true,
            wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL );
         S.Id( ID_MP3_DOWN_BUTTON ).AddButton( _("&Download"), 
            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL );
      S.EndHorizontalLay();
   }
   S.EndStatic();
   S.AddFixedText( _("Note: Export quality options can be chosen by clicking the Options button in the Export dialog."));
   S.StartStatic( _("When importing audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/CopyOrEditUncompressedData"),wxT("edit"));
      S.TieRadioButton( _("&Make a copy of uncompressed audio files before editing (safer)"),wxT("copy"));
      S.TieRadioButton( _("&Read uncompressed audio files directly from the original (faster)"),wxT("edit"));
      S.EndRadioButtonGroup();
      S.TieCheckBox( _("&Normalize all tracks in project"), 
         wxT("/AudioFiles/NormalizeOnLoad"), false );
   }
   S.EndStatic();

   S.StartStatic( _("When saving a project that depends on other audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/SaveProjectWithDependencies"),wxT("ask"));
      S.TieRadioButton( _("Always &copy all audio into project (safest)"), wxT("copy"));
      S.TieRadioButton( _("&Do not copy any audio"), wxT("never"));
      S.TieRadioButton( _("&Ask user"), wxT("ask"));
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
   S.StartStatic( _("When exporting tracks to an audio file"));
   {
      S.StartRadioButtonGroup( wxT("/FileFormats/ExportDownMix" ), true );
      S.TieRadioButton( _("A&lways mix all tracks down to Stereo or Mono channel(s)."), true);
      S.TieRadioButton( _("&Use custom mix (for example to export a 5.1 multichannel file)"),false );
      S.EndRadioButtonGroup();
      S.TieCheckBox( _("S&how Metadata Editor prior to export step"), 
         wxT("/AudioFiles/ShowId3Dialog"), true);
   }
   S.EndStatic();
}

/// Sets the a text area on the dialog to have the name
/// of the MP3 Library version.
void FileFormatPrefs::SetMP3VersionText(bool prompt)
{
   mMP3Version->SetLabel(GetMP3Version(this, prompt));
}

/// Opens a file-finder dialog so that the user can
/// tell us where the MP3 library is.
void FileFormatPrefs::OnMP3FindButton(wxCommandEvent& evt)
{
   SetMP3VersionText(true);
}

/// Opens a file-finder dialog so that the user can
/// tell us where the MP3 library is.
void FileFormatPrefs::OnMP3DownButton(wxCommandEvent& evt)
{
   wxString url = wxT("http://audacity.sourceforge.net/lame");
   ::OpenInDefaultBrowser(url);
}

/// Takes the settings from the dilaog and puts them into prefs.
/// Most of the preferences are set by the ShuttleGui, but for some
/// specially handled ones we need to do this ourselves.
bool FileFormatPrefs::Apply()
{  
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );    
   
   return true;
}

FileFormatPrefs::~FileFormatPrefs()
{
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 427b9e64-3fc6-40ef-bbf8-e6fff1d442f0


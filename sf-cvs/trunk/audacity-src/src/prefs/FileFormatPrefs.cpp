/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class FileFormatPrefs
\brief A PrefsPanel used to select file format preferences and to 
locate the MP3 encoding library.

*//*******************************************************************/

 

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/choice.h>
#include <wx/stattext.h>
#include <wx/radiobut.h>
#include <wx/button.h>
#include <wx/arrstr.h>

#include "sndfile.h"

#include "../export/ExportMP3.h"
#include "../FileFormats.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "FileFormatPrefs.h"

#define ID_MP3_FIND_BUTTON         7001
#define ID_EXPORT_OPTIONS_BUTTON   7002

BEGIN_EVENT_TABLE(FileFormatPrefs, wxPanel)
   EVT_BUTTON(ID_MP3_FIND_BUTTON, FileFormatPrefs::OnMP3FindButton)
END_EVENT_TABLE()

FileFormatPrefs::FileFormatPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   SetLabel(_("File Formats"));         // Provide visual label
   SetName(_("File Formats"));          // Provide audible label
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
   S.StartStatic( _("When importing uncompressed audio files into Audacity"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/CopyOrEditUncompressedData"),wxT("edit"));
      S.TieRadioButton( _("&Make a copy of the file before editing (safer)"),wxT("copy"));
      S.TieRadioButton( _("&Read directly from the original file (faster)"),wxT("edit"));
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
   S.StartStatic( _("When saving a project that depends on other audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/SaveProjectWithDependencies"),wxT("ask"));
      S.TieRadioButton( _("&Ask user"), wxT("ask"));
      S.TieRadioButton( _("Always &copy all audio (safest)"), wxT("copy"));
      S.TieRadioButton( _("&Never copy any audio"), wxT("never"));
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
   S.StartStatic( _("When exporting tracks"));
   {
      S.StartRadioButtonGroup( wxT("/FileFormats/ExportDownMix" ), true );
      S.TieRadioButton( _("A&lways mix all tracks down to Stereo or Mono channel(s)."), true);
      S.TieRadioButton( _("&Use Advanced Mixing Options"),false );
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
   S.StartStatic( _("MP3 Export Setup"));
   {
      S.StartThreeColumn();
         S.AddFixedText( _("MP3 Library Version:"), true);
         mMP3Version = S.AddVariableText( wxT("CAPITAL LETTERS"), true);
      S.Id( ID_MP3_FIND_BUTTON ).AddButton( _("&Find Library"), 
         wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
      S.EndThreeColumn();
   }
   S.EndStatic();
}

/// Sets the a text area on the dialog to have the name
/// of the MP3 Library version.
void FileFormatPrefs::SetMP3VersionText()
{
   wxString versionString = _("MP3 exporting plugin not found");
   bool doMP3 = GetMP3Exporter()->LoadLibrary(this, false);

   if (doMP3)
      doMP3 = GetMP3Exporter()->ValidLibraryLoaded();
   if (doMP3)
      versionString = GetMP3Exporter()->GetLibraryVersion();
   
   mMP3Version->SetLabel(versionString);
}

/// Opens a file-finder dialog so that the user can
/// tell us where the MP3 library is.
void FileFormatPrefs::OnMP3FindButton(wxCommandEvent& evt)
{
   if (GetMP3Exporter()->FindLibrary(this, false)) {
      SetMP3VersionText();
   }
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


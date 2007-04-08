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

*//****************************************************************//**

\class OtherFormatDialog
\brief A fairly complex small dialog that is called up by 
FileFormatPrefs to allow selection of *any* audio file format, not 
just the standard combinations.

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
#define ID_FORMAT_CHOICE           7003

BEGIN_EVENT_TABLE(FileFormatPrefs, wxPanel)
   EVT_BUTTON(ID_MP3_FIND_BUTTON, FileFormatPrefs::OnMP3FindButton)
   EVT_CHOICE(ID_FORMAT_CHOICE,   FileFormatPrefs::OnFormatChoice)
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
   mFormat = ReadExportFormatPref();

   //----- Gather our strings used in choices.
   GetNamesAndLabels();
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

/// Gets ArrayStrings that will be used in wxChoices.
void FileFormatPrefs::GetNamesAndLabels()
{
   int numSimpleFormats = sf_num_simple_formats();
   int sel = numSimpleFormats;
   for(int i=0; i<numSimpleFormats; i++) 
   {
      mFormatNames.Add( wxString(sf_simple_format(i)->name, wxConvISO8859_1));
      if (mFormat == sf_simple_format(i)->format)
         sel = i;
   }
   mFormatNames.Add( _("Other..."));
   // the selected format from within the list.
   mFormatFromChoice = mFormatNames[sel];
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
   S.StartHorizontalLay(wxEXPAND,0);
   S.StartStatic(_("Uncompressed Export Format"),1);
   {
      // We need the pointers to these controls for the special behaviour.      
      mDefaultExportFormat = S.Id( ID_FORMAT_CHOICE ).TieChoice( 
            wxT(""),           // No prompt required.
            mFormatFromChoice, // Value (as a string)
            &mFormatNames      // List of possible options, including 'Other...'
            );
   }
   S.EndStatic();
   S.StartHorizontalLay(wxEXPAND,0);
   S.StartStatic( _("MP3 Export Setup"),0);
   {
      S.StartThreeColumn();
         S.AddFixedText( _("MP3 Library Version:"), true);
         mMP3Version = S.AddVariableText( wxT("CAPITAL LETTERS"), true);
      S.Id( ID_MP3_FIND_BUTTON ).AddButton( _("&Find Library"), 
         wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
      S.EndThreeColumn();
   }
   S.EndStatic();
   S.EndHorizontalLay();
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

/// Updates the dialog when a format preference has changed.
/// If the user selected 'Other... ' for the format, then a 
/// more detailed dialog for format preferences is opened
/// up in which header and encoding can be selected separately.
void FileFormatPrefs::OnFormatChoice(wxCommandEvent& evt)
{
   int sel = mDefaultExportFormat->GetSelection();
   int numSimpleFormats = sf_num_simple_formats();

   if (sel == numSimpleFormats) //i.e. if we selected 'Other..' 
      OpenOtherFormatDialog();
   else 
      mFormat = sf_simple_format(sel)->format;
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
   
   // JKC: I'd like to include the body of this one-line function here.
   // so that the code is more regular/readable, and the same with the read.
   WriteExportFormatPref(mFormat);

   return true;
}

FileFormatPrefs::~FileFormatPrefs()
{
}

#define ID_HEADER_CHOICE           7101
#define ID_BITS_CHOICE             7102
#define ID_ENCODING_CHOICE         7103

/// A helper Dialog to help FileFormatPrefs with sound format selection.
///
/// The dialog shows a header and an encoding, and the user can change 
/// the selection of either.  However some combinations are illegal and
/// for these the OK button is disabled.
class OtherFormatDialog : public wxDialog 
{
public:
   // constructors and destructors
   OtherFormatDialog(wxWindow * parent, wxWindowID id,
                     unsigned int format);

public:
   unsigned int  mFormat;

private:
   void OnChoice(wxCommandEvent & event);
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void ValidateChoices();

   void Populate();
   void PopulateOrExchange( ShuttleGui & S );

   wxArrayString mHeaderNames;
   wxArrayString mEncodingNames;
   wxString mHeader;
   wxString mEncoding;
   wxButton   *mOK;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(OtherFormatDialog, wxDialog)
   EVT_CHOICE(ID_HEADER_CHOICE,   OtherFormatDialog::OnChoice)
   EVT_CHOICE(ID_BITS_CHOICE,     OtherFormatDialog::OnChoice)
   EVT_CHOICE(ID_ENCODING_CHOICE, OtherFormatDialog::OnChoice)
   EVT_BUTTON(wxID_OK,            OtherFormatDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL,        OtherFormatDialog::OnCancel)
END_EVENT_TABLE()

OtherFormatDialog::OtherFormatDialog(wxWindow * parent, wxWindowID id,
                                     unsigned int format):
   wxDialog(parent, id,
            _("File Format"),
            wxDefaultPosition, wxDefaultSize)
{
   mFormat = format;
   Populate();
}

/// Creates the dialog.
void OtherFormatDialog::Populate()
{
   //----- Gather our strings used in choices.
   int selection=0;
   for(int i=0; i<sf_num_headers(); i++) {
      mHeaderNames.Add( sf_header_index_name(i));
      if ((mFormat & SF_FORMAT_TYPEMASK) == sf_header_index_to_type(i))
         selection = i;
   }
   mHeader = mHeaderNames[selection];
   selection = 0;
   for(int i=0; i<sf_num_encodings(); i++) {
      mEncodingNames.Add( sf_encoding_index_name(i));
      if ((mFormat & SF_FORMAT_SUBMASK) == sf_encoding_index_to_subtype(i))
         selection = i;
   }
   mEncoding = mEncodingNames[selection];
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
   Fit();
}

/// Defines the dialog's contents and is used both in creation
/// and in exchanging data with the dialog.
void OtherFormatDialog::PopulateOrExchange( ShuttleGui & S )
{
   S.AddFixedText( _("(Not all combinations of headers\nand encodings are possible.)"));
   S.StartTwoColumn();
   S.Id( ID_HEADER_CHOICE).TieChoice( _("Header: "), mHeader, &mHeaderNames );
   S.Id( ID_ENCODING_CHOICE).TieChoice( _("Encoding: "), mEncoding, &mEncodingNames );
   S.EndTwoColumn();
   S.StartHorizontalLay(wxALIGN_CENTRE, 1);
   S.Id( wxID_CANCEL ).AddButton( _("&Cancel"));
   mOK = S.Id( wxID_OK).AddButton( _("&OK") );
   S.EndHorizontalLay();
}

/// Calls a libsndfile library function to determine whether the user's
/// choice of sample encoding (e.g. pcm 16-bit or GSM 6.10 compression)
/// is compatible with their choice of file format (e.g. WAV, AIFF)
/// and enables/disables the OK button accordingly.
void OtherFormatDialog::ValidateChoices()
{
   SF_INFO info;
   memset(&info, 0, sizeof(info));
   info.frames = 0;
   info.samplerate = 44100;
   info.channels = 1;
   info.format = mFormat;
   info.sections = 1;
   info.seekable = 0;

   mOK->Enable(sf_format_check(&info) ? true : false);
}

/// When a choice is changed, update our values and validate the
/// combination.
void OtherFormatDialog::OnChoice(wxCommandEvent & event)
{
   // Get all values from the GUI.
   ShuttleGui S( this, eIsGettingFromDialog );
   PopulateOrExchange( S );

   int h = mHeaderNames.Index( mHeader );
   int e = mEncodingNames.Index( mEncoding );

   mFormat = sf_header_index_to_type(h) | sf_encoding_index_to_subtype(e);
   ValidateChoices();
}

/// Finish when OK.
void OtherFormatDialog::OnOk(wxCommandEvent & event)
{
   EndModal(true);
}

/// Finish when Cancel.
void OtherFormatDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

/// The user has selected 'Other...' for the format, so show the 
/// extra dialog and process the result.
void FileFormatPrefs::OpenOtherFormatDialog()
{
   OtherFormatDialog dlog(this, -1, mFormat);
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode()) {
      mFormat = dlog.mFormat;
   }
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


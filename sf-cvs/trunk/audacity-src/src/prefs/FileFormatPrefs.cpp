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
#define ID_MP3_VBR                 7004
#define ID_MP3_CBR                 7005

BEGIN_EVENT_TABLE(FileFormatPrefs, wxPanel)
   EVT_BUTTON(ID_MP3_FIND_BUTTON, FileFormatPrefs::OnMP3FindButton)
   EVT_CHOICE(ID_FORMAT_CHOICE,   FileFormatPrefs::OnFormatChoice)
   EVT_RADIOBUTTON(ID_MP3_VBR,    FileFormatPrefs::OnMP3VBR)
   EVT_RADIOBUTTON(ID_MP3_CBR,    FileFormatPrefs::OnMP3CBR)
END_EVENT_TABLE()

FileFormatPrefs::FileFormatPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   SetLabel(_("File Formats"));         // Provide visual label
   SetName(_("File Formats"));          // Provide audible label
   mFormatText = NULL;
   Populate( );
}

int iBitrates[] = {
   16, 24, 32, 40, 48, 56, 64,
   80, 96, 112, 128, 160,
   192, 224, 256, 320 
};

/// Creates the dialog and its contents.
void FileFormatPrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.
   mOggQualityUnscaled = gPrefs->Read( wxT("/FileFormats/OggExportQuality"),50)/10;
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
   // Set the MP3 Version string.  Also enable/disable bitrate.
   SetMP3VersionText();
   if(!GetMP3Exporter()->ValidLibraryLoaded()) {
      mMP3Bitrate->Enable(false);
      mMP3Stereo->Enable(false);
      mMP3Joint->Enable(false);
      mMP3VBR->Enable(false);
      mMP3CBR->Enable(false);
   }
   // Set the format string.
   SetFormatText();
}

/// Gets ArrayStrings that will be used in wxChoices.
void FileFormatPrefs::GetNamesAndLabels()
{
   for(unsigned int i=0;i<(sizeof(iBitrates)/sizeof(int));i++)
   {
      mBitRateNames.Add( wxString::Format(wxT("%i"),iBitrates[i] ));
      mBitRateLabels.Add( iBitrates[i] );
   }

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
      S.StartMultiColumn(1);
      // We need the pointers to these controls for the special behaviour.      
      mDefaultExportFormat = S.Id( ID_FORMAT_CHOICE ).TieChoice( 
            wxT(""),           // No prompt required.
            mFormatFromChoice, // Value (as a string)
            &mFormatNames      // List of possible options, including 'Other...'
            );
      mFormatText = S.AddVariableText( wxT("CAPITAL LETTERS"));
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.StartStatic( _("OGG Export Setup"),1);
   {
      S.StartMultiColumn(2, wxEXPAND);
      S.SetStretchyCol(1);
      S.TieSlider( _("OGG Quality:"),mOggQualityUnscaled,10 );
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.EndHorizontalLay();
   S.StartHorizontalLay(wxEXPAND,0);
   S.StartStatic( _("MP3 Export Setup"),0);
   {
      // Some rather fiddly sizers to get the 'Find Library' button 
      // exactly where we want it.
      S.StartMultiColumn(2,wxEXPAND);
         S.SetStretchyCol(1);
         S.StartTwoColumn();
            S.AddFixedText( _("MP3 Library Version:"));
            mMP3Version = S.AddVariableText( wxT("CAPITAL LETTERS"));
            S.AddPrompt( _("Bit Rate:"));
            S.StartTwoColumn();
               S.StartRadioButtonGroup(wxT("/FileFormats/MP3RateMode"),wxT("cbr"));
                  mMP3VBR = S.Id( ID_MP3_VBR ).TieRadioButton( _("Variable"), wxT("vbr"));
                  mMP3CBR = S.Id( ID_MP3_CBR ).TieRadioButton( _("Constant"), wxT("cbr"));
               S.EndRadioButtonGroup();
            S.EndTwoColumn();

            if (mMP3VBR->GetValue()) {
               mMP3RateNames.Add(_("0 (Best quality)"));
               mMP3RateLabels.Add( 0 );
               for(unsigned int i=1;i<9;i++)
               {
                  mMP3RateNames.Add(wxString::Format(wxT("%i"),i));
                  mMP3RateLabels.Add( i );
               }
               mMP3RateNames.Add(_("9 (Smaller files)"));
               mMP3RateLabels.Add( 9 );
            }
            else {
               for(unsigned int i=0;i<(sizeof(iBitrates)/sizeof(int));i++)
               {
                  mMP3RateNames.Add( wxString::Format(wxT("%i"),iBitrates[i] ));
                  mMP3RateLabels.Add( iBitrates[i] );
               }
            }

            mMP3Bitrate = S.TieChoice( _("Quality:"), wxT("/FileFormats/MP3Bitrate"), 
                                       128, mMP3RateNames, mMP3RateLabels );
            S.AddPrompt( _("Mode:"));
            S.StartTwoColumn();
               S.StartRadioButtonGroup(wxT("/FileFormats/MP3ChannelMode"),wxT("joint"));
                  mMP3Joint = S.TieRadioButton( _("Joint Stereo"), wxT("joint"));
                  mMP3Stereo = S.TieRadioButton( _("Stereo"), wxT("stereo"));
               S.EndRadioButtonGroup();
            S.EndTwoColumn();
         S.EndTwoColumn();
         S.Id( ID_MP3_FIND_BUTTON ).AddButton( _("&Find Library"), 
            wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.StartStatic( _("FLAC Export Setup"),0);
   {
      wxArrayString flacBitDepthNames, flacBitDepthLabels;
      flacBitDepthLabels.Add(wxT("16")); flacBitDepthNames.Add(_("16 bit"));
      flacBitDepthLabels.Add(wxT("24")); flacBitDepthNames.Add(_("24 bit"));
      S.StartMultiColumn(2, wxEXPAND);
      S.SetStretchyCol(1);
      S.TieChoice(_("Bit depth:"), wxT("/FileFormats/FLACBitDepth"),
                  wxT("16"), flacBitDepthNames, flacBitDepthLabels);
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.EndHorizontalLay();
   S.StartHorizontalLay(wxEXPAND,0);
   S.StartStatic( _("MP2 Export Setup"),0);
   {
      S.StartTwoColumn();
      S.TieChoice( _("Bit Rate:"), wxT("/FileFormats/MP2Bitrate"), 
         160, mBitRateNames, mBitRateLabels );
      S.EndTwoColumn();
   }
   S.EndStatic();
   S.EndHorizontalLay();
}

/// Sets the a text area on the dialog to have the name
/// of the MP3 Library version.
void FileFormatPrefs::SetMP3VersionText()
{
   wxString versionString = _("MP3 exporting plugin not found");
   bool doMP3 = GetMP3Exporter()->LoadLibrary();

   if (doMP3)
      doMP3 = GetMP3Exporter()->ValidLibraryLoaded();
   if(doMP3)
      versionString = GetMP3Exporter()->GetLibraryVersion();
   
   mMP3Version->SetLabel(versionString);
}

/// Sets a text area on the dialog to have the name
/// of the currently selected format.
void FileFormatPrefs::SetFormatText()
{
   wxString formatString;
   formatString = sf_header_name(mFormat & SF_FORMAT_TYPEMASK);
   formatString += wxT(", ") + sf_encoding_name(mFormat & SF_FORMAT_SUBMASK);
   mFormatText->SetLabel(formatString);
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

   SetFormatText();
}

/// Opens a file-finder dialog so that the user can
/// tell us where the MP3 library is.
void FileFormatPrefs::OnMP3FindButton(wxCommandEvent& evt)
{
   // We go round the houses here, because GetMP3Exporter 
   // takes its path from gPrefs rather than as a parameter.
   wxString oldPath = gPrefs->Read(wxT("/MP3/MP3LibPath"), wxT(""));
   gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
   
   if (GetMP3Exporter()->FindLibrary(this))
      SetMP3VersionText();
   else {
      gPrefs->Write(wxT("/MP3/MP3LibPath"), oldPath);
   }
   
   if(GetMP3Exporter()->GetConfigurationCaps() & MP3CONFIG_BITRATE) {
      bool valid = GetMP3Exporter()->ValidLibraryLoaded();
      mMP3Bitrate->Enable(valid);
      mMP3Stereo->Enable(valid);
      mMP3Joint->Enable(valid);
      mMP3VBR->Enable(valid);
      mMP3CBR->Enable(valid);
   }
}

/// 
/// 
void FileFormatPrefs::OnMP3VBR(wxCommandEvent& evt)
{
   mMP3Bitrate->Clear();
   mMP3RateNames.Clear();
   mMP3RateLabels.Clear();

   mMP3RateNames.Add(_("0 (Best quality)"));
   mMP3RateLabels.Add( 0 );
   for(unsigned int i=1;i<9;i++)
   {
      mMP3RateNames.Add(wxString::Format(wxT("%i"),i));
      mMP3RateLabels.Add( i );
   }
   mMP3RateNames.Add(_("9 (Smaller files)"));
   mMP3RateLabels.Add( 9 );

   mMP3Bitrate->Append(mMP3RateNames);
   mMP3Bitrate->SetSelection(5);
   mMP3Bitrate->Refresh();
}

/// 
/// 
void FileFormatPrefs::OnMP3CBR(wxCommandEvent& evt)
{
   mMP3Bitrate->Clear();
   mMP3RateNames.Clear();
   mMP3RateLabels.Clear();

   for(unsigned int i=0;i<(sizeof(iBitrates)/sizeof(int));i++)
   {
      mMP3RateNames.Add( wxString::Format(wxT("%i"),iBitrates[i] ));
      mMP3RateLabels.Add( iBitrates[i] );
   }

   mMP3Bitrate->Append(mMP3RateNames);
   mMP3Bitrate->SetSelection(10);
   mMP3Bitrate->Refresh();
}

/// Takes the settings from the dilaog and puts them into prefs.
/// Most of the preferences are set by the ShuttleGui, but for some
/// specially handled ones we need to do this ourselves.
bool FileFormatPrefs::Apply()
{  
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );    
   
   gPrefs->Write( wxT("/FileFormats/OggExportQuality"),mOggQualityUnscaled * 10);
   // JKC: I'd like to include the body of this one-line function here.
   // so that the code is more regular/readable, and the same with the read.
   WriteExportFormatPref(mFormat);

   //-- Isn't the next parameter long gone??  Can we remove it?
   //gPrefs->Write( wxT("/FileFormats/LossyExportFormat"), wxT("MP3"));

   //-- We always write the bitrate, so I don't think we need the commmented out
   // code anymore.
#if 0
   if(GetMP3Exporter()->GetConfigurationCaps() & MP3CONFIG_BITRATE) {
      long bitrate;
      mMP3Bitrate->GetStringSelection().ToLong(&bitrate);
      gPrefs->Write(wxT("/FileFormats/MP3Bitrate"), bitrate);
   }
#endif

   // Tell all open projects to modify their menu bar to reflect
   // the new export formats.
   for(unsigned int i = 0; i < gAudacityProjects.GetCount(); i++)
   {
      if(gAudacityProjects[i])
         gAudacityProjects[i]->ModifyExportMenus();
   }
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


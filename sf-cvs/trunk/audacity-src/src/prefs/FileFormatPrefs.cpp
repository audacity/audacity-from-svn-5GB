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
#include "../FFmpeg.h"  // always needs to go before wx headers

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/stattext.h>
#include <wx/button.h>

#include "../export/ExportMP3.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../widgets/LinkingHtmlWindow.h"

#include "FileFormatPrefs.h"

////////////////////////////////////////////////////////////////////////////////

#define ID_MP3_FIND_BUTTON          7001
#define ID_MP3_DOWN_BUTTON          7002
#define ID_FFMPEG_FIND_BUTTON       7003
#define ID_FFMPEG_DOWN_BUTTON       7004

BEGIN_EVENT_TABLE(LibraryPrefs, PrefsPanel)
   EVT_BUTTON(ID_MP3_FIND_BUTTON, LibraryPrefs::OnMP3FindButton)
   EVT_BUTTON(ID_MP3_DOWN_BUTTON, LibraryPrefs::OnMP3DownButton)
   EVT_BUTTON(ID_FFMPEG_FIND_BUTTON, LibraryPrefs::OnFFmpegFindButton)
   EVT_BUTTON(ID_FFMPEG_DOWN_BUTTON, LibraryPrefs::OnFFmpegDownButton)
END_EVENT_TABLE()

LibraryPrefs::LibraryPrefs(wxWindow * parent)
:   PrefsPanel(parent, _("Libraries"))
{
   Populate();
}

LibraryPrefs::~LibraryPrefs()
{
}

/// Creates the dialog and its contents.
void LibraryPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   // Set the MP3 Version string.
   SetMP3VersionText();
   SetFFmpegVersionText();
}

/// This PopulateOrExchange function is a good example of mixing the fully 
/// automatic style of reading/writing from GUI to prefs with the partial form.
/// 
/// You'll notice that some of the Tie functions have Prefs identifiers in them
/// and others don't.  
void LibraryPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartStatic(_("MP3 Export Library"));
   {
      S.StartTwoColumn();
      {
         S.AddVariableText(_("MP3 Library Version:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         mMP3Version = S.AddVariableText(wxT("9.99"),
                                         true,
                                         wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
         S.AddVariableText(_("MP3 Library:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         S.Id(ID_MP3_FIND_BUTTON).AddButton(_("&Locate..."),
                                            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
         S.AddVariableText(_("LAME MP3 Library:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         S.Id(ID_MP3_DOWN_BUTTON).AddButton(_("&Download"),
                                            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
      }
      S.EndTwoColumn();
   }
   S.EndStatic();

   S.StartStatic(_("FFmpeg Import/Export Library"));
   {
      S.StartTwoColumn();
      {
         S.AddVariableText(_("FFmpeg Library Version:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
#if defined(USE_FFMPEG)
         mFFmpegVersion = S.AddVariableText(_("No compatible FFmpeg library was found"),
                                            true,
                                            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
#else
         mFFmpegVersion = S.AddVariableText(wxT("FFmpeg support is not compiled in"),
                                            true,
                                            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
#endif
         S.AddVariableText(_("FFmpeg Library:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         S.Id(ID_FFMPEG_FIND_BUTTON);
         wxButton *bfnd = S.AddButton(_("&Locate..."), 
                                      wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
         S.AddVariableText(_("FFmpeg Library:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         S.Id(ID_FFMPEG_DOWN_BUTTON);
         wxButton *bdwn = S.AddButton(_("&Download"),
                                      wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
#if !defined(USE_FFMPEG)
         bdwn->Enable(FALSE);
         bfnd->Enable(FALSE);
#endif
      }
      S.EndTwoColumn();
   }
   S.EndStatic();
}

/// Sets the a text area on the dialog to have the name
/// of the MP3 Library version.
void LibraryPrefs::SetMP3VersionText(bool prompt)
{
   mMP3Version->SetLabel(GetMP3Version(this, prompt));
}

/// Opens a file-finder dialog so that the user can
/// tell us where the MP3 library is.
void LibraryPrefs::OnMP3FindButton(wxCommandEvent & e)
{
   SetMP3VersionText(true);
}

/// Opens a file-finder dialog so that the user can
/// tell us where the MP3 library is.
void LibraryPrefs::OnMP3DownButton(wxCommandEvent & e)
{
   wxString url = wxT("http://www.audacityteam.org/manual/index.php?title=FAQ:Installation_and_Plug-Ins%23How_do_I_download_and_install_the_LAME_MP3_encoder.3F");
   ::OpenInDefaultBrowser(url);
}

void LibraryPrefs::SetFFmpegVersionText()
{
   mFFmpegVersion->SetLabel(GetFFmpegVersion(this));
}

void LibraryPrefs::OnFFmpegFindButton(wxCommandEvent & e)
{
#ifdef USE_FFMPEG
   FFmpegLibs* FFmpegLibsInst = PickFFmpegLibs();
   bool showerrs =
#if defined(__WXDEBUG__)
      true;
#else
      false;
#endif

   FFmpegLibsInst->FreeLibs();
   // Load the libs ('true' means that all errors will be shown)
   bool locate = !LoadFFmpeg(showerrs);

   // Libs are fine, don't show "locate" dialog unless user really wants it
   if (!locate) {
      int response = wxMessageBox(wxT("Audacity has automatically detected valid FFmpeg libraries.\
                                      \nDo you still want to locate them manually?"),
                                  wxT("Success"),
                                  wxCENTRE | wxYES_NO | wxNO_DEFAULT |wxICON_QUESTION);
      if (response == wxYES) {
        locate = true;
      }
   }

   if (locate) {
      // Show "Locate FFmpeg" dialog
      FFmpegLibsInst->FindLibs(this);
      FFmpegLibsInst->FreeLibs();
      LoadFFmpeg(showerrs);
   }
   SetFFmpegVersionText();

   DropFFmpegLibs();
#endif
}

void LibraryPrefs::OnFFmpegDownButton(wxCommandEvent & e)
{
   wxString url = wxT("http://www.audacityteam.org/manual/index.php?title=FAQ:Installation_and_Plug-Ins%23installffmpeg");
   ::OpenInDefaultBrowser(url);
}

bool LibraryPrefs::Apply()
{  
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);    
   
   return true;
}

////////////////////////////////////////////////////////////////////////////////

FileFormatPrefs::FileFormatPrefs(wxWindow * parent)
:   PrefsPanel(parent, _("Import / Export"))
{
   Populate();
}

FileFormatPrefs::~FileFormatPrefs()
{
}

/// Creates the dialog and its contents.
void FileFormatPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void FileFormatPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("When importing audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/CopyOrEditUncompressedData"),wxT("edit"));
      S.TieRadioButton(_("&Make a copy of uncompressed audio files before editing (safer)"),wxT("copy"));
      S.TieRadioButton(_("&Read uncompressed audio files directly from the original (faster)"),wxT("edit"));
      S.EndRadioButtonGroup();
      S.TieCheckBox(_("&Normalize all tracks in project"), 
                    wxT("/AudioFiles/NormalizeOnLoad"),
                    false);
   }
   S.EndStatic();

   S.StartStatic(_("When saving a project that depends on other audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/SaveProjectWithDependencies"),wxT("ask"));
      S.TieRadioButton(_("Always &copy all audio into project (safest)"), wxT("copy"));
      S.TieRadioButton(_("&Do not copy any audio"), wxT("never"));
      S.TieRadioButton(_("&Ask user"), wxT("ask"));
      S.EndRadioButtonGroup();
   }
   S.EndStatic();

   S.StartStatic(_("When exporting tracks to an audio file"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/ExportDownMix"),true);
      S.TieRadioButton(_("A&lways mix all tracks down to Stereo or Mono channel(s)."),true);
      S.TieRadioButton(_("&Use custom mix (for example to export a 5.1 multichannel file)"),false);
      S.EndRadioButtonGroup();
      S.TieCheckBox(_("S&how Metadata Editor prior to export step"),
                    wxT("/AudioFiles/ShowId3Dialog"),
                    true);
      S.AddFixedText(_("Note: Export quality options can be chosen by clicking the Options\nbutton in the Export dialog."));
   }
   S.EndStatic();
}

bool FileFormatPrefs::Apply()
{  
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);    
   
   return true;
}

////////////////////////////////////////////////////////////////////////////////

ProjectPrefs::ProjectPrefs(wxWindow * parent)
:   PrefsPanel(parent, _("Projects"))
{
   Populate();
}

ProjectPrefs::~ProjectPrefs()
{
}

/// Creates the dialog and its contents.
void ProjectPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void ProjectPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("When saving a project that depends on other audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/SaveProjectWithDependencies"),wxT("ask"));
      S.TieRadioButton(_("Always &copy all audio into project (safest)"), wxT("copy"));
      S.TieRadioButton(_("&Do not copy any audio"), wxT("never"));
      S.TieRadioButton(_("&Ask user"), wxT("ask"));
      S.EndRadioButtonGroup();
   }
   S.EndStatic();

   S.StartStatic(_("Auto save"));
   {
      S.TieCheckBox(_("Auto save a copy of the project in a separate folder"),
                    wxT("/Directories/AutoSaveEnabled"),
                    true);

      S.StartThreeColumn();
      {
         S.TieTextBox(_("Auto save interval:"),
                      wxT("/Directories/AutoSaveMinutes"),
                      2.0,
                      9);
         S.AddUnits(_("minutes"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();
}

bool ProjectPrefs::Apply()
{  
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);    
   
   return true;
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


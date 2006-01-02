/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.cpp

  Dominic Mazzoni

  Audacity uses wxWindows' wxConfig class to handle preferences.
  See Prefs.h for more information on how it works...

  Preference field specification:
  	/
		Version					- Audacity Version that created these prefs
		DefaultOpenPath			- Default directory for new file selector
	/FileFormats
		CopyOrEditUncompressedData - Copy data from uncompressed files or
			[ "copy", "edit"]   - edit in place?
		ExportFormat_SF1		   - Format to export PCM data in
                             (this number is a libsndfile1.0 format)
	/SamplingRate
		DefaultProjectSampleRate- New projects will have this rate
			[ 8000, 11025, 16000, 22050, 44100, 48000 ]
	/AudioIO
		PlaybackDevice			- device to use for playback
		RecordingDevice			- device to use for recording
			(these are device names understood by PortAudio)
	/Display
		WaveformColor			- 0xRRGGBB  --since it will be stored in
		ShadowColor				- 			  decimal, it will be somewhat
		SpectrumLowColor		- 			  non-intuitive to edit, but
		SpectrumHighColor		- 			  much easier to parse.
	/Locale
		Language				- two-letter language code for translations

	(*): wxGTK
	(+): wxWin
	($): wxMac

**********************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/app.h>
#include <wx/config.h>
#include <wx/intl.h>

#include "sndfile.h"

#ifdef __MACOSX__
#include <CoreServices/CoreServices.h>
#endif

#ifdef __MACOS9__
#include <Files.h>
#include <Folders.h>
#endif

#ifdef __WXMAC__
/* prototype of MoreFiles fn, included in wxMac already */
pascal OSErr FSpGetFullPath(const FSSpec * spec,
                            short *fullPathLength, Handle * fullPath);
#endif

#include "Prefs.h"

wxConfig *gPrefs = NULL;
int gMenusDirty = 0;

void InitPreferences()
{
//MERGE:
//Everything now uses Audacity name for preferences.
//(Audacity and CleanSpeech the same program and use
//the same preferences file).
#ifdef AUDACITY_NAME
   wxString appName = wxT(AUDACITY_NAME);
   wxString vendorName = wxT(AUDACITY_NAME);
#else
   wxString vendorName = wxT("Audacity");
   wxString appName = wxT("Audacity");
#endif

   wxTheApp->SetVendorName(vendorName);
   wxTheApp->SetAppName(appName);

   gPrefs = new wxConfig(appName);
   wxConfigBase::Set(gPrefs);

#ifdef __WXMAC__
#ifndef __UNIX__
   // This fixes changes in Mac filenames under wxWindows between versions
   // 0.95 and 0.96 of Audacity.
   wxString path;
   bool fix = false;   
   path = gPrefs->Read(wxT("/DefaultOpenPath"), wxT(""));
   if (path.Length() > 0 && path.Left(1)==wxT("/"))
      fix = true;
   path = gPrefs->Read(wxT("/DefaultExportPath"), wxT(""));
   if (path.Length() > 0 && path.Left(1)==wxT("/"))
      fix = true;
   path = gPrefs->Read(wxT("/Directories/TempDir"), wxT(""));
   if (path.Length() > 0 && path.Left(1)==wxT("/"))
      fix = true;
   if (fix) {
      gPrefs->Write(wxT("/DefaultOpenPath"), FROMFILENAME(::wxGetCwd()));
      gPrefs->Write(wxT("/DefaultExportPath"), FROMFILENAME(::wxGetCwd()));
      gPrefs->Write(wxT("/Directories/TempDir"), wxT(""));
      wxMessageBox(_("Some of your preferences were from an earlier version "
                     "of Audacity and have been reset."));
   }
#endif
#endif

   gPrefs->Write(wxT("/Version"), wxString(wxT(AUDACITY_VERSION_STRING)));

   // BG: Make sure the users prefs are up to date
   // BG: Otherwise reset some of them to their defaults
   wxString prefsversion;
   prefsversion = gPrefs->Read(wxT("/PrefsVersion"), wxT(""));

   if(prefsversion.CmpNoCase(wxString(wxT(AUDACITY_PREFS_VERSION_STRING))))
   {
      // BG: Reset the prefs by removing them
      if(gPrefs->Exists(wxT("/Keyboard")))
         gPrefs->DeleteGroup(wxT("/Keyboard"));
      if(gPrefs->Exists(wxT("/Locale")))
         gPrefs->DeleteGroup(wxT("/Locale"));
      gPrefs->Write(wxT("/PrefsVersion"), wxString(wxT(AUDACITY_PREFS_VERSION_STRING)));
   }
}

void FinishPreferences()
{
   if (gPrefs) {
      wxConfigBase::Set(NULL);
      delete gPrefs;
      gPrefs = NULL;
   }
}

int ReadExportFormatPref()
{
   return gPrefs->Read(wxT("/FileFormats/ExportFormat_SF1"),
                       (long int)(SF_FORMAT_WAV | SF_FORMAT_PCM_16));
}

void WriteExportFormatPref(int format)
{
   gPrefs->Write(wxT("/FileFormats/ExportFormat_SF1"), (long int)format);
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
// arch-tag: 4a8a9054-aec5-4093-8e02-fd65b646aeca


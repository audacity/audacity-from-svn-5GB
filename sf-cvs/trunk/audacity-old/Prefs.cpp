/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.cpp

  Dominic Mazzoni


  Preference field specification:
  	/
		Version					- Audacity Version that created these prefs
		DefaultOpenPath			- Default directory for new file selector
	/FileFormats
		CopyOrEditUncompressedData - Copy data from uncompressed files or
			[ "copy", "edit"]   - edit in place?
		DefaultExportFormat		- Default format to export data in
			[ "AIFF", "WAV", "IRCAM", "AU", "Ogg Vorbis", "MP3" ]
	/SamplingRate
		DefaultProjectSampleRate- New projects will have this rate
			[ 8000, 11025, 22050, 44100, 48000 ]
	/AudioIO
		PlaybackDevice(*)		- device file to use for playback
		RecordingDevice(*)		- device file to use for recording
	/Display
		WaveformColor			- 0xRRGGBB  --since it will be stored in
		ShadowColor				- 			  decimal, it will be somewhat
		SpectrumLowColor		-			  non-intuitive to edit, but
		SpectrumHighColor		-			  much easier to parse.
	

	(*): wxGTK
	(+): wxWin
	($): wxMac

**********************************************************************/

#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/config.h>
#endif

#ifdef __WXMAC__
#include <Files.h>
#include <Folders.h>

/* prototype of MoreFiles fn, included in wxMac already */
pascal OSErr FSpGetFullPath(const FSSpec * spec,
                            short *fullPathLength, Handle * fullPath);

#endif

#include "Audacity.h"
#include "Prefs.h"

wxConfig *gPrefs = NULL;

void InitPreferences()
{
   wxString vendorName = "Audacity";
   wxString appName = "Audacity";

   wxTheApp->SetVendorName(vendorName);
   wxTheApp->SetAppName(appName);

   gPrefs = new wxConfig(appName);
   wxConfigBase::Set(gPrefs);

#ifdef __WXMAC__
   // This fixes changes in Mac filenames under wxWindows between versions
   // 0.95 and 0.96 of Audacity.
   wxString path;
   bool fix = false;   
   path = gPrefs->Read("/DefaultOpenPath", "");
   if (path.Length() > 0 && path.Left(1)=="/")
      fix = true;
   path = gPrefs->Read("/DefaultExportPath", "");
   if (path.Length() > 0 && path.Left(1)=="/")
      fix = true;
   path = gPrefs->Read("/Directories/TempDir", "");
   if (path.Length() > 0 && path.Left(1)=="/")
      fix = true;
   if (fix) {
      gPrefs->Write("/DefaultOpenPath", ::wxGetCwd());
      gPrefs->Write("/DefaultExportPath", ::wxGetCwd());
      gPrefs->Write("/Directories/TempDir", "");
      wxMessageBox("Some of your preferences were from an earlier version of Audacity "
                   "and have been reset.");
   }
#endif

   gPrefs->Write("/Version", AUDACITY_VERSION_STRING);
}

void FinishPreferences()
{
   if (gPrefs) {
      wxConfigBase::Set(NULL);
      delete gPrefs;
      gPrefs = NULL;
   }
}

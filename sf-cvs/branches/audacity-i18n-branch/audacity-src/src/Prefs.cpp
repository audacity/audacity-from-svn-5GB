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
		ExportFormat		   - Format to export PCM data in
                             (this number is a libsndfile format)
      ExportFormatBits     - Bitsize of exported PCM data
         [ 8, 16, 24, 32 ]
	/SamplingRate
		DefaultProjectSampleRate- New projects will have this rate
			[ 8000, 11025, 16000, 22050, 44100, 48000 ]
	/AudioIO
      PlaybackDevice          - device to use for playback
		RecordingDevice         - device to use for recording
                     (these are device names understood by PortAudio)
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

#include "sndfile.h"

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
int gMenusDirty = 0;

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

int ReadExportFormatPref()
{
   return gPrefs->Read("/FileFormats/ExportFormat",
                       (long int)(SF_FORMAT_WAV | SF_FORMAT_PCM));
}

void WriteExportFormatPref(unsigned int format)
{
   gPrefs->Write("/FileFormats/ExportFormat", (long int)format);
}

int ReadExportFormatBitsPref()
{
   return gPrefs->Read("/FileFormats/ExportFormatBits",
                       (long int)16);
}

void WriteExportFormatBitsPref(int bits)
{
   gPrefs->Write("/FileFormats/ExportFormatBits", (long int)bits);
}


/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.cpp

  Dominic Mazzoni

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
pascal	OSErr	FSpGetFullPath(const FSSpec *spec,
							               short *fullPathLength,
							               Handle *fullPath);

#endif

#include "AudacityApp.h"
#include "Prefs.h"

wxConfig *gPrefs = NULL;

void InitPreferences()
{
  wxString vendorName = "Audacity";
  wxString appName = "Audacity";

  wxTheApp->SetVendorName(vendorName);
  wxTheApp->SetAppName(appName);

  #ifndef __WXMAC__
  
  gPrefs = new wxConfig(appName);
  wxConfigBase::Set(gPrefs);

  #else
  
  wxString prefsFileName = "Audacity Preferences";
  wxString prefsPath;
  FSSpec theSpec;
  OSErr err;
  
  err = FindFolder(kOnAppropriateDisk,
                   'pref',
                   true,
                   &theSpec.vRefNum,
                   &theSpec.parID);
  
  if (err) {
    gPrefs = (wxConfig *)wxConfigBase::Get();
    return;
  }

  Handle nameh;
  short namelen;
  theSpec.name[0] = 0;
  err = FSpGetFullPath(&theSpec,
                       &namelen,
                       &nameh);
  
  if (err) {
    gPrefs = (wxConfig *)wxConfigBase::Get();
    return;
  }
  
  namelen--; // FSpGetFullPath returns an extra ':' we don't want
  HLock(nameh);
  char *str = new char[namelen+1];
  memcpy(str, (char *)*nameh, namelen);
  str[namelen] = 0;
  HUnlock(nameh);
  DisposeHandle(nameh);
  
  prefsPath = ::wxMac2UnixFilename(str);
  prefsPath += prefsFileName;
  
  delete[] str;

  gPrefs = (wxConfig *) new wxFileConfig(appName,
                                         vendorName,
                                         prefsPath,
                                         prefsPath,
                                         wxCONFIG_USE_LOCAL_FILE);
  
  wxConfigBase::Set(gPrefs);
  
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

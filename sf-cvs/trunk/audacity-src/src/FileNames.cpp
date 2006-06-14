/**********************************************************************

  Audacity: A Digital Audio Editor

  FileNames.cpp

  James Crook

********************************************************************//**

\class FileNames
\brief Provides Static functions to yield filenames.

This class helps us with setting a base path, and makes it easier 
for us to keep track of the different kinds of files we read and write 
from.

JKC: In time I plan to add all file names and file extensions
used throughout Audacity into this one place.

*//********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include "Prefs.h"
#include "FileNames.h"

/// Returns the directory used for temp files.
/// \todo put a counter in here to see if it gets used a lot.
/// if it does, then maybe we should cache the path name 
/// each time.
wxString FileNames::TempDir()
{
   return gPrefs->Read(wxT("/Directories/TempDir"), wxT(""));
}

wxString FileNames::ThemeDir()
{
   return TempDir();
}

wxString FileNames::ThemeComponentsDir()
{
   return wxFileName( ThemeDir(), wxT("Components") ).GetFullPath();
}

wxString FileNames::ThemeCachePng()
{
   return wxFileName( ThemeDir(), wxT("ImageCache.png") ).GetFullPath();
}

wxString FileNames::ThemeCacheAsCee( )
{
   return wxFileName( ThemeDir(), wxT("ThemeAsCeeCode.h") ).GetFullPath();
}

wxString FileNames::ThemeComponent(const wxString &Str)
{
   return wxFileName( ThemeComponentsDir(), Str, wxT("png") ).GetFullPath();
}

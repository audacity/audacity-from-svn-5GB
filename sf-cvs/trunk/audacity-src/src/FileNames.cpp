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
#include <wx/stdpaths.h>
#include "Prefs.h"
#include "FileNames.h"

wxString FileNames::MkDir(const wxString &Str)
{
   wxFileName fn = Str;
printf("fn = %s %d\n", fn.GetPath().c_str(),fn.DirExists());

   // If the directory doesn't exist...
   if( !fn.DirExists() )
   {
printf("making\n"); 
      // Attempt to create it
      fn.Mkdir( fn.GetFullPath(), 511, wxPATH_MKDIR_FULL );
   }

   return fn.GetFullPath();
}

/// Returns the directory used for temp files.
/// \todo put a counter in here to see if it gets used a lot.
/// if it does, then maybe we should cache the path name 
/// each time.
wxString FileNames::TempDir()
{
   return FileNames::MkDir(gPrefs->Read(wxT("/Directories/TempDir"), wxT("")));
}

wxString FileNames::DataDir()
{
   // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
   //       between wxStandardPaths and wxConfig under Linux.  The latter
   //       creates a normal file as "$HOME/.audacity", while the former
   //       expects the ".audacity" portion to be a directory.
#if defined( __WXGTK__ )
   return FileNames::MkDir( wxStandardPaths::Get().GetUserDataDir() + wxT("-data") );
#else
   return FileNames::MkDir( wxStandardPaths::Get().GetUserDataDir() );
#endif
}

wxString FileNames::ThemeDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Theme") ).GetFullPath() );
}

wxString FileNames::ThemeComponentsDir()
{
   return FileNames::MkDir( wxFileName( ThemeDir(), wxT("Components") ).GetFullPath() );
}

wxString FileNames::ThemeCachePng()
{
   return wxFileName( ThemeDir(), wxT("ImageCache.png") ).GetFullPath();
}

wxString FileNames::ThemeCacheHtm()
{
   return wxFileName( ThemeDir(), wxT("ImageCache.htm") ).GetFullPath();
}

wxString FileNames::ThemeImageDefsAsCee()
{
   return wxFileName( ThemeDir(), wxT("ThemeImageDefsAsCee.h") ).GetFullPath();
}

wxString FileNames::ThemeCacheAsCee( )
{
   return wxFileName( ThemeDir(), wxT("ThemeAsCeeCode.h") ).GetFullPath();
}

wxString FileNames::ThemeComponent(const wxString &Str)
{
   return wxFileName( ThemeComponentsDir(), Str, wxT("png") ).GetFullPath();
}

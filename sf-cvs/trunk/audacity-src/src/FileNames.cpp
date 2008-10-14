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
#include "PlatformCompatibility.h"

static wxString gDataDir;

wxString FileNames::MkDir(const wxString &Str)
{
   // Behaviour of wxFileName::DirExists() and wxFileName::MkDir() has
   // changed between wx2.6 and wx2.8, so we use static functions instead.
   if (!wxFileName::DirExists(Str))
      wxFileName::Mkdir(Str, 511, wxPATH_MKDIR_FULL);

   return Str;
}

/// Returns the directory used for temp files.
/// \todo put a counter in here to see if it gets used a lot.
/// if it does, then maybe we should cache the path name 
/// each time.
wxString FileNames::TempDir()
{
   return FileNames::MkDir(gPrefs->Read(wxT("/Directories/TempDir"), wxT("")));
}

// originally an ExportMultiple method. Append suffix if newName appears in otherNames.
void FileNames::MakeNameUnique(wxArrayString &otherNames, wxFileName &newName)
{
   if (otherNames.Index(newName.GetFullName(), false) >= 0) {
      int i=2;
      wxString orig = newName.GetName();
      do {
         newName.SetName(wxString::Format(wxT("%s-%d"), orig.c_str(), i));
         i++;
      } while (otherNames.Index(newName.GetFullName(), false) >= 0);
   }
   otherNames.Add(newName.GetFullName());
}



//
// Audacity user data directories
wxString FileNames::AutoSaveDir()
{
   wxFileName autoSaveDir(FileNames::DataDir(), wxT("AutoSave"));
   return FileNames::MkDir(autoSaveDir.GetFullPath());
}

wxString FileNames::DataDir()
{
   // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
   //       between wxStandardPaths and wxConfig under Linux.  The latter
   //       creates a normal file as "$HOME/.audacity", while the former
   //       expects the ".audacity" portion to be a directory.
   if (gDataDir.IsEmpty())
   {
      // If there is a directory "Portable Settings" relative to the
      // executable's EXE file, the prefs are stored in there, otherwise
      // the prefs are stored in the user data dir provided by the OS.
      wxFileName exePath(PlatformCompatibility::GetExecutablePath());
      wxFileName portablePrefsPath(exePath.GetPath(), wxT("Portable Settings"));
      
      if (::wxDirExists(portablePrefsPath.GetFullPath()))
      {
         // Use "Portable Settings" folder
         gDataDir = portablePrefsPath.GetFullPath();
      } else
      {
         // Use OS-provided user data dir folder
         wxString dataDir;
#if defined( __WXGTK__ )
         dataDir = wxStandardPaths::Get().GetUserDataDir() + wxT("-data");
#else
         dataDir = wxStandardPaths::Get().GetUserDataDir();
#endif

         gDataDir = FileNames::MkDir(dataDir);
      }
   }
   
   return gDataDir;
}

wxString FileNames::HtmlHelpDir()
{
   // FIX-ME uses DataDir().  Should use directory where we expect help to be.
   // Perhaps it's same directory as Audacity.exe?
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Help") ).GetFullPath() );
}

wxString FileNames::HtmlHelpIndexFile()
{
   return wxFileName( HtmlHelpDir(), wxT("index.htm") ).GetFullPath();
}

wxString FileNames::ChainDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Chains") ).GetFullPath() );
}

wxString FileNames::NRPDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("NRP") ).GetFullPath() );
}

wxString FileNames::NRPFile()
{
   return wxFileName( NRPDir(), wxT("noisegate.nrp") ).GetFullPath();
}

wxString FileNames::PlugInDir()
{
   return FileNames::MkDir( wxFileName( DataDir(), wxT("Plug-Ins") ).GetFullPath() );
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

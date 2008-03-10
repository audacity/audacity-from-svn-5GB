/**********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibility.cpp

  Markus Meyer

*******************************************************************//*!

\class PlatformCompatibility
\brief Filename Compatibility utilities.

\see FileNames

*//*******************************************************************/


#ifdef _WIN32
#include <windows.h>
#endif

#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>
#include <wx/app.h>

#ifdef __WXMAC__
#include <wx/mac/private.h>
#endif

#include "AudacityApp.h"
#include "PlatformCompatibility.h"

wxString PlatformCompatibility::GetLongFileName(const wxString& shortFileName)
{
#ifndef _WIN32

   // On other platforms than Win32, this function is only a dummy
   return shortFileName;

#else

   wxString stringLeft = shortFileName;
   wxString s;
   wxString longFileName;

   while (!stringLeft.IsEmpty())
   {
      // Get next path component
      int end = stringLeft.Find('\\');
      
      if (end == -1) {
         s = stringLeft;
         stringLeft.Empty();
      } else {
         s = stringLeft.Left(end);
         stringLeft = stringLeft.Mid(end + 1);
      }

      // Process it unless it is empty, or the drive name
      if (!(s.IsEmpty() || (s.Length() == 2 && s.GetChar(1) == ':')))
      {
         // Now resolve for _every_ path component the long file name.
         // The standard way (only resolve components with '~' in it) is not
         // enough, because the user may have NameNumericTail turned off.
         wxString pathToFind = longFileName + s;
         WIN32_FIND_DATA findData;
         HANDLE findHandle = FindFirstFile(pathToFind.fn_str(), &findData);

         if (findHandle != INVALID_HANDLE_VALUE)
         {
            s = findData.cFileName; // cFileName always contains long file name
            FindClose(findHandle);
         }
      }

      // Note that we must be careful here to prevent discarding any
      // backslashes. It could e.g. be a network name, too!
      longFileName += s;
      if (end != -1)
         longFileName += wxT("\\");
   }

   return longFileName;

#endif
}

//
// Taken from http://wxwidgets.org/docs/technote/install.htm
//
wxString PlatformCompatibility::GetExecutablePath()
{
    static bool found = false;
    static wxString path;

    if (found)
        return path;
    else
    {
#ifdef __WXMSW__

        wxChar buf[512];
        *buf = '\0';
        GetModuleFileName(NULL, buf, 511);
        path = buf;

#elif defined(__WXMAC__)

        ProcessInfoRec processinfo;
        ProcessSerialNumber procno ;
        FSSpec fsSpec;

        procno.highLongOfPSN = 0 ;
        procno.lowLongOfPSN = kCurrentProcess ;
        processinfo.processInfoLength = sizeof(ProcessInfoRec);
        processinfo.processName = NULL;
        processinfo.processAppSpec = &fsSpec;

        GetProcessInformation( &procno , &processinfo ) ;
        path = wxMacFSSpec2MacFilename(&fsSpec);
#else
        wxString argv0 = wxGetApp().argv[0];

        if (wxIsAbsolutePath(argv0))
            path = argv0;
        else
        {
            wxPathList pathlist;
            pathlist.AddEnvList(wxT("PATH"));
            path = pathlist.FindAbsoluteValidPath(argv0);
        }

        wxFileName filename(path);
        filename.Normalize();
        path = filename.GetFullPath();
#endif
        found = true;
        return path;
    }
}

wxString PlatformCompatibility::ConvertSlashInFileName(const wxString& filePath)
{
#ifdef __WXMAC__
   wxString path = filePath;
   wxString filename;
   wxString newPath = filePath;
   int pathLen = 1;
   while (!wxDirExists(wxPathOnly(newPath)) && ! path.IsEmpty()) {
      path = newPath.BeforeLast('/');
      filename = newPath.AfterLast('/');
      newPath = path;
      newPath += ':';
      newPath += filename;
   }
   return newPath;
#else
   return filePath;
#endif
}
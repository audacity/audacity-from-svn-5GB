/**********************************************************************

  Audacity: A Digital Audio Editor

  FileNames.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_FILE_NAMES__
#define __AUDACITY_FILE_NAMES__

#include <wx/string.h>


// Uh, this is really a namespace rather than a class,
// since all the functions are static.
class FileNames
{
public:
   static wxString MkDir(const wxString &Str);
   static wxString TempDir();
   static wxString DataDir();
   static wxString AutoSaveDir();
   static wxString ChainDir();
   static wxString ThemeDir();
   static wxString ThemeComponentsDir();
   static wxString ThemeCachePng();
   static wxString ThemeCacheAsCee();
   static wxString ThemeComponent(const wxString &Str);
   static wxString ThemeCacheHtm();
   static wxString ThemeImageDefsAsCee();

private:
   // Private constructors: No one is ever going to instantiate it.
   // 
   FileNames(){;};
   ~FileNames(){;};
};

#endif

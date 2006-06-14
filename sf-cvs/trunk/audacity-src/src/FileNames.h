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
   static wxString TempDir();
   static wxString ThemeDir();
   static wxString ThemeComponentsDir();
   static wxString ThemeCachePng();
   static wxString ThemeCacheAsCee();
   static wxString ThemeComponent(const wxString &Str);
   
private:
   // Private constructors: No one is ever going to instantiate it.
   // 
   FileNames(){;};
   ~FileNames(){;};
};

#endif

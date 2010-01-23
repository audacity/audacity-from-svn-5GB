/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  platform/DiskFunctions.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_DISK_FUNCTIONS__
#define __MEZZO_DISK_FUNCTIONS__

#include <string>

namespace Mezzo {

namespace Platform {

   class FileName {
    public:
      FileName(std::string fileName):mFileName(fileName) { }
      FileName(char *fileName):mFileName(fileName) { }
      FileName() { }

      const FileName& operator=(const std::string fileName) { mFileName = fileName; return *this; }

      std::string GetFullPath() { return mFileName; }

    private:
      std::string mFileName;
   };

   bool FileExists(FileName fileName);
   bool DirExists(std::string dirName);

   // if directory cannot be created, or is already created but cannot be
   // written to (perhaps for permissions reasons) throw an exception.
   void CreateDirectory(std::string dirName);
   void DeleteDirectory(std::string dirName, bool recurse=true);
   int GetFilesInDir(std::string dirName);

   void DeleteFile(FileName fileName);

   void CopyFile(FileName from, FileName to);
   void MoveFile(FileName from, FileName to);

   extern const char DirSeparator;

} // namespace

} // namespace

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


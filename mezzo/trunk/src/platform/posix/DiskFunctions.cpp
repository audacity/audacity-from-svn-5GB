/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  platform/posix/DiskFunctions.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "../DiskFunctions.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <dirent.h>

namespace Mezzo {

namespace Platform {

const char DirSeparator = '/';

bool FileExists(FileName fileName)
{
   struct stat fileInfo;
   int ret = stat(fileName.GetFullPath().c_str(), &fileInfo);

   return (ret == 0 && S_ISREG(fileInfo.st_mode));
}

bool DirExists(std::string dirName)
{
   struct stat fileInfo;
   int ret = stat(dirName.c_str(), &fileInfo);

   return (ret == 0 && S_ISDIR(fileInfo.st_mode));
}

// if directory cannot be created, or is already created but cannot be
// written to (perhaps for permissions reasons) throw an exception.
void CreateDirectory(std::string dirName)
{
   if(DirExists(dirName)) return;

   //mkdir(dirName.c_str(), DEFFILEMODE);
   std::string command = "mkdir " + dirName;
   system(command.c_str());
}

void DeleteDirectory(std::string dirName, bool recurse)
{
   std::string command;
   if(recurse)
      command = "rm -rf " + dirName;
   else
      command = "rmdir " + dirName;

   //printf(command.c_str());
   system(command.c_str());
}

int GetFilesInDir(std::string dirName)
{
   DIR *dir = opendir(dirName.c_str());
   int numFiles = -2;  // there are always two entries for . and ..

   while(readdir(dir))
      numFiles++;

   return numFiles;
}

void DeleteFile(FileName fileName)
{
   unlink(fileName.GetFullPath().c_str());
}

void CopyFile(FileName from, FileName to)
{
   /*
   std::string destFileName = dirName;
   int sourceFile = open(fileName.c_str(), O_RDONLY, 0);
   int destFile   = open(destFileName.c_str(), O_WRONLY, DEFFILEMODE);
   */

   // ... more to come
}

void MoveFile(FileName from, FileName to)
{
   // if on the same volume, link new and unlink old, otherwise copy
   // and delete
   std::string command = "mv " + from.GetFullPath() + " " + to.GetFullPath();
   system(command.c_str());
}

} // namespace

} // namespace

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


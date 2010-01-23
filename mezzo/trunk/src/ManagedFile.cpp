/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  ManagedFile.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "ManagedFile.h"

#include "ManagedFileContext.h"

namespace Mezzo {

ManagedFile::ManagedFile(std::string fileName, std::string fullPathName):
   mLocked(false),
   mRefCount(1),
   mFileName(fileName),
   mFullPathName(fullPathName)
{
}

ManagedFile::~ManagedFile()
{
}

} // namespace

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

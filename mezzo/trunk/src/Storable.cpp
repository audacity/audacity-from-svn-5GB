/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Storable.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "Storable.h"
#include "Util.h"
#include "Exceptions.h"

namespace Mezzo {

std::string Storer::CreateID(Storable *obj)
{
   std::string id = fmt("%d", ++mLastID);
   mIdObjects[obj] = id;
   return id;
}

std::string Storer::GetID(Storable *obj)
{
   ClientAssert(mIdObjects.count(obj) == 1, "This storer doesn't have an ID for that object");
   return mIdObjects[obj];
}

void Loader::RegisterObj(std::string id, Storable *obj)
{
   mIdObjects[id] = obj;
}

Storable *Loader::GetObj(std::string id)
{
   ClientAssert(mIdObjects.count(id) == 1, "This loader doesn't have an object for ID " + id);
   return mIdObjects[id];
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


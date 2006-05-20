/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Storable.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_STORABLE__
#define __MEZZO_STORABLE__

#include <map>
#include <string>
#include <set>

namespace Mezzo {

typedef std::map<std::string,std::string> AttrDict;

class ManagedFile;
class Storable;

/// An object that can store hierarchical data in a persistent medium (eg. a disk file)

/// An abstract interface for an object that can store hierarchical data in
/// a persistent medium (eg. to a disk file).  The most common implementation
/// is to store to an XML file, as in XMLStorer.
///
/// Since the structures being stored are not strictly hierarchical, there
/// is a facility for creating identifiers for objects that makes it possible
/// to create references between objects from different hierarchies.  When
/// an object that may be referenced saves itself, it will create an ID
/// that is associated with its memory address.  Later, when a different object
/// wants to refer to the first object, it asks the storer if there is an
/// ID associated with that address, and if so it can just store the ID.

class Storer
{
 public:
   virtual ~Storer() { }

   /// Store the beginning of a hierarchical node.
   virtual void StoreBeginNode(std::string name, AttrDict attr) = 0;

   /// Store the end of a hierarchical node.
   virtual void StoreEndNode(std::string name) = 0;

   /// Store a node that doesn't have any children.
   virtual void StoreLeafNode(std::string name, AttrDict attr) = 0;

   /// Create an ID that is associated with this object
   std::string CreateID(Storable *obj);

   /// Retrieve an ID associated with this object
   std::string GetID(Storable *obj);

 protected:
   Storer():mLastID(0) { }

 private:
   std::map<Storable*,std::string> mIdObjects;
   int mLastID;
};

/// An object that can load hierarchical data from a persistent medium (eg. a disk file)

/// An abstract interface for an object that can load hierarchical data from
/// a persistent medium (eg. to a disk file).  The most common implementation
/// is to load from an XML file, as in XMLLoader.  This class loads data
/// stored by a corresponding Storer class.
///
/// Since the structures being loaded are not strictly hierarchical, there
/// is a facility for creating identifiers for objects that makes it possible
/// to create references between objects from different hierarchies.  When
/// an object that that is being loaded sees that an ID has been stored for that
/// object, it will register itself with the loader so that other objects can
/// obtain a pointer to this object.

class Loader
{
 public:
   virtual ~Loader() { }

   struct Token {
      enum {
         beginNode,
         endNode
      } type;
      std::string name;
      AttrDict attrs;
   };

   virtual Token GetNextToken() = 0;
   virtual Token PeekNextToken() = 0;

   void RegisterObj(std::string id, Storable *obj);
   Storable *GetObj(std::string id);

 private:
   std::map<std::string,Storable*> mIdObjects;
};

class Storable
{
 public:
   virtual ~Storable() { }

   virtual void Store(Storer& storer) = 0;

   /// Get the list of storables (if any) that must be stored before this object can be stored
   virtual std::set<Storable*> GetPrereqStorables() { std::set<Storable*> x; return x; }

   /// Get the list of ManagedFiles (if any) that must be locked for this object to be loadable
   virtual std::set<ManagedFile*> GetManagedFilesInUse() { std::set<ManagedFile*> x; return x; }

   static Storable* Load(Loader& loader);
};

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


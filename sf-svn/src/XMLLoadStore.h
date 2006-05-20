/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  XMLLoadStore.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_XMLLOADSTORE__
#define __MEZZO_XMLLOADSTORE__

#include <stdio.h>
#include <queue>

#include <expat/xmlparse.h>
#include "Storable.h"

namespace Mezzo {

class XMLStorer : public Storer {
 public:
   XMLStorer(std::string outFile, int startingIndent = 0);

   ~XMLStorer();
   void StoreBeginNode(std::string name, AttrDict attrs);
   void StoreEndNode(std::string name);

   // short for StoreBeginNode() and StoreEndNode()
   void StoreLeafNode(std::string name, AttrDict attrs);

 private:
   FILE* mFile;
   int   mLevel;
};

class XMLLoader : public Loader {
 public:
   XMLLoader(std::string fileName);
   ~XMLLoader();

   Token GetNextToken();
   Token PeekNextToken();

 private:
   static void StartElementCallback(void *userData, const char *name, const char **attr);
   static void EndElementCallback(void *userData, const char *name);

   FILE* mFile;
   std::queue<Token> mPendingTokens;
   XML_Parser mParser;
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


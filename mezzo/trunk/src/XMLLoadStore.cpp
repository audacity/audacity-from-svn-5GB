/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  XMLLoadStore.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "XMLLoadStore.h"

#include "Exceptions.h"
#include "Util.h"

namespace Mezzo {

static void PrintTabs(FILE *file, int numTabs)
{
   for(int i = 0; i < numTabs; i++)
      fprintf(file, "\t");
}

XMLStorer::XMLStorer(std::string outFile, int startingIndent):
   mFile(fopen(outFile.c_str(), "w")),
   mLevel(startingIndent)
{
   UserAssert(mFile, "Unable to open output file " + outFile);
}

XMLStorer::~XMLStorer()
{
   fclose(mFile);
}

void XMLStorer::StoreBeginNode(std::string name, AttrDict attrs)
{
   PrintTabs(mFile, mLevel++);
   fprintf(mFile, "<%s", name.c_str());

   for(AttrDict::const_iterator i = attrs.begin(); i != attrs.end(); i++)
      fprintf(mFile, " %s=\"%s\"", i->first.c_str(), i->second.c_str());

   fprintf(mFile, ">\n");
}

void XMLStorer::StoreEndNode(std::string name)
{
   PrintTabs(mFile, --mLevel);
   fprintf(mFile, "</%s>\n", name.c_str());
}

void XMLStorer::StoreLeafNode(std::string name, AttrDict attrs)
{
   PrintTabs(mFile, mLevel);
   fprintf(mFile, "<%s", name.c_str());

   for(AttrDict::const_iterator i = attrs.begin(); i != attrs.end(); i++)
      fprintf(mFile, " %s=\"%s\"", i->first.c_str(), i->second.c_str());

   fprintf(mFile, "/>\n");
}

// -------------------------------------------------------------------------------------
//
// XMLLoader
//
// -------------------------------------------------------------------------------------

XMLLoader::XMLLoader(std::string fileName):
   mFile(fopen(fileName.c_str(), "r")),
   mParser(XML_ParserCreate(NULL))
{
   UserAssert(mFile, fmt("Unable to open XML file %s for reading", fileName.c_str()));
   UserAssert(mParser, "Unable to create expat XML Parser");

   XML_SetElementHandler(mParser, XMLLoader::StartElementCallback, XMLLoader::EndElementCallback);
   XML_SetUserData(mParser, this);
}

XMLLoader::~XMLLoader()
{
   fclose(mFile);
   XML_ParserFree(mParser);
}

Loader::Token XMLLoader::PeekNextToken()
{
   while(mPendingTokens.size() == 0)
   {
      const int CHARS_PER_RUN = 50;
      void *buf = XML_GetBuffer(mParser, CHARS_PER_RUN);
      int read = fread(buf, 1, CHARS_PER_RUN, mFile);
      int ret = XML_ParseBuffer(mParser, read, feof(mFile));

      UserAssert(ret != 0, fmt("Error parsing XML: %s at line %d, column %d",
                                           XML_ErrorString(XML_GetErrorCode(mParser)),
                                           XML_GetCurrentLineNumber(mParser),
                                           XML_GetCurrentColumnNumber(mParser)));
   }

   return mPendingTokens.front();
}

Loader::Token XMLLoader::GetNextToken()
{
   Token toReturn = PeekNextToken();
   mPendingTokens.pop();
   return toReturn;
}

// static
void XMLLoader::StartElementCallback(void *userData, const char *name, const char **attrs)
{
   XMLLoader *This = static_cast<XMLLoader*>(userData);

   Token token;
   token.name = name;
   token.type = Loader::Token::beginNode;

   while(*attrs)
   {
      const char *attr = *attrs++;
      const char *value = *attrs++;

      if(!value)
         break;

      token.attrs[attr] = value;
   }

   This->mPendingTokens.push(token);
}

// static
void XMLLoader::EndElementCallback(void *userData, const char *name)
{
   XMLLoader *This = static_cast<XMLLoader*>(userData);

   Token token;
   token.name = name;
   token.type = Loader::Token::endNode;

   This->mPendingTokens.push(token);
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


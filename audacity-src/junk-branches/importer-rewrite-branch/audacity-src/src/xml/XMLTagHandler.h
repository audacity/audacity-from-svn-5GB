/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLTagHandler.h

  Dominic Mazzoni

  This class is an interface which should be implemented by
  classes which wish to be able to load and save themselves
  using XML files.

**********************************************************************/

#include <stdio.h>

#ifndef __AUDACITY_XML_TAG_HANDLER__
#define __AUDACITY_XML_TAG_HANDLER__

class XMLTagHandler {
 public:

   // This method will be called on your class if your class has
   // been registered to handle this particular tag.  Parse the
   // tag and the attribute-value pairs (null-terminated), and
   // return true on success, and false on failure.  If you return
   // false, you will not get any calls about children.
   virtual bool HandleXMLTag(const char *tag, const char **attrs) = 0;

   // If the XML document has children of your tag, this method
   // should be called.  Typically you should construct a new
   // object for the child, insert it into your own local data
   // structures, and then return it.  If you do not wish to
   // handle this child, return NULL and it will be ignored.
   virtual XMLTagHandler *HandleXMLChild(const char *tag) = 0;

   // When this method is called, write your own tag and tags for
   // all of your children to the file.  One tag should appear
   // per line, and each tag should be preceded with [depth]
   // tab characters.
   virtual void WriteXML(int depth, FILE *fp) = 0;
};

#endif // define __AUDACITY_XML_TAG_HANDLER__

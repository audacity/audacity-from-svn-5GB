/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandsReader.h

  Brian Gunlogson

  This class handles the parsing of the commands.cfg file.

**********************************************************************/

#ifndef __AUDACITY_COMMANDS_READER__
#define __AUDACITY_COMMANDS_READER__

#include "xmlparse/xmlparse.h"
#include "CommandsMenu.h"

struct CommandsTranslationItem
{
   wxString translatedName;
   wxString untranslatedName;
};

WX_DEFINE_ARRAY(CommandsTranslationItem *, CommandsTranslationArray);

class CommandsReader
{
public:
   CommandsReader();
   ~CommandsReader();

   //forewarder functions
   CommandsMenu * GetCommandsMenu() { return &mCommandsMenu; };

   bool ParseFile();

private:
   void StartTag(const char *tag);
   void EndTag(const char *tag);

   const char * GetAttributeData(const char *attribute, const char **attrs);

   void HandleXMLTagStart(const char *tag, const char **attrs);
   void HandleXMLTagEnd(const char *tag);

   // Callback functions for expat
   static void startElement(void *userData, const char *name, const char **atts);
   static void endElement(void *userData, const char *name);

   XML_Parser mParser;
   int mDepth;

   CommandsMenu mCommandsMenu;

private:
   void InitializeTranslationStrings();
   CommandsTranslationItem * GetTranslationItem(wxString& untranslatedName);
   wxString GetTranslatedName(wxString& untranslatedName);
   const char * GetTranslation(const char *utName, const char *tName);

   CommandsTranslationArray mCommandsTranslationArray;
};

#endif

/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandsReader.cpp

  Brian Gunlogson

  This class handles the parsing of the commands XML file.

**********************************************************************/

#include "CommandsReader.h"
#include "CommandsCfg.h"

#include <wx/string.h>
#include <wx/file.h>
#include <wx/msgdlg.h>
#include <wx/intl.h>

#include "../Prefs.h"
#include "../FileFormats.h"
#include "sndfile.h"

CommandsReader::CommandsReader() : mParser(NULL), mDepth(-1)
{
   InitializeTranslationStrings();
}

CommandsReader::~CommandsReader()
{
   if(mParser)
      XML_ParserFree(mParser);

   WX_CLEAR_ARRAY(mCommandsTranslationArray);
   mCommandsTranslationArray.Clear();
}

bool CommandsReader::ParseFile()
{
   if(mParser)
      XML_ParserFree(mParser);

   mParser = XML_ParserCreate(NULL);
   XML_SetUserData(mParser, (void *)this);
   XML_SetElementHandler(mParser, startElement, endElement);

   if(!wxFileExists(gCommandsCfgLocation))
   {
      wxMessageBox(gCommandsCfgLocation + " does not exist!");
      return false;
   }

   FILE *fp = fopen(gCommandsCfgLocation, "rb");
   if(!fp || ferror(fp))
   {
      wxMessageBox("Could not open file: " + gCommandsCfgLocation);
      return false;
   }

   const size_t bufferSize = 16384;
   char buffer[16384];
   int done = 0;
   do
   {
      size_t len = fread(buffer, 1, bufferSize, fp);
      done = (len < bufferSize);

      if(!XML_Parse(mParser, buffer, len, done))
      {
         const char *formatStr = "Error: %s at line %d";
         const char *errorStr = XML_ErrorString(XML_GetErrorCode(mParser));
         wxMessageBox(wxString::Format(formatStr, errorStr, XML_GetCurrentLineNumber(mParser)));
         fclose(fp);
         return false;
      }
   } while (!done);

   fclose(fp);

   XML_ParserFree(mParser);
   mParser = NULL;

   return true;
}

const char * CommandsReader::GetAttributeData(const char *attribute, const char **attrs)
{
   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while(*attrs)
   {
      const char *attr = *attrs++;
      const char *value = *attrs++;

      if (!value)
         break;

      if (!strcmp(attr, attribute))
      {
         return value;
      }
   }

   return NULL;
}

void CommandsReader::HandleXMLTagStart(const char *tag, const char **attrs)
{
   if(!mDepth && strcmp(tag, "commands"))
   {
      wxMessageBox(wxString::Format("Error: The commands tag must be the first and outermost user defined tag. Detected tag %s. Line %d", XML_GetCurrentLineNumber(mParser), tag));
      return;
   }

   if(!strcmp(tag, "commands"))
   {
      if(mDepth != 0)
      {
         wxMessageBox(wxString::Format("Error: The commands tag is nested. Line %d", XML_GetCurrentLineNumber(mParser)));
         return;
      }
   }
   else if(!strcmp(tag, "menubar"))
   {
      const char *menubar_name = GetAttributeData("name", attrs);

      if(!menubar_name)
      {
         wxMessageBox(wxString::Format("Error: menubar tag did not have the required attribute 'name'. Line %d", XML_GetCurrentLineNumber(mParser)));
         return;
      }

      mCommandsMenu.AddMenuBar((wxString)menubar_name);
   }
   else if(!strcmp(tag, "menu"))
   {
      const char *menu_name = GetTranslation(GetAttributeData("name", attrs), GetAttributeData("translation", attrs));

      if(!menu_name)
      {
         wxMessageBox(wxString::Format("Error: menu tag did not have the required attribute 'name' (or 'translation'). Line %d", XML_GetCurrentLineNumber(mParser)));
         return;
      }

      mCommandsMenu.BeginMenu((wxString)menu_name);
   }
   else if(!strcmp(tag, "submenu"))
   {
      const char *submenu_name = GetTranslation(GetAttributeData("name", attrs), GetAttributeData("translation", attrs));

      if(!submenu_name)
      {
         wxMessageBox(wxString::Format("Error: submenu tag did not have the required attribute 'name' (or 'translation'). Line %d", XML_GetCurrentLineNumber(mParser)));
         return;
      }

      mCommandsMenu.BeginSubMenu((wxString)submenu_name);
   }
   else if(!strcmp(tag, "item"))
   {
      wxString s_item_keys;
      const char *item_name = GetTranslation(GetAttributeData("name", attrs), GetAttributeData("translation", attrs));
      const char *item_functions = GetAttributeData("functions", attrs);
      const char *item_keys = GetAttributeData("keys", attrs);

      if(!item_name)
      {
         wxMessageBox(wxString::Format("Error: item tag did not have the required attribute 'name' (or 'translation'). Line %d", XML_GetCurrentLineNumber(mParser)));
         return;
      }

      if(!item_functions)
      {
         wxMessageBox(wxString::Format("Error: item tag did not have the required attribute 'functions'. Line %d", XML_GetCurrentLineNumber(mParser)));
         return;
      }

      if(!item_keys)
      {
         s_item_keys = "";
      }
      else
      {
         s_item_keys = wxString(item_keys);
      }

      mCommandsMenu.AddItem(wxString(item_name), wxString(item_functions), s_item_keys);
   }
   else if(!strcmp(tag, "itemseparator"))
   {
      mCommandsMenu.AddSeparator();
   }
   else if(!strcmp(tag, "dynamicitems"))
   {
      const char *dynamicitem_name = GetAttributeData("name", attrs);

      if(!dynamicitem_name)
      {
         wxMessageBox(wxString::Format("Error: dynamicitems tag did not have the required attribute 'name'. Line %d", XML_GetCurrentLineNumber(mParser)));
         return;
      }

      mCommandsMenu.AddDynamicItem((wxString)dynamicitem_name);
   }
   else
   {
      wxMessageBox(wxString::Format("Error: Unknown tag %s. Line %d", tag, XML_GetCurrentLineNumber(mParser)));
      return;
   }
}

void CommandsReader::HandleXMLTagEnd(const char *tag)
{
   if(!strcmp(tag, "menu"))
   {
      mCommandsMenu.EndMenu();
   }
   else if(!strcmp(tag, "submenu"))
   {
      mCommandsMenu.EndSubMenu();
   }
}

void CommandsReader::startElement(void *userData, const char *name, const char **atts)
{
   CommandsReader *This = (CommandsReader *)userData;

   This->mDepth++;

   This->HandleXMLTagStart(name, atts);
}

void CommandsReader::endElement(void *userData, const char *name)
{
   CommandsReader *This = (CommandsReader *)userData;

   This->HandleXMLTagEnd(name);

   This->mDepth--;
}

void CommandsReader::InitializeTranslationStrings()
{
//BG: Generate an array of translated command names, and descriptions
#define AUDACITY_COMMANDS_TRANSLATION_STRINGS
#include "CommandsDefaultData.h"
#undef AUDACITY_COMMANDS_TRANSLATION_STRINGS
}

CommandsTranslationItem * CommandsReader::GetTranslationItem(wxString untranslatedName)
{
   for(int i = 0; i < (int)mCommandsTranslationArray.GetCount(); i++)
   {
      if(untranslatedName.CmpNoCase(mCommandsTranslationArray[i]->untranslatedName) == 0)
         return mCommandsTranslationArray[i];
   }

   return NULL;
}

wxString CommandsReader::GetTranslatedName(wxString untranslatedName)
{
   CommandsTranslationItem *tmpTranslation = GetTranslationItem(untranslatedName);

   if(!tmpTranslation)
      return "";

   return tmpTranslation->translatedName;
}

const char * CommandsReader::GetTranslation(const char *utName, const char *tName)
{
   if(!utName)
   {
      if(!tName)
      {
         return NULL;
      }
      else
      {
         return GetTranslatedName((wxString)tName).c_str();
      }
   }

   return utName;
}

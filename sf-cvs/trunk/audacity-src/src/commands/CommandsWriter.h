/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandsWriter.h

  Brian Gunlogson

  This class handles writing the default commands XML file.

**********************************************************************/

#ifndef __AUDACITY_COMMANDS_DEFAULT__
#define __AUDACITY_COMMANDS_DEFAULT__

class CommandsWriter
{
public:
   CommandsWriter();
   ~CommandsWriter();

   bool WriteXML();
};

#endif

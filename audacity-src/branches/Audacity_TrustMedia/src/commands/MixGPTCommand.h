/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2011 Audacity Team.

   MixGPT.h
   Michael Chinen
***********************************************************************/

#ifndef __MIXGPT_H__
#define __MIXGPT_H__

#include "Command.h"
#include "CommandType.h"

class MixGPTCommandType : public CommandType
{
public:

   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class MixGPTCommand : public CommandImplementation
{
public:
   MixGPTCommand(CommandType &type,
                    CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~MixGPTCommand();
   virtual bool Apply(CommandExecutionContext context);
};

#endif 


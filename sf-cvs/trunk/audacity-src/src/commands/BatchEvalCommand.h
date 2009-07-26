/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   File License: wxwidgets

   BatchEvalCommand.h
   Dan Horgan

******************************************************************//**

\class BatchEvalCommand
\brief Given a string representing a command, pass it to the BatchCommands
system.

The eventual aim is to move the code from BatchCommands out into separate
command classes, but until this happens, BatchEvalCommand can act as a 'bridge'
to that system.

*//*******************************************************************/

#ifndef __BATCHEVALCOMMAND__
#define __BATCHEVALCOMMAND__

#include "Command.h"
#include "../BatchCommands.h"

class BatchEvalCommand : public Command
{
public:
   BatchEvalCommand(const wxString &cmdName,
                    const ParamMap &signature,
                    CommandOutputTarget *target)
      : Command(cmdName, signature, target)
   { }

   BatchEvalCommand(CommandOutputTarget *target)
      : Command(BuildName(), BuildSignature(), target)
   { }

   virtual ~BatchEvalCommand() { }

   virtual bool Apply(CommandExecutionContext context);

   static wxString BuildName();
   static ParamMap BuildSignature();
};

#endif /* End of include guard: __BATCHEVALCOMMAND__ */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: TBD

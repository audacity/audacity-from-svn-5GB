/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   License: GPL v2.  See License.txt.

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
private:
   wxString mCmdName;
   wxString mCmdParams;

public:
   BatchEvalCommand(const wxString &cmdName, const wxString &cmdParams) : Command(wxT("BatchEvalCommand")), mCmdName(cmdName), mCmdParams(cmdParams) {}
   ~BatchEvalCommand() {}

   virtual bool Apply(CommandExecutionContext context)
   {
      // Create a Batch that will have just one command in it...
      BatchCommands Batch;

      return Batch.ApplyCommand(mCmdName, mCmdParams);
   }
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

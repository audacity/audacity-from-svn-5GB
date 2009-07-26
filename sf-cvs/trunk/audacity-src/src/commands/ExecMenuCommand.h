/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ExecMenuCommand.h
\brief Contains declaration of ExecMenuCommand class.

\class ExecMenuCommand
\brief A command which asks the CommandManager to execute a menu command by
name.

*//*******************************************************************/

#ifndef __EXECMENUCOMMAND__
#define __EXECMENUCOMMAND__

#include "Command.h"
#include "CommandManager.h"
#include "../Project.h"

class ExecMenuCommand : public Command
{
   public:
      ExecMenuCommand(const wxString &cmdName,
                      const ParamMap &signature,
                      CommandOutputTarget *target)
         : Command(cmdName, signature, target)
      { }
      ~ExecMenuCommand() { }

      virtual bool Apply(CommandExecutionContext context);
      static wxString BuildName();
      static ParamMap BuildSignature();
};

#endif /* End of include guard: __EXECMENUCOMMAND__ */

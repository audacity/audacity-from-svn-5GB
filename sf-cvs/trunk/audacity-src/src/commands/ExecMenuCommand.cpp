/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ExecMenuCommand.cpp
\brief Contains definitions for ExecMenuCommand class.

*//*******************************************************************/

#include "ExecMenuCommand.h"

bool ExecMenuCommand::Apply(CommandExecutionContext context)
{
   CommandManager *cmdManager = context.proj->GetCommandManager();

   wxString cmdName = GetString(wxT("CommandName"));
   wxUint32 cmdFlags = 0; // TODO ?
   wxUint32 cmdMask = 0;
   return cmdManager->HandleTextualCommand(cmdName, cmdFlags, cmdMask);
}

wxString ExecMenuCommand::BuildName()
{
   return wxT("MenuCommand");
}

ParamMap ExecMenuCommand::BuildSignature()
{
   ParamMap signature;
   Validator menuCommandValidator;
   signature[wxT("CommandName")] =
      std::pair<wxVariant, Validator>(wxT(""), menuCommandValidator);
   return signature;
}

/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file GetAllMenuCommands.cpp
\brief Contains definitions for GetAllMenuCommands class.

*//*******************************************************************/

#include "GetAllMenuCommands.h"
#include "../Project.h"
#include "CommandManager.h"

wxString GetAllMenuCommandsType::BuildName()
{
   return wxT("GetAllMenuCommands");
}

void GetAllMenuCommandsType::BuildSignature(CommandSignature &signature)
{
   return;
}

Command *GetAllMenuCommandsType::Create(CommandOutputTarget *target)
{
   return new GetAllMenuCommands(*this, target);
}

bool GetAllMenuCommands::Apply(CommandExecutionContext context)
{
   wxArrayString names;
   context.proj->GetCommandManager()->GetAllCommandNames(names, false);
   wxArrayString::iterator iter;
   for (iter = names.begin(); iter != names.end(); ++iter)
   {
      Status(*iter);
   }
   return true;
}


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

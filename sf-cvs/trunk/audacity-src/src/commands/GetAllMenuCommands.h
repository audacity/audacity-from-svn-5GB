/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file GetAllMenuCommands.h
\brief Contains declaration of GetAllMenuCommands class.

\class GetAllMenuCommands
\brief Command which outputs a list of available menu commands on the status
channel.

*//*******************************************************************/

#ifndef __GETALLMENUCOMMANDS__
#define __GETALLMENUCOMMANDS__

#include "Command.h"

class GetAllMenuCommands : public Command
{
public:
   GetAllMenuCommands(const wxString &cmdName,
                      const ParamMap &signature,
                      CommandOutputTarget *target)
      : Command(cmdName, signature, target)
   { }

   virtual ~GetAllMenuCommands() 
   { }

   virtual bool Apply(CommandExecutionContext context);
   static wxString BuildName();
   static ParamMap BuildSignature();
};

#endif /* End of include guard: __GETALLMENUCOMMANDS__ */

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

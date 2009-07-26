/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file DebugPrintCommand.h
\brief Contains definition of DebugPrintCommand class.

*//***************************************************************//***

\class DebugPrintCommand
\brief Command to tell Audacity to print out a message.

Intended for testing and debugging.

*//*******************************************************************/

#ifndef __DEBUGPRINTCOMMAND__
#define __DEBUGPRINTCOMMAND__

#include <wx/msgout.h>
#include "Command.h"

class DebugPrintCommand : public Command
{
public:
   DebugPrintCommand(const wxString &cmdName,
                     const ParamMap &signature,
                     CommandOutputTarget *target)
      : Command(cmdName, signature, target) {}
   ~DebugPrintCommand() {}

   virtual bool Apply(CommandExecutionContext context)
   {
      wxString message = GetString(wxT("DebugString"));
      wxMessageOutputDebug.Printf(message);
      return true;
   }

   static wxString BuildName()
   {
      return wxT("DebugMessage");
   }

   static ParamMap BuildSignature()
   {
      ParamMap signature;
      Validator stringValidator;
      signature[wxT("DebugString")] =
         std::pair<wxVariant, Validator>(wxT(""), stringValidator);
      return signature;
   }
};

#endif /* End of include guard: __DEBUGPRINTCOMMAND__ */

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

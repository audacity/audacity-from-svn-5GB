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

#include "Command.h"

class DebugPrintCommand : public Command
{
private:
   wxString mMessage;

public:
   DebugPrintCommand(const wxString &message) : Command(wxT("DebugPrintCommand")), mMessage(message) {}
   ~DebugPrintCommand() {}

   virtual bool Apply(CommandExecutionContext context)
   {
      wxMessageOutputDebug().Printf(wxT("In DebugPrintCommand::Apply"));
      wxMessageOutputDebug().Printf(mMessage);
      return true;
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

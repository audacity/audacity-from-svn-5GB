/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file ScriptCommandRelay.cpp
\brief Contains definitions for ScriptCommandRelay

*//****************************************************************//**

\class ScriptCommandRelay
\brief ScriptCommandRelay is just a way to move some of the scripting-specific
code out of LoadModules.

*//*******************************************************************/

#include "ScriptCommandRelay.h"
#include "../BatchCommands.h"
#include "../Audacity.h"
#include "Command.h"
#include "CommandBuilder.h"
#include "AppCommandEvent.h"
#include "CommandHandler.h"
#include "ResponseQueue.h"
#include <wx/wx.h>

#include "../Project.h"

// Declare static class members
CommandHandler *ScriptCommandRelay::sCmdHandler;
tpRegScriptServerFunc ScriptCommandRelay::sScriptFn;
ResponseQueue ScriptCommandRelay::sResponseQueue;

void ScriptCommandRelay::SetRegScriptServerFunc(tpRegScriptServerFunc scriptFn)
{ sScriptFn = scriptFn; }

void ScriptCommandRelay::SetCommandHandler(CommandHandler &ch)
{ sCmdHandler = &ch; }

void ScriptCommandRelay::Run()
{
   wxASSERT( sScriptFn != NULL );
   while( true )
      sScriptFn(&ExecCommand);
}

/// This is the function which actually obeys one command.  Rather than applying
/// the command directly, an event containing a reference to the command is sent
/// to the main (GUI) thread. This is because having more than one thread access
/// the GUI at a time causes problems with wxwidgets.
int ExecCommand(wxString *pIn, wxString *pOut)
{
   CommandBuilder builder(*pIn);
   if (builder.WasValid())
   {
      Command *cmd = builder.GetCommand();
      AppCommandEvent ev;
      ev.SetCommand(cmd);
      GetActiveProject()->AddPendingEvent(ev);
      Response r = ScriptCommandRelay::ReceiveResponse();
      *pOut      = r.GetMessage();
   } else
   {
      *pOut = wxT("Syntax error!\n");
      *pOut += builder.GetErrorMessage();
   }

   return 0;
}

void ScriptCommandRelay::SendResponse(const wxString &response)
{
   sResponseQueue.AddResponse(response);
}

Response ScriptCommandRelay::ReceiveResponse()
{
   return ScriptCommandRelay::sResponseQueue.WaitAndGetResponse();
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

/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandBuilder.cpp
\brief Contains definitions for CommandBuilder class.

*//****************************************************************//**

\class CommandBuilder
\brief A type of factory for Commands of various sorts.

CommandBuilder has the task of deciding what command is meant by a given
command string, and producing a suitable command object. For now, it doesn't
actually do any processing - it just passes everything on to the BatchCommand
system by constructing BatchCommandEval objects.

*//*******************************************************************/

#include "CommandBuilder.h"
#include <wx/log.h>
#include <wx/wx.h>
#include "DebugPrintCommand.h"
#include "BatchEvalCommand.h"
#include "ScreenshotCommand.h"
#include "../Shuttle.h"

CommandBuilder::CommandBuilder(const wxString &cmdString)
: mValid(false), mCommand(NULL)
{
   BuildCommand(cmdString);
}

CommandBuilder::CommandBuilder(const wxString &cmdName, const wxString &params)
: mValid(false), mCommand(NULL)
{
   BuildCommand(cmdName, params);
}

CommandBuilder::~CommandBuilder() { }

bool CommandBuilder::WasValid() { return mValid; }

const wxString &CommandBuilder::GetErrorMessage() { return mError; }

Command *CommandBuilder::GetCommand()
{
   wxASSERT(mValid);
   wxASSERT(NULL != mCommand);
   return mCommand;
}

// Short-term solution!
bool CommandBuilder::LookUpCommand(wxString name, std::pair<Command*,ParamMap> &result)
{
   if (name.IsSameAs(wxT("screenshot")))
   {
      CommandOutputTarget *output
         = new CommandOutputTarget(new NullProgressTarget(),
                                   new MessageBoxTarget(),
                                   new MessageBoxTarget());
      result = std::pair<Command*,ParamMap>(new ScreenshotCommand(output),
                                            ScreenshotCommand::GetSignature());
      return true;
   } // Other cases...
   return false;
}

// Short-term solution!
ParamMap CommandBuilder::GetSignature(wxString name)
{
   if(name.IsSameAs(wxT("screenshot")))
   {
      return ScreenshotCommand::GetSignature();
   }
   return Command::GetSignature();
}

void CommandBuilder::BuildCommand(const wxString &cmdName, const wxString &cmdParams)
{
   // Stage 1: work out what command to create

   ParamMap signature;

   std::pair<Command*,ParamMap> commandEntry;
   if (LookUpCommand(cmdName, commandEntry))
   {
      mCommand = commandEntry.first;
      signature = commandEntry.second;
   }
   else
   {
      // Fall back to hoping the Batch Command system can handle it
      mCommand = new BatchEvalCommand();
      mCommand->SetParameter(wxT("CommandName"), cmdName);
      mCommand->SetParameter(wxT("ParamString"), cmdParams);
      mValid = true;
      return;
   }

   // Stage 2: set the parameters

   ShuttleCli shuttle;
   shuttle.mParams = cmdParams;
   shuttle.mbStoreInClient = true;

   ParamMap::const_iterator iter;
   for (iter = signature.begin(); iter != signature.end(); ++iter)
   {
      wxString paramString;
      if (shuttle.TransferString(iter->first, paramString, wxT(""))
         && (!mCommand->SetParameter(iter->first, paramString)))
      {
         mError = wxT("Invalid value for parameter '") + iter->first;
         mValid = false;
         return;
      }
   }
   // TODO check for unrecognised parameters

   mValid = true;
}

void CommandBuilder::BuildCommand(const wxString &cmdString)
{
   // Find the command name terminator...  If there is more than one word and
   // no terminator, the command is badly formed
   int splitAt = cmdString.Find(wxT(':'));
   if (splitAt < 0 && cmdString.Strip(wxString::both).Find(wxT(' ')) >= 0) {
      mError = wxT("BAD - Missing ':'?");
      mValid = false;
      return;
   }

   wxString cmdName = cmdString.Left(splitAt).Strip(wxString::both);
   wxString cmdParams = cmdString.Mid(splitAt+1).Strip(wxString::both);
   BuildCommand(cmdName, cmdParams);
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

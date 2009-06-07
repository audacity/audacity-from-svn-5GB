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

void CommandBuilder::BuildCommand(const wxString &cmdName, const wxString &cmdParams)
{

   /*
   // Split up parameters and add them to the map

   // The checking below should be replaced by a lookup + polymorphism
   // eventually

   // See if the name refers to a 'special command'

   // See if the name refers to an effect
   //Effect *f = EffectManager::Get().GetEffectByIdentifier( cmdName );
   //if( f!=NULL )
   //{
   //   ApplyEffectCommand( f, command, params );
   //   // TODO store command somewhere else...
   //   mCommand = new EffectCommand(f);
   //}
   //
   // See if the name refers to a menu command
   */

   mCommand = new BatchEvalCommand(cmdName, cmdParams);
   mValid   = true;
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

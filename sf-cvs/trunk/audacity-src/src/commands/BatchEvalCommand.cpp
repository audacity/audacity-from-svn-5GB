/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file BatchEvalCommand.cpp
\brief Contains definitions for the BatchEvalCommand class

*//*******************************************************************/

#include "BatchEvalCommand.h"

bool BatchEvalCommand::Apply(CommandExecutionContext context)
{
   wxString cmdName = GetString(wxT("CommandName"));
   wxString cmdParams = GetString(wxT("ParamString"));

   // Create a Batch that will have just one command in it...
   BatchCommands Batch;

   return Batch.ApplyCommand(cmdName, cmdParams);
}

wxString BatchEvalCommand::BuildName()
{
   return wxT("BatchEval");
}

ParamMap BatchEvalCommand::BuildSignature()
{
   ParamMap signature;
   Validator commandNameValidator;
   signature[wxT("CommandName")] =
      std::pair<wxVariant, Validator>(wxT(""), commandNameValidator);
   Validator paramValidator;
   signature[wxT("ParamString")] =
      std::pair<wxVariant, Validator>(wxT(""), paramValidator);
   return signature;
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

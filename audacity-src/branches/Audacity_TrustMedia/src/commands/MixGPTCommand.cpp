/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   MixGPT.cpp
   Michael Chinen

******************************************************************/


#include "MixGPTCommand.h"

wxString MixGPTCommandType::BuildName()
{
   return wxT("MixGPT");
}

void MixGPTCommandType::BuildSignature(CommandSignature &signature)
{
   Validator *commandNameValidator(new Validator());
   signature.AddParameter(wxT("Tracks"), wxT(""), commandNameValidator);
   Validator *paramValidator(new Validator());
   signature.AddParameter(wxT("Groups"), wxT(""), paramValidator);
   Validator *chainValidator(new Validator());
   signature.AddParameter(wxT("Parts"), wxT(""), chainValidator);
}

Command *MixGPTCommandType::Create(CommandOutputTarget *target)
{
   return new MixGPTCommand(*this, target);
}

bool MixGPTCommand::Apply(CommandExecutionContext context)
{

   wxString chainName = GetString(wxT("ChainName"));

   wxString cmdName = GetString(wxT("CommandName"));
   wxString cmdParams = GetString(wxT("ParamString"));

   // do a batch command but save the state so we can undo it.
   return true;
}

MixGPTCommand::~MixGPTCommand()
{ }


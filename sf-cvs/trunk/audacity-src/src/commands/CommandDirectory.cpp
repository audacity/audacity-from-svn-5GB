/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandDirectory.cpp
\brief Contains definitions for the CommandDirectory class

*//*******************************************************************/

#include "CommandDirectory.h"
#include "CommandMisc.h"

#include "ScreenshotCommand.h"
#include "BatchEvalCommand.h"
#include "ExecMenuCommand.h"
#include "GetAllMenuCommands.h"

CommandDirectory *CommandDirectory::mInstance = NULL;

CommandDirectory::CommandDirectory()
{
   wxASSERT(mInstance == NULL);
   mInstance = this;

   // Create the command map. 
   // Adding an entry here is the easiest way to register a Command class.
   AddCommand<ScreenshotCommand>();
   AddCommand<BatchEvalCommand>();
   AddCommand<ExecMenuCommand>();
   AddCommand<GetAllMenuCommands>();
}

CommandDirectory::~CommandDirectory()
{
   // Delete the factories
   CommandMap::iterator iter;
   for (iter = mCmdMap.begin(); iter != mCmdMap.end(); ++iter)
   {
      delete iter->second;
   }
}

CommandFactory *CommandDirectory::LookUp(const wxString &cmdName) const
{
   CommandMap::const_iterator iter = mCmdMap.find(cmdName);
   if (iter == mCmdMap.end())
   {
      return NULL;
   }
   return iter->second;
}

CommandDirectory *CommandDirectory::Get()
{
   if (mInstance == NULL)
   {
      return new CommandDirectory();
   }
   return mInstance;
}

void CommandDirectory::Destroy()
{
   if (mInstance != NULL)
   {
      delete mInstance;
   }
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

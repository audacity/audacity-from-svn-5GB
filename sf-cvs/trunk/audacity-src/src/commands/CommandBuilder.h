/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandBuilder.h
\brief Contains declaration of CommandBuilder class.

*//*******************************************************************/

#include <wx/string.h>

class Command;

// CommandBuilder has the task of validating and interpreting a command string.
// If the string represents a valid command, it builds the command object.

class CommandBuilder
{
   private:
      bool mValid;
      Command *mCommand;
      wxString mError;

      void BuildCommand(const wxString &cmdName, const wxString &cmdParams);
      void BuildCommand(const wxString &cmdString);
   public:
      CommandBuilder(const wxString &cmdString);
      CommandBuilder(const wxString &cmdName, const wxString &cmdParams);
      ~CommandBuilder();
      bool WasValid();
      Command *GetCommand();
      const wxString &GetErrorMessage();
};

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

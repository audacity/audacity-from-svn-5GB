/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file Command.h
\brief Contains declaration of Command base class.

*//*******************************************************************/

#ifndef __COMMAND__
#define __COMMAND__

#include "../WrappedType.h"
#include <wx/msgout.h>
#include <map>

class AudacityApp;
class AudacityProject;
class wxString;

class CommandExecutionContext {
   public:
      AudacityApp *app;
      AudacityProject *proj;
      CommandExecutionContext(AudacityApp *app, AudacityProject *proj)
         : app(app), proj(proj) {}
};

// Map from parameter name to the value of the parameter
typedef std::map<wxString, WrappedType> tParamMap ;

class Command
{
   private:
      wxString mName;
      tParamMap mParams; // NB. This is not really used yet! (30 May 2009)

   public:
      Command(const wxString &name) : mName(name) {}
      virtual ~Command() {};
      void SetParameter(const wxString &paramName, const WrappedType &paramValue)
      {
         mParams[paramName] = paramValue;
      }

      // Subclasses should override this.
      virtual bool Apply(CommandExecutionContext context)
      {
         return true;
      }
};
#endif /* End of include guard: __COMMAND__ */

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

/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file Command.h
\brief Contains declaration of Command base class.

\class CommandExecutionContext
\brief Represents a context to which a command may be applied.

\class Command
\brief Base class which encapsulates a process.

That process may depend on certain parameters (determined by the command's
signature) and may produce output on various channels. Any process which is to
be controlled by a script should be separated out into its own Command class.
(And that class should be registered with the CommandDirectory).

*//*******************************************************************/

#ifndef __COMMAND__
#define __COMMAND__

#include <map>
#include <utility>
#include <wx/string.h>
#include <wx/variant.h>
#include <wx/arrstr.h>

#include "Validators.h"
#include "CommandMisc.h"
#include "CommandBuilder.h"
#include "CommandTargets.h"
#include "CommandDirectory.h"

class AudacityApp;
class AudacityProject;

class CommandExecutionContext
{
   public:
      AudacityApp *app;
      AudacityProject *proj;
      CommandExecutionContext(AudacityApp *app, AudacityProject *proj)
         : app(app), proj(proj) {}
};

class Command
{
private:
   wxString mName;
   ParamMap mParams;

   /// Using the command signature, looks up a possible parameter value and
   /// checks whether it passes the validator.
   bool Valid(wxString paramName, const wxVariant &paramValue)
   {
      return mParams[paramName].second.Validate(paramValue);
   }

protected:
   CommandOutputTarget *mOutput;

   // Convenience methods for allowing subclasses to access parameters
   bool GetBool(const wxString &paramName)
   {
      const wxVariant &v = mParams[paramName].first;
      wxASSERT(v.IsType(wxT("bool")));
      return v.GetBool();
   }
   long GetLong(const wxString &paramName)
   {
      const wxVariant &v = mParams[paramName].first;
      wxASSERT(v.IsType(wxT("double")));
      return (long)v.GetDouble();
   }
   double GetDouble(const wxString &paramName)
   {
      const wxVariant &v = mParams[paramName].first;
      wxASSERT(v.IsType(wxT("double")));
      return v.GetDouble();
   }
   wxString GetString(const wxString &paramName)
   {
      const wxVariant &v = mParams[paramName].first;
      wxASSERT(v.IsType(wxT("string")));
      return v.GetString();
   }

   // Convenience methods for passing messages to the output target
   void Progress(double completed)
   {
      mOutput->Progress(completed);
   }
   void Status(wxString status)
   {
      mOutput->Status(status);
   }
   void Error(wxString message)
   {
      mOutput->Error(message);
   }


public:
   /// Constructor should not be called directly; only by a factory which
   /// ensures name and params are set appropriately for the command.
   Command(const wxString &name, 
           const ParamMap &params,
           CommandOutputTarget *output)
      : mName(name),
        mParams(params),
        mOutput(output)
   { 
      wxASSERT(output != NULL);
   }

   virtual ~Command()
   {
      delete mOutput;
   }

   /// Attempt to one of the command's parameters to a particular value.
   /// (Note: wxVariant is reference counted)
   bool SetParameter(const wxString &paramName, wxVariant paramValue)
   {
      wxASSERT(!paramValue.IsType(wxT("null")));

      if (!Valid(paramName, paramValue))
      {
         Error(wxT("Invalid value for parameter '")
               + paramName + wxT("': should be ")
               + mParams[paramName].second.GetDescription());
         return false;
      }
      mParams[paramName].first = paramValue;
      return true;
   }

   /// Apply the command and send a response message on the Status channel
   void ApplyWithResponse(CommandExecutionContext context)
   {
      wxString response = GetName() + wxT(" finished: ");
      if (Apply(context))
      {
         response += wxT("OK");
      } else
      {
         response += wxT("Failed!");
      }
      Status(response);
   }

   /// An instance method for getting the command name (for consistency)
   virtual wxString GetName()
   {
      return BuildName();
   }

   /// Get the signature of the command without having to build it.
   /// (Instead, it is looked up in the CommandDirectory.)
   virtual ParamMap GetSignature()
   {
      CommandFactory *factory = CommandDirectory::Get()->LookUp(GetName());
      wxASSERT_MSG(factory != NULL, 
                   wxT("Failed to get signature for ") + GetName());
      return factory->GetSignature();
   }

   // Subclasses should override the following:
   // =========================================

   /// Actually carry out the command. Return true if successful and false
   /// otherwise.
   virtual bool Apply(CommandExecutionContext context)
   {
      return true;
   }

   /// Construct a 'signature' map containing parameter names, validators and
   /// default values.
   static ParamMap BuildSignature()
   {
      return ParamMap();
   }

   /// Construct the name of the command. (This is the name used to call the
   /// command from a script.)
   static wxString BuildName()
   {
      return wxEmptyString;
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

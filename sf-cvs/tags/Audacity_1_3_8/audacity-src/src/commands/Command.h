/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file Command.h
\brief Contains declaration of Command base class.

*//*******************************************************************/

#ifndef __COMMAND__
#define __COMMAND__

#include <map>
#include <utility>
#include <wx/string.h>
#include <wx/msgout.h>
#include <wx/variant.h>
#include <wx/arrstr.h>

#include "CommandMisc.h"
#include "CommandBuilder.h"
#include "CommandTargets.h"

class AudacityApp;
class AudacityProject;
class wxVariant;

class CommandExecutionContext
{
   public:
      AudacityApp *app;
      AudacityProject *proj;
      CommandExecutionContext(AudacityApp *app, AudacityProject *proj)
         : app(app), proj(proj) {}
};

/// A Validator is an object which checks whether a wxVariant satisfies
//  a certain criterion. This is a base validator which allows anything.
class Validator
{
public:
   Validator() {};
   virtual ~Validator() {};

   virtual bool Validate(const wxVariant &v) const
   {
      return true;
   }

   // For error messages
   virtual wxString GetDescription() const
   {
      return wxT("any value");
   }
};

/// Must be one of the defined options
class OptionValidator : public Validator
{
private:
   wxArrayString mOptions;

public:
   void AddOption(const wxString &option)
   {
      mOptions.Add(option);
   }
   virtual bool Validate(const wxVariant &v) const
   {
      return (mOptions.Index(v.GetString()) != wxNOT_FOUND);
   }
   virtual wxString GetDescription()
   {
      wxString desc = wxT("one of: ");
      int optionCount = mOptions.GetCount();
      int i = 0;
      for (i = 0; i+1 < optionCount; ++i)
      {
         desc += mOptions[i] + wxT(", ");
      }
      desc += mOptions[optionCount-1];
      return desc;
   }
};

/// Must be a boolean
class BoolValidator : public Validator
{
public:
   virtual bool Validate(const wxVariant &v) const
   {
      return v.IsType(wxT("bool"));
   }
   virtual wxString GetDescription() const
   {
      return wxT("y/n");
   }
};

/// Must be a floating-point number
class DoubleValidator : public Validator
{
public:
   virtual bool Validate(const wxVariant &v) const
   {
      return v.IsType(wxT("double"));
   }
   virtual wxString GetDescription() const
   {
      return wxT("a floating-point number");
   }
};

/// Must lie between the two given numbers
class RangeValidator : public Validator
{
private:
   double lower, upper;
public:

   virtual bool Validate(const wxVariant &v) const
   {
      double d = v.GetDouble();
      return ((lower < d) && (d < upper));
   }
   virtual wxString GetDescription() const
   {
      return wxString::Format(wxT("between %d and %d"), lower, upper);
   }
};

/// Must be integral
class IntValidator : public Validator
{
public:
   virtual bool Validate(const wxVariant &v) const
   {
      if (!v.IsType(wxT("double"))) return false;
      double d = v.GetDouble();
      return ((long)d == d);
   }
   virtual wxString GetDescription() const
   {
      return wxT("an integer");
   }
};

/// Must pass both of the supplied validators
class AndValidator : public Validator
{
   private:
      const Validator &v1, &v2;
   public:
      AndValidator(const Validator &u1, const Validator &u2)
         : v1(u1), v2(u2)
      { }
      virtual bool Validate(const wxVariant &v) const
      {
         if (!v1.Validate(v)) return false;
         return v2.Validate(v);
      }
      virtual wxString GetDescription() const
      {
         return v1.GetDescription() + wxT(" and ") + v2.GetDescription();
      }
};

class Command
{
private:
   wxString mName;
   ParamMap mParams;

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

   void Progress(double completed)
   {
      if (mOutput)
         mOutput->Progress(completed);
   }
   void Status(wxString status)
   {
      if (mOutput)
         mOutput->Status(status);
   }
   void Error(wxString message)
   {
      if (mOutput)
         mOutput->Error(message);
   }

public:
   Command(const wxString &name, CommandOutputTarget *output = new CommandOutputTarget())
      : mName(name),
        mParams(CommandBuilder::GetSignature(name)),
        mOutput(output)
   { }

   virtual ~Command()
   {
      if (mOutput)
         delete mOutput;
   }

   // Note: wxVariant is reference counted
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

   // Subclasses should override the following:

   virtual bool Apply(CommandExecutionContext context)
   {
      return true;
   }

   static ParamMap GetSignature()
   {
      return ParamMap();
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

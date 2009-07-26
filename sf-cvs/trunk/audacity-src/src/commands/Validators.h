/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file Validators.h
\brief Contains declarations and definitions for Validator, OptionValidator,
BoolValidator, DoubleValidator, RangeValidator, IntValidator and AndValidator
classes.

\class Validator
\brief A Validator is an object which checks whether a wxVariant satisfies
a certain criterion. This is a base validator which allows anything.

\class OptionValidator
\brief Parameter must be one of the defined options

\class BoolValidator
\brief Parameter must be a boolean

\class DoubleValidator
\brief Parameter must be a floating-point number

\class RangeValidator
\brief Parameter must lie between the two given numbers

\class IntValidator
\brief Parameter must be integral

\class AndValidator
\brief Parameter must pass both of the supplied validators

*//*******************************************************************/

#ifndef __VALIDATORS__
#define __VALIDATORS__

class Validator
{
public:
   Validator() {};
   virtual ~Validator() {};

   /// Judge whether the passed value satisfies the Validator
   virtual bool Validate(const wxVariant &v) const
   {
      return true;
   }

   /// Return a description (for error messages)
   /// should be of the form 'v must be $description'
   virtual wxString GetDescription() const
   {
      return wxT("any value");
   }
};

class OptionValidator : public Validator
{
private:
   wxArrayString mOptions;

public:
   void AddOption(const wxString &option)
   {
      mOptions.Add(option);
   }
   void AddOptions(const wxArrayString &options)
   {
      mOptions.insert(mOptions.begin(), options.begin(), options.end());
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
#endif /* End of include guard: __VALIDATORS__ */

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

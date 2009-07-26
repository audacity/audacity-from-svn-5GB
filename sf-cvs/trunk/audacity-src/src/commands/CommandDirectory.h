/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file CommandDirectory.h
\brief Contains declarations for CommandDirectory, CommandFactory and
TypedCommandFactory<T> classes.

\class CommandDirectory
\brief Allows registration and lookup (by name) of command types.  

A singleton. This fulfills a similar purpose to CommandManager, but for general
Commands rather than menu items. Eventually they could be unified but for now
they are kept separate to make things simpler.

\class CommandFactory
\brief Interface for a class which creates Command objects

\class TypedCommandFactory<T>
\brief Template class for concrete factories which create Command objects of a
specific type.

*//*******************************************************************/

#ifndef __COMMANDDIRECTORY__
#define __COMMANDDIRECTORY__

#include "CommandMisc.h"

class Command;
class CommandOutputTarget;

class CommandFactory
{
public:
   virtual Command *Create(CommandOutputTarget *target) = 0;
   virtual const ParamMap &GetSignature() = 0;
};

template <typename T>
class TypedCommandFactory : public CommandFactory
{
private:
   ParamMap mSignature;
public:
   TypedCommandFactory()
      : mSignature(T::BuildSignature())
   { }

   Command *Create(CommandOutputTarget *target)
   {
      return new T(T::BuildName(), mSignature, target);
   }

   const ParamMap &GetSignature()
   {
      return mSignature;
   }
};

class CommandDirectory
{
private:
   static CommandDirectory *mInstance;
   CommandMap mCmdMap;
public:
   CommandDirectory();
   ~CommandDirectory();

   /// If a command with the given name has been registered in the directory,
   /// return a pointer to the factory for commands of that type.
   /// Otherwise return NULL.
   CommandFactory *LookUp(const wxString &cmdName) const;
   
   /// A convenient way to register a type of Command object with the 
   /// directory.
   ///
   /// T must be a subclass of Command which has a constructor of the form
   /// T(const wxString &cmdName,
   ///   const ParamMap &signature,
   ///   CommandOutputTarget *target)
   /// and suitable static BuildName and BuildSignature methods
   template <typename T>
   void AddCommand()
   {
      wxString cmdName = T::BuildName(); 
      wxASSERT_MSG(mCmdMap.find(cmdName) == mCmdMap.end()
                  , wxT("A command named ") + cmdName 
                    + wxT(" already exists."));

      mCmdMap[cmdName] = new TypedCommandFactory<T>();
   }

   /// Get a pointer to the singleton instance
   static CommandDirectory *Get();

   /// Manually delete the singleton instance
   static void Destroy();
};

#endif /* End of include guard: __COMMANDDIRECTORY__ */

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

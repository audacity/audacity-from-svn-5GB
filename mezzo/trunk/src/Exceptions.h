/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Exceptions.h

  Copyright (c) 2004 Dominic Mazzoni, Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_EXCEPTIONS__
#define __MEZZO_EXCEPTIONS__

#include <string>
#include <exception>

namespace Mezzo {

class Exception : public std::exception {
 public:
   Exception(std::string _description, std::string _location):
      description(_description),
      location(_location)
   {
   }

   ~Exception() throw() { }

   std::string description;
   std::string location;
};

/// An error within the library that is no fault of the client.
/// It is unlikely that the client can recover in any useful way.
class InternalException : public Exception
{
 public:
   InternalException(std::string _description, std::string _location):
      Exception(_description, _location)
   {
   }
};

/// An error that indicates a bug in the client program.  This probably
/// means the client passed invalid parameters to a library method.
class ClientException : public Exception
{
 public:
   ClientException(std::string _description, std::string _location):
      Exception(_description, _location)
   {
   }
};

/// An error that is beyond either the library or the client's control.
/// Perhaps the disk is full or a file that the library wrote disappeared.
class UserException : public Exception
{
 public:
   UserException(std::string _description, std::string _location):
      Exception(_description, _location)
   {
   }
};

#define UserAssert(cond, msg) do {if(!(cond)) UserAssertFailure(#cond, msg, __FILE__, __LINE__);} while (0)
#define ClientAssert(cond, msg) do {if(!(cond)) ClientAssertFailure(#cond, msg, __FILE__, __LINE__);} while (0)
#define InternalAssert(cond, msg) do {if(!(cond)) InternalAssertFailure(#cond, msg, __FILE__, __LINE__);} while(0)

void UserAssertFailure    (std::string expr, std::string msg, std::string file, int line);
void ClientAssertFailure  (std::string expr, std::string msg, std::string file, int line);
void InternalAssertFailure(std::string expr, std::string msg, std::string file, int line);


} // namespace

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


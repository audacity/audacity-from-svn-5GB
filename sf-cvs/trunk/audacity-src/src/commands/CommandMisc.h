/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file CommandMisc
\brief Some typedefs which are used in various Command-related files

*//*******************************************************************/

#ifndef __COMMANDMISC__
#define __COMMANDMISC__

#include <map>
#include <utility>
#include <wx/string.h>
#include <wx/variant.h>

class Validator;
class CommandFactory;

// Map from parameter name to the value of the parameter, with a suitable Validator
typedef std::map<wxString, std::pair<wxVariant, Validator> > ParamMap;

// Map from command name to factory
typedef std::map<wxString, CommandFactory*> CommandMap;

#endif /* End of include guard: __COMMANDMISC__ */

/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Exceptions.cpp

  Copyright (c) 2004 Joshua Haberman, Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include <stdarg.h>

#include "Exceptions.h"
#include "Util.h"

#define SEGFAULT_ON_EXCEPTION

#ifdef SEGFAULT_ON_EXCEPTION
#define PRINT_AND_SEGFAULT \
   printf("%s\n%s\n%s %d\n",                                   \
          expr.c_str(), msg.c_str(), file.c_str(), line);      \
   int *p = 0; p[0] = 0;
#else
#define PRINT_AND_SEGFAULT
#endif

namespace Mezzo {

void UserAssertFailure    (std::string expr, std::string msg, std::string file, int line)
{
   PRINT_AND_SEGFAULT
   std::string loc = fmt("%s:%d %s", file.c_str(), line, expr.c_str());
   throw(UserException(msg, loc));
}

void ClientAssertFailure  (std::string expr, std::string msg, std::string file, int line)
{
   PRINT_AND_SEGFAULT
   std::string loc = fmt("%s:%d %s", file.c_str(), line, expr.c_str());
   throw(ClientException(msg, loc));
}

void InternalAssertFailure(std::string expr, std::string msg, std::string file, int line)
{
   PRINT_AND_SEGFAULT
   std::string loc = fmt("%s:%d %s", file.c_str(), line, expr.c_str());
   throw(InternalException(msg, loc));
}

} // namespace

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


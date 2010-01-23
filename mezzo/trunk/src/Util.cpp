/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Util.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "Util.h"

#include <stdarg.h>

std::string fmt(const char *format, ...)
{
   va_list args;
   char buffer[1000];

   va_start(args, format);
   /** vsnprintf seems better, but it doesn't guarantee null termination,
    ** so it's not available as a normal function from VC++...
     vsnprintf(buffer, sizeof(buffer), format, args);
    **/
   vsprintf(buffer, format, args);
   va_end(args);

   return std::string(buffer);
}

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


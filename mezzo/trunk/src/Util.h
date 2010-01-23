/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Util.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_UTIL__
#define __MEZZO_UTIL__

#include <set>
#include <string>

std::string fmt(const char *format, ...);

// It is kind of silly to me that STL doesn't have a set_intesection function with
// this signature.  To use set_intersection directly, you have to construct an
// insert_iterator and do other stuff that isn't pretty.
template<class T>
std::set<T> SetIntersection(std::set<T> x, std::set<T> y)
{
   std::set<T> out;
   std::insert_iterator<std::set<T> > out_iterator(out, out.begin());
   set_intersection(x.begin(), x.end(), y.begin(), y.end(), out_iterator);
   return out;
}


#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


/*****************************************************************************
 * 
 * Custom variable types used in SoundTouch sound processing library.
 *
 * Author        : Copyright (c) Olli Parviainen 2002
 * Author e-mail : oparviai@iki.fi
 * File created  : 13-Jan-2002
 * Last modified : 13-Jan-2002
 *
 * License :
 *
 *  This file is part of SoundTouch sound processing library.
 *
 *  SoundTouch is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  SoundTouch is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with SoundTouch; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *****************************************************************************/

#ifndef STTypes_H
#define STTypes_H

#include <exception>

typedef unsigned int    uint;

#define SAMPLES_ARE_SHORT 0

#if SAMPLES_ARE_SHORT
typedef short           Sample;
typedef int             BiggerSample;
#else
typedef float           Sample;
typedef float           BiggerSample;
#endif

class soundtouch_exception : public std::exception {
  public:
	virtual const char *what() const throw() { return "soundtouch library st_assertion failure"; }
};

#define st_assert(expr) ((expr) ? (void) 0 : throw soundtouch_exception())

#endif

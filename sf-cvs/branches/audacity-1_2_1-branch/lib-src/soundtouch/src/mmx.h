/*****************************************************************************
 *
 * A header file for detecting the Intel MMX instructions set extension.
 *
 * Please see 'mmx_win.cpp', 'mmx_cpp.cpp' and 'mmx_non_x86.cpp' for the 
 * routine implementations for x86 Windows, x86 gnu version and non-x86 
 * platforms, respectively.
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

#ifndef MMXDetect_H
#define MMXDetect_H

#include "STTypes.h"

// Checks if the processor supports MMX instructions. Returns '1' if it the
// processor supports MMX instructions, otherwise '0'
bool mmxDetect(void);

// Forces the 'detect_mmx' function to report no MMX support, if called with
// nonzero value in parameter 'disable'.
void mmxDisable(const bool disable);

#endif

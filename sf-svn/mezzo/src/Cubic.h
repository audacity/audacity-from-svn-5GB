/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Cubic.h

  Copyright (c) 2004 Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_CUBIC__
#define __MEZZO_CUBIC__

float CubicInterpolate(float y0, float y1, float y2, float y3, float x);

float CubicMaximize(float y0, float y1, float y2, float y3);

#endif


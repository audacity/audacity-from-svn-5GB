/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Cubic.cpp

  Copyright (c) 2004 Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

  @file Cubic.cpp
  @brief Cubic interpolation routines.

**********************************************************************/

#include <math.h>

float CubicInterpolate(float y0, float y1, float y2, float y3, float x)
{
   float a, b, c, d;

   a = y0 / -6.0 + y1 / 2.0 - y2 / 2.0 + y3 / 6.0;
   b = y0 - 5.0 * y1 / 2.0 + 2.0 * y2 - y3 / 2.0;
   c = -11.0 * y0 / 6.0 + 3.0 * y1 - 3.0 * y2 / 2.0 + y3 / 3.0;
   d = y0;

   float xx = x * x;
   float xxx = xx * x;

   return (a * xxx + b * xx + c * x + d);
}

float CubicMaximize(float y0, float y1, float y2, float y3)
{
   // Find coefficients of cubic

   float a, b, c, d;

   a = y0 / -6.0 + y1 / 2.0 - y2 / 2.0 + y3 / 6.0;
   b = y0 - 5.0 * y1 / 2.0 + 2.0 * y2 - y3 / 2.0;
   c = -11.0 * y0 / 6.0 + 3.0 * y1 - 3.0 * y2 / 2.0 + y3 / 3.0;
   d = y0;

   // Take derivative

   float da, db, dc;

   da = 3 * a;
   db = 2 * b;
   dc = c;

   // Find zeroes of derivative using quadratic equation

   float discriminant = db * db - 4 * da * dc;
   if (discriminant < 0.0)
      return float(-1.0);              // error

   float x1 = (-db + sqrt(discriminant)) / (2 * da);
   float x2 = (-db - sqrt(discriminant)) / (2 * da);

   // The one which corresponds to a local _maximum_ in the
   // cubic is the one we want - the one with a negative
   // second derivative

   float dda = 2 * da;
   float ddb = db;

   if (dda * x1 + ddb < 0)
      return x1;
   else
      return x2;
}

/**********************************************************************

  Round.cpp

  Steve Jolly

  August 2002

  This file contains implementations of rint and lrint for the benefit
  of platforms whose libcs don't feature them (eg W32/MSVC)

  This library should not be included on platforms whose libcs *do*
  feature them.

**********************************************************************/

#include <math.h>

#include "Round.h"

int rint(float y) {
   double trunc;
   int itrunc;
   double frac;
   bool odd;

   frac=modf(y,&trunc); //split y into integer and fractional parts
   itrunc=(int)trunc;
   odd=(itrunc & 1==1); //find out if the integer part is odd or even
   if (odd&&frac<0.5) return itrunc;
   if (odd&&frac>=0.5) return ++itrunc;
   if (!odd&&frac<=0.5) return itrunc;
   return ++itrunc;
}

long lrint(float y) {
   double trunc;
   long itrunc;
   double frac;
   bool odd;

   frac=modf(y,&trunc); //split y into integer and fractional parts
   itrunc=(long)trunc;
   odd=(itrunc & 1==1); //find out if the integer part is odd or even
   if (odd&&frac<0.5) return itrunc;
   if (odd&&frac>=0.5) return ++itrunc;
   if (!odd&&frac<=0.5) return itrunc;
   return ++itrunc;
}

/**********************************************************************

  Round.h

  Steve Jolly

  August 2002

  This file contains implementations of rint and lrint for the benefit
  of platforms whose libcs don't feature them (eg W32/MSVC)

  This library should not be included on platforms whose libcs *do*
  feature them.

**********************************************************************/


/*
 * converts a float to an int, rounding to the nearest even integer
 * - see ISO 31-0:1992(E) "Quantities and units -- General Principles"
 * Annex B (informative) "Guide to rounding numbers" if you want to
 * argue about this :-)
 */

int rint(float y);

/*
 * converts a float to a long int
 */

long lrint(float y);

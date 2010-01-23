sound_type snd_make_white(time_type t0, rate_type sr, time_type d);
sound_type snd_white(time_type t0, rate_type sr, time_type d);
    /* LISP: (snd-white ANYNUM ANYNUM ANYNUM) */

#ifndef __APPLE__
#include <math.h>
#endif

#include "switches.h" /* to define (or not) MACINTOSH */

#if defined(MACINTOSH) || defined(UNIX)

/* rand returns from 0 to RAND_MAX. Scale and offset
 * to get range from -1 to +1
 */
#define random rand
#define random_scale (2.0/RAND_MAX)

#else
#ifdef UNIX

/* random is already defined */
#define random rand
#define random_scale (2.0/RAND_MAX)

#else

/* random returns from 0 to 0x7FFFFFF.  Scale and offset
 * to get range from -1 to +1
 */
#define random_scale (2.0/0x7FFFFFFF)

long random(void);

#endif
#endif

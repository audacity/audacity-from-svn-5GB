(WHITE-ALG
(NAME "white")
(ARGUMENTS ("time_type" "t0") ("rate_type" "sr") ("time_type" "d"))
(STATE )
(TERMINATE (AFTER "d"))
(INNER-LOOP "output = (sample_type) (random() * random_scale - 1.0);")
(SAMPLE-RATE "sr")
(SUPPORT-HEADER "#include <math.h>
#include \"switches.h\" /* to define (or not) MACINTOSH */

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
")
)


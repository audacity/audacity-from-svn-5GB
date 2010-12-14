/* sndmac.h -- system-specific definitions */

typedef double FASTFLOAT;
typedef float MEMFLOAT;

/* avoid conflicts if already defined: */

#ifndef max

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#endif

#ifndef ntohl
#define ntohl(x) (x)
#endif

#ifndef ntohs
#define ntohs(x) (x)
#endif

#ifndef htonl
#define htonl(x) (x)
#endif

#ifndef htons
#define htons(x) (x)
#endif

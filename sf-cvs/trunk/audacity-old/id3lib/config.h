/*
 * custom-built for audacity, so that we can compile for different
 * platforms without modifying the source code
 */

#ifdef WIN32
#include "config.h.win32"
#elif defined(macintosh)
#include "config.h.mac"
#else
#include "config.h.unix"
#endif

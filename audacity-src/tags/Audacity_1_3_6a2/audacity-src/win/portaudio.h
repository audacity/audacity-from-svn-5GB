
// Set to 0 for version 1.8 or 1 for version 1.9
#define USE_PORTAUDIO_V19 1

// Don't change anything below here
#if USE_PORTAUDIO_V19

#  include "../../../lib-src/portaudio-v19/include/portaudio.h"
#  pragma comment(lib, "portaudio-v19")

#else

#  include "../../../lib-src/portaudio/pa_common/portaudio.h"
#  pragma comment(lib, "portaudio")

#endif

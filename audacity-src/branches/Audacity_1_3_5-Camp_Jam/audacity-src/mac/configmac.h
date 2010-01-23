// Macintosh-specific include file

// Audacity's four-character "creator" code and project type code
#define AUDACITY_CREATOR      'auDy'
#define AUDACITY_PROJECT_TYPE 'auDp'

#ifdef __MACH__

# define __MACOSX__
# include <wx/defs.h>
# include "../src/configunix.h"
# define USE_AQUA_THEME 1

#else

# define __MACOS9__
# define MP3SUPPORT        // Use libmad for importing MP3 on the mac
# define USE_LIBMAD 1
# define USE_LIBVORBIS 1     // Ogg Vorbis support (?)
# define USE_LIBID3TAG 1     // ID3 support

# define USE_NYQUIST 1

#define INSTALL_PREFIX "."

#endif

// arch-tag: 1f1d80ef-d1b4-4c9e-8226-ec3b93dcb46c


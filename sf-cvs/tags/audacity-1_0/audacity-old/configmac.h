// Macintosh-specific include file

// Audacity's four-character "creator" code and project type code
#define AUDACITY_CREATOR      'auDy'
#define AUDACITY_PROJECT_TYPE 'auDp'

#ifdef __DARWIN__

// Mac OS X, compiled with g++

// Use libmad for importing MP3 on OS X
#define MP3SUPPORT
#define USE_LIBMAD

// Ogg Vorbis support
#define USE_LIBVORBIS

#else

// Mac OS 9, compiled with CodeWarrior

// Use xaudio for importing MP3 on the mac
#define USE_XAUDIO
#define MP3SUPPORT

// Ogg Vorbis support
#define USE_LIBVORBIS

// ID3 support
#define USE_ID3LIB

#endif


// Microsoft Windows specific include file

// These settings are used for official stable releases of Audacity
// with all of its support libraries included.  However, if you
// downloaded the source tarball of Audacity, some of these libraries
// might be missing, so there might be lines later in this file that
// disables support for these libraries.

#define MP3SUPPORT 1
#define USE_LADSPA 1
#define USE_LIBID3TAG 1
#define USE_LIBMAD 1
#define USE_LIBRESAMPLE 1
#undef USE_LIBSAMPLERATE
#define USE_LIBVORBIS 1
#define USE_NYQUIST 1
#define USE_PORTMIXER 1
#define USE_SOUNDTOUCH 1
#undef USE_VST

#define INSTALL_PREFIX "."

#define rint(x)   (floor((x)+0.5f)) 


AC_DEFUN([AUDACITY_CHECKLIB_LIBNYQUIST], [
   AC_ARG_WITH(nyquist,
               [AC_HELP_STRING([--with-nyquist],
                               [compile with Nyquist support [default=yes]])],
               LIBNYQUIST_ARGUMENT=$withval,
               LIBNYQUIST_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_NYQUIST, 1,
                [Define if Nyquist support should be enabled])
   fi

   dnl Nyquist is never installed on the system

   LIBNYQUIST_SYSTEM_AVAILABLE="no"

   dnl see if Nyquist is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libnyquist/nyx/nyx.h,
                 nyx_h_found="yes",
                 nyx_h_found="no")

   if test $nyx_h_found = yes ; then
      LIBNYQUIST_LOCAL_AVAILABLE="yes"
      LIBNYQUIST_LOCAL_LIBS="libnyquist.a"
      LIBNYQUIST_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libnyquist/nyx'
      LIBNYQUIST_LOCAL_CPPSYMBOLS="USE_NYQUIST"
      LIBNYQUIST_LOCAL_OPTOBJS="effects/nyquist/Nyquist.o"
      LIBNYQUIST_LOCAL_OPTOBJS="$LIBNYQUIST_LOCAL_OBJS effects/nyquist/LoadNyquist.o"

      AC_MSG_NOTICE([nyquist libraries are available in the local tree])
   else
      LIBNYQUIST_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([nyquist libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBSOUNDTOUCH], [
   AC_ARG_WITH(soundtouch,
               [AC_HELP_STRING([--with-soundtouch],
                               [compile with SoundTouch [default=yes]])],
               LIBSOUNDTOUCH_ARGUMENT=$withval,
               LIBSOUNDTOUCH_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_SOUNDTOUCH, 1,
                [Define if SoundTouch support should be enabled])
   fi

   dnl see if soundtouch is installed on the system

   dnl ... TODO: I don't know if stock, unmodified soundtouch is
   dnl suitable for audacity, so I don't allow it

   LIBSOUNDTOUCH_SYSTEM_AVAILABLE="no"

   dnl see if libresample is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/soundtouch/src/SoundTouch.h,
                 soundtouch_h_found="yes",
                 soundtouch_h_found="no")

   if test $soundtouch_h_found = yes ; then
      LIBSOUNDTOUCH_LOCAL_AVAILABLE="yes"
      LIBSOUNDTOUCH_LOCAL_LIBS="libSoundTouch.a"
      LIBSOUNDTOUCH_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/soundtouch/include'
      LIBSOUNDTOUCH_LOCAL_CPPSYMBOLS="USE_SOUNDTOUCH"

      if test ! -f lib-src/soundtouch/Makefile ; then
         LIBSOUNDTOUCH_LOCAL_CONFIG_SUBDIRS="lib-src/soundtouch"
      fi
      AC_MSG_NOTICE([libsoundtouch libraries are available in the local tree])
   else
      LIBSOUNDTOUCH_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsoundtouch libraries are NOT available in the local tree])
   fi

])

AC_DEFUN([AUDACITY_CHECKLIB_LIBRESAMPLE], [

   AC_ARG_WITH(libresample,
               [AC_HELP_STRING([--with-libresample],
                               [use libresample: [yes no]])],
               LIBRESAMPLE_ARGUMENT=$withval,
               LIBRESAMPLE_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBRESAMPLE, 1,
                [Define if libresample support should be enabled])
   fi

   dnl see if libresample is installed on the system

   dnl ... but libresample isn't generally installed as a system library...

   LIBRESAMPLE_SYSTEM_AVAILABLE="no"

   dnl see if libresample is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libresample/include/libresample.h,
                 resample_h_found="yes",
                 resample_h_found="no")

   if test $resample_h_found = yes ; then
      LIBRESAMPLE_LOCAL_AVAILABLE="yes"
      LIBRESAMPLE_LOCAL_LIBS="libresample.a"
      LIBRESAMPLE_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libresample/include'
      LIBRESAMPLE_LOCAL_CPPSYMBOLS="USE_LIBRESAMPLE"

      if test ! -f lib-src/libresample/Makefile ; then
         LIBRESAMPLE_LOCAL_CONFIG_SUBDIRS="lib-src/libresample"
      fi
      AC_MSG_NOTICE([libresample libraries are available in the local tree])
   else
      LIBRESAMPLE_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libresample libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBSAMPLERATE], [

   AC_ARG_WITH(libsamplerate,
               [AC_HELP_STRING([--with-libsamplerate],
                               [use libsamplerate (instead of libresample)])],
               LIBSAMPLERATE_ARGUMENT=$withval,
               LIBSAMPLERATE_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBSAMPLERATE, 1,
                [Define if libsamplerate support should be enabled])
   fi

   dnl see if libsamplerate is installed on the system

   PKG_CHECK_MODULES(SAMPLERATE, samplerate >= 0.15.0,
                     samplerate_available_system="yes",
                     samplerate_available_system="no")

   if test $samplerate_available_system = yes ; then
      LIBSAMPLERATE_SYSTEM_AVAILABLE="yes"
      LIBSAMPLERATE_SYSTEM_LIBS=$SAMPLERATE_LIBS
      LIBSAMPLERATE_SYSTEM_CXXFLAGS=$SAMPLERATE_CFLAGS
      AC_MSG_NOTICE([Libsamplerate libraries are available as system libraries])
   else
      LIBSAMPLERATE_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsamplerate libraries are NOT available as system libraries])
   fi

   dnl see if libsamplerate is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libsamplerate/src/samplerate.h,
                 samplerate_h_found="yes",
                 samplerate_h_found="no")

   if test $samplerate_h_found = yes ; then
      LIBSAMPLERATE_LOCAL_AVAILABLE="yes"
      LIBSAMPLERATE_LOCAL_LIBS="libsamplerate.a"
      LIBSAMPLERATE_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libsamplerate/src'
      LIBSAMPLERATE_LOCAL_CPPSYMBOLS="USE_LIBSAMPLERATE"

      if test ! -f lib-src/libsamplerate/Makefile ; then
         LIBSAMPLERATE_LOCAL_CONFIG_SUBDIRS="lib-src/libsamplerate"
      fi
      AC_MSG_NOTICE([libsamplerate libraries are available in the local tree])
   else
      LIBSAMPLERATE_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsamplerate libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBID3TAG], [

   AC_ARG_WITH(id3tag,
               [AC_HELP_STRING([--with-id3tag],
                               [use libid3tag for mp3 id3 tag support])],
               LIBID3TAG_ARGUMENT=$withval,
               LIBID3TAG_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBID3TAG, 1,
                [Define if libid3tag is present])
   fi

   dnl see if libid3tag is installed on the system

   AC_CHECK_LIB(id3tag, id3_file_open,
                libid3tag_found="yes",
                libid3tag_found="no",
               -lz)

   AC_CHECK_HEADER(id3tag.h,
                   id3tag_h_found="yes",
                   id3tag_h_found="no")

   # I don't know if any released id3tag is sufficient, so disabling
   # system use for now...

   #if test $libid3tag_found = yes && test $id3tag_h_found = yes ; then
   #   LIBID3TAG_SYSTEM_AVAILABLE="yes"
   #   LIBID3TAG_SYSTEM_LIBS=-lid3tag
   #   LIBID3TAG_SYSTEM_CPPSYMBOLS="USE_LIBID3TAG"
   #   AC_MSG_NOTICE([Libid3tag libraries are available as system libraries])
   #else
      LIBID3TAG_SYSTEM_AVAILABLE="no"
   #   AC_MSG_NOTICE([Libid3tag libraries are NOT available as system libraries])
   #fi

   dnl see if libid3tag is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libid3tag/frame.h,
                 frame_h_found="yes",
                 frame_h_found="no")


   if test $frame_h_found = yes ; then
      LIBID3TAG_LOCAL_AVAILABLE="yes"
      LIBID3TAG_LOCAL_LIBS="libid3tag.a"
      LIBID3TAG_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libid3tag'
      LIBID3TAG_LOCAL_CPPSYMBOLS="USE_LIBID3TAG"

      if test ! -f lib-src/libid3tag/Makefile ; then
         LIBID3TAG_LOCAL_CONFIG_SUBDIRS="lib-src/libid3tag"
      fi
      AC_MSG_NOTICE([libid3tag libraries are available in the local tree])
   else
      LIBID3TAG_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libid3tag libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBSNDFILE], [

   AC_ARG_WITH(libsndfile,
               [AC_HELP_STRING([--with-libsndfile],
                               [which libsndfile to use: [local system]])],
               LIBSNDFILE_ARGUMENT=$withval,
               LIBSNDFILE_ARGUMENT="unspecified")

   dnl see if libsndfile is installed in the system

   PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0.0,
                     sndfile_available_system="yes",
                     sndfile_available_system="no")

   if test $sndfile_available_system = yes ; then
      LIBSNDFILE_SYSTEM_AVAILABLE="yes"
      LIBSNDFILE_SYSTEM_LIBS=$SNDFILE_LIBS
      LIBSNDFILE_SYSTEM_CXXFLAGS=$SNDFILE_CFLAGS
      AC_MSG_NOTICE([Libsndfile libraries are available as system libraries])
   else
      LIBSNDFILE_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsndfile libraries are NOT available as system libraries])
   fi

   dnl see if libsndfile is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libsndfile/src/sndfile.h,
                 libsndfile_h_found="yes",
                 libsndfile_h_found="no")

   if test $libsndfile_h_found = yes ; then
      LIBSNDFILE_LOCAL_AVAILABLE="yes"
      LIBSNDFILE_LOCAL_LIBS="libsndfile.a"
      LIBSNDFILE_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libsndfile/src'
      if test ! -f "lib-src/libsndfile/Makefile"; then
         LIBSNDFILE_LOCAL_CONFIG_SUBDIRS="lib-src/libsndfile"
      fi
      AC_MSG_NOTICE([libsndfile libraries are available in this source tree])
   else
      LIBSNDFILE_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsndfile libraries are NOT available in this source tree])
   fi

])


AC_DEFUN([AUDACITY_CHECKLIB_LIBMAD], [

   if false ; then
      AC_DEFINE(USE_LIBMAD, 1,
                [Define if mp3 support is implemented with the libmad library])
   fi

   AC_ARG_WITH(libmad,
               [AC_HELP_STRING([--with-libmad],
                               [use libmad for mp3 decoding support])],
               LIBMAD_ARGUMENT=$withval,
               LIBMAD_ARGUMENT="unspecified")

   dnl see if libmad is installed in the system

   AC_CHECK_LIB(mad, mad_decoder_init,
                libmad_found="yes",
                libmad_found="no")

   AC_CHECK_HEADER(mad.h,
                   mad_h_found="yes",
                   mad_h_found="no")

   dnl make sure libmad is at least version 0.14.2b

   AC_TRY_RUN([
#include <mad.h>
int main() {
#if MAD_VERSION_MAJOR == 0  && MAD_VERSION_MINOR <= 13
   return 1; /* <= 0.13, too old */
#elsif MAD_VERSION_MAJOR == 0  &&  MAD_VERSION_MINOR == 14  &&  MAD_VERSION_PATCH < 2
   return 1; /* 0.14.0 <= x < 0.14.2, too old */
#else
   return 0;
#endif
}],
              libmad_newenough="yes",
              libmad_newenough="no")

   if test $mad_h_found = yes && test $libmad_newenough = no ; then
      AC_MSG_WARN([system installation of libmad found, but it is too old.  Upgrade to at least 0.14.2b to use with Audacity])
   fi

   if test $libmad_found = yes && test $mad_h_found = yes && test $libmad_newenough = yes ; then
      LIBMAD_SYSTEM_AVAILABLE="yes"
      LIBMAD_SYSTEM_LIBS="-lmad"
      LIBMAD_SYSTEM_CPPSYMBOLS="USE_LIBMAD"
      AC_MSG_NOTICE([libmad libraries are available as system libraries])
   else
      AC_MSG_NOTICE([libmad libraries are NOT available as system libraries])
   fi

   dnl see if libmad is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libmad/frame.h,
                 frame_h_found="yes",
                 frame_h_found="no")

   if test $frame_h_found = yes ; then
      LIBMAD_LOCAL_AVAILABLE="yes"
      LIBMAD_LOCAL_LIBS="libmad.a"
      LIBMAD_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libmad'
      LIBMAD_LOCAL_CPPSYMBOLS="USE_LIBMAD"

      if test ! -f lib-src/libmad/Makefile ; then
         LIBMAD_LOCAL_CONFIG_SUBDIRS="lib-src/libmad"
      fi
      AC_MSG_NOTICE([libmad libraries are available in the local tree])
   else
      LIBMAD_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libmad libraries are NOT available in the local tree])
   fi
])


AC_DEFUN([AUDACITY_CHECKLIB_LIBVORBIS], [

   if false ; then
      AC_DEFINE(USE_LIBVORBIS, 1,
                [Define if the ogg vorbis decoding library is present])
   fi

   AC_ARG_WITH(vorbis,
               [AC_HELP_STRING([--with-vorbis],
                               [enable ogg vorbis support])],
               LIBVORBIS_ARGUMENT=$withval,
               LIBVORBIS_ARGUMENT="unspecified")

   dnl See if Vorbis is installed in the system

   AC_CHECK_LIB(vorbisfile,
                vorbis_bitrate_addblock,
                lib_found="yes",
                lib_found="no",
                -lvorbis -logg)

   AC_CHECK_HEADER(vorbis/vorbisfile.h,
                   header_found="yes",
                   header_found="no")

   if test $lib_found = yes && test $header_found = yes ; then
      LIBVORBIS_SYSTEM_AVAILABLE="yes"
      LIBVORBIS_SYSTEM_LIBS="-lvorbisenc -lvorbisfile -lvorbis -logg"
      LIBVORBIS_SYSTEM_CPPSYMBOLS="USE_LIBVORBIS"
      AC_MSG_NOTICE([Vorbis libraries are available as system libraries])
   else
      LIBVORBIS_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Vorbis libraries are NOT available as system libraries])
   fi

   dnl see if Vorbis is available in the source dir

   AC_CHECK_FILE(${srcdir}/lib-src/libvorbis/include/vorbis/vorbisenc.h,
                 vorbisenc_h_available="yes",
                 vorbisenc_h_available="no")

   AC_CHECK_FILE(${srcdir}/lib-src/libogg/include/ogg/ogg.h,
                 ogg_h_available="yes",
                 ogg_h_available="no")

   if test $vorbisenc_h_available = yes && test $ogg_h_available="yes" ; then
      LIBVORBIS_LOCAL_AVAILABLE="yes"

      LIBVORBIS_LOCAL_LIBS="libvorbisenc.a libvorbisfile.a libvorbis.a libogg.a"

      LIBVORBIS_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libogg/include'
      LIBVORBIS_LOCAL_CXXFLAGS="$LIBVORBIS_LOCAL_CXXFLAGS -I\$(top_srcdir)/lib-src/libvorbis/include"

      LIBVORBIS_LOCAL_CPPSYMBOLS="USE_LIBVORBIS"

      LIBVORBIS_LOCAL_CONFIG_SUBDIRS="lib-src/libogg lib-src/libvorbis"
      AC_MSG_NOTICE([Vorbis libraries are available in this source tree])
   else
      AC_MSG_NOTICE([Vorbis libraries are NOT available in this source tree])
   fi
])


dnl @synopsis AC_C99_FUNC_LRINT
dnl
dnl Check whether C99's lrint function is available.
dnl @version 1.1
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any 
dnl purpose is hereby granted without fee, provided that the above copyright 
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is 
dnl provided "as is" without express or implied warranty.
dnl
AC_DEFUN([AC_C99_FUNC_LRINT],
[AC_CACHE_CHECK(for lrint,
  ac_cv_c99_lrint,
[AC_TRY_COMPILE([
#define		_ISOC9X_SOURCE	1
#define 	_ISOC99_SOURCE	1
#define		__USE_ISOC99	1
#define 	__USE_ISOC9X	1
#include	<math.h>],
[	int value = lrint (0.432) ; ], ac_cv_c99_lrint=yes, ac_cv_c99_lrint=no)])
if test $ac_cv_c99_lrint = yes; then
  AC_DEFINE(HAVE_LRINT, 1,
            [Define if you have C99's lrint function.])
fi
])# AC_C99_LRINT

dnl @synopsis AC_C99_FUNC_LRINTF
dnl
dnl Check whether C99's lrintf function is available.
dnl @version 1.1
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any 
dnl purpose is hereby granted without fee, provided that the above copyright 
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is 
dnl provided "as is" without express or implied warranty.
dnl
AC_DEFUN([AC_C99_FUNC_LRINTF],
[AC_CACHE_CHECK(for lrintf,
  ac_cv_c99_lrintf,
[AC_TRY_COMPILE([
#define		_ISOC9X_SOURCE	1
#define 	_ISOC99_SOURCE	1
#define		__USE_ISOC99	1
#define 	__USE_ISOC9X	1
#include	<math.h>],
[	int value = lrintf (0.432) ; ], ac_cv_c99_lrintf=yes, ac_cv_c99_lrintf=no)])
if test $ac_cv_c99_lrintf = yes; then
  AC_DEFINE(HAVE_LRINTF, 1,
            [Define if you have C99's lrintf function.])
fi
])# AC_C99_LRINTF


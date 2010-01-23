
AC_DEFUN([AUDACITY_CHECKLIB_LIBNYQUIST], [
   AC_ARG_ENABLE(nyquist,
               [AS_HELP_STRING([--enable-nyquist],
                               [enable Nyquist plug-in support [default=yes]])],
               LIBNYQUIST_ARGUMENT=$enableval,
               LIBNYQUIST_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_NYQUIST, 1,
                [Define if Nyquist support should be enabled])
   fi

   dnl Nyquist is never installed on the system
   dnl nyx is home-cooked by us, so system copy is unlikely
   LIBNYQUIST_SYSTEM_AVAILABLE="no"

   dnl see if Nyquist is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libnyquist/nyx/nyx.h,
                 nyx_h_found="yes",
                 nyx_h_found="no")

   if test "x$nyx_h_found" = "xyes" ; then
      LIBNYQUIST_LOCAL_AVAILABLE="yes"
      LIBNYQUIST_LOCAL_LIBS="libnyquist.a"
      LIBNYQUIST_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libnyquist/nyx'
      LIBNYQUIST_LOCAL_CPPSYMBOLS="USE_NYQUIST"
      LIBNYQUIST_LOCAL_OPTOBJS="effects/nyquist/Nyquist.o"
      LIBNYQUIST_LOCAL_OPTOBJS="$LIBNYQUIST_LOCAL_OPTOBJS effects/nyquist/LoadNyquist.o"

      AC_MSG_NOTICE([nyquist libraries are available in the local tree])
   else
      LIBNYQUIST_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([nyquist libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBVAMP], [
   AC_ARG_ENABLE(vamp,
               [AS_HELP_STRING([--enable-vamp],
                               [enable Vamp plug-in support [default=yes]])],
               LIBVAMP_ARGUMENT=$enableval,
               LIBVAMP_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_VAMP, 1,
                [Define if Vamp analysis plugin support should be enabled])
   fi

   dnl System may include Vamp headers and library, though we prefer local ones

   PKG_CHECK_MODULES(VAMP, vamp-hostsdk >= 1.1.0,
                     vamp_available_system="yes",
                     vamp_available_system="no")

   if test "x$vamp_available_system" = "xyes" ; then
      LIBVAMP_SYSTEM_AVAILABLE="yes"
      LIBVAMP_SYSTEM_LIBS=$VAMP_LIBS
      LIBVAMP_SYSTEM_CXXFLAGS=$VAMP_CFLAGS
	  dnl still need these local objects for the support in audacity
      LIBVAMP_SYSTEM_OPTOBJS="effects/vamp/VampEffect.o effects/vamp/LoadVamp.o"
      LIBVAMP_SYSTEM_CPPSYMBOLS="USE_VAMP"
      AC_MSG_NOTICE([Vamp libraries are available as system libraries])
   else
      LIBVAMP_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Vamp libraries are NOT available as system libraries])
   fi

   dnl see if Vamp is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libvamp/vamp-sdk/hostext/PluginLoader.h,
                 vamp_h_found="yes",
                 vamp_h_found="no")

   if test "x$vamp_h_found" = "xyes" ; then
      LIBVAMP_LOCAL_AVAILABLE="yes"
      LIBVAMP_LOCAL_LIBS="libvamp-hostsdk.a"
      LIBVAMP_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libvamp'
      LIBVAMP_LOCAL_OPTOBJS="effects/vamp/VampEffect.o effects/vamp/LoadVamp.o"
      LIBVAMP_LOCAL_CPPSYMBOLS="USE_VAMP"
      if test ! -f lib-src/libvamp/Makefile ; then
         LIBVAMP_LOCAL_CONFIG_SUBDIRS="lib-src/libvamp"
      fi
      AC_MSG_NOTICE([Vamp libraries are available in the local tree])
   else
      LIBVAMP_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([Vamp libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBTWOLAME], [
   AC_ARG_WITH(libtwolame,
               [AS_HELP_STRING([--with-libtwolame],
                               [use libtwolame for MP2 export support ])],
               LIBTWOLAME_ARGUMENT=$withval,
               LIBTWOLAME_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBTWOLAME, 1,
                [Define if libtwolame (MP2 export) support should be enabled])
   fi

   dnl Check for a system copy of libtwolame to use, which needs to be
   dnl pretty current to work

   PKG_CHECK_MODULES(LIBTWOLAME, twolame >= 0.3.9,
                     twolame_available_system="yes",
                     twolame_available_system="no")

   if test "x$twolame_available_system" = "xyes" ; then
      LIBTWOLAME_SYSTEM_AVAILABLE="yes"
      LIBTWOLAME_SYSTEM_LIBS=$LIBTWOLAME_LIBS
      LIBTWOLAME_SYSTEM_CXXFLAGS=$LIBTWOLAME_CFLAGS
      LIBTWOLAME_SYSTEM_CPPSYMBOLS="USE_LIBTWOLAME"
      AC_MSG_NOTICE([Libtwolame library available as system library])
   else
      LIBTWOLAME_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libtwolame library NOT available as system library])
   fi

   dnl see if libtwolame is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/twolame/libtwolame/twolame.h,
                 twolame_h_found="yes",
                 twolame_h_found="no")

   if test "x$twolame_h_found" = "xyes" ; then
      LIBTWOLAME_LOCAL_AVAILABLE="yes"
      LIBTWOLAME_LOCAL_LIBS="libtwolame.a"
      LIBTWOLAME_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/twolame/libtwolame'
      LIBTWOLAME_LOCAL_CPPSYMBOLS="USE_LIBTWOLAME"

      if test ! -f lib-src/twolame/Makefile ; then
         LIBTWOLAME_LOCAL_CONFIG_SUBDIRS="lib-src/twolame"
      fi
      AC_MSG_NOTICE([libtwolame library is available in the local tree])
   else
      LIBTWOLAME_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libtwolame library is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBSOUNDTOUCH], [
   AC_ARG_WITH(soundtouch,
               [AS_HELP_STRING([--with-soundtouch],
                      [use libSoundTouch for pitch and tempo changing])],
               LIBSOUNDTOUCH_ARGUMENT=$withval,
               LIBSOUNDTOUCH_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_SOUNDTOUCH, 1,
                [Define if SoundTouch support should be enabled])
   fi

   dnl see if soundtouch is installed on the system

   PKG_CHECK_MODULES(SOUNDTOUCH, soundtouch-1.0 >= 1.3.0,
                     soundtouch_available_system="yes",
                     soundtouch_available_system="no")


   if test "x$soundtouch_available_system" = "xyes" ; then
      LIBSOUNDTOUCH_SYSTEM_AVAILABLE="yes"
      LIBSOUNDTOUCH_SYSTEM_LIBS=$SOUNDTOUCH_LIBS
      LIBSOUNDTOUCH_SYSTEM_CXXFLAGS=$SOUNDTOUCH_CFLAGS
      LIBSOUNDTOUCH_SYSTEM_CPPSYMBOLS="USE_SOUNDTOUCH"
      AC_MSG_NOTICE([Libsoundtouch libraries are available as system libraries])
   else
      LIBSOUNDTOUCH_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsoundtouch libraries are NOT available as system libraries])
   fi

   dnl see if libsoundtouch is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/soundtouch/include/SoundTouch.h,
                 soundtouch_h_found="yes",
                 soundtouch_h_found="no")

   if test "x$soundtouch_h_found" = "xyes" ; then
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
               [AS_HELP_STRING([--with-libresample],
                               [use libresample for sample rate conversion: [yes,no]])],
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

   if test "x$resample_h_found" = "xyes" ; then
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
               [AS_HELP_STRING([--with-libsamplerate],
                               [use libsamplerate instead of libresample for sample rate conversion. Do not use in conjunction with VST plug-in support!])],
               LIBSAMPLERATE_ARGUMENT=$withval,
               LIBSAMPLERATE_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBSAMPLERATE, 1,
                [Define if libsamplerate support should be enabled])
   fi

   dnl see if libsamplerate is installed on the system

   PKG_CHECK_MODULES(SAMPLERATE, samplerate >= 0.1.2,
                     samplerate_available_system="yes",
                     samplerate_available_system="no")

   if test "x$samplerate_available_system" = "xyes" ; then
      LIBSAMPLERATE_SYSTEM_AVAILABLE="yes"
      LIBSAMPLERATE_SYSTEM_LIBS=$SAMPLERATE_LIBS
      LIBSAMPLERATE_SYSTEM_CXXFLAGS=$SAMPLERATE_CFLAGS
      LIBSAMPLERATE_SYSTEM_CPPSYMBOLS="USE_LIBSAMPLERATE"
      AC_MSG_NOTICE([Libsamplerate libraries are available as system libraries])
   else
      LIBSAMPLERATE_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsamplerate libraries are NOT available as system libraries])
   fi

   dnl see if libsamplerate is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libsamplerate/src/samplerate.h,
                 samplerate_h_found="yes",
                 samplerate_h_found="no")

   if test "x$samplerate_h_found" = "xyes" ; then
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

   AC_ARG_WITH(libid3tag,
               [AS_HELP_STRING([--with-libid3tag],
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

   if test "x$libid3tag_found" = "xyes" && test "x$id3tag_h_found" = "xyes" ; then
      LIBID3TAG_SYSTEM_AVAILABLE="yes"
      LIBID3TAG_SYSTEM_LIBS=-lid3tag
      LIBID3TAG_SYSTEM_CPPSYMBOLS="USE_LIBID3TAG"
      AC_MSG_NOTICE([Libid3tag libraries are available as system libraries])
   else
      LIBID3TAG_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libid3tag libraries are NOT available as system libraries])
   fi

   dnl see if libid3tag is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libid3tag/frame.h,
                 frame_h_found="yes",
                 frame_h_found="no")


   if test "x$frame_h_found" = "xyes" ; then
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
               [AS_HELP_STRING([--with-libsndfile],
                               [which libsndfile to use (required): [system,local]])],
               LIBSNDFILE_ARGUMENT=$withval,
               LIBSNDFILE_ARGUMENT="unspecified")

   dnl see if libsndfile is installed in the system

   PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0.0,
                     sndfile_available_system="yes",
                     sndfile_available_system="no")

   if test "x$sndfile_available_system" = "xyes" ; then
      LIBSNDFILE_SYSTEM_AVAILABLE="yes"
      LIBSNDFILE_SYSTEM_LIBS=$SNDFILE_LIBS
      LIBSNDFILE_SYSTEM_CXXFLAGS=$SNDFILE_CFLAGS
      AC_MSG_NOTICE([Libsndfile libraries are available as system libraries])
   else
      LIBSNDFILE_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsndfile libraries are NOT available as system libraries])
   fi

   dnl see if libsndfile is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libsndfile/src/sndfile.h.in,
                 libsndfile_found="yes",
                 libsndfile_found="no")

   if test "x$libsndfile_found" = "xyes" ; then
      LIBSNDFILE_LOCAL_AVAILABLE="yes"
      LIBSNDFILE_LOCAL_LIBS="libsndfile.a"
      LIBSNDFILE_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libsndfile/src'
      if test ! -f "lib-src/libsndfile/Makefile"; then
         LIBSNDFILE_LOCAL_CONFIG_SUBDIRS="lib-src/libsndfile"
      fi
      AC_MSG_NOTICE([libsndfile libraries are available in this source tree])

      dnl Temporary fix for bug #248
      ac_configure_args="$ac_configure_args --disable-sqlite --disable-flac --disable-alsa"
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
               [AS_HELP_STRING([--with-libmad],
                               [use libmad for mp2/3 decoding support])],
               LIBMAD_ARGUMENT=$withval,
               LIBMAD_ARGUMENT="unspecified")

   dnl see if libmad is installed in the system >= 0.14.2b

   PKG_CHECK_MODULES(LIBMAD, mad >= 0.14.2b,
                     libmad_available_system="yes",
                     libmad_available_system="no")

   dnl if we don't have the version we want, do we have any at all?

   AC_CHECK_LIB(mad, mad_decoder_init,
                libmad_found="yes",
                libmad_found="no")

   if test "x$libmad_available_system" = "xyes" && test "x$libmad_found" = "xno" ; then
      AC_MSG_WARN([system installation of libmad found, but it is too old.  Upgrade to at least 0.14.2b to use with Audacity])
   fi

   if test "x$libmad_available_system" = "xyes" ; then
      LIBMAD_SYSTEM_AVAILABLE="yes"
      LIBMAD_SYSTEM_LIBS="$LIBMAD_LIBS"
	  LIBMAD_SYSTEM_CXXFLAGS="$LIBMAD_CFLAGS"
      LIBMAD_SYSTEM_CPPSYMBOLS="USE_LIBMAD"
      AC_MSG_NOTICE([libmad libraries are available as system libraries])
   else
      AC_MSG_NOTICE([libmad libraries are NOT available as system libraries])
   fi

   dnl see if libmad is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libmad/frame.h,
                 frame_h_found="yes",
                 frame_h_found="no")

   if test "x$frame_h_found" = "xyes" ; then
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

   AC_ARG_WITH(libvorbis,
               [AS_HELP_STRING([--with-libvorbis],
                               [use libvorbis for Ogg Vorbis support])],
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

   if test "x$lib_found" = "xyes" && test "x$header_found" = "xyes" ; then
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

   if test "x$vorbisenc_h_available" = "xyes" && test "x$ogg_h_available" = "xyes" ; then
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

AC_DEFUN([AUDACITY_CHECKLIB_LIBFLAC], [

   if false ; then
      AC_DEFINE(USE_LIBFLAC, 1,
                [Define if the FLAC library is present])
   fi

   AC_ARG_WITH(libflac,
               [AS_HELP_STRING([--with-libflac],
                               [use libFLAC for FLAC support])],
               LIBFLAC_ARGUMENT=$withval,
               LIBFLAC_ARGUMENT="unspecified")

   dnl See if FLAC is installed in the system

   AC_CHECK_LIB(FLAC,
                FLAC__stream_decoder_new,
                lib_found="yes",
                lib_found="no",
                -lFLAC++ -lFLAC)

   AC_CHECK_HEADER(FLAC/format.h,
                   header_found="yes",
                   header_found="no")

   if test "x$lib_found" = "xyes" && test "x$header_found" = "xyes" ; then
      LIBFLAC_SYSTEM_AVAILABLE="yes"
      LIBFLAC_SYSTEM_LIBS="-lFLAC++ -lFLAC"
      LIBFLAC_SYSTEM_CPPSYMBOLS="USE_LIBFLAC"
      AC_MSG_NOTICE([FLAC libraries are available as system libraries])
   else
      LIBFLAC_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([FLAC/FLAC++ libraries are NOT available as system libraries])
   fi

   dnl see if FLAC is available in the source dir

   AC_CHECK_FILE(${srcdir}/lib-src/libflac/include/FLAC/format.h,
                 flac_h_available="yes",
                 flac_h_available="no")

   AC_CHECK_FILE(${srcdir}/lib-src/libflac/include/FLAC++/decoder.h,
                 flacpp_h_available="yes",
                 flacpp_h_available="no")

   if test "x$flac_h_available" = "xyes" && test "x$flacpp_h_available" = "xyes" ; then
      LIBFLAC_LOCAL_AVAILABLE="yes"

      LIBFLAC_LOCAL_LIBS="libFLAC++.a libFLAC.a"

      LIBFLAC_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libflac/include'
      LIBFLAC_LOCAL_CXXFLAGS="$LIBFLAC_LOCAL_CXXFLAGS -I\$(top_srcdir)/lib-src/libflac/include"

      LIBFLAC_LOCAL_CPPSYMBOLS="USE_LIBFLAC"

      if test ! -f lib-src/libflac/Makefile ; then
         LIBFLAC_LOCAL_CONFIG_SUBDIRS="lib-src/libflac"
      fi
	
      AC_MSG_NOTICE([FLAC libraries are available in this source tree])
   else
      AC_MSG_NOTICE([FLAC libraries are NOT available in this source tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBEXPAT], [

   AC_ARG_WITH(expat,
               [AS_HELP_STRING([--with-expat],
                               [which expat to use for XML file support: [system,local]])],
               LIBEXPAT_ARGUMENT=$withval,
               LIBEXPAT_ARGUMENT="unspecified")

   dnl see if libexpat is installed on the system

   AC_CHECK_LIB(expat, XML_ParserCreate,
                libexpat_found="yes",
                libexpat_found="no")

   expat_h_found="no"

   AC_CHECK_HEADER(expat.h,
                   expat_h_found="yes",
                   expat_h_found="no")

   if test "x$libexpat_found" = "xyes" && test "x$expat_h_found" = "xyes" ; then
      LIBEXPAT_SYSTEM_AVAILABLE="yes"
      LIBEXPAT_SYSTEM_LIBS="-lexpat"
      LIBEXPAT_SYSTEM_CPPSYMBOLS="USE_SYSTEM_EXPAT"
      AC_MSG_NOTICE([Expat libraries are available as system libraries])
   else
      LIBEXPAT_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Expat libraries are NOT available as system libraries])
   fi

   dnl see if expat is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/expat/xmlparse/xmlparse.h,
                 xmlparse_h_found="yes",
                 xmlparse_h_found="no")

   if test "x$xmlparse_h_found" = "xyes" ; then
      LIBEXPAT_LOCAL_AVAILABLE="yes"
      LIBEXPAT_LOCAL_LIBS="expat.a"
      LIBEXPAT_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/expat'
      LIBEXPAT_LOCAL_CPPSYMBOLS="USE_LOCAL_EXPAT"

      AC_MSG_NOTICE([Expat libraries are available in the local tree])
   else
      LIBEXPAT_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([Expat libraries are NOT available in the local tree])
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
if test "x$ac_cv_c99_lrint" = "xyes"; then
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
if test "x$ac_cv_c99_lrintf" = "xyes"; then
  AC_DEFINE(HAVE_LRINTF, 1,
            [Define if you have C99's lrintf function.])
fi
])# AC_C99_LRINTF


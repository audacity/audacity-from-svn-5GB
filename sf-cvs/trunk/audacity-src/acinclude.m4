dnl Function to configure a sub-library now, because we need to know the result
dnl of the configuration now in order to take decisions.
dnl We don't worry about whether the configuration worked or not - it is
dnl assumed that the next thing after this will be a package-specific check to
dnl see if the package is actually available. (Hint: use pkg-config and
dnl -uninstalled.pc files if available).
dnl code based on a simplification of _AC_OUTPUT_SUBDIRS in 
dnl /usr/share/autoconf/autoconf/status.m4 which implements part of 
dnl AC_CONFIG_SUBDIRS

AC_DEFUN([AX_CONFIG_DIR], [
  # Remove --cache-file and --srcdir arguments so they do not pile up.
  ax_sub_configure_args=
  ax_prev=
  eval "set x $ac_configure_args"
  shift
  for ax_arg
  do
    if test -n "$ax_prev"; then
      ax_prev=
      continue
    fi
    case $ax_arg in
    -cache-file | --cache-file | --cache-fil | --cache-fi \
    | --cache-f | --cache- | --cache | --cach | --cac | --ca | --c)
      ax_prev=cache_file ;;
    -cache-file=* | --cache-file=* | --cache-fil=* | --cache-fi=* \
    | --cache-f=* | --cache-=* | --cache=* | --cach=* | --cac=* | --ca=* \
    | --c=*)
      ;;
    --config-cache | -C)
      ;;
    -srcdir | --srcdir | --srcdi | --srcd | --src | --sr)
      ax_prev=srcdir ;;
    -srcdir=* | --srcdir=* | --srcdi=* | --srcd=* | --src=* | --sr=*)
      ;;
    -prefix | --prefix | --prefi | --pref | --pre | --pr | --p)
      ax_prev=prefix ;;
    -prefix=* | --prefix=* | --prefi=* | --pref=* | --pre=* | --pr=* | --p=*)
      ;;
    *)
      case $ax_arg in
      *\'*) ax_arg=`echo "$ax_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
      esac
      ax_sub_configure_args="$ax_sub_configure_args '$ax_arg'" ;;
    esac
  done

  # Always prepend --prefix to ensure using the same prefix
  # in subdir configurations.
  ax_arg="--prefix=$prefix"
  case $ax_arg in
  *\'*) ax_arg=`echo "$ax_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
  esac
  ax_sub_configure_args="'$ax_arg' $ax_sub_configure_args"

  # Pass --silent
  if test "$silent" = yes; then
    ax_sub_configure_args="--silent $ax_sub_configure_args"
  fi

  ax_popdir=`pwd`
  AC_MSG_NOTICE([Configuring sources in $1])
  dnl for out-of-place builds srcdir and builddir will be different, and
  dnl builddir may not exist, so we must create it
  AS_MKDIR_P(["$1"])
  dnl and also set the variables. As this isn't autoconf, the following may be
  dnl risky:
  _AC_SRCDIRS(["$1"])
  cd "$1"

  # Check for guested configure; otherwise get Cygnus style configure.
  if test -f "configure.gnu"; then
    ax_sub_configure=$ac_srcdir/configure.gnu
  elif test -f "$ac_srcdir/configure"; then
    ax_sub_configure=$ac_srcdir/configure
  elif test -f "$ac_srcdir/configure.in"; then
    # This should be Cygnus configure.
	ax_sub_configure=$ac_aux_dir/configure
  else
    AC_MSG_WARN([no configuration information is in $1])
    ax_sub_configure=
  fi

  # The recursion is here.
  if test -n "$ax_sub_configure"; then
    # Make the cache file name correct relative to the subdirectory.
    case $cache_file in
    [[\\/]]* | ?:[[\\/]]* ) ax_sub_cache_file=$cache_file ;;
    *) # Relative name.
	ax_sub_cache_file=$ac_top_build_prefix$cache_file ;;
    esac

    AC_MSG_NOTICE([running $SHELL $ax_sub_configure $ax_sub_configure_args --cache-file=$ax_sub_cache_file --srcdir=$ac_srcdir])
    # The eval makes quoting arguments work.
    eval "\$SHELL \"\$ax_sub_configure\" $ax_sub_configure_args \
	   --cache-file=\"\$ax_sub_cache_file\" --srcdir=\"\$ax_srcdir\""
  fi

  cd "$ax_popdir"
  AC_MSG_NOTICE([Done configuring in $1])
])

dnl Function to test whether the compiler can do hidden symbol visibility. This
dnl test is from automake 2.62.

# visibility.m4 serial 1 (gettext-0.15)
dnl Copyright (C) 2005 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

dnl Tests whether the compiler supports the command-line option
dnl -fvisibility=hidden and the function and variable attributes
dnl __attribute__((__visibility__("hidden"))) and
dnl __attribute__((__visibility__("default"))).
dnl Does *not* test for __visibility__("protected") - which has tricky
dnl semantics (see the 'vismain' test in glibc) and does not exist e.g. on
dnl MacOS X.
dnl Does *not* test for __visibility__("internal") - which has processor
dnl dependent semantics.
dnl Does *not* test for #pragma GCC visibility push(hidden) - which is
dnl "really only recommended for legacy code".
dnl Set the variable CFLAG_VISIBILITY.
dnl Defines and sets the variable HAVE_VISIBILITY.

AC_DEFUN([AUDACITY_CHECK_VISIBILITY],
[
  AC_REQUIRE([AC_PROG_CC])
  CFLAG_VISIBILITY=
  HAVE_VISIBILITY=0
  if test -n "$GCC"; then
    AC_MSG_CHECKING([for simple visibility declarations])
    AC_CACHE_VAL(gl_cv_cc_visibility, [
      gl_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -fvisibility=hidden"
      AC_TRY_COMPILE(
        [extern __attribute__((__visibility__("hidden"))) int hiddenvar;
         extern __attribute__((__visibility__("default"))) int exportedvar;
         extern __attribute__((__visibility__("hidden"))) int hiddenfunc (void);
         extern __attribute__((__visibility__("default"))) int exportedfunc (void);],
        [],
        gl_cv_cc_visibility=yes,
        gl_cv_cc_visibility=no)
      CFLAGS="$gl_save_CFLAGS"])
    AC_MSG_RESULT([$gl_cv_cc_visibility])
    if test $gl_cv_cc_visibility = yes; then
      CFLAG_VISIBILITY="-fvisibility=hidden"
      HAVE_VISIBILITY=1
    fi
  fi
  AC_SUBST([CFLAG_VISIBILITY])
  AC_SUBST([HAVE_VISIBILITY])
  AC_DEFINE_UNQUOTED([HAVE_VISIBILITY], [$HAVE_VISIBILITY],
    [Define to 1 or 0, depending whether the compiler supports simple visibility declarations.])
])

dnl A function to check for the correct presence of lib-widget-extra in the 
dnl lib-src tree. Note that this is a mandatory library, and
dnl that because we maintain it, we don't support system copies.

dnl You would have thought you could use pkg-config for this. But the
dnl pkg-config file doesn't exist until configure has been run for
dnl lib-widget-extra. Using AC_CONFIG_SUBDIRS, that doesn't happen until
dnl after everything in the main configure script has happened, so
dnl we can't detect anything about the configured library, because it isn't
dnl configured when this runs.
dnl To get round this we have created our own subdirectory configuration
dnl function, AX_CONFIG_DIR based on a subset of the code that implements
dnl AC_CONFIG_SUBDIRS.

AC_DEFUN([AUDACITY_CHECKLIB_WIDGETEXTRA], [
   dnl we always need to configure libwidgetextra, so just call the script
   dnl regardless.
   AX_CONFIG_DIR(["${srcdir}/lib-src/lib-widget-extra"])
   dnl having done that we get a pkg-config file we can use
   dnl add the directory with lib-widget-extra in to the pkg-config search path
   export PKG_CONFIG_PATH="${srcdir}/lib-src/lib-widget-extra/:$PKG_CONFIG_PATH"
   PKG_CHECK_MODULES(WIDGETEXTRA, libwidgetextra,
                     widgetextra_available="yes",
                     widgetextra_available="no")

   if test "x$widgetextra_available" != "xyes" ; then
      AC_MSG_ERROR([lib-widget-extra is required to build audacity. A copy is included in the audacity source distribution at lib-src/lib-widget-extra/.])
   fi
   dnl otherwise good - got it. Flags will be available for use in
   dnl WIDGETEXTRA_LIBS and friends
])

AC_DEFUN([AUDACITY_CHECKLIB_PORTSMF], [

   AC_ARG_WITH(midi,
               [AS_HELP_STRING([--with-midi],
                               [use portSMF for Midi support ])],
               PORTSMF_ARGUMENT=$withval,
               PORTSMF_ARGUMENT="unspecified")
   if false ; then
      AC_DEFINE(USE_MIDI, 1,
                [Define if midi support should be enabled])
   fi

   dnl we need to configure portSMF if there is a local copy
   if test -d ${srcdir}/lib-src/portsmf ; then
      AX_CONFIG_DIR(["${srcdir}/lib-src/portsmf"])
   fi
   dnl having done that we get a pkg-config file we can use
   dnl add the directory with portSMF in to the pkg-config search path
   export PKG_CONFIG_PATH="${srcdir}/lib-src/portsmf/:$PKG_CONFIG_PATH"
   PKG_CHECK_MODULES(PORTSMF, portSMF,
                     portsmf_available="yes",
                     portsmf_available="no")

   if test "x$portsmf_available" = "xyes" ; then
      PORTSMF_LOCAL_AVAILABLE="yes"
      PORTSMF_LOCAL_LDFLAGS=$PORTSMF_LIBS
      PORTSMF_LOCAL_CXXFLAGS=$PORTSMF_CFLAGS
      PORTSMF_LOCAL_CPPSYMBOLS="USE_MIDI"
	  dnl extra objects we can now compile
      PORTSMF_LOCAL_OPTOBJS="NoteTrack.o import/ImportMIDI.o"
	  dnl this bit makes sure that we compile the lib-src copy
	  PORTSMF_LOCAL_BUILD="portSMF"
   else
	  PORTSMF_LOCAL_AVAILABLE="no"
   fi
])

dnl Check for Nyquist as a library
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
   AC_ARG_WITH(libvamp,
               [AS_HELP_STRING([--with-libvamp],
                               [use libvamp for Vamp plug-in support [default=yes]])],
               LIBVAMP_ARGUMENT=$withval,
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

AC_DEFUN([AUDACITY_CHECKLIB_FFMPEG], [
   AC_ARG_WITH(ffmpeg,
               [AS_HELP_STRING([--with-ffmpeg],
                               [use ffmpeg for import and export support ])],
               FFMPEG_ARGUMENT=$withval,
               FFMPEG_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_FFMPEG, 1,
                [Define if ffmpeg (multi-format import and export) support should be enabled])
   fi

   dnl Check for a system copy of ffmpeg to use. For now I'm insiting on a
   dnl current version to make maintenance easier. We need both avcodec and
   dnl avformat, so I'm going to check for both

   PKG_CHECK_MODULES(AVCODEC, libavcodec >= 51.53,
                     avcodec_available_system="yes",
                     avcodec_available_system="no")
   PKG_CHECK_MODULES(AVFORMAT, libavformat >= 52.12,
                     avformat_available_system="yes",
                     avformat_available_system="no")

   FFMPEG_SYSTEM_AVAILABLE="no"
   if test "x$avcodec_available_system" = "xyes" ; then
      if test "x$avformat_available_system" = "xyes" ; then
	     FFMPEG_SYSTEM_AVAILABLE="yes"
         FFMPEG_SYSTEM_CXXFLAGS="$AVCODEC_CFLAGS $AVFORMAT_CFLAGS"
         FFMPEG_SYSTEM_CPPSYMBOLS="USE_FFMPEG"
		 dnl build the extra object files needed to use FFmpeg. Paths inside
		 dnl the audacity src/ dir, as this is subsitiuted into src/Makefile.in
		 FFMPEG_SYSTEM_OPTOBJS="import/ImportFFmpeg.o export/ExportFFmpeg.o \
		 	export/ExportFFmpegDialogs.o"
         AC_MSG_NOTICE([FFmpeg library available as system library])
      fi
   fi
   if test "x$FFMPEG_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([FFmpeg library NOT available as system library])
   fi

   dnl see if ffmpeg is available locally, or rather that we have some headers
   dnl in lib-src/ffmpeg/ we can use.
   FFMPEG_LOCAL_AVAILABLE="no"
   AC_CHECK_FILE(${srcdir}/lib-src/ffmpeg/libavcodec/avcodec.h,
                 avcodec_h_found="yes",
                 avcodec_h_found="no")

   AC_CHECK_FILE(${srcdir}/lib-src/ffmpeg/libavformat/avformat.h,
                 avformat_h_found="yes",
                 avformat_h_found="no")

   if test "x$avcodec_h_found" = "xyes" ; then
     if test "x$avformat_h_found" = "xyes" ; then
        FFMPEG_LOCAL_AVAILABLE="yes"
        FFMPEG_LOCAL_LIBS=""
        FFMPEG_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/ffmpeg'
         FFMPEG_LOCAL_CPPSYMBOLS="USE_FFMPEG"
         dnl build the extra object files needed to use FFmpeg. Paths inside
         dnl the audacity src/ dir, as this is subsitiuted into src/Makefile.in
         FFMPEG_LOCAL_OPTOBJS="import/ImportFFmpeg.o export/ExportFFmpeg.o"
        AC_MSG_NOTICE([FFmpeg headers are available in the local tree])
     fi
   fi
   if test "x$FFMPEG_LOCAL_AVAILABLE" = "xno" ; then
     AC_MSG_NOTICE([ffmpeg library is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBRAPTOR], [
   AC_ARG_WITH(libraptor,
               [AS_HELP_STRING([--with-libraptor],
                               [libraptor is needed for categorisation of LADSPA plugins])],
               LIBRAPTOR_ARGUMENT=$withval,
               LIBRAPTOR_ARGUMENT="unspecified")

   dnl Check for a system copy of libraptor to use. 

   PKG_CHECK_MODULES(LIBRAPTOR, raptor >= 1.4.17,
                     libraptor_available_system="yes",
                     libraptor_available_system="no")

   LIBRAPTOR_SYSTEM_AVAILABLE="no"
   if test "x$libraptor_available_system" = "xyes" ; then
      LIBRAPTOR_SYSTEM_AVAILABLE="yes"
      LIBRAPTOR_SYSTEM_LIBS="$LIBRAPTOR_LIBS"
      LIBRAPTOR_SYSTEM_CXXFLAGS="$LIBRAPTOR_CFLAGS"
      AC_MSG_NOTICE([libraptor available as system library])
   fi
   if test "x$LIBRAPTOR_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([libraptor NOT available as system library])
   fi

   dnl see if libraptor is available locally
   AC_CHECK_FILE(${srcdir}/lib-src/libraptor/src/raptor.h,
                 raptor_h_found="yes",
                 raptor_h_found="no")

   if test "x$raptor_h_found" = "xyes" ; then
      LIBRAPTOR_LOCAL_AVAILABLE="yes"
      LIBRAPTOR_LOCAL_LIBS="libraptor.a"
      LIBRAPTOR_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libraptor/src'
      if test ! -f lib-src/libraptor/Makefile ; then
         LIBRAPTOR_LOCAL_CONFIG_SUBDIRS="lib-src/libraptor"
      fi
      AC_MSG_NOTICE([libraptor is available in the local tree])
      if test "x$LIBEXPAT_SYSTEM_AVAILABLE" = "xno" ; then
      ac_configure_args="$ac_configure_args \"--with-expat-source=${srcdir}/src/include\""
      fi
      ac_configure_args="$ac_configure_args RAPTOR_CFLAGS='-I../../libraptor/src' RAPTOR_LIBS='-L../.. -lraptor'"
   else
      LIBRAPTOR_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libraptor is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_LIBLRDF], [
   AC_ARG_WITH(liblrdf,
               [AS_HELP_STRING([--with-liblrdf],
                               [use liblrdf for categorisation of LADSPA plugins ])],
               LIBLRDF_ARGUMENT=$withval,
               LIBLRDF_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBLRDF, 1,
                [Define if liblrdf (metadata for LADSPA plugins) support should be enabled])
   fi

   dnl Check for a system copy of liblrdf to use. I've only tested with 
   dnl version 0.4.0, this requirement might be relaxed in the future if
   dnl someone else has it working with an earlier version.

   PKG_CHECK_MODULES(LIBLRDF, lrdf >= 0.4.0,
                     liblrdf_available_system="yes",
                     liblrdf_available_system="no")

   LIBLRDF_SYSTEM_AVAILABLE="no"
   if test "x$liblrdf_available_system" = "xyes" ; then
      LIBLRDF_SYSTEM_AVAILABLE="yes"
      LIBLRDF_SYSTEM_LIBS="$LIBLRDF_LIBS"
      LIBLRDF_SYSTEM_CXXFLAGS="$LIBLRDF_CFLAGS"
      LIBLRDF_SYSTEM_CPPSYMBOLS="USE_LIBLRDF"
      AC_MSG_NOTICE([liblrdf available as system library])
   fi
   if test "x$LIBLRDF_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([liblrdf NOT available as system library])
   fi

   dnl see if liblrdf is available locally
   AC_CHECK_FILE(${srcdir}/lib-src/liblrdf/lrdf.h,
                 lrdf_h_found="yes",
                 lrdf_h_found="no")

   if test "x$lrdf_h_found" = "xyes" ; then
      LIBLRDF_LOCAL_AVAILABLE="yes"
      LIBLRDF_LOCAL_LIBS="liblrdf.a"
      LIBLRDF_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/liblrdf'
      LIBLRDF_LOCAL_CPPSYMBOLS="USE_LIBLRDF"
      if test ! -f lib-src/liblrdf/Makefile ; then
         LIBLRDF_LOCAL_CONFIG_SUBDIRS="lib-src/liblrdf"
      fi
      AC_MSG_NOTICE([liblrdf is available in the local tree])
   else
      LIBLRDF_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([liblrdf is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_SLV2], [
   AC_ARG_WITH(slv2,
               [AS_HELP_STRING([--with-slv2],
                               [use SLV2 for loading LV2 plugins ])],
               SLV2_ARGUMENT=$withval,
               SLV2_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_SLV2, 1,
                [Define if SLV2 (library for loading LV2 plugins) should be enabled])
   fi

   dnl Check for a system copy of SLV2 to use. We need at least version 0.6.

   PKG_CHECK_MODULES(SLV2, slv2 >= 0.6.0,
                     slv2_available_system="yes",
                     slv2_available_system="no")
   SLV2_SYSTEM_AVAILABLE="no"
   if test "x$slv2_available_system" = "xyes" ; then
      SLV2_SYSTEM_AVAILABLE="yes"
      SLV2_SYSTEM_LIBS="$SLV2_LIBS"
      SLV2_SYSTEM_CXXFLAGS="$SLV2_CFLAGS"
      SLV2_SYSTEM_CPPSYMBOLS="USE_SLV2"
      SLV2_SYSTEM_OPTOBJS="effects/lv2/LoadLV2.o effects/lv2/LV2Effect.o"
      AC_MSG_NOTICE([SLV2 available as system library])
   fi
   if test "x$SLV2_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([SLV2 NOT available as system library])
   fi

   dnl Check if SLV2 is available locally.
   AC_CHECK_FILE(${srcdir}/lib-src/slv2/slv2/slv2.h,
                 slv2_h_found="yes",
                 slv2_h_found="no")

   if test "x$slv2_h_found" = "xyes" ; then
      SLV2_LOCAL_AVAILABLE="yes"
      SLV2_LOCAL_LIBS="libslv2.a"
      SLV2_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/slv2'
      SLV2_LOCAL_CPPSYMBOLS="USE_SLV2"
      SLV2_LOCAL_OPTOBJS="effects/lv2/LoadLV2.o effects/lv2/LV2Effect.o"
      if test ! -f lib-src/slv2/Makefile ; then
         SLV2_LOCAL_CONFIG_SUBDIRS="lib-src/slv2"
      fi
      AC_MSG_NOTICE([SLV2 is available in the local tree])
   else
      SLV2_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([SLV2 is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CHECKLIB_REDLAND], [
   AC_ARG_WITH(redland,
               [AS_HELP_STRING([--with-redland],
                               [use Redland for reading RDF data ])],
               REDLAND_ARGUMENT=$withval,
               REDLAND_ARGUMENT="unspecified")

   dnl Check for a system copy of Redland to use.
   PKG_CHECK_MODULES(REDLAND, redland >= 1.0.7,
                     redland_available_system="yes",
                     redland_available_system="no")
   REDLAND_SYSTEM_AVAILABLE="no"
   if test "x$redland_available_system" = "xyes" ; then
      REDLAND_SYSTEM_AVAILABLE="yes"
      REDLAND_SYSTEM_LIBS="$REDLAND_LIBS"
      REDLAND_SYSTEM_CXXFLAGS="$REDLAND_CFLAGS"
      AC_MSG_NOTICE([Redland available as system library])
   fi
   if test "x$REDLAND_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([Redland NOT available as system library])
   fi

   dnl Check if Redland is available locally.
   AC_CHECK_FILE(${srcdir}/lib-src/redland/librdf/librdf.h,
                 librdf_h_found="yes",
                 librdf_h_found="no")

   if test "x$librdf_h_found" = "xyes" ; then
      REDLAND_LOCAL_AVAILABLE="yes"
      REDLAND_LOCAL_LIBS="librdf.a libraptor.a librasqal.a"
      REDLAND_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/redland/librdf -I$(top_srcdir)/lib-src/redland/raptor/src -I$(top_srcdir)/lib-src/redland/rasqal/src'
      if test ! -f lib-src/redland/Makefile ; then
         REDLAND_LOCAL_CONFIG_SUBDIRS="lib-src/redland"
      fi
      if test "x$LIBEXPAT_SYSTEM_AVAILABLE" = "xno" ; then
         # This is a horrible hack to keep from having to modify the raptor/configure.ac.  It makes
         # the raptor configure think there's a full expat source tree.  But, all we have is expat.h
         # tucked away in audacity/src/include.  So, we trick it...
         ac_configure_args="$ac_configure_args \"--with-expat-source=dummy_magic\" CPPFLAGS='$CPPFLAGS -I../../../src/include'"
      fi
      ac_configure_args="$ac_configure_args RAPTOR_CFLAGS='-I../../redland/raptor/src' RAPTOR_LIBS='-L.. -L../.. -lraptor' REDLAND_CFLAGS='-I../../redland/raptor/src -I../../redland/rasqal/src -I../../redland/librdf' REDLAND_LIBS='-L.. -L../.. -lrdf -lraptor -lrasqal'"
      AC_MSG_NOTICE([Redland is available in the local tree])
   else
      REDLAND_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([Redland is NOT available in the local tree])
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
      LIBID3TAG_LOCAL_CONFIG_SUBDIRS="lib-src/libid3tag"
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
      LIBSNDFILE_LOCAL_CONFIG_SUBDIRS="lib-src/libsndfile"
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
      LIBMAD_LOCAL_CONFIG_SUBDIRS="lib-src/libmad"
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
      LIBFLAC_LOCAL_CONFIG_SUBDIRS="lib-src/libflac"
	
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


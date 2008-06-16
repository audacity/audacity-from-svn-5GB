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
dnl test is basically from Glibc 2.6.1 configure.in

dnl If the C++ compiler supports making symbols within audacity hidden then
dnl we would like to do so. This means that only the required symbols for
dnl plug-in functionality are exposed, rather than everything in the program.
dnl For GCC > 4.0 we get this by adding -fvisibility=hidden to the CXXFLAGS.
dnl-------------------------------------------------------------------------
AC_DEFUN([AUDACITY_CHECK_VISIBILITY], [
dnl Step 1 - look for required support in assembler. 
  AC_CACHE_CHECK(for .protected and .hidden assembler directive,
		 as_protected_directive, [dnl
  cat > conftest.s <<EOF
.protected foo
foo:
.hidden bar
bar:
EOF
  if AC_TRY_COMMAND(${CC} -c $ASFLAGS conftest.s 1>&AS_MESSAGE_LOG_FD); then
    as_protected_directive="yes"
  else
    AC_MSG_WARN(no assembler support for symbol visibility)
	as_protected_directive="no"
  fi
  rm -f conftest*])

dnl Step 2 - check if the compiler will take __attribute__ declarations
  AC_CACHE_CHECK(whether __attribute__((visibility())) is supported,
	 cc_visibility_attribute,
	 [cat > conftest.c <<EOF
	 int foo __attribute__ ((visibility ("hidden"))) = 1;
	 int bar __attribute__ ((visibility ("protected"))) = 1;
EOF
	  cc_visibility_attribute=no
	  if AC_TRY_COMMAND(${CXX} -Werror -S conftest.c -o conftest.s 1>&AS_MESSAGE_LOG_FD); then
	    if grep '\.hidden.*foo' conftest.s >/dev/null; then
	      if grep '\.protected.*bar' conftest.s >/dev/null; then
			cc_visibility_attribute=yes
	      fi
	    fi
	  fi
	  rm -f conftest.[cs]
	 ])
	 if test x$cc_visibility_attribute != "xyes"; then
	   AC_MSG_WARN(no compiler support for visibility attributes available)
     fi

dnl Step 3 - see if __attribute__ is broke or not
  AC_CACHE_CHECK(for broken __attribute__((visibility())),
		 cc_broken_visibility_attribute,
		 [cat > conftest.c <<EOF
		  int foo (int x);
		  int bar (int x) __asm__ ("foo") __attribute__ ((visibility ("hidden")));
		  int bar (int x) { return x; }
EOF
	  cc_broken_visibility_attribute=yes
	  if AC_TRY_COMMAND(${CC} -Werror -S conftest.c -o conftest.s 1>&AS_MESSAGE_LOG_FD); then
changequote(,)dnl
	    if grep '\.hidden[ 	_]foo' conftest.s >/dev/null; then
changequote([,])dnl
	      cc_broken_visibility_attribute=no
	    fi
	  fi
	  rm -f conftest.c conftest.s
	 ])
    if test x$cc_broken_visibility_attribute = xyes; then
      AC_MSG_WARN(no working compiler support for visibility attribute available)
    fi

dnl Step 4 - check if we can control the default visibility of symbols
	AX_CXX_CHECK_FLAG([-fvisibility=hidden], [[int foo;]], [[foo = 1;]], cxx_does_vis="yes", cxx_does_vis="no")
	if test "x$cxx_does_vis" = "xyes" ; then
	  dnl can use default visibility flag on the C++ compiler
      CXXFLAGS="${CXXFLAGS} -fvisibility=hidden"
	fi

dnl Step 5 - Do we have all the right things to turn visibility stuff on or not?
	if test "x$as_protected_directive" = "xyes" ; then
      if test "x$cc_visibility_attribute" = "xyes" ; then
	    if test "x$cc_broken_visibility_attribute" = "xno" ; then
		  if test "x$cxx_does_vis" = "xyes" ; then
			dnl Phew - it actually works!
            AC_DEFINE(CC_HASVISIBILITY, 1,
          [Define if the compiler supports the GCC symbol visibility functions])
		  fi
		fi
      fi
	fi

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
   export PKG_CONFIG_PATH="./lib-src/lib-widget-extra/:$PKG_CONFIG_PATH"
   PKG_CHECK_MODULES(WIDGETEXTRA, libwidgetextra,
                     widgetextra_available="yes",
                     widgetextra_available="no")

   if test "x$widgetextra_available" != "xyes" ; then
      AC_MSG_ERROR([lib-widget-extra is required to build audacity. A copy is included in the audacity source distribution at lib-src/lib-widget-extra/.])
   fi
   dnl otherwise good - got it. Flags will be available for use in
   dnl WIDGETEXTRA_LIBS and friends
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
         FFMPEG_SYSTEM_LIBS="$AVCODEC_LIBS $AVFORMAT_LIBS"
         FFMPEG_SYSTEM_CXXFLAGS="$AVCODEC_CFLAGS $AVFORMAT_CFLAGS"
         FFMPEG_SYSTEM_CPPSYMBOLS="USE_FFMPEG"
		 dnl build the extra object files needed to use FFmpeg. Paths inside
		 dnl the audacity src/ dir, as this is subsitiuted into src/Makefile.in
		 FFMPEG_SYSTEM_OPTOBJS="FFmpeg.o import/ImportFFmpeg.o"
         AC_MSG_NOTICE([FFmpeg library available as system library])
      fi
   fi
   if test "x$FFMPEG_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([FFmpeg library NOT available as system library])
   fi

   dnl see if ffmpeg is available locally. Right now it isn't.

   FFMPEG_LOCAL_AVAILABLE="no"
   AC_MSG_NOTICE([ffmpeg library is NOT available in the local tree])
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

   dnl see if liblrdf is available locally. Right now it isn't.

   LIBLRDF_LOCAL_AVAILABLE="no"
   AC_MSG_NOTICE([liblrdf is NOT available in the local tree])
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


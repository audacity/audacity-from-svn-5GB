# Copyright (C) 2002,2003 Erik de Castro Lopo (erikd AT mega-nerd DOT com).

dnl Require autoconf version >= 2.54)
AC_PREREQ(2.54)

AC_INIT
AC_CONFIG_SRCDIR([src/samplerate.c])
AC_CANONICAL_TARGET([])
AM_INIT_AUTOMAKE(libsamplerate,0.0.14)
AM_CONFIG_HEADER(src/config.h)

#------------------------------------------------------------------------------------
# This is the version info according to the libtool versioning system.

SHARED_VERSION_INFO="0:14:0"

AC_PROG_CC
AC_PROG_INSTALL

AC_PROG_MAKE_SET
AC_PROG_LN_S
AM_PROG_LIBTOOL

AC_HEADER_STDC

AC_CHECK_HEADERS(stdint.h)

#====================================================================================
# Couple of initializations here. Fill in real values later.

SHLIB_VERSION_ARG=""

#====================================================================================
# Finished checking, handle options.

AC_ARG_ENABLE(gcc-pipe, 
		[  --disable-gcc-pipe           disable gcc -pipe option ],
		ac_arg_gcc_pipe="N", ac_arg_gcc_pipe="Y")

AC_ARG_ENABLE(gcc-opt,  
		[  --disable-gcc-opt            disable gcc optimisations ],
		ac_arg_gcc_opt="N", ac_arg_gcc_opt="Y")

AC_ARG_ENABLE(debug,  
		[  --enable-debug               enable debugging ],
		ac_arg_debug=yes, ac_arg_debug=no)

if test x$ac_arg_debug = xyes ; then
	AC_DEFINE([ENABLE_DEBUG],1,[Set to 1 to enable debugging.])
else
	AC_DEFINE([ENABLE_DEBUG],0,[Set to 1 to enable debugging.])
	fi

if test $ac_cv_c_compiler_gnu = yes ; then
	CFLAGS="$CFLAGS -W -Wstrict-prototypes -Wmissing-prototypes -Wall -pedantic -Waggregate-return -Wcast-align -Wcast-qual -Wnested-externs -Wshadow"
	# -Wpointer-arith -Wundef -Wbad-function-cast -Wmissing-declarations -Wconversion -Winline"
	if test "$ac_arg_gcc_opt" = "N" ; then
		temp_CFLAGS=`echo $CFLAGS | sed "s/O2/O0/"`
		CFLAGS=$temp_CFLAGS
		AC_MSG_WARN([[*** Compiler optimisations switched off. ***]])
		fi

	# Disable -Wall and -pedantic for Apple Darwin/Rhapsody.
	# System headers on these systems are broken.
	case "$target_os" in
		darwin* | rhapsody*)
			temp_CFLAGS=`echo $CFLAGS | sed "s/-Wall -pedantic//"`
			CFLAGS=$temp_CFLAGS
			;;
		linux*)
			SHLIB_VERSION_ARG="-Wl,--version-script=Version_script"
			;;
		*)
			;;
		esac
	if test $ac_arg_gcc_pipe != "N" ; then
	 	CFLAGS="$CFLAGS -pipe"
	 	fi

	AC_DEFINE([COMPILER_IS_GCC],1, [Set to 1 if the compile is GNU GCC.])
	GCC_MAJOR_VERSION=`$CC -dumpversion | sed "s/\..*//"`
	AC_DEFINE_UNQUOTED([GCC_MAJOR_VERSION],${GCC_MAJOR_VERSION}, [Major version of GCC or 3 otherwise.])
	fi

#====================================================================================
# Check types and their sizes.

AC_CHECK_SIZEOF(int,0)
AC_CHECK_SIZEOF(long,0)
AC_CHECK_SIZEOF(float,4)
AC_CHECK_SIZEOF(double,8)

#====================================================================================
# Want to find libefence if it exists, but only want to link it to the test
# programs. Therefore, save $LIBS, check for libefence and then reset $LIBS
# to the previously saved value.

ac_cv_lib_efence_memalign=no
EFENCE_LIB=""

if test x$ac_arg_debug = xyes ; then
	saved_LIBS=$LIBS
	AC_CHECK_LIB([efence],memalign)
	LIBS=$saved_LIBS

	if test x$ac_cv_lib_efence_memalign = xyes ; then
		EFENCE_LIB="-lefence"
		fi
	fi

#====================================================================================
# Check for functions.

AC_CHECK_FUNCS(malloc calloc free memcpy memmove)

AC_CHECK_LIB([m],floor)
AC_CHECK_FUNCS(floor ceil fmod)

case "$target_os" in
	darwin* | rhapsody*)
		AC_MSG_WARN([[Not using native lrint() and lrintf() because they are missing on 10.1.]])
		;;
	*)
		AC_C99_FUNC_LRINT
		AC_C99_FUNC_LRINTF
		# AC_C99_FUNC_LLRINT  Don't need this (yet?).

		if test x$ac_cv_c99_lrint = xno || test x$ac_cv_c99_lrintf = xno ; then
	AC_MSG_WARN([[*** Missing C99 standard functions lrint() and lrintf().]])
	AC_MSG_WARN([[*** This may cause benign compiler warnings on some systems (ie Solaris).]])
	fi
		;;
	esac


#====================================================================================
# Check for libsndfile which is required for the test and example programs.

PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0.2, HAVE_SNDFILE=yes, HAVE_SNDFILE=no)

if test x$HAVE_SNDFILE = xyes ; then
	ac_cv_sndfile=1
else
	ac_cv_sndfile=0
	fi

AC_DEFINE_UNQUOTED([HAVE_SNDFILE],${ac_cv_sndfile},
			[Set to 1 if you have libsndfile.])
		
#====================================================================================
# Check for libfftw and librfftw which are required for the test programs.

# Save $LIBS for later.
src_save_LIBS=$LIBS

AC_CHECK_HEADER(fftw.h,
	AC_CHECK_LIB([fftw], fftw_create_plan, , AC_MSG_WARN([*** Could not find libfftw. Test programs cannot be run.])),
	AC_MSG_WARN([*** Header file fftw.h not found. Test programs cannot be run.])
	)

AC_CHECK_HEADER(rfftw.h,
	AC_CHECK_LIB([rfftw], rfftw_create_plan, , AC_MSG_WARN([*** Could not find librfftw. Test programs cannot be run.])),
	AC_MSG_WARN([*** Header file rfftw.h not found. Test programs cannot be run.])
	)

# The above checks add libfftw and librfftw to $LIBS, so remove then and
# create a separate FFTW_LIBS variable.

if test "x$src_save_LIBS" = x ; then
	FFTW_LIBS=$LIBS
else
	FFTW_LIBS=`echo $LIBS | sed "s/${src_save_LIBS}//"`
	fi

# Restore $LIBS
LIBS=$src_save_LIBS

HAVE_FFTW=0
ac_cv_have_fftw=no
if test x$ac_cv_lib_fftw_fftw_create_plan = xyes ; then
	if test x$ac_cv_lib_rfftw_rfftw_create_plan = xyes ; then
		HAVE_FFTW=1
		ac_cv_have_fftw=yes
		fi
	fi

#====================================================================================
# Find known target OS.

OS_SPECIFIC_INCLUDES=""

case "$target_os" in
	darwin* | rhapsody*)
		OS_SPECIFIC_INCLUDES="-fpascal-strings -I/Developer/Headers/FlatCarbon"
		OS_SPECIFIC_LINKS="-framework CoreAudio"
		;;
	*)
		OS_SPECIFIC_INCLUDES=""
		OS_SPECIFIC_LINKS=""
		;;
	esac

#====================================================================================
# Now use the information from the checking stage.

# Need to make examples directory before tests directory.
subdirs="src doc examples tests"

AC_DEFINE([OS_IS_WIN32],0, [Set to 1 if compiling for Win32])

AC_SUBST(subdirs)
AC_SUBST(SHLIB_VERSION_ARG)
AC_SUBST(SHARED_VERSION_INFO)
AC_SUBST(OS_SPECIFIC_INCLUDES)
AC_SUBST(OS_SPECIFIC_LINKS)

AC_SUBST(COMPILER_IS_GCC)
AC_SUBST(GCC_MAJOR_VERSION)
AC_SUBST(ENABLE_DEBUG)

AC_SUBST(HAVE_FFTW)
AC_SUBST(FFTW_LIBS)
AC_SUBST(EFENCE_LIB)

AC_SUBST(SNDFILE_CFLAGS)
AC_SUBST(SNDFILE_LIBS)

AC_CONFIG_FILES([Makefile src/Version_script Win32/Makefile \
			src/Makefile examples/Makefile tests/Makefile doc/Makefile \
			libsamplerate.spec samplerate.pc])
AC_OUTPUT

#====================================================================================

AC_MSG_RESULT([
-=-=-=-=-=-=-=-=-=-= Configuration Complete =-=-=-=-=-=-=-=-=-=-=-

  Configuration summary :

    Version : ..................... ${VERSION}])
AC_MSG_RESULT([    Enable debugging : ............ ${ac_arg_debug}
])


if test x$ac_cv_c_compiler_gnu = xyes ; then
	echo -e "  Tools :\n"
	echo "    Compiler is GCC : ............. ${ac_cv_c_compiler_gnu}"
	echo "    GCC major version : ........... ${GCC_MAJOR_VERSION}"
	fi

AC_MSG_RESULT([
  Extra tools required for testing and examples :

    Have FFTW : ................... ${ac_cv_have_fftw}])

if test x$ac_cv_have_fftw = xno ; then
	echo
	echo "      * FFTW is available from http://www.fftw.org/ or if you "
	echo "      * are using Debian : \"apt-get install fftw2 fftw-dev\""
	echo
	fi

AC_MSG_RESULT([    Have libsndfile : ............. ${HAVE_SNDFILE}])
AC_MSG_RESULT([    Have libefence : .............. ${ac_cv_lib_efence_memalign}
])

AC_MSG_RESULT([  Installation directories :

    Library directory : ........... ${prefix}/lib
    Program directory : ........... ${prefix}/bin
    Pkgconfig directory : ......... ${prefix}/lib/pkgconfig
])

if test x$prefix != "x/usr" ; then
	echo "Compiling some other packages against ${PACKAGE} may require "
	echo -e "the addition of \"${prefix}/lib/pkgconfig\" to the "
	echo -e "PKG_CONFIG_PATH environment variable.\n"
	fi

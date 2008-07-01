# Process this file with autoconf to produce a configure script.
AC_INIT(src/lrdf.c)
AM_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE(liblrdf, 0.4.0)

LRDF_LIBTOOL_VERSION=2:0:0

# Checks for programs.
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_DISABLE_SHARED
AM_PROG_LIBTOOL

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([errno.h limits.h stdlib.h string.h unistd.h])

. ../../audacity_local_libraries.sh
if test x$LIBRAPTOR_USE_LOCAL = x; then
  PKG_CHECK_MODULES(RAPTOR, raptor >= 0.9.11)
else
  RAPTOR_LIBS=
  RAPTOR_CFLAGS=`echo $LIBRAPTOR_LOCAL_CXXFLAGS | sed s@-I@-I../@`
  AC_SUBST(RAPTOR_LIBS)
  AC_SUBST(RAPTOR_CFLAGS)
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

# Checks for library functions.
dnl AC_FUNC_MALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([getcwd strcasecmp strchr strdup strerror strncasecmp strrchr])

LIBS="$LIBS $RAPTOR_LIBS"
AM_CFLAGS="$CFLAGS $RAPTOR_CFLAGS"

AC_SUBST(AM_CFLAGS)
AC_SUBST(LIBS)
AC_SUBST(LRDF_LIBTOOL_VERSION)

AC_OUTPUT([lrdf.pc Makefile src/Makefile examples/Makefile])

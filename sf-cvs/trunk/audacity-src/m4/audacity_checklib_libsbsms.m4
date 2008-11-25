AC_DEFUN([AUDACITY_CHECKLIB_LIBSBSMS], [
   AC_ARG_WITH(sbsms,
               [AS_HELP_STRING([--with-sbsms],
                      [use libsbsms for pitch and tempo changing])],
               LIBSBSMS_ARGUMENT=$withval,
               LIBSBSMS_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_SBSMS, 1,
                [Define if SBSMS support should be enabled])
   fi

   dnl see if sbsms is installed on the system

   PKG_CHECK_MODULES(SBSMS, sbsms >= 1.6.0,
                     sbsms_available_system="yes",
                     sbsms_available_system="no")


   if test "x$sbsms_available_system" = "xyes" ; then
      LIBSBSMS_SYSTEM_AVAILABLE="yes"
      LIBSBSMS_SYSTEM_LIBS=$SBSMS_LIBS
      LIBSBSMS_SYSTEM_CXXFLAGS=$SBSMS_CFLAGS
      LIBSBSMS_SYSTEM_CPPSYMBOLS="USE_SBSMS"
      AC_MSG_NOTICE([Libsbsms libraries are available as system libraries])
   else
      LIBSBSMS_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsbsms libraries are NOT available as system libraries])
   fi

   dnl see if libsbsms is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/sbsms/include/sbsms.h,
                 sbsms_h_found="yes",
                 sbsms_h_found="no")

   if test "x$sbsms_h_found" = "xyes" ; then
      LIBSBSMS_LOCAL_AVAILABLE="yes"
      LIBSBSMS_LOCAL_LIBS="libsbsms.a"
      LIBSBSMS_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/sbsms/include'
      LIBSBSMS_LOCAL_CPPSYMBOLS="USE_SBSMS"

      if test ! -f lib-src/sbsms/Makefile ; then
         LIBSBSMS_LOCAL_CONFIG_SUBDIRS="lib-src/sbsms"
      fi
      AC_MSG_NOTICE([libsbsms libraries are available in the local tree])
   else
      LIBSBSMS_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsbsms libraries are NOT available in the local tree])
   fi

])


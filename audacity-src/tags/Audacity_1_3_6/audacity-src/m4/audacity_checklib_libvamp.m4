dnl Todo: add Vamp / Audacity license?

dnl Check for system copy of libvamp we can use for Vamp plug-in support
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


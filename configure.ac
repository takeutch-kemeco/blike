#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([c_blike], [1.5.3], [takeutchkemeco@gmail.com])

AC_CONFIG_SRCDIR([src/common/main.c])
AC_CONFIG_HEADERS([include/config.h])
AC_CONFIG_MACRO_DIR([m4])
LT_INIT
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CC
AC_PROG_MAKE_SET

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE

# Checks for library functions.
AC_FUNC_MALLOC

AC_CONFIG_FILES([Makefile
                 src/common/Makefile
		 src/drv/drvgtk/Makefile
		 blike.pc])
AC_OUTPUT


AC_PREREQ([2.69])
AC_INIT([hs-bindgen-libclang],[0.1.0])
AC_CONFIG_MACRO_DIRS([m4])

AC_PROG_CC

dnl The discovery logic is following
dnl * look for llvm-config (LLVM_CONFIG if set, in PATH otherwise)
dnl * if not found
dnl   - if LLVM_PATH is set, use it (skip version check)
dnl   - otherwise fail.

AC_ARG_VAR(LLVM_CONFIG, [Location of llvm-config])
AC_ARG_VAR(LLVM_PATH, [Location of LLVM installation])

AC_PATH_PROG([LLVM_CONFIG],[llvm-config],[])
if test -z "$LLVM_CONFIG"; then
  if test -z "$LLVM_PATH"; then
    AC_MSG_ERROR([could not find llvm-config])
  else
    AC_MSG_NOTICE([could not find llvm-config; using LLVM_PATH=$LLVM_PATH])
  fi
fi

dnl This also checks that we can run LLVM_CONFIG
AC_MSG_CHECKING([LLVM version])

if test -z "$LLVM_CONFIG"; then
  AC_MSG_RESULT([skipped])
else
LLVM_VERSION="$("$LLVM_CONFIG" --version)"
if test $? == 0; then
  AC_MSG_RESULT([$LLVM_VERSION])
  dnl TODO: Use https://www.gnu.org/software/autoconf-archive/ax_compare_version.html to compare version
else
  AC_MSG_RESULT([error])
  AC_MSG_ERROR([could not run llvm-config])
fi
fi

dnl LIBS/LIBDIR/INCLUDEDIR

HS_BINDGEN_EXTRA_LIBS="clang"

AC_MSG_CHECKING([LLVM library directory])
if test -z "$LLVM_CONFIG"; then
HS_BINDGEN_LIBDIR="$LLVM_PATH/lib"
else
HS_BINDGEN_LIBDIR="$("$LLVM_CONFIG" --libdir)";
fi
AC_MSG_RESULT($HS_BINDGEN_LIBDIR)

AC_MSG_CHECKING([LLVM include directory])
if test -z "$LLVM_CONFIG"; then
HS_BINDGEN_INCLUDEDIR="$LLVM_PATH/include"
else
HS_BINDGEN_INCLUDEDIR="$("$LLVM_CONFIG" --includedir)"
fi
AC_MSG_RESULT($HS_BINDGEN_INCLUDEDIR)

dnl Runtime checks

dnl Set some C related environment variables for further tests
LIBS="$LIBS -l$HS_BINDGEN_EXTRA_LIBS"
LDFLAGS="$LDFLAGS -L$(printf %q "$HS_BINDGEN_LIBDIR")"
CPPFLAGS="$CPPFLAGS -I$(printf %q "$HS_BINDGEN_INCLUDEDIR")"

AC_CHECK_HEADER([clang-c/Index.h],[],[AC_MSG_ERROR([Cannot find libclang headers])])
AC_CHECK_LIB([clang], [clang_createIndex],[],[AC_MSG_ERROR([Cannot link against libclang])])

dnl Substitution

AC_SUBST([HS_BINDGEN_EXTRA_LIBS])
AC_SUBST([HS_BINDGEN_LIBDIR])
AC_SUBST([HS_BINDGEN_INCLUDEDIR])

AC_CONFIG_FILES([hs-bindgen-libclang.buildinfo])

AC_OUTPUT

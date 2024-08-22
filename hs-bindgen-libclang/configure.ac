AC_PREREQ([2.69])
AC_INIT([hs-bindgen-libclang],[0.1.0])
AC_CONFIG_MACRO_DIRS([m4])

AC_PROG_CC
AC_PATH_PROG([LLVM_CONFIG],[llvm-config],[])
if test ! -x "$LLVM_CONFIG"; then
  AC_MSG_ERROR([Could not find llvm-config])
fi

# TODO: Use https://www.gnu.org/software/autoconf-archive/ax_compare_version.html
# to compare version

HS_BINDGEN_EXTRA_LIBS="clang"

AC_MSG_CHECKING([LLVM library directory])
HS_BINDGEN_LIBDIR=$(llvm-config --libdir)
AC_MSG_RESULT($HS_BINDGEN_LIBDIR)

AC_MSG_CHECKING([LLVM include directory])
HS_BINDGEN_INCLUDEDIR=$(llvm-config --includedir)
AC_MSG_RESULT($HS_BINDGEN_INCLUDEDIR)

LIBS="$LIBS -l$HS_BINDGEN_EXTRA_LIBS"
LDFLAGS="$LDFLAGS -L$HS_BINDGEN_LIBDIR"
CPPFLAGS="$CPPFLAGS -I$HS_BINDGEN_INCLUDEDIR"

AC_CHECK_HEADER([clang-c/Index.h],[],[AC_MSG_ERROR([Cannot find libclang headers])])
AC_CHECK_LIB([clang], [clang_createIndex],[],[AC_MSG_ERROR([Cannot link against libclang])])

AC_SUBST([HS_BINDGEN_EXTRA_LIBS])
AC_SUBST([HS_BINDGEN_LIBDIR])
AC_SUBST([HS_BINDGEN_INCLUDEDIR])

AC_CONFIG_FILES([hs-bindgen-libclang.buildinfo])

AC_OUTPUT

// This test declares typedefs around stdlib types.  The resulting Haskell
// module derives the instances that are configured for each stdlib type, and
// compiling the module tests that all of those instances exist.

#include <fenv.h>
#include <inttypes.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <uchar.h>
#include <wchar.h>
#include <wctype.h>

// bool types
typedef bool stdlib_CBool;

// integral types
typedef int8_t    stdlib_Int8;
typedef int16_t   stdlib_Int16;
typedef int32_t   stdlib_Int32;
typedef int64_t   stdlib_Int64;
typedef uint8_t   stdlib_Word8;
typedef uint16_t  stdlib_Word16;
typedef uint32_t  stdlib_Word32;
typedef uint64_t  stdlib_Word64;
typedef intmax_t  stdlib_CIntMax;
typedef uintmax_t stdlib_CUIntMax;
typedef intptr_t  stdlib_CIntPtr;
typedef uintptr_t stdlib_CUIntPtr;

// floating types
typedef fenv_t    stdlib_CFenvT;
typedef fexcept_t stdlib_CFexceptT;

// math types
typedef div_t   stdlib_CDivT;
typedef ldiv_t  stdlib_CLdivT;
typedef lldiv_t stdlib_CLldivT;
typedef imaxdiv_t stdlib_CImaxdivT;

// standard types
typedef size_t    stdlib_CSize;
typedef ptrdiff_t stdlib_CPtrdiff;

// non-local jump types
typedef jmp_buf stdlib_CJmpBuf;

// wchar types
typedef wchar_t   stdlib_CWchar;
typedef wint_t    stdlib_CWintT;
typedef mbstate_t stdlib_CMbstateT;
typedef wctrans_t stdlib_CWctransT;
typedef wctype_t  stdlib_CWctypeT;
typedef char16_t  stdlib_CChar16T;
typedef char32_t  stdlib_CChar32T;

// time types
typedef time_t    stdlib_CTime;
typedef clock_t   stdlib_CClock;
typedef struct tm stdlib_CTm;

// file types
typedef FILE   stdlib_CFile;
typedef fpos_t stdlib_CFpos;

// signal types
typedef sig_atomic_t stdlib_CSigAtomic;

// This test declares typedefs around stdlib types.  The resulting Haskell
// module derives the instances that are configured for each stdlib type, and
// compiling the module tests that all of those instances exist.

#include <fenv.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <uchar.h>
#include <wchar.h>
#include <wctype.h>

// Boolean types
typedef bool stdlib_CBool;

// Integral types
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

// Floating types
typedef fenv_t    stdlib_CFenvT;
typedef fexcept_t stdlib_CFexceptT;

// Standard types
typedef size_t    stdlib_CSize;
typedef ptrdiff_t stdlib_CPtrdiff;

// Non-local jump types
typedef jmp_buf stdlib_CJmpBuf;

// Wide character types
typedef wchar_t   stdlib_CWchar;
typedef wint_t    stdlib_CWintT;
typedef mbstate_t stdlib_CMbstateT;
typedef wctrans_t stdlib_CWctransT;
typedef wctype_t  stdlib_CWctypeT;
typedef char16_t  stdlib_CChar16T;
typedef char32_t  stdlib_CChar32T;

// Time types
typedef time_t    stdlib_CTime;
typedef clock_t   stdlib_CClock;
typedef struct tm stdlib_CTm;

// File types
typedef FILE   stdlib_CFile;
typedef fpos_t stdlib_CFpos;

// Signal types
typedef sig_atomic_t stdlib_CSigAtomic;

// This test declares typedefs around types that are translated to HsPrimType
// primitive types.  The resulting Haskell module derives the instances that are
// configured for each primitive type, and compiling the module tests that all
// of those instances exist.

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Nothing translates to HsPrimVoid
// Nothing translates to HsPrimUnit
// Nothing translates to HsPrimCStringLen
typedef ptrdiff_t          prim_HsPrimCPtrdiff;
// Nothing translates to HsPrimChar
// Nothing translates to HsPrimInt
// Nothing translates to HsPrimDouble
// Nothing translates to HsPrimFloat
// Nothing translates to HsPrimBool
typedef int8_t             prim_HsPrimInt8;
typedef int16_t            prim_HsPrimInt16;
typedef int32_t            prim_HsPrimInt32;
typedef int64_t            prim_HsPrimInt64;
// Nothing translates to HsPrimWord
typedef uint8_t            prim_HsPrimWord8;
typedef uint16_t           prim_HsPrimWord16;
typedef uint32_t           prim_HsPrimWord32;
typedef uint64_t           prim_HsPrimWord64;
typedef char               prim_HsPrimCChar;
typedef signed char        prim_HsPrimCSChar;
typedef unsigned char      prim_HsPrimCUChar;
typedef short              prim_HsPrimCShort;
typedef unsigned short     prim_HsPrimCUShort;
typedef int                prim_HsPrimCInt;
typedef unsigned int       prim_HsPrimCUInt;
typedef long               prim_HsPrimCLong;
typedef unsigned long      prim_HsPrimCULong;
typedef long long          prim_HsPrimCLLong;
typedef unsigned long long prim_HsPrimCULLong;
typedef bool               prim_HsPrimCBool;
typedef float              prim_HsPrimCFloat;
typedef double             prim_HsPrimCDouble;

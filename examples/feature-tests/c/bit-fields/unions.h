/*******************************************************************************
 * Bit-field tests for unions
 *
 * This header defines various unions with bit-fields as well as static
 * functions used to test them.
 *
 * Comments concerning alignment of fields in packed unions are valid for
 * objects at 64-bit aligned addresses, but note that hs-bindgen is able to
 * read/write bit-fields using only aligned peeks/pokes even when an object is
 * not aligned.
 ******************************************************************************/

#pragma once

#include <stdbool.h>

/*******************************************************************************
 * not packed, <=8-bit fields
 ******************************************************************************/

union U_8 {
  signed char a : 3;
  signed char b : 3;
  signed char c : 2;
  signed char d : 3;
  signed char e : 8;
  signed char f : 5;
};

#define MkSetFunName_U_8(fieldName) set_U_8_ ## fieldName
#define MkSetFun_U_8(fieldName, fieldType)\
  static inline void MkSetFunName_U_8(fieldName) (\
        union U_8 *x\
      , fieldType fieldValue) {\
    x->fieldName = fieldValue;\
  };

MkSetFun_U_8(a, signed char)
MkSetFun_U_8(b, signed char)
MkSetFun_U_8(c, signed char)
MkSetFun_U_8(d, signed char)
MkSetFun_U_8(e, signed char)
MkSetFun_U_8(f, signed char)

#define MkGetFunName_U_8(fieldName) get_U_8_ ## fieldName
#define MkGetFun_U_8(fieldName, fieldType)\
  static inline fieldType MkGetFunName_U_8(fieldName) (\
        union U_8 *x) {\
    return x->fieldName;\
  };

MkGetFun_U_8(a, signed char)
MkGetFun_U_8(b, signed char)
MkGetFun_U_8(c, signed char)
MkGetFun_U_8(d, signed char)
MkGetFun_U_8(e, signed char)
MkGetFun_U_8(f, signed char)


#define MkEqFunName_U_8(fieldName) eq_U_8_ ## fieldName
#define MkEqFun_U_8(fieldName, fieldType)\
  static inline bool MkEqFunName_U_8(fieldName) (\
        union U_8 *x\
      , fieldType fieldValue) {\
    return x->fieldName == fieldValue;\
  };

MkEqFun_U_8(a, signed char)
MkEqFun_U_8(b, signed char)
MkEqFun_U_8(c, signed char)
MkEqFun_U_8(d, signed char)
MkEqFun_U_8(e, signed char)
MkEqFun_U_8(f, signed char)

/*******************************************************************************
 * not packed, <=16-bit fields
 ******************************************************************************/

union U_16 {
  signed char a :  6;
  signed int  b : 10;
  signed int  c : 16;
  signed int  d : 16;
  signed int  e : 12;
  signed int  f : 12;
};

#define MkSetFunName_U_16(fieldName) set_U_16_ ## fieldName
#define MkSetFun_U_16(fieldName, fieldType)\
  static inline void MkSetFunName_U_16(fieldName) (\
        union U_16 *x\
      , fieldType fieldValue) {\
    x->fieldName = fieldValue;\
  };

MkSetFun_U_16(a, signed char)
MkSetFun_U_16(b, signed int)
MkSetFun_U_16(c, signed int)
MkSetFun_U_16(d, signed int)
MkSetFun_U_16(e, signed int)
MkSetFun_U_16(f, signed int)

#define MkGetFunName_U_16(fieldName) get_U_16_ ## fieldName
#define MkGetFun_U_16(fieldName, fieldType)\
  static inline fieldType MkGetFunName_U_16(fieldName) (\
        union U_16 *x) {\
    return x->fieldName;\
  };

MkGetFun_U_16(a, signed char)
MkGetFun_U_16(b, signed int)
MkGetFun_U_16(c, signed int)
MkGetFun_U_16(d, signed int)
MkGetFun_U_16(e, signed int)
MkGetFun_U_16(f, signed int)

#define MkEqFunName_U_16(fieldName) eq_U_16_ ## fieldName
#define MkEqFun_U_16(fieldName, fieldType)\
  static inline bool MkEqFunName_U_16(fieldName) (\
        union U_16 *x\
      , fieldType fieldValue) {\
    return x->fieldName == fieldValue;\
  };

MkEqFun_U_16(a, signed char)
MkEqFun_U_16(b, signed int)
MkEqFun_U_16(c, signed int)
MkEqFun_U_16(d, signed int)
MkEqFun_U_16(e, signed int)
MkEqFun_U_16(f, signed int)

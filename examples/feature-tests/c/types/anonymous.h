#pragma once

#include <stddef.h>

/* -------------------------------------------------------------------------- */
/* anonymous structs/unions, implicit fields */

// The C code here is used in the Test.Types.Anonymous Haskell module to verify
// that implicit fields are generated correctly for nested anonymous structs and
// unions.

/* -------------------------------------------------------------------------- */
/* macro */

// constructs a non-nested type for testing purposes, where:
//
// * t is the type tag (struct or union)
// * n is the name of the type

#define InnerType(t, n)\
  t n {\
    int : 17;\
    int fieldX;\
    int fieldY;\
  }\

// constructs a nested type for testing purposes, where:
//
// * t1 is the outer type tag (struct or union)
// * n1 is the name of the outer type
// * t2 is the inner type tag (struct or union)
// * n2 is the name of the inner type
// * f2 is the field name for the inner type
//
// When n2 and f2 are empty, then the inner type is an anonymous struct or union
//
// The inner type is constructed using the InnerType macro

#define OuterType(t1, n1, t2, n2, f2)\
  typedef t1 n1 {\
    char fieldA;\
    InnerType(t2, n2) f2;\
    int fieldC;\
  } n1;\

/* -------------------------------------------------------------------------- */
/* inner type */

typedef InnerType(struct, S) S;
typedef InnerType(union, U) U;

/* -------------------------------------------------------------------------- */
/* struct in struct */

OuterType(struct, SS1, struct, ,       ); // anonymous nested object
OuterType(struct, SS2, struct, , fieldB); // non-anonymous nested object

// The former should be equal to the latter
static const size_t offset_SS1_fieldX = offsetof(SS1, fieldX) - offsetof(S, fieldX);
static const size_t offset_SS2_fieldB = offsetof(SS2, fieldB);

/* -------------------------------------------------------------------------- */
/* union in struct */


OuterType(struct, SU1, union, ,       ); // anonymous nested object
OuterType(struct, SU2, union, , fieldB); // non-anonymous nested object

// The former should be equal to the latter
static const size_t offset_SU1_fieldX = offsetof(SU1, fieldX) - offsetof(U, fieldX);
static const size_t offset_SU2_fieldB = offsetof(SU2, fieldB);

/* -------------------------------------------------------------------------- */
/* struct in union */

OuterType(union, US1, struct, ,       ); // anonymous nested object
OuterType(union, US2, struct, , fieldB); // non-anonymous nested object

// The former should be equal to the latter
static const size_t offset_US1_fieldX = offsetof(US1, fieldX) - offsetof(S, fieldX);
static const size_t offset_US2_fieldB = offsetof(US2, fieldB);

/* -------------------------------------------------------------------------- */
/* union in union */

OuterType(union, UU1, union, ,       ); // anonymous nested object
OuterType(union, UU2, union, , fieldB); // non-anonymous nested object

// The former should be equal to the latter
static const size_t offset_UU1_fieldX = offsetof(UU1, fieldX) - offsetof(U, fieldX);
static const size_t offset_UU2_fieldB = offsetof(UU2, fieldB);

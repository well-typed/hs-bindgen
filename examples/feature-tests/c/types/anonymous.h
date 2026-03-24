#pragma once

#include <stddef.h>

/* -------------------------------------------------------------------------- */
/* anonymous structs/unions, implicit fields */

// The C code here is used in the Test.Types.Anonymous Haskell module to verify
// that implicit fields are generated correctly for nested anonymous structs and
// unions.

/* -------------------------------------------------------------------------- */
/* macro */

// constructs a nested type for testing purposes, where:
//
// * t1 is the outer type tag (struct or union)
// * n1 is the name of the outer type
// * t2 is the inner type tag (struct or union)
// * f2 is the field name for the inner type
//
// When f2 is empty, then the inner type is an anonymous struct or union

#define ExampleType(t1, n1, t2, f2)\
  typedef t1 n1 {\
    char fieldA;\
    t2 {\
      int fieldX;\
      int fieldY;\
    } f2;\
    int fieldC;\
  } n1;\

/* -------------------------------------------------------------------------- */
/* struct in struct */

ExampleType(struct, SS1, struct, );       // anonymous nested object
ExampleType(struct, SS2, struct, fieldB); // non-anonymous nested object

// The former should be equal to the latter
static const size_t offset_SS1_fieldX = offsetof(SS1, fieldX);
static const size_t offset_SS2_fieldB = offsetof(SS2, fieldB);

/* -------------------------------------------------------------------------- */
/* union in struct */

ExampleType(struct, SU1, union, );       // anonymous nested object
ExampleType(struct, SU2, union, fieldB); // non-anonymous nested object

// The former should be equal to the latter
static const size_t offset_SU1_fieldX = offsetof(SU1, fieldX);
static const size_t offset_SU2_fieldB = offsetof(SU2, fieldB);

/* -------------------------------------------------------------------------- */
/* struct in union */

ExampleType(union, US1, struct, );       // anonymous nested object
ExampleType(union, US2, struct, fieldB); // non-anonymous nested object

// The former should be equal to the latter
static const size_t offset_US1_fieldX = offsetof(US1, fieldX);
static const size_t offset_US2_fieldB = offsetof(US2, fieldB);

/* -------------------------------------------------------------------------- */
/* union in union */

ExampleType(union, UU1, union, );       // anonymous nested object
ExampleType(union, UU2, union, fieldB); // non-anonymous nested object

// The former should be equal to the latter
static const size_t offset_UU1_fieldX = offsetof(UU1, fieldX);
static const size_t offset_UU2_fieldB = offsetof(UU2, fieldB);

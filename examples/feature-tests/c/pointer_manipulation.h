#pragma once

#include <stdint.h>

/* -------------------------------------------------------------------------- */
/* anonymous structs/unions, implicit fields */

// The C code here is uesd in the Test.PointerManipulation Haskell module to
// test the pointer manipulation API. The pointer manipulation API allows a
// nested value that is stored in a pointer to be manipulated without
// marshalling its enclosing data structure(s) to pure Haskell values and back.
// In particular we test that the pointer manipulation API is equivalent to the
// more typical approach of peek-manipulate-poke.

/* -------------------------------------------------------------------------- */
/* Structs */

struct MyStruct {
  int x;
  char y;
};

/* -------------------------------------------------------------------------- */
/* Unions */

union MyUnion {
  int x;
  char y;
};

/* -------------------------------------------------------------------------- */
/* Enums */

enum MyEnum {
  MyEnumerator
};

/* -------------------------------------------------------------------------- */
/* Bit-fields */

struct MyStructBF {
  unsigned int x : 7;
  unsigned char y : 6;
};

/**
 * The Haskell bindings for bit-fields (i.e., Haskell record fields) do not
 * perform packing in any way like the C code does. So if we want to create a
 * Haskell value of a MyStructBF, then we use this C function instead.
 */
static inline struct MyStructBF __attribute__((const)) make_MyStructBF(
    unsigned int x,
    unsigned char y) {
  struct MyStructBF s = {.x=x, .y=y};
  return s;
}

union MyUnionBF {
  unsigned int x : 3;
  unsigned char y : 5;
};

/**
 * The Haskell bindings for bit-fields (i.e., Haskell record fields) do not
 * perform packing in any way like the C code does. So if we want to create a
 * Haskell value of a MyUnionBF (x alternative), then we use this C function
 * instead.
 */
static inline union MyUnionBF __attribute__((const)) make_MyUnionBF_x(
  unsigned int x) {
  union MyUnionBF u = {.x=x};
  return u;
}

/**
 * The Haskell bindings for bit-fields (i.e., Haskell record fields) do not
 * perform packing in any way like the C code does. So if we want to create a
 * Haskell value of a MyUnionBF (y alternative), then we use this C function
 * instead.
 */
static inline union MyUnionBF __attribute__((const)) make_MyUnionBF_y(
  unsigned char y) {
  union MyUnionBF u = {.y=y};
  return u;
}

/* -------------------------------------------------------------------------- */
/* Typedefs */

typedef int MyTypedef;

/* -------------------------------------------------------------------------- */
/* Macro types */

#define MyMacroType int

/* -------------------------------------------------------------------------- */
/* Arrays */

typedef int MyArrayKnownSize[3];

typedef int MyArrayUnknownSize[];

/* -------------------------------------------------------------------------- */
/* FLAM */

struct MyStructFLAM {
  int len;
  char data[];
};

/* -------------------------------------------------------------------------- */
/* Anonymous structs/unions */

struct MyStructAnon {
  struct {
    int x;
    char y;
  };
};

union MyUnionAnon {
  union {
    int x;
    char y;
  };
};

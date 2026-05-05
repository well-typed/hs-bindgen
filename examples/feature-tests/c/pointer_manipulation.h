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
  unsigned int x : 6;
  unsigned char y : 6;
};

static inline struct MyStructBF __attribute__((const)) make_MyStructBF(
    unsigned int x,
    unsigned char y) {
  struct MyStructBF s = {x, y};
  return s;
}

union MyUnionBF {
  unsigned int x : 3;
  unsigned char y : 3;
};

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

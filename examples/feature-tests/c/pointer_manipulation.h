/*
 * Pointer manipulation API
 */

#pragma once

#include <stdint.h>

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
  int x : 3;
  char y : 3;
};

union MyUnionBF {
  int x : 3;
  char y : 3;
};

/* -------------------------------------------------------------------------- */
/* Typedefs */

typedef int MyTypedef;

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
#pragma once

/*
 * Unprefixed field names demonstration
 *
 * This example demonstrates the --unprefixed-field-names flag, which generates
 * Haskell records with unprefixed field names. This requires the
 * DuplicateRecordFields extension.
 */

/* Struct with common field names */
struct Point {
  int x;
  int y;
};

/* Struct with overlapping field names */
struct Size {
  int width;
  int height;
};

/* Struct that reuses field names from Point */
struct Rect {
  int x;
  int y;
  int width;
  int height;
};

/* Enum that reuses field names from Point */
enum E {
  x,
  y
};

/* Newtype example */
typedef int Value;

/* Union with common field names */
union U1 {
  int x;
  int y;
};

/* Typedef union */
typedef union U2 {
  char a;
  int b;
} U2_t;

/* Union containing struct with common field names */
union U3 {
  struct Point p;
  struct Size s;
};

/* Typedef for a function pointer */
struct Driver;
typedef int (*RunDriver)(struct Driver* self);

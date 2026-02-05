#pragma once

/*
 * Unprefixed field names demonstration
 *
 * This example demonstrates the --enable-record-dot flag, which generates
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

/* Typedef for a function pointer */
struct Driver;
typedef int (*RunDriver)(struct Driver* self);

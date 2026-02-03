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

/* Newtype example */
typedef int Value;

/*
 * Pointer manipulation API
 */

#pragma once

struct point {
  int x;
  int y;
};

struct rectangle {
  struct point topleft;
  struct point bottomright;
};

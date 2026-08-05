#pragma once

/* -------------------------------------------------------------------------- */
/* struct in struct */

struct outer1 {
  char fieldA;
  struct {
    int fieldX;
    int fieldY;
  }; // <-- an anonymous struct: an untagged struct with an unnamed field
  int fieldC;
};

struct outer2 {
  char fieldA;
  struct {
    int fieldX;
    int fieldY;
  } fieldB; // <-- an untagged struct with a named field
  int fieldC;
};

struct outer3 {
  char fieldA;
  struct inner3 {
    int fieldX;
    int fieldY;
  } fieldB; // <-- a named struct with a named field
  int fieldC;
};

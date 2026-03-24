#pragma once

/* -------------------------------------------------------------------------- */
/* struct in union */

union outer1 {
  char fieldA;
  struct {
    int fieldX;
    int fieldY;
  }; // <-- an anonymous struct: an unnamed struct with an unnamed field
  int fieldC;
};

union outer2 {
  char fieldA;
  struct {
    int fieldX;
    int fieldY;
  } fieldB; // <-- an unnamed struct with a named field
  int fieldC;
};

union outer3 {
  char fieldA;
  struct inner3 {
    int fieldX;
    int fieldY;
  } fieldB; // <-- a named struct with a named field
  int fieldC;
};

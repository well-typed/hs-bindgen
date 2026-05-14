#pragma once

/* -------------------------------------------------------------------------- */
/* union in union */

union outer1 {
  char fieldA;
  union {
    int fieldX;
    int fieldY;
  }; // <-- an anonymous union: an unnamed union with an unnamed field
  int fieldC;
};

union outer2 {
  char fieldA;
  union {
    int fieldX;
    int fieldY;
  } fieldB; // <-- an unnamed union with a named field
  int fieldC;
};

union outer3 {
  char fieldA;
  union inner3 {
    int fieldX;
    int fieldY;
  } fieldB; // <-- a named union with a named field
  int fieldC;
};

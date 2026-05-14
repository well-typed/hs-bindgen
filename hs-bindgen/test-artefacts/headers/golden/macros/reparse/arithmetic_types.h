// The goal of this test is to test that the reparse pass parses primitive type
// (arithmetic types in the C reference) correctly. The reparser is triggered by
// the use of the A macro in the functions below, which should produce function
// types for f1 up to and include f29 that went through the reparser.
//
// Previously, the reparser was producing the wrong types (see issue #1685) in
// some come cases. This test should guard against reintroducing that bug in the
// future.
//
// <https://github.com/well-typed/hs-bindgen/issues/1685>


/* -------------------------------------------------------------------------- */
/* Macro */

#define A int

/* -------------------------------------------------------------------------- */
/* Character types */

A f1(char x);
A f2(signed char x);
A f3(unsigned char x);

/* -------------------------------------------------------------------------- */
/* Integer types */

A f4(short x);
A f5(short int x);
A f6(signed short x);
A f7(signed short int x);
A f8(unsigned short x);
A f9(unsigned short int x);

A f10(int x);
A f11(signed x);
A f12(signed int x);
A f13(unsigned x);
A f14(unsigned int x);

A f15(long x);
A f16(long int x);
A f17(signed long x);
A f18(signed long int x);
A f19(unsigned long x);
A f20(unsigned long int x);

A f21(long long x);
A f22(long long int x);
A f23(signed long long x);
A f24(signed long long int x);
A f25(unsigned long long x);
A f26(unsigned long long int x);

/* -------------------------------------------------------------------------- */
/* Real floating types */

A f27(float x);
A f28(double x);
A f29(long double x);

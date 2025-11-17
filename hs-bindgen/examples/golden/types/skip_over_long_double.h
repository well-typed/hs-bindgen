/*
 * Test that we can skip over declarations with unsupported features.
 */

// unsupported type in function signature
void fun1(long double);
void fun2(int);

// unsupported type in struct field
struct struct1 {
  long double x;
};
struct struct2 {
  int x;
};

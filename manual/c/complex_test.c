#include "complex_test.h"

unsigned short  complex global_complex_unsigned_short = 1 + 2 * I;
         short  complex global_complex_short          = -1 + 3 * I;
unsigned int    complex global_complex_unsigned_int   = 5 + 7 * I;
         int    complex global_complex_int            = -5 + -7 * I;
         char   complex global_complex_char           = 1 + 1 * I;
         float  complex global_complex_float          = 3.14f + 2.71f * I;
         double complex global_complex_double         = 3.141592653589793 + 2.718281828459045 * I;

float  _Complex global_Complex_float  = 1.5f + 2.5f * I;
double _Complex global_Complex_double = 1.618033988749895 + 1.414213562373095 * I;

const float  complex const_complex_float  = 0.5f + 1.5f * I;
const double complex const_complex_double = 0.707106781186548 + 1.732050807568877 * I;

float  complex complex_float_array[10] = {
  1.0f + 1.0f * I, 2.0f + 2.0f * I, 3.0f + 3.0f * I, 4.0f + 4.0f * I, 5.0f  + 5.0f * I,
  6.0f + 6.0f * I, 7.0f + 7.0f * I, 8.0f + 8.0f * I, 9.0f + 9.0f * I, 10.0f + 10.0f * I
};

double complex complex_double_array[10] = {
  1.0 + 1.0 * I, 2.0 + 2.0 * I, 3.0 + 3.0 * I, 4.0 + 4.0 * I, 5.0  + 5.0  * I,
  6.0 + 6.0 * I, 7.0 + 7.0 * I, 8.0 + 8.0 * I, 9.0 + 9.0 * I, 10.0 + 10.0 * I
};

float complex multiply_complex_f(float complex a, float complex b) {
    return a * b;
}

double complex add_complex(double complex a, double complex b) {
    return a + b;
}

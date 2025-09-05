#include <complex.h>
#include "complex_test.h"

unsigned short  complex global_complex_unsigned_short = 0;
         short  complex global_complex_short          = 0;
unsigned int    complex global_complex_unsigned_int   = 0;
         int    complex global_complex_int            = 0;
         char   complex global_complex_char           = 0;

float  complex global_complex_float  = 1.0f + 2.0f * I;
double complex global_complex_double = 3.0 + 4.0 * I;

complex float  global_complex_float_flipped  = 5.0f + 6.0f * I;
complex double global_complex_double_flipped = 7.0 + 8.0 * I;

float  _Complex global_Complex_float  = 9.0f + 10.0f * I;
double _Complex global_Complex_double = 11.0 + 12.0 * I;

_Complex float  global_Complex_float_flipped  = 13.0f + 14.0f * I;
_Complex double global_Complex_double_flipped = 15.0 + 16.0 * I;

const float  complex const_complex_float  = 17.0f + 18.0f * I;
const double complex const_complex_double = 19.0 + 20.0 * I;

float  complex volatile_complex_float  = 21.0f + 22.0f * I;
double complex volatile_complex_double = 23.0 + 24.0 * I;

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

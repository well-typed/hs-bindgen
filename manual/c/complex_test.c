#include "complex_test.h"

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

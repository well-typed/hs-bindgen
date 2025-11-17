#include <complex.h>

extern float  complex global_complex_float;
extern double complex global_complex_double;

extern complex float  global_complex_float_flipped;
extern complex double global_complex_double_flipped;

extern float  _Complex global_Complex_float;
extern double _Complex global_Complex_double;

extern _Complex float  global_Complex_float_flipped;
extern _Complex double global_Complex_double_flipped;

extern const float  complex const_complex_float;
extern const double complex const_complex_double;

extern float  complex volatile_complex_float;
extern double complex volatile_complex_double;

float  complex multiply_complex_f(float complex a, float complex b);
double complex add_complex(double complex a, double complex b);

typedef struct {
    float  complex velocity;
    double complex position;
    int id;
} complex_object_t;

extern float  complex complex_float_array[10];
extern double complex complex_double_array[10];

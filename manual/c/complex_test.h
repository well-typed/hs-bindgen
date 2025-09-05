#include <complex.h>

extern unsigned short  complex global_complex_unsigned_short;
extern          short  complex global_complex_short;
extern unsigned int    complex global_complex_unsigned_int;
extern          int    complex global_complex_int;
extern          char   complex global_complex_char;
extern          float  complex global_complex_float;
extern          double complex global_complex_double;

extern float  _Complex global_Complex_float;
extern double _Complex global_Complex_double;

extern const float  complex const_complex_float;
extern const double complex const_complex_double;

float  complex multiply_complex_f(float complex a, float complex b);
double complex add_complex(double complex a, double complex b);

typedef struct {
    float  complex velocity;
    double complex position;
    int id;
} complex_object_t;

extern float  complex complex_float_array[10];
extern double complex complex_double_array[10];


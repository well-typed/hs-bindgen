double erf(double arg);

static inline double bad_fma(double x, double y, double z) {
    return (x * y) + z;
}
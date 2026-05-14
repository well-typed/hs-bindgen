double erf(double arg);

static inline double bad_fma(double x, double y, double z) {
    return (x * y) + z;
}

void no_args(void);

void no_args_no_void();

int fun(char x, double y);

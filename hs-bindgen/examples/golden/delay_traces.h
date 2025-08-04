// The following declarations are skipped when parsed, and, if selected, lead to
// warnings.
void long_double_function(long double);
void var_arg_function(const char *fmt, ...);

struct long_double_s {
  long double x;
  double y;
};

struct nested_long_double_s {
  struct {
    long double inner_int_member;
  } inner_s_member;
};

// In the tests, we do not select these. No traces should be emitted.
long double long_double_var1;
long double long_double_var2;
void long_double_fun(long double);

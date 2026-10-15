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

struct nested_long_double_u {
  union {
    long double inner_int_member;
  } inner_u_member;
};

// A struct defined inline in a variable or function declaration is parsed
// twice: at the top level, and nested in the variable or function. Its failure
// must be reported, not a conflict with itself.
struct inline_var_s { long double x; } inline_var;
struct inline_fun_s { long double x; } inline_fun(void);
struct { long double x; } inline_unnamed_var;
extern struct { long double x; } inline_unnamed_extern_var;

// In the tests, we do not select these. No traces should be emitted.
long double long_double_var1;
long double long_double_var2;
void long_double_fun(long double);

// See issue #1490

struct S {
  int x;
};

typedef struct S T;

T fun(T x);

/* We must mangle function parameter names */
void param_underscore(T _);
void param_uppercase(T Type);
void param_undersore_capital(T _T);
void param_haskell_reserved_name(T type);

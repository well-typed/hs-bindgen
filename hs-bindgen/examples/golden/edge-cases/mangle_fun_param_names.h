/* We must mangle function parameter names */

struct S {
  int x;
};

typedef struct S T;

void param_underscore(T _);
void param_uppercase(T Type);
void param_undersore_capital(T _T);
void param_haskell_reserved_name(T type);

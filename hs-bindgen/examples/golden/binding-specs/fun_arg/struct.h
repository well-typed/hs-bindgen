struct S { int x; };
typedef struct S A;
typedef A B;
void foo (struct S x);
void bar (A x);
void baz (B x);
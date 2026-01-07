enum E { x };
typedef enum E A;
typedef A B;
void foo (enum E x);
void bar (A x);
void baz (B x);
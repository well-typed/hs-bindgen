union U { int x; };
typedef union U A;
typedef A B;
void foo (union U x);
void bar (A x);
void baz (B x);
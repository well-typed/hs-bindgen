#define INCR(x) x + 1
#define ADD(x, y) x + y

#define ID(X) X
#define CONST(X,Y) X

#define CMP(X,Y) X < Y
#define FUN1(X,Y) X + 12ull * Y
#define FUN2(X,Y) X << ( 3ull * Y )

#define G(X,Y) CONST(INCR(Y),ID(X))

#define DIV1(X,Y) X / ( Y + 12u )
#define DIV2(X,Y) 10.0f * X / Y

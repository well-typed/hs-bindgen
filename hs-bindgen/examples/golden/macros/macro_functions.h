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


// Tests for #793
#define SWAP32(w) (((w)>>24)&0xff) | (((w)<<8)&0xff0000)
#define AV_VERSION_INT(a, b, c) ((a)<<16 | (b)<<8 | (c))

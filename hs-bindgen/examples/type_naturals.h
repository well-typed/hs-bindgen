// Test type-level naturals for array sizes


#define N 3
#define M 1 + N
#define F(A,B) A + 2 * B - 1
#define G(U,X,Y) 10 * X + 16 * Y
#define K G(11.77, F(F(2,M), N))

#define Arr1 int[N]
#define Arr2 int[2 * M]
#define Arr3 int[F(N, 2 * M)]
#define Arr4 int[G(11.77, F(F(2,M), N), 7)]

// #define Bad int[F(0,0)] // F(0,0) = 0 + 2 * 0 - 1 = -1

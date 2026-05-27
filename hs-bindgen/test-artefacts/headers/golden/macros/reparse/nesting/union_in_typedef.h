#define MyInt int

typedef union { MyInt x; }    T1;
typedef union { MyInt x; } *  T2;
typedef union { MyInt x; } ** T3;
